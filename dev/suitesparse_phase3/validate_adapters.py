"""Serial resource-bounded adapter probes with independent-process repeats."""
import argparse
from pathlib import Path
import sys
import time
sys.path.insert(0,str(Path(__file__).resolve().parents[1]/'suitesparse_phase1'))
import numpy as np
from common import atomic_json,read_json,sha256
from run_pilot import supervise
from adapters import METHODS

def validate(root,methods):
    root=Path(root);root.mkdir(parents=True,exist_ok=True)
    source=Path(__file__).resolve().parent
    code={p.name:sha256(p) for p in sorted(source.glob('*.py'))}
    results=[]
    for method in methods:
        trials=[]
        for label,seed in [('repeat_a',17),('repeat_b',17),('different_seed',29)]:
            dest=root/f'{method}_{label}'
            dest.mkdir()  # Never overwrite prior probes.
            print(method,label,'running',flush=True)
            timing=supervise([sys.executable,str(Path(__file__).with_name('probe.py')),
                              method,str(seed),str(dest)],dest)
            trials.append(dict(label=label,seed=seed,directory=str(dest),**timing))
            atomic_json(dest/'timing.json',timing)
            print(method,label,timing['status'],round(timing['elapsed_seconds'],2),flush=True)
            if timing['status']!='completed': break
        valid=len(trials)==3 and all(t['status']=='completed' for t in trials)
        detail={}
        if valid:
            z=[np.loadtxt(Path(t['directory'])/'coords.csv',delimiter=',') for t in trials]
            detail=dict(same_seed_max_abs=float(np.abs(z[0]-z[1]).max()),
                        different_seed_max_abs=float(np.abs(z[0]-z[2]).max()))
            valid=np.allclose(z[0],z[1],rtol=0,atol=1e-10) and not np.allclose(z[0],z[2])
        results.append(dict(method=method,status='validated' if valid else 'failed_contract',
                            trials=trials,**detail))
        if code!={p.name:sha256(p) for p in sorted(source.glob('*.py'))}:
            raise RuntimeError('source changed during validation; preserve trials and rerun frozen source')
        atomic_json(root/'contracts.json',dict(schema_version=1,code=code,results=results))
    if any(r['status']!='validated' for r in results): raise SystemExit(1)

if __name__=='__main__':
    p=argparse.ArgumentParser();p.add_argument('root');p.add_argument('--methods',nargs='+',choices=METHODS,default=METHODS)
    a=p.parse_args();validate(a.root,a.methods)
