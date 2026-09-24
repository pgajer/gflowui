"""Tiny, serial phase-02 demonstration; writes scientific evidence atomically."""
import argparse
import contextlib
import hashlib
import importlib.metadata
import io
import json
import os
from pathlib import Path
import platform
import resource
import subprocess
import sys
import time
import unittest

ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0,str(ROOT))
sys.path.insert(0,str(ROOT/'tests'))
import numpy as np
from lgs_reference import reference2d as ref, evaluator2d as ev
from lgs_reference.upstream import TinyGraph, load_functions, load_native
from test_reference import fixtures


def atomic_json(path, value):
    temporary = path.with_suffix(path.suffix+'.tmp')
    temporary.write_text(json.dumps(value,indent=2,allow_nan=False)+'\n')
    temporary.replace(path)


def sha(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--output',type=Path,required=True)
    args = parser.parse_args()
    args.output.mkdir(parents=True,exist_ok=True)
    started = time.perf_counter()
    output = io.StringIO()
    suite = unittest.defaultTestLoader.discover(str(ROOT/'tests'))
    result = unittest.TextTestRunner(stream=output,verbosity=2).run(suite)
    temp = args.output/'tests.txt.tmp'
    temp.write_text(output.getvalue())
    temp.replace(args.output/'tests.txt')
    if not result.wasSuccessful():
        print(output.getvalue(),file=sys.stderr)
        raise SystemExit(1)
    up,native = load_functions(),load_native()
    rows,checks = [],[]
    for name,a,x in fixtures():
        d = ev.shortest_paths(a)
        for k in sorted({1,len(a)-1}):
            w = up.find_neighbors(TinyGraph(a),k,5)
            steps = native.test_schedule(d,w,60)
            alpha = float(np.float32(0.6))
            diagnostic,g_diag = ev.upstream_diagnostic(x,d,w,alpha)
            fd = []
            for model in ('paper','code','upstream_diagnostic'):
                evaluator = ((lambda X: ev.upstream_diagnostic(X,d,w,alpha)) if model=='upstream_diagnostic'
                             else (lambda X,m=model: ev.evaluate(X,d,w,alpha,m)))
                _,gradient = evaluator(x)
                for h in (1e-4,1e-5,1e-6):
                    numeric = ev.finite_difference(lambda X: evaluator(X)[0],x,h)
                    fd.append({'model':model,'step':h,
                               'max_absolute_error':float(np.max(abs(numeric-gradient))),
                               'relative_l2_error':float(np.linalg.norm(numeric-gradient)/max(np.linalg.norm(gradient),1e-15))})
            checks.append({'graph':name,'locality_k':k,'gradient_checks':fd,
                           'diagnostic_value_abs_error':abs(diagnostic-up.get_cost(x,d,w,alpha))})
            for seed in (17,314,2026):
                orders = native.test_orders(d,w,60,seed)
                actual = native.test_run(d,w,x.ravel(),steps,alpha,seed)
                translated = ref.run(x,d,w,steps,orders,alpha)
                rows.append({'graph':name,'vertices':len(a),'locality_k':k,'locality_fraction':k/(len(a)-1),
                             'seed':seed,'walk_depth':5,'epochs':60,'repulsion_requested':0.6,
                             'repulsion_effective_float32':alpha,'termination':'fixed_epoch_budget',
                             'attractive_unordered_pairs':int(w.sum()//2),
                             'flags':w.tolist(),'coordinates':actual.tolist(),
                             'coordinate_max_abs_difference':float(np.max(abs(actual-translated))),
                             'objective_code_initial':ev.evaluate(x,d,w,alpha)[0],
                             'objective_code_final':ev.evaluate(actual,d,w,alpha)[0],
                             'objective_paper_initial':ev.evaluate(x,d,w,alpha,'paper')[0],
                             'objective_paper_final':ev.evaluate(actual,d,w,alpha,'paper')[0],
                             'diagnostic_upstream_initial':up.get_cost(x,d,w,alpha),
                             'diagnostic_upstream_final':up.get_cost(actual,d,w,alpha),
                             'mean_relative_stress_fitted':up.get_stress(actual,d),
                             'schedule':steps.tolist(),
                             'pair_orders_sha256':hashlib.sha256(json.dumps(orders).encode()).hexdigest(),
                             'first_epoch_pair_order':orders[0]})
    config = io.StringIO()
    with contextlib.redirect_stdout(config):
        np.show_config()
    environment = {'python':sys.version,'platform':platform.platform(),
                   'packages':{name:importlib.metadata.version(name) for name in ('numpy','Cython','setuptools','pip')},
                   'thread_settings':{name:os.environ.get(name) for name in ('OPENBLAS_NUM_THREADS','OMP_NUM_THREADS')},
                   'numpy_config':config.getvalue()}
    atomic_json(args.output/'environment.json',environment)
    rss = resource.getrusage(resource.RUSAGE_SELF).ru_maxrss
    peak_bytes = int(rss if sys.platform=='darwin' else 1024*rss)
    commit = subprocess.check_output(['git','rev-parse','HEAD'],cwd=ROOT,text=True).strip()
    dirty = subprocess.check_output(['git','status','--porcelain','--','research/lgs3d'],cwd=ROOT,text=True).strip()
    report = {'schema_version':1,'covered_phases':[1,2],
              'implementation_commit':commit,'worktree_changes_at_run':dirty,
              'upstream_commit':json.loads((ROOT/'upstream.json').read_text())['commit'],
              'variant':'upstream-9af0e3b-reference-2d','dimension':2,
              'test_count':result.testsRun,'tests_passed':result.wasSuccessful(),
              'fixture_sha256':sha(ROOT/'fixtures/small_graphs.json'),
              'environment_sha256':sha(args.output/'environment.json'),
              'elapsed_seconds':time.perf_counter()-started,'process_peak_memory_bytes':peak_bytes,
              'resource_limit_enforcement':'not implemented in phase 02; tiny fixed fixtures only',
              'absolute_fd_tolerance':3e-7,'relative_fd_tolerance':3e-5,
              'max_coordinate_difference':max(r['coordinate_max_abs_difference'] for r in rows),
              'checks':checks,'runs':rows,
              'limits':['No 3D implementation or graph-tool end-to-end run',
                        'No integration, scaling, portable adapter or quality-ranking claim',
                        'Float32 alpha and runtime-specific C rand retained',
                        'Failed upstream behavior is preserved in regression counterexamples']}
    atomic_json(args.output/'summary.json',report)
    print(json.dumps({'tests':result.testsRun,'runs':len(rows),'seconds':report['elapsed_seconds'],
                      'peak_bytes':peak_bytes,'max_coordinate_difference':report['max_coordinate_difference']}))


if __name__ == '__main__':
    main()
