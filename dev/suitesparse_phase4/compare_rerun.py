"""Retain corrective reruns separately; compare numerical outcomes, not timings."""
import argparse
from pathlib import Path
import sys
sys.path.insert(0,str(Path(__file__).resolve().parents[1]/'suitesparse_phase1'))
from common import atomic_json,read_json,sha256

def compare(original,rerun,output):
    original=Path(original);rerun=Path(rerun)
    key=lambda r:(r['graph_id'],str(r['locality']['requested']),r['seed'])
    first=read_json(original);second=read_json(rerun)
    old={key(r):r for r in first['runs']};new={key(r):r for r in second['runs']}
    if len(old)!=34 or len(new)!=34 or set(old)!=set(new) or len(second['runs'])!=34:
        raise ValueError('corrective rerun must account for the complete 34-case matrix')
    rows=[]
    for k,a in old.items():
        b=new[k];dest=Path(b['run_dir']);manifest=read_json(dest/'manifest.json')
        row=dict(graph_id=b['graph_id'],locality=b['locality'],seed=b['seed'],status=b['status'],
            reason=b['reason'],run_dir=b['run_dir'],manifest_sha256=sha256(dest/'manifest.json'),
            memory_fallback_reads=manifest.get('memory_fallback_reads',0),
            vanished_process_reads=manifest.get('vanished_process_reads',0),numerically_equal=False)
        if b['status']=='completed':
            original_result=read_json(Path(a['run_dir'])/'result.json');result=read_json(dest/'result.json')
            row['numerically_equal']=(sha256(dest/'coords_raw.csv')==sha256(Path(a['run_dir'])/'coords_raw.csv') and
                                      result['summary']==original_result['summary'])
        rows.append(row)
    passed=all(r['status']=='completed' and r['numerically_equal'] for r in rows)
    atomic_json(output,dict(schema_version=1,original_index=str(original),original_sha256=sha256(original),
        rerun_index=str(rerun),rerun_sha256=sha256(rerun),rerun_commit=second['commit'],
        interpretation='Corrective monitoring rerun; original scientific results remain unchanged; timings are not equated.',
        complete_matrix=True,all_numerically_equal=passed,runs=rows))
    print(len(rows),'terminal cases;',sum(r['numerically_equal'] for r in rows),'identical coordinate/score results;',
          sum(r['memory_fallback_reads'] for r in rows),'bounded RSS fallback measurements')
    if not passed: raise SystemExit(1)

if __name__=='__main__':
    p=argparse.ArgumentParser();p.add_argument('original');p.add_argument('rerun');p.add_argument('output')
    a=p.parse_args();compare(a.original,a.rerun,a.output)
