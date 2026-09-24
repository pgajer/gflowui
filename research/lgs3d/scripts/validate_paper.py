"""Reproducible phase03 numerical evidence, without adapter or quality claims."""
import argparse
from dataclasses import asdict
import hashlib
import io
import json
from pathlib import Path
import platform
import resource
import subprocess
import sys
import time
import unittest

ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0,str(ROOT))
import numpy as np
from lgs_paper import prepare,Controls,optimize,objective_gradient,VARIANT
from lgs_paper.reference import evaluate,finite_difference


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--output',type=Path,required=True)
    args = parser.parse_args()
    args.output.mkdir(parents=True,exist_ok=True)
    started = time.perf_counter()
    captured = io.StringIO()
    tests = unittest.TextTestRunner(stream=captured,verbosity=2).run(
        unittest.defaultTestLoader.discover(str(ROOT/'tests')))
    testfile = args.output/'tests.txt.tmp'
    testfile.write_text(captured.getvalue())
    testfile.replace(args.output/'tests.txt')
    if not tests.wasSuccessful():
        print(captured.getvalue(),file=sys.stderr)
        raise SystemExit(1)
    fixture = ROOT/'fixtures/paper_3d.json'
    rows,checks = [],[]
    for case in json.loads(fixture.read_text())['cases']:
        names = case['vertex_ids']; n = len(names)
        adjacency = np.zeros((n,n),dtype=int)
        for i,j in case['edges']:
            adjacency[i,j] = adjacency[j,i] = 1
        for k in (1,n-1):
            p = prepare(names,adjacency,k)
            for dimension in (2,3):
                initial = np.array(case['initial'])[:,:dimension]
                cost,g = objective_gradient(p,initial)
                brute,bg = evaluate(initial,p.distances,p.attractive,.2)
                for h in (1e-4,1e-5,1e-6):
                    numeric = finite_difference(lambda x:evaluate(x,p.distances,p.attractive,.2)[0],initial,h)
                    np.testing.assert_allclose(g,numeric,rtol=3e-5,atol=3e-7)
                    np.testing.assert_allclose(cost,brute,rtol=2e-13,atol=2e-13)
                    np.testing.assert_allclose(g,bg,rtol=2e-13,atol=2e-13)
                    checks.append({'graph':case['name'],'k':k,'dimension':dimension,'h':h,
                                   'objective_abs_error':abs(cost-brute),
                                   'gradient_reference_max_abs_error':float(np.max(abs(g-bg))),
                                   'fd_max_absolute_error':float(np.max(abs(g-numeric))),
                                   'fd_max_combined_tolerance_ratio':float(np.max(abs(g-numeric)/(3e-7+3e-5*abs(numeric)))),
                                   'fd_relative_l2_error':float(np.linalg.norm(g-numeric)/max(np.linalg.norm(g),1e-15))})
                for seed in (17,314,2026):
                    controls = Controls(epochs=200)
                    result = optimize(p,dimension,seed,initial,controls)
                    x = result.coordinates
                    centered = x-x.mean(axis=0)
                    scalar,sg = evaluate(x,p.distances,p.attractive,.2)
                    rows.append({'graph':case['name'],'vertex_ids':names,'dimension':dimension,
                                 'locality_k':k,'locality_fraction':k/(n-1),'seed':seed,
                                 'walk_depth':p.walk_depth,'walk_decay':p.walk_decay,
                                 'attractive_pair_count':int(p.attractive.sum()//2),
                                 'attractive_components':p.attractive_components,
                                 'controls':asdict(controls),'initial_coordinates':initial.tolist(),
                                 'coordinates':x.tolist(),'centered_rank_at_1e-6':int(np.linalg.matrix_rank(centered,tol=1e-6)),
                                 'singular_values':np.linalg.svd(centered,compute_uv=False).tolist(),
                                 'initial_objective':result.history[0]['objective'],
                                 'final_objective':result.history[-1]['objective'],
                                 'independent_final_objective':scalar,
                                 'final_gradient_norm':result.history[-1]['gradient_norm'],
                                 'epochs_completed':result.epochs_completed,'termination':result.termination,
                                 'warnings':result.warnings,
                                 'epochs_with_objective_increase':sum(b['objective']>a['objective']+1e-12 for a,b in zip(result.history,result.history[1:])),
                                 'final_max_pair_movement':result.history[-1]['max_pair_movement'],
                                 'total_roundoff_skipped_pairs':sum(row['roundoff_skipped_pairs'] for row in result.history),
                                 'total_backtracks':sum(row['backtracks'] for row in result.history)})
    rss = resource.getrusage(resource.RUSAGE_SELF).ru_maxrss
    report = {'schema_version':1,'phase':3,'variant':VARIANT,
              'implementation_commit':subprocess.check_output(['git','rev-parse','HEAD'],cwd=ROOT,text=True).strip(),
              'worktree_changes':subprocess.check_output(['git','status','--porcelain','--','.'],cwd=ROOT,text=True).strip(),
              'python':sys.version,'numpy':np.__version__,'platform':platform.platform(),
              'test_count':tests.testsRun,'tests_passed':tests.wasSuccessful(),
              'fixture_sha256':hashlib.sha256(fixture.read_bytes()).hexdigest(),
              'fd_absolute_tolerance':3e-7,'fd_relative_tolerance':3e-5,
              'elapsed_seconds':time.perf_counter()-started,
              'process_peak_memory_bytes':int(rss if sys.platform=='darwin' else 1024*rss),
              'checks':checks,'runs':rows,
              'limitations':['Tiny numerical validation, no comparative layout quality conclusion',
                             'Full objective may increase during shuffled pair optimization',
                             'Disconnected attraction can make the logarithmic objective unbounded below',
                             'No adapter, resource supervision, cache or app integration']}
    temporary = args.output/'summary.json.tmp'
    temporary.write_text(json.dumps(report,indent=2,allow_nan=False)+'\n')
    temporary.replace(args.output/'summary.json')
    print(json.dumps({'tests':tests.testsRun,'runs':len(rows),'seconds':report['elapsed_seconds'],
                      'peak_bytes':report['process_peak_memory_bytes'],
                      'max_fd_error':max(x['fd_max_absolute_error'] for x in checks),
                      'tetrahedron_3d_objectives':[r['final_objective'] for r in rows if r['graph']=='complete_four_nonplanar' and r['dimension']==3 and r['locality_k']==3]}))


if __name__ == '__main__':
    main()
