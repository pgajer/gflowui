#!/usr/bin/env python3
"""Rebuild, test and demonstrate the committed standalone paper-form adapter."""
import argparse
import csv
import io
import json
import math
import os
from pathlib import Path
import re
import subprocess
import sys
import time
import uuid
import numpy as np
from jsonschema import Draft202012Validator
ROOT=Path(__file__).resolve().parents[1]
sys.path.insert(0,str(ROOT))
from lgs_paper.adapter import atomic_json,canonical,digest,implementation_identity


def command(args,output,env):
    run=subprocess.run(args,capture_output=True,text=True,env=env,timeout=600)
    output.with_suffix('.stdout.txt').write_text(run.stdout)
    output.with_suffix('.stderr.txt').write_text(run.stderr)
    if run.returncode:
        raise RuntimeError(f'{output.name} exited {run.returncode}; see saved stdout/stderr')
    return run


def main():
    parser=argparse.ArgumentParser();parser.add_argument('--output',type=Path,required=True)
    args=parser.parse_args();out=args.output.resolve();out.mkdir(parents=True,exist_ok=True)
    env={**os.environ,'PYTHONPATH':str(ROOT),'OPENBLAS_NUM_THREADS':'1','OMP_NUM_THREADS':'1',
         'VECLIB_MAXIMUM_THREADS':'1','MKL_NUM_THREADS':'1'}
    started=time.perf_counter()
    summary={'schema_version':1,'status':'running','implementation':implementation_identity(),
             'variant':'lgs-paper-union-v1','seeds':[17,314,2026],
             'graph':'complete graph on four vertices, all six unit-distance targets',
             'initialization':'frozen paper_3d.json coordinates; 2D uses first two columns',
             'locality_k':3,'epochs':200,'runs':[],
             'scope':'analytical dimensional/CLI demonstration, not a cohort quality or scaling claim'}
    try:
        command([sys.executable,str(ROOT/'scripts/build_oracle.py')],out/'build',env)
        tests=command([sys.executable,'-m','unittest','discover','-s',str(ROOT/'tests'),'-v'],out/'tests',env)
        match=re.search(r'Ran (\d+) tests',tests.stderr)
        if not match:raise RuntimeError('test count missing')
        summary['tests_passed']=int(match.group(1))
        request_schema=Draft202012Validator(json.loads((ROOT/'schemas/request-v1.json').read_text()))
        response_schema=Draft202012Validator(json.loads((ROOT/'schemas/response-v1.json').read_text()))
        fixture=ROOT/'fixtures/adapter_k4'
        paper=json.loads((ROOT/'fixtures/paper_3d.json').read_text())['cases'][0]
        starts=dict(zip(paper['vertex_ids'],paper['initial']))
        ids=[row['vertex_id'] for row in csv.DictReader((fixture/'vertices.csv').open())]
        for dimension in (2,3):
            for seed in summary['seeds']:
                directory=out/f'k4-d{dimension}-s{seed}';directory.mkdir(exist_ok=True)
                for name in ('vertices.csv','edges.csv'):
                    (directory/name).write_bytes((fixture/name).read_bytes())
                initial=io.StringIO(newline='');writer=csv.writer(initial,lineterminator='\n')
                writer.writerow(['vertex_id','x','y']+(['z'] if dimension==3 else []))
                for vertex in ids:writer.writerow([vertex,*starts[vertex][:dimension]])
                data=initial.getvalue().encode();(directory/'initial.csv').write_bytes(data)
                req=json.loads((fixture/'request.json').read_text())
                req.update(dimension=dimension,seed=seed,output_directory='artifacts',
                           initial_coordinate_file='initial.csv',initial_coordinate_sha256=digest(data))
                req['parameters']['epochs']=200
                request_schema.validate(req);atomic_json(directory/'request.json',req)
                invocation=[sys.executable,str(ROOT/'run.py'),str(directory/'request.json')]
                run=command(invocation,directory/'first',env);response=json.loads(run.stdout)
                response_schema.validate(response)
                again=json.loads(command(invocation,directory/'cached',env).stdout)
                response_schema.validate(again)
                if not again['cache_hit'] or response['coordinate_sha256']!=again['coordinate_sha256']:
                    raise RuntimeError('cache repeat did not preserve coordinates')
                coordinate_file=Path(response['coordinate_path']);raw=coordinate_file.read_bytes()
                if digest(raw)!=response['coordinate_sha256']:raise RuntimeError('coordinate checksum mismatch')
                records=list(csv.DictReader(io.StringIO(raw.decode())))
                if [r['vertex_id'] for r in records]!=ids:raise RuntimeError('vertex order changed')
                axes=('x','y','z')[:dimension]
                x=np.array([[float(r[a]) for a in axes] for r in records])
                if x.shape!=(4,dimension) or not np.isfinite(x).all():raise RuntimeError('invalid finite dimensions')
                # Independent scalar residual against all six unit targets.
                stress=sum((math.sqrt(sum((float(x[i,a])-float(x[j,a]))**2
                                           for a in range(dimension)))-1)**2
                           for i in range(4) for j in range(i))
                singular=np.linalg.svd(x-x.mean(axis=0),compute_uv=False)
                rank=int(np.sum(singular>1e-6))
                if abs(stress-response['objective']['final'])>1e-10:
                    raise RuntimeError('independent endpoint objective mismatch')
                record={key:response[key] for key in ('dimension','seed','termination','epochs_completed',
                         'coordinate_path','coordinate_sha256','elapsed_seconds','peak_memory_bytes','cache_key','quality')}
                record.update(initial_coordinate_sha256=req['initial_coordinate_sha256'],
                              request_path=str(directory/'request.json'),raw_stress=stress,
                              centered_rank_at_1e_6=rank,singular_values=singular.tolist(),
                              cached_repeat_identical=True,first_invocation_was_cache_hit=response['cache_hit'])
                if dimension==3:
                    volume=abs(float(np.linalg.det((x[1:]-x[0]).T)))/6
                    record['tetrahedron_volume']=volume
                    if rank!=3 or stress>=1e-10 or abs(volume-math.sqrt(2)/12)>1e-6:
                        raise RuntimeError('nonplanar tetrahedron check failed')
                elif rank!=2 or stress<=.1:
                    raise RuntimeError('unexpected 2D endpoint diagnostic')
                summary['runs'].append(record)
                atomic_json(out/'summary.json',summary)
        # Fresh output directory forces numerical recomputation with the same
        # request identity; it is distinct from the cache-repeat check above.
        first=summary['runs'][3]
        repeat_output=out/('independent-repeat-'+uuid.uuid4().hex)
        repeat=json.loads(command([sys.executable,str(ROOT/'run.py'),first['request_path'],
                                   '--output-dir',str(repeat_output)],out/'independent-repeat',env).stdout)
        response_schema.validate(repeat)
        if repeat['cache_hit'] or repeat['cache_key']!=first['cache_key'] or repeat['coordinate_sha256']!=first['coordinate_sha256']:
            raise RuntimeError('independent run changed coordinates')
        summary['independent_repeat']={'coordinate_sha256':repeat['coordinate_sha256'],
                                      'coordinate_path':repeat['coordinate_path'],
                                      'cache_hit':repeat['cache_hit'],'identical':True}
        summary.update(status='completed',elapsed_seconds=time.perf_counter()-started)
    except Exception as exc:
        summary.update(status='failed',reason=f'{type(exc).__name__}: {exc}',elapsed_seconds=time.perf_counter()-started)
        atomic_json(out/'summary.json',summary)
        raise
    atomic_json(out/'summary.json',summary)
    print(json.dumps({'status':summary['status'],'tests_passed':summary['tests_passed'],
                      'runs':len(summary['runs']),'summary':str(out/'summary.json')}))


if __name__=='__main__':main()
