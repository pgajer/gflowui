"""Portable bridge to the separately accepted, unchanged LGS paper-form kernel."""
import csv
import hashlib
import json
from pathlib import Path
import subprocess
import sys
import warnings
import numpy as np
from scipy.sparse import triu
sys.path.insert(0,str(Path(__file__).resolve().parents[1]/'suitesparse_phase1'))
from common import read_json,atomic_json,sha256

ACCEPTED='1a89dfd350591f25ef2c68b1037df316dd84416c'
PARAMETERS=dict(walk_depth=10,walk_decay=.1,repulsion_alpha=.2,epochs=60,
    transition_epochs=30,schedule_epsilon=.01,movement_tolerance=1e-7,
    collision_distance=1e-12,max_pair_displacement=1.,armijo=1e-4,max_backtracks=60)

def graph_hash(vertices,edges):
    value=dict(format='csv-graph-v1',vertex_sha256=vertices,edge_sha256=edges)
    return hashlib.sha256(json.dumps(value,sort_keys=True,separators=(',',':'),ensure_ascii=False,allow_nan=False).encode()).hexdigest()

def dependency(root,python):
    root=Path(root).resolve();source=root/'research/lgs3d'
    commit=subprocess.check_output(['git','rev-parse','HEAD'],cwd=root,text=True).strip()
    if commit!=ACCEPTED: raise ValueError('LGS dependency is not the accepted commit')
    subprocess.run(['git','diff','--exit-code','HEAD','--','research/lgs3d'],cwd=root,check=True,stdout=subprocess.DEVNULL)
    names=subprocess.check_output(['git','ls-files','research/lgs3d'],cwd=root,text=True).splitlines()
    files={name:sha256(root/name) for name in names}
    runtime={str(p.relative_to(source)):sha256(p) for p in sorted((source/'lgs_paper').glob('*.py'))}
    runtime['run.py']=sha256(source/'run.py')
    runtime_hash=hashlib.sha256(json.dumps(runtime,sort_keys=True,separators=(',',':'),ensure_ascii=False).encode()).hexdigest()
    environment=json.loads(subprocess.check_output([str(python),'-c',
        'import json,platform,numpy;print(json.dumps(dict(python=platform.python_version(),numpy=numpy.__version__)))'],text=True))
    if environment['numpy']!='2.1.0' or not environment['python'].startswith('3.12.'):
        raise ValueError('LGS runtime does not match the accepted dependency lock')
    return dict(root=str(root),python=str(Path(python).absolute()),accepted_commit=commit,
                files=files,runtime_source_sha256=runtime_hash,environment=environment)

def locality_settings(sizes):
    """Deduplicate clipped vectors, not only the largest component's setting."""
    rows=[];seen=set()
    for requested in [16,32,64,128,256,'all']:
        actual=tuple((min(requested,n-1) if requested!='all' else n-1) if n>1 else 0 for n in sizes)
        if actual in seen: continue
        seen.add(actual)
        rows.append(dict(requested=requested,component_k=list(actual),
                         component_fraction=[k/(n-1) if n>1 else None for k,n in zip(actual,sizes)]))
    return rows

def validate_response(response,request,ids,dest,dep,exit_code):
    if exit_code!=0 or response.get('status')!='completed':
        raise RuntimeError('LGS did not complete: '+str(response.get('termination',exit_code)))
    for key in ['graph_id','graph_sha256','dimension','seed','locality_k','parameters']:
        if response.get(key)!=request[key]: raise ValueError('LGS response identity mismatch: '+key)
    if response.get('variant')!='lgs-paper-union-v1': raise ValueError('unexpected LGS variant')
    impl=response.get('implementation',{})
    if impl.get('commit')!=ACCEPTED or impl.get('source_sha256')!=dep['runtime_source_sha256']:
        raise ValueError('LGS implementation identity changed')
    for key in ['vertex_sha256','edge_sha256']:
        if response.get('input_hashes',{}).get(key)!=request[key]: raise ValueError('LGS input hash mismatch')
    for key,value in dep['environment'].items():
        if response.get('environment',{}).get(key)!=value: raise ValueError('LGS runtime changed')
    path=Path(response['coordinate_path']).resolve()
    if not path.is_relative_to(Path(dest).resolve()/'lgs_artifacts') or sha256(path)!=response.get('coordinate_sha256'):
        raise ValueError('LGS coordinate path/checksum mismatch')
    with path.open(newline='') as stream:
        reader=csv.DictReader(stream)
        if reader.fieldnames!=['vertex_id','x','y','z']: raise ValueError('LGS coordinate header mismatch')
        rows=list(reader)
    if [r['vertex_id'] for r in rows]!=ids: raise ValueError('LGS vertex order changed')
    z=np.asarray([[float(r[k]) for k in ['x','y','z']] for r in rows])
    if z.shape!=(len(ids),3) or not np.isfinite(z).all(): raise ValueError('invalid LGS coordinates')
    return z

def make_embedder(config):
    dep=config['lgs_dependency']
    def embed(method,adjacency,ids,d,features,seed,dest,initial=None):
        if method!='lgs_paper': raise ValueError('wrong LGS adapter method')
        dest=Path(dest).resolve()
        if (dest/'lgs_artifacts').exists(): raise ValueError('fresh component output required')
        k=len(ids)-1 if config['locality_requested']=='all' else min(config['locality_requested'],len(ids)-1)
        for name,header,rows in [
            ('vertices.csv',['vertex_id'],[[i] for i in ids]),
            ('edges.csv',['source','target','length'],
             [[ids[i],ids[j],1] for i,j in zip(triu(adjacency,k=1).tocoo().row,triu(adjacency,k=1).tocoo().col)])]:
            with (dest/name).open('w',newline='') as stream:
                out=csv.writer(stream);out.writerow(header);out.writerows(rows)
        v=sha256(dest/'vertices.csv');e=sha256(dest/'edges.csv')
        request=dict(schema_version=1,graph_id=config['graph_id']+'/'+dest.name,
            graph_sha256=graph_hash(v,e),vertex_file='vertices.csv',vertex_sha256=v,
            edge_file='edges.csv',edge_sha256=e,dimension=3,seed=seed,locality_k=k,
            parameters=PARAMETERS,job_limits=dict(wall_seconds=600,memory_mib=2048),output_directory='lgs_artifacts')
        atomic_json(dest/'lgs_request.json',request)
        call=subprocess.run([dep['python'],str(Path(dep['root'])/'research/lgs3d/run.py'),str(dest/'lgs_request.json')],
                            capture_output=True,text=True)
        (dest/'lgs_stdout.json').write_text(call.stdout);(dest/'lgs_stderr.log').write_text(call.stderr)
        response=json.loads(call.stdout)
        z=validate_response(response,request,ids,dest,dep,call.returncode)
        for warning in response.get('warnings',[]): warnings.warn(str(warning))
        return z,dict(variant='lgs-paper-union-v1',locality_k=k,locality_fraction=k/(len(ids)-1),
                      main_graph_sha256=config['graph_sha256'],portable_graph_sha256=request['graph_sha256'],
                      response=response,termination=response['termination'],
                      warning='Experimental paper-form variant; pair descent is not full-objective descent.')
    return embed
