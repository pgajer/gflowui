"""Version-1 JSON/CSV adapter with supervised, isolated numerical execution."""
import argparse
from dataclasses import asdict,fields
import csv
import fcntl
import hashlib
import io
import json
import math
import os
from pathlib import Path
import platform
import resource
import shutil
import signal
import subprocess
import sys
import time
import uuid
import numpy as np
from .core import VARIANT,Controls,NumericalFailure,integer,real,prepare,optimize
from .metrics import evaluate_quality

ROOT=Path(__file__).resolve().parents[1]
PREPARATION='csv-graph-v1/stable-id-walk-v1'
DEFAULT_LIMITS={'wall_seconds':600.,'memory_mib':2048}
CONTROL_KEYS={f.name for f in fields(Controls)}
PARAMETER_KEYS=CONTROL_KEYS|{'walk_depth','walk_decay'}
REQUIRED={'schema_version','graph_id','graph_sha256','vertex_file','vertex_sha256',
          'edge_file','edge_sha256','dimension','seed','locality_k','parameters'}
OPTIONAL={'initial_coordinate_file','initial_coordinate_sha256','job_limits','output_directory'}
INPUT_BOUNDS={'vertex_file':1024**2,'edge_file':16*1024**2,'initial_coordinate_file':2*1024**2}
POLL_SECONDS=.05


class InputFailure(ValueError):
    def __init__(self,reason,status='invalid_input'):
        self.status=status
        super().__init__(reason)


def canonical(value):
    return json.dumps(value,sort_keys=True,separators=(',',':'),ensure_ascii=False,allow_nan=False).encode('utf8')


def digest(data):
    return hashlib.sha256(data).hexdigest()


def file_hash(path):
    return digest(Path(path).read_bytes())


def graph_hash(vertex_hash,edge_hash):
    return digest(canonical({'format':'csv-graph-v1','vertex_sha256':vertex_hash,'edge_sha256':edge_hash}))


def reject_constant(value):
    raise ValueError('nonfinite JSON constant')


def unique_object(pairs):
    result={}
    for key,value in pairs:
        if key in result:
            raise ValueError('duplicate JSON key')
        result[key]=value
    return result


def parse_json(data):
    return json.loads(data,parse_constant=reject_constant,object_pairs_hook=unique_object)


def atomic_json(path,value):
    path=Path(path)
    temporary=path.with_name(path.name+'.'+uuid.uuid4().hex+'.tmp')
    with temporary.open('xb') as out:
        out.write(canonical(value)+b'\n');out.flush();os.fsync(out.fileno())
    temporary.replace(path)


def implementation_identity():
    try:
        commit=subprocess.check_output(['git','rev-parse','HEAD'],cwd=ROOT,stderr=subprocess.DEVNULL,text=True).strip()
    except (OSError,subprocess.CalledProcessError):
        commit=None
    source={str(p.relative_to(ROOT)):file_hash(p) for p in sorted((ROOT/'lgs_paper').glob('*.py'))}
    source['run.py']=file_hash(ROOT/'run.py')
    return {'commit':commit,'source_sha256':digest(canonical(source)),
            'requirements_sha256':file_hash(ROOT/'requirements.lock')}


def environment():
    return {'python':platform.python_version(),'numpy':np.__version__,'system':platform.system(),
            'machine':platform.machine(),'blas_threads':1,'arithmetic':'float64'}


def normalize_request(raw,base):
    if not isinstance(raw,dict) or not REQUIRED.issubset(raw) or set(raw)-REQUIRED-OPTIONAL:
        raise InputFailure('missing or unknown request fields')
    integer(raw['schema_version'],'schema_version',1,1)
    if not isinstance(raw['graph_id'],str) or not raw['graph_id']:
        raise InputFailure('graph_id must be a nonempty string')
    dimension=integer(raw['dimension'],'dimension',1)
    if dimension not in (2,3):
        raise InputFailure('unsupported_dimension','unsupported')
    integer(raw['seed'],'seed',0,2**64-1)
    integer(raw['locality_k'],'locality_k',1)
    params=raw['parameters']
    if not isinstance(params,dict) or set(params)-PARAMETER_KEYS:
        raise InputFailure('unknown or invalid algorithm parameters')
    if not {'walk_depth','walk_decay','repulsion_alpha'}.issubset(params):
        raise InputFailure('parameters must specify walk_depth, walk_decay, repulsion_alpha')
    controls=Controls(**{k:v for k,v in params.items() if k in CONTROL_KEYS})
    integer(params['walk_depth'],'walk_depth',1)
    decay=real(params['walk_decay'],'walk_decay')
    if not 0<decay<1: raise InputFailure('walk_decay must lie in (0,1)')
    result=dict(raw)
    result['parameters']={**asdict(controls),'walk_depth':params['walk_depth'],'walk_decay':decay}
    initial_fields={'initial_coordinate_file','initial_coordinate_sha256'}&set(raw)
    if initial_fields and len(initial_fields)!=2:
        raise InputFailure('initial coordinate path and checksum must be provided together')
    for key in ('graph_sha256','vertex_sha256','edge_sha256','initial_coordinate_sha256'):
        if key in raw and (not isinstance(raw[key],str) or len(raw[key])!=64 or
                           any(c not in '0123456789abcdef' for c in raw[key])):
            raise InputFailure(f'{key} must be a lowercase SHA-256 digest')
    for key in INPUT_BOUNDS:
        if key in raw:
            if not isinstance(raw[key],str) or not raw[key]: raise InputFailure(f'invalid {key}')
            result[key]=str((base/raw[key]).resolve())
    limits=raw.get('job_limits',{})
    if not isinstance(limits,dict) or set(limits)-set(DEFAULT_LIMITS):
        raise InputFailure('invalid job_limits')
    limits={**DEFAULT_LIMITS,**limits}
    real(limits['wall_seconds'],'wall_seconds',strict=True)
    integer(limits['memory_mib'],'memory_mib',1,2048)
    if limits['wall_seconds']>600: raise InputFailure('wall_seconds exceeds the 600-second experimental cap')
    result['job_limits']=limits
    return result


def read_csv(data,header):
    try:
        rows=list(csv.reader(io.StringIO(data.decode('utf-8')),strict=True))
    except (UnicodeError,csv.Error) as exc:
        raise InputFailure('invalid UTF-8 CSV') from exc
    if not rows or rows[0]!=header or any(len(row)!=len(header) for row in rows[1:]):
        raise InputFailure('CSV header or row width mismatch')
    return rows[1:]


def bounded_input(req,key,hash_key):
    p=Path(req[key])
    if not p.is_file(): raise InputFailure(f'{key} is not a regular file')
    with p.open('rb') as f:
        data=f.read(INPUT_BOUNDS[key]+1)
    if len(data)>INPUT_BOUNDS[key]: raise InputFailure(f'{key} exceeds file-size admission limit','resource_limited')
    if digest(data)!=req[hash_key]: raise InputFailure(f'{key} checksum mismatch')
    return data


def load_inputs(req):
    vertices=bounded_input(req,'vertex_file','vertex_sha256')
    edges=bounded_input(req,'edge_file','edge_sha256')
    if graph_hash(digest(vertices),digest(edges))!=req['graph_sha256']:
        raise InputFailure('graph_sha256 mismatch')
    ids=[row[0] for row in read_csv(vertices,['vertex_id'])]
    if any(not v for v in ids) or len(set(ids))!=len(ids): raise InputFailure('duplicate or empty vertex_id')
    n=len(ids)
    if n<2: raise InputFailure('unsupported_small_component','unsupported')
    if n>2000: raise InputFailure('vertex admission limit is 2000','resource_limited')
    integer(req['locality_k'],'locality_k',1,n-1)
    erows=read_csv(edges,['source','target','length'])
    if len(erows)>100000: raise InputFailure('edge admission limit is 100000','resource_limited')
    estimate=128*1024**2+128*n*n+512*len(erows)
    if estimate>req['job_limits']['memory_mib']*1024**2:
        raise InputFailure('dense_preflight_memory_estimate_exceeds_limit','resource_limited')
    index={v:i for i,v in enumerate(ids)}
    adjacency=np.zeros((n,n),dtype=np.uint8)
    seen=set()
    for source,target,length_text in erows:
        if source not in index or target not in index: raise InputFailure('unknown edge vertex ID')
        if source==target: raise InputFailure('self loops are unsupported')
        pair=tuple(sorted((index[source],index[target])))
        if pair in seen: raise InputFailure('duplicate undirected edge')
        seen.add(pair)
        try:length=float(length_text)
        except ValueError as exc: raise InputFailure('invalid edge length') from exc
        if not math.isfinite(length) or length<=0: raise InputFailure('edge lengths must be positive and finite')
        if length!=1: raise InputFailure('weighted graphs are unsupported','unsupported')
        adjacency[pair]=adjacency[pair[::-1]]=1
    initial=None
    if 'initial_coordinate_file' in req:
        data=bounded_input(req,'initial_coordinate_file','initial_coordinate_sha256')
        header=['vertex_id','x','y']+(['z'] if req['dimension']==3 else [])
        rows=read_csv(data,header)
        if [row[0] for row in rows]!=ids: raise InputFailure('initial coordinates must follow declared vertex order exactly')
        try:initial=np.array([[float(v) for v in row[1:]] for row in rows])
        except ValueError as exc:raise InputFailure('invalid initial coordinate') from exc
        if not np.isfinite(initial).all(): raise InputFailure('nonfinite initial coordinate')
    return ids,adjacency,initial,estimate


def coordinate_bytes(ids,points,dimension):
    out=io.StringIO(newline='')
    writer=csv.writer(out,lineterminator='\n')
    writer.writerow(['vertex_id','x','y']+(['z'] if dimension==3 else []))
    for vertex,row in zip(ids,points):writer.writerow([vertex,*map(repr,map(float,row))])
    return out.getvalue().encode('utf-8')


def validate_coordinates(data,ids,dimension):
    rows=read_csv(data,['vertex_id','x','y']+(['z'] if dimension==3 else []))
    if [r[0] for r in rows]!=list(ids): raise InputFailure('coordinate vertex order mismatch')
    values=np.array([[float(v) for v in row[1:]] for row in rows])
    if values.shape!=(len(ids),dimension) or not np.isfinite(values).all():
        raise InputFailure('invalid coordinate values')
    return values


def cache_identity(req,ids):
    return {'implementation':implementation_identity(),'environment':environment(),
            'variant':VARIANT,'preparation':PREPARATION,
            'graph_id':req['graph_id'],'graph_sha256':req['graph_sha256'],
            'vertex_sha256':req['vertex_sha256'],'edge_sha256':req['edge_sha256'],
            'vertex_order_sha256':digest(canonical(ids)),
            'initial_coordinate_sha256':req.get('initial_coordinate_sha256'),
            'dimension':req['dimension'],'seed':req['seed'],'locality_k':req['locality_k'],
            'parameters':req['parameters']}


def cached_result(output,key,identity,ids):
    try:
        pointer=parse_json((output/'cache'/f'{key}.json').read_bytes())
        name=pointer['result_directory']
        if not isinstance(name,str) or Path(name).name!=name or not name.startswith(key+'-'):
            raise ValueError('invalid cache pointer')
        directory=(output/'results'/name).resolve()
        if directory.parent!=(output/'results').resolve():raise ValueError('cache path escaped result root')
        receipt=parse_json((directory/'COMPLETED.json').read_bytes())
        raw=(directory/'result.json').read_bytes()
        if digest(raw)!=receipt['result_sha256']:raise ValueError('manifest checksum mismatch')
        result=parse_json(raw)
        if result['status']!='completed' or result['cache_identity']!=identity or result['cache_key']!=key:
            raise ValueError('cached run identity mismatch')
        data=(directory/'coordinates.csv').read_bytes()
        if digest(data)!=result['coordinate_sha256']:raise ValueError('coordinate checksum mismatch')
        validate_coordinates(data,ids,identity['dimension'])
        if result['coordinate_path']!=str(directory/'coordinates.csv'):
            raise ValueError('coordinate path mismatch')
        return result,None
    except FileNotFoundError:
        return None,None
    except (ValueError,KeyError,TypeError,OSError):
        return None,'invalid_cache_entry_ignored'


def base_response(req):
    return {'schema_version':1,'method':'LGS paper-form experimental variant','variant':VARIANT,
            'dimension':req.get('dimension'),'graph_id':req.get('graph_id'),'seed':req.get('seed'),
            'locality_k':req.get('locality_k'),'parameters':req.get('parameters'),
            'graph_sha256':req.get('graph_sha256'),
            'input_hashes':{key:req.get(key) for key in ('vertex_sha256','edge_sha256','initial_coordinate_sha256')},
            'implementation':implementation_identity(),
            'upstream_commit':json.loads((ROOT/'upstream.json').read_text())['commit'],
            'environment':environment(),'warnings':[]}


def worker(request_path,stage_path):
    stage=Path(stage_path)
    (stage/'worker.pid').write_text(str(os.getpid()))
    started=time.perf_counter()
    req=parse_json(Path(request_path).read_bytes())
    response=base_response(req)
    try:
        ids,adjacency,initial,estimate=load_inputs(req)
        identity=cache_identity(req,ids)
        key=digest(canonical(identity))
        cached,warning=cached_result(Path(req['output_directory']),key,identity,ids)
        if cached is not None:
            response={'cached_manifest':cached,'cache_hit':True}
        else:
            params=req['parameters']
            p=prepare(ids,adjacency,req['locality_k'],params['walk_depth'],params['walk_decay'])
            result=optimize(p,req['dimension'],req['seed'],initial,Controls(**{k:params[k] for k in CONTROL_KEYS}))
            data=coordinate_bytes(ids,result.coordinates,req['dimension'])
            validate_coordinates(data,ids,req['dimension'])
            coordinate_file=stage/'coordinates.csv'
            with coordinate_file.open('xb') as out:out.write(data);out.flush();os.fsync(out.fileno())
            response.update(status='completed',cache_hit=False,cache_key=key,cache_identity=identity,
                            coordinate_sha256=digest(data),vertex_count=len(ids),
                            coordinate_transform='none; raw optimizer coordinates',
                            termination=result.termination,epochs_completed=result.epochs_completed,
                            objective={'definition':'unordered raw attraction minus alpha log repulsion',
                                       'initial':result.history[0]['objective'],'final':result.history[-1]['objective'],
                                       'final_gradient_norm':result.history[-1]['gradient_norm'],
                                       'history':result.history},
                            attractive_components=p.attractive_components,
                            quality=evaluate_quality(ids,adjacency,p.distances,result.coordinates),
                            preflight_memory_bytes=estimate,worker_elapsed_seconds=time.perf_counter()-started,
                            warnings=result.warnings+([warning] if warning else []))
    except InputFailure as exc:
        response.update(status=exc.status,termination=str(exc))
    except NumericalFailure as exc:
        response.update(status='failed',termination=str(exc))
    except (ValueError,TypeError,OSError) as exc:
        response.update(status='invalid_input',termination=str(exc)[:1000])
    except Exception as exc:
        response.update(status='failed',termination=f'{type(exc).__name__}: {str(exc)[:1000]}')
    atomic_json(stage/'worker_result.json',response)


def rss_bytes(pids):
    output=subprocess.check_output(['ps','-o','rss=','-p',','.join(map(str,pids))],text=True,stderr=subprocess.DEVNULL)
    return sum(int(v) for v in output.split())*1024


def stop_child(child):
    if child.poll() is None:
        try:os.killpg(child.pid,signal.SIGTERM)
        except ProcessLookupError:pass
        try:child.wait(timeout=.5)
        except subprocess.TimeoutExpired:
            try:os.killpg(child.pid,signal.SIGKILL)
            except ProcessLookupError:pass
            child.wait()


def supervise(req,output):
    stage=output/('.attempt-'+uuid.uuid4().hex)
    stage.mkdir()
    snapshot=stage/'request.json'
    atomic_json(snapshot,req)
    response=base_response(req)
    child=None
    started=time.monotonic()
    last=started;largest_gap=0.;peak=0
    limit_bytes=req['job_limits']['memory_mib']*1024**2
    failure=None
    try:
        with (stage/'worker.log').open('wb') as log:
            env={**os.environ,'OPENBLAS_NUM_THREADS':'1','OMP_NUM_THREADS':'1','MKL_NUM_THREADS':'1',
                 'VECLIB_MAXIMUM_THREADS':'1','NUMEXPR_NUM_THREADS':'1','PYTHONHASHSEED':'0'}
            child=subprocess.Popen([sys.executable,str(ROOT/'run.py'),'--worker',str(snapshot),str(stage),str(os.getpid())],
                                   stdout=log,stderr=log,env=env,start_new_session=True)
            while True:
                now=time.monotonic();largest_gap=max(largest_gap,now-last);last=now
                peak=max(peak,rss_bytes([os.getpid(),child.pid]))
                if peak>limit_bytes:
                    failure=('resource_limited','memory_limit');break
                if now-started>req['job_limits']['wall_seconds']:
                    failure=('resource_limited','wall_timeout');break
                if child.poll() is not None:break
                time.sleep(POLL_SECONDS)
            if failure:stop_child(child)
            else:child.wait()
        # ru_maxrss supplies a conservative post-exit peak bound, including a
        # fast allocation that occurred between RSS samples. Shared pages may
        # be counted twice in the parent+child sum.
        unit=1 if sys.platform=='darwin' else 1024
        peak=max(peak,int((resource.getrusage(resource.RUSAGE_SELF).ru_maxrss+
                           resource.getrusage(resource.RUSAGE_CHILDREN).ru_maxrss)*unit))
        if not failure and peak>limit_bytes:failure=('resource_limited','memory_limit_postcheck')
        if not failure and time.monotonic()-started>req['job_limits']['wall_seconds']:
            failure=('resource_limited','wall_timeout')
        if failure:
            response.update(status=failure[0],termination=failure[1])
        elif child.returncode!=0 or not (stage/'worker_result.json').is_file():
            response.update(status='failed',termination=f'worker_exit_{child.returncode}')
        else:
            response=parse_json((stage/'worker_result.json').read_bytes())
    except KeyboardInterrupt:
        if child:stop_child(child)
        response.update(status='cancelled',termination='interrupted')
    except Exception as exc:
        if child:stop_child(child)
        response.update(status='failed',termination=f'supervisor_error: {str(exc)[:1000]}')
    elapsed=time.monotonic()-started
    if 'cached_manifest' in response:
        original=response['cached_manifest']
        response={**original,'cache_hit':True,
                  'source_run_elapsed_seconds':original['elapsed_seconds'],
                  'source_run_peak_memory_bytes':original['peak_memory_bytes']}
    response.update(elapsed_seconds=elapsed,peak_memory_bytes=peak,job_limits=req['job_limits'],
                    resource_measurement={'scope':'sum of supervisor and worker RSS; shared pages may count twice',
                                          'poll_interval_seconds':POLL_SECONDS,'max_observed_poll_gap_seconds':largest_gap,
                                          'memory_overshoot_bytes':max(0,peak-limit_bytes),
                                          'timeout_overshoot_seconds':max(0,elapsed-req['job_limits']['wall_seconds']),
                                          'enforcement':'supervised termination plus post-exit peak check; transient overshoot possible'})
    try:
        if response.get('status')=='completed' and not response.get('cache_hit'):
            key=response['cache_key'];name=key+'-'+uuid.uuid4().hex
            destination=output/'results'/name
            destination.parent.mkdir(exist_ok=True)
            response['coordinate_path']=str(destination/'coordinates.csv')
            atomic_json(stage/'result.json',response)
            atomic_json(stage/'COMPLETED.json',{'result_sha256':file_hash(stage/'result.json')})
            # Only this directory rename publishes a completed numerical result.
            stage.rename(destination)
            (output/'cache').mkdir(exist_ok=True)
            atomic_json(output/'cache'/f'{key}.json',{'result_directory':name})
        atomic_json(output/'response.json',response)
    except (OSError,ValueError,TypeError) as exc:
        response=base_response(req)
        response.update(status='failed',termination=f'publication_error: {str(exc)[:1000]}',
                        elapsed_seconds=time.monotonic()-started,peak_memory_bytes=peak)
        try:atomic_json(output/'response.json',response)
        except OSError:pass
    finally:
        if stage.exists():shutil.rmtree(stage)
    return response


def main():
    parser=argparse.ArgumentParser(description='Standalone LGS paper-form JSON/CSV adapter')
    parser.add_argument('request',type=Path)
    parser.add_argument('--output-dir',type=Path)
    args=parser.parse_args()
    request_path=args.request.resolve()
    raw={};output=None;lock=None
    try:
        with request_path.open('rb') as inp:data=inp.read(1024**2+1)
        if len(data)>1024**2:raise InputFailure('request exceeds 1 MiB')
        raw=parse_json(data)
        if not isinstance(raw,dict):raise InputFailure('request must be an object')
        configured=args.output_dir if args.output_dir is not None else raw.get('output_directory','lgs3d-output')
        if not isinstance(configured,(str,Path)):raise InputFailure('invalid output_directory')
        output=(request_path.parent/configured).resolve()
        inputs=[request_path]+[(request_path.parent/raw[k]).resolve() for k in INPUT_BOUNDS if isinstance(raw.get(k),str)]
        if output/'response.json' in inputs:
            output=None
            raise InputFailure('output response would overwrite an input file')
        output.mkdir(parents=True,exist_ok=True)
        lock=(output/'.adapter.lock').open('a+')
        try:fcntl.flock(lock,fcntl.LOCK_EX|fcntl.LOCK_NB)
        except BlockingIOError:
            print(json.dumps({'schema_version':1,'status':'busy','termination':'output_directory_locked'}))
            return 2
        req=normalize_request(raw,request_path.parent)
        req['output_directory']=str(output)
        def interrupt(signum,frame):raise KeyboardInterrupt()
        signal.signal(signal.SIGTERM,interrupt)
        response=supervise(req,output)
    except KeyboardInterrupt:
        response={'schema_version':1,'status':'cancelled','termination':'interrupted','variant':VARIANT,'warnings':[]}
    except (OSError,ValueError,TypeError) as exc:
        response={'schema_version':1,'status':getattr(exc,'status','invalid_input'),'termination':str(exc)[:1000],
                  'variant':VARIANT,'warnings':[]}
        # If output could overwrite an input, do not publish anything there.
        if output and str(exc)!='output response would overwrite an input file':
            try:atomic_json(output/'response.json',response)
            except OSError:pass
    finally:
        if lock:lock.close()
    print(json.dumps(response,allow_nan=False))
    return 0 if response['status']=='completed' else 1
