#!/usr/bin/env python3
"""Serial adapter experiment; all starts, seeds, settings and failures retained."""
import argparse
import json
from pathlib import Path
import subprocess
import sys
import time
sys.path.insert(0,str(Path(__file__).resolve().parents[1]))
from lgs_paper.adapter import canonical,digest,graph_hash,implementation_identity,atomic_json
ROOT=Path(__file__).resolve().parents[1]


def graph(name):
    if name=='grid49':
        n=49;edges=[(i,j) for i in range(n) for j in range(i+1,n)
                    if (j==i+7 or (j==i+1 and i//7==j//7))]
    elif name=='two_cliques48':
        n=48;edges=[(i,j) for i in range(n) for j in range(i+1,n) if i//24==j//24]+[(23,24)]
    else:
        n=int(name.removeprefix('path'));edges=[(i,i+1) for i in range(n-1)]
    return n,edges


def make_request(directory,name,k,seed,epochs=60):
    directory.mkdir(parents=True,exist_ok=True)
    n,edges=graph(name)
    ids=[f'v{i:04d}' for i in range(n)]
    vertices=('vertex_id\n'+'\n'.join(ids)+'\n').encode()
    links=('source,target,length\n'+''.join(f'{ids[i]},{ids[j]},1\n' for i,j in edges)).encode()
    (directory/'vertices.csv').write_bytes(vertices);(directory/'edges.csv').write_bytes(links)
    req={'schema_version':1,'graph_id':name,'graph_sha256':graph_hash(digest(vertices),digest(links)),
         'vertex_file':'vertices.csv','vertex_sha256':digest(vertices),
         'edge_file':'edges.csv','edge_sha256':digest(links),'dimension':3,'seed':seed,'locality_k':k,
         'parameters':{'walk_depth':10,'walk_decay':.1,'repulsion_alpha':.2,'epochs':epochs},
         'job_limits':{'wall_seconds':600,'memory_mib':2048},'output_directory':'artifacts'}
    atomic_json(directory/'request.json',req)
    return directory/'request.json',n,len(edges)


def run_case(output,name,k,seed,epochs=60):
    request,n,edges=make_request(output/f'{name}-k{k}-s{seed}',name,k,seed,epochs)
    run=subprocess.run([sys.executable,str(ROOT/'run.py'),str(request)],capture_output=True,text=True,timeout=615)
    (request.parent/'stdout.json').write_text(run.stdout)
    (request.parent/'stderr.txt').write_text(run.stderr)
    response=json.loads(run.stdout)
    result={key:response.get(key) for key in ('status','termination','cache_hit','elapsed_seconds','peak_memory_bytes',
            'epochs_completed','quality','warnings','coordinate_path','coordinate_sha256','resource_measurement','attractive_components','implementation')}
    result.update(graph=name,vertices=n,edges=edges,seed=seed,locality_k=k,locality_fraction=k/(n-1),
                  request_path=str(request),response_sha256=digest(run.stdout.encode()),exit_code=run.returncode)
    if response['status']=='completed':
        history=response['objective']['history']
        result.update(initial_objective=history[0]['objective'],final_objective=history[-1]['objective'],
                      objective_increasing_epochs=sum(b['objective']>a['objective'] for a,b in zip(history,history[1:])))
    print(f'{name} k={k} seed={seed}: {response["status"]}, {response.get("elapsed_seconds",0):.2f}s',flush=True)
    return result


def main():
    parser=argparse.ArgumentParser();parser.add_argument('--output',type=Path,required=True)
    args=parser.parse_args();out=args.output.resolve();out.mkdir(parents=True,exist_ok=True)
    summary={'implementation':implementation_identity(),'dimension':3,'epochs':60,'seeds':[17,314,2026],
             'requested_k':[16,32,64,128,256],'caps':{'wall_seconds':600,'memory_mib':2048},
             'initialization':'canonical PCG64 seed; independent order stream; same start per graph/seed across k',
             'runs':[],'scaling':[]}
    for name in ('path48','grid49','two_cliques48'):
        n,_=graph(name)
        ks=sorted({min(k,n-1) for k in summary['requested_k']}|{n-1})
        for k in ks:
            for seed in summary['seeds']:
                summary['runs'].append(run_case(out,name,k,seed))
                atomic_json(out/'summary.json',summary)
    # Conservative empirical projection: all-vertex pair guard gives cubic
    # worst-case cost; safety factor 2. This is admission evidence, not a bound.
    completed=[r for r in summary['runs'] if r['status']=='completed' and not r['cache_hit']]
    for n in (128,2000):
        estimated=max((r['elapsed_seconds']*(n/r['vertices'])**3*2 for r in completed),default=float('inf'))
        estimate_bytes=128*1024**2+128*n*n+512*(n-1)
        decision={'vertices':n,'projected_seconds':estimated if estimated!=float('inf') else None,
                  'preflight_memory_bytes':estimate_bytes,'method':'2 * max(measured_seconds * (target_n / measured_n)^3)',
                  'executed':False}
        if estimated<600 and estimate_bytes<2048*1024**2:
            result=run_case(out,f'path{n}',min(16,n-1),17)
            decision.update(executed=True,result=result)
            if result['status']=='completed' and not result['cache_hit']:completed.append(result)
        else:decision['reason']='conservative_time_or_memory_preflight_does_not_permit_run'
        summary['scaling'].append(decision);atomic_json(out/'summary.json',summary)
    return 0 if all(r['status']=='completed' for r in summary['runs']) else 1


if __name__=='__main__':raise SystemExit(main())
