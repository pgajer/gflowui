import sys
from pathlib import Path
import pytest
sys.path.insert(0,str(Path(__file__).resolve().parent))
import run_mds_budget as budget
from common import atomic_json,read_json,sha256
import run_pilot

def test_unlimited_wall_time_still_enforces_memory_and_finite_limits(tmp_path,monkeypatch):
    real=run_pilot.time.monotonic
    origin=real()
    # Apparent elapsed time crosses 600 seconds; no wall-clock stop when None.
    calls=[]
    def clock():
        calls.append(1);return origin+len(calls)*10000+(real()-origin)
    monkeypatch.setattr(run_pilot.time,'monotonic',clock)
    cmd=[sys.executable,'-c','import time;time.sleep(.3)']
    result=run_pilot.supervise(cmd,tmp_path,seconds=None,memory=30*1024**3)
    assert result['status']=='completed' and result['limits']['seconds'] is None
    assert result['elapsed_seconds']>600
    result=run_pilot.supervise(cmd,tmp_path,seconds=None,memory=1)
    assert result['status']=='resource_limited' and result['reason']=='memory_limit'
    result=run_pilot.supervise(cmd,tmp_path,seconds=600,memory=30*1024**3)
    assert result['reason']=='timeout'
    for invalid in [0,-1,float('inf'),float('nan')]:
        with pytest.raises(ValueError):run_pilot.supervise(cmd,tmp_path,seconds=invalid)

def test_serial_lock_rejects_another_runner_and_releases(tmp_path):
    with budget.serial_lock(tmp_path):
        with pytest.raises(RuntimeError,match='active'):
            with budget.serial_lock(tmp_path):pass
    with budget.serial_lock(tmp_path):pass

@pytest.mark.parametrize('failed_seed',[None,29])
def test_retry_order_parentage_and_history_preservation(tmp_path,monkeypatch,failed_seed):
    monkeypatch.setattr(budget.subprocess,'check_output',lambda args,**kw: '' if 'status' in args else 'commit\n')
    monkeypatch.setattr(budget,'capture_environment',lambda:dict(fixture=True))
    rows=[]
    for graph in budget.GRAPHS:
        atomic_json(tmp_path/'graphs'/graph.replace('/','__')/'graph.json',dict(graph_sha256=graph,component_sizes=[5]))
        for method in ['metric_mds','metric_mds_edge_kk']:
            for seed in budget.SEEDS:
                rows.append(dict(graph_id=graph,method=method,seed=seed,status='resource_limited' if method=='metric_mds' else 'unavailable'))
    atomic_json(tmp_path/'pilot_results.json',dict(runs=rows[:6]))
    atomic_json(tmp_path/'phase05_results.json',dict(runs=rows[6:]))
    old={p:sha256(tmp_path/p) for p in ['pilot_results.json','phase05_results.json']}
    calls=[]
    def supervise(command,dest,seconds,memory):
        r=read_json(dest/'request.json');calls.append((r['graph_dir'],r['method'],r['seed']))
        assert seconds is None and memory==30*1024**3
        assert Path(command[1]).name==('worker.py' if 'Meszaros' in r['graph_dir'] else 'expanded_worker.py')
        if r['method']=='metric_mds' and r['seed']==failed_seed:
            return dict(status='resource_limited',reason='memory_limit',elapsed_seconds=3.,peak_rss_bytes=31*1024**3,limits=budget.LIMITS)
        if r['method']=='metric_mds_edge_kk':
            parent=Path(r['initial_run']);m=read_json(parent/'manifest.json')
            assert m['request']['seed']==r['seed'] and sha256(parent/'manifest.json')==r['initial_manifest_sha256']
        atomic_json(dest/'result.json',dict(summary={}))
        for name in ['coords_raw.csv','coords_display.csv','vertices.json']:(dest/name).write_text('fixture')
        return dict(status='completed',reason=None,elapsed_seconds=3.,peak_rss_bytes=123,limits=budget.LIMITS)
    monkeypatch.setattr(budget,'supervise',supervise)
    result=budget.run(tmp_path)
    assert len(result)==18
    assert sum(r['status']=='completed' for r in result)==(18 if failed_seed is None else 12)
    assert sum(r['status']=='unavailable' for r in result)==(0 if failed_seed is None else 3)
    assert [x[1:] for x in calls]==[(m,s) for _ in budget.GRAPHS for m in ['metric_mds','metric_mds_edge_kk'] for s in budget.SEEDS if not(m=='metric_mds_edge_kk' and s==failed_seed)]
    assert old=={p:sha256(tmp_path/p) for p in old}
    with pytest.raises(ValueError,match='already recorded'):budget.run(tmp_path)
