"""Deterministic process-race and persistent-denial regressions; own children only."""
import os
from pathlib import Path
import subprocess
import sys
import pytest
sys.path.insert(0,str(Path(__file__).resolve().parents[1]/'suitesparse_phase1'))
import run_pilot
sys.path.insert(0,str(Path(__file__).resolve().parent))
import run_lgs
from common import read_json

def test_disappearing_descendant_does_not_abort_owned_worker(tmp_path,monkeypatch):
    real=run_pilot.psutil.Process
    class Gone:
        pid=99999999
        def memory_info(self): raise run_pilot.psutil.NoSuchProcess(self.pid)
    class Parent:
        def __init__(self,pid): self.proc=real(pid);self.pid=pid
        def memory_info(self): return self.proc.memory_info()
        def children(self,recursive): return [Gone()]
    monkeypatch.setattr(run_pilot.psutil,'Process',Parent)
    result=run_pilot.supervise([sys.executable,'-c','import time;time.sleep(.25)'],tmp_path)
    assert result['status']=='completed' and result['exit_code']==0
    assert result['vanished_process_reads']>=1

def test_denied_live_rss_uses_bounded_measured_fallback(monkeypatch):
    class Denied:
        pid=123
        def memory_info(self): raise run_pilot.psutil.AccessDenied(self.pid)
        def is_running(self): return True
        def status(self): return run_pilot.psutil.STATUS_RUNNING
    calls=[]
    def query(command,**kwargs):
        calls.append((command,kwargs));return '16384\n'
    monkeypatch.setattr(run_pilot.subprocess,'check_output',query)
    assert run_pilot.owned_process_rss(Denied())==(16384*1024,'ps_fallback')
    assert len(calls)==1 and calls[0][1]['timeout']==.5
    assert calls[0][0]==['ps','-o','rss=','-p','123']
    class Exited(Denied):
        def is_running(self): return False
    assert run_pilot.owned_process_rss(Exited())==(0,'vanished')
    assert len(calls)==1

def test_persistent_denial_kills_reaps_and_reports_failure(tmp_path,monkeypatch):
    real=run_pilot.psutil.Process;owned=[]
    class Denied:
        def __init__(self,pid): self.pid=pid;owned.append(pid)
        def memory_info(self): raise run_pilot.psutil.AccessDenied(self.pid)
        def is_running(self): return True
        def status(self): return run_pilot.psutil.STATUS_RUNNING
        def children(self,recursive): return []
    def unavailable(*args,**kwargs): raise subprocess.TimeoutExpired(args[0],.5)
    monkeypatch.setattr(run_pilot.psutil,'Process',Denied)
    monkeypatch.setattr(run_pilot.subprocess,'check_output',unavailable)
    result=run_pilot.supervise([sys.executable,'-c','import time;time.sleep(30)'],tmp_path)
    assert result['status']=='failed' and 'memory_telemetry_unavailable' in result['reason']
    assert result['exit_code']!=0
    assert result['peak_rss_bytes'] is None and result['memory_complete_samples']==0
    assert owned and all(not run_pilot.psutil.pid_exists(pid) for pid in owned)
    with pytest.raises(ChildProcessError): os.waitpid(owned[0],os.WNOHANG)

def test_matrix_publishes_terminal_rows_after_supervisor_exceptions(tmp_path,monkeypatch):
    def graphs(root):
        return [run_lgs.fixture(root,name,4,[(i,j) for i in range(4) for j in range(i+1,4)])
                for name in ['first','second']]
    monkeypatch.setattr(run_lgs,'make_fixtures',graphs)
    monkeypatch.setattr(run_lgs.subprocess,'check_output',lambda args,**kwargs: '' if 'status' in args else 'test-commit\n')
    def denied(*args,**kwargs): raise run_pilot.psutil.AccessDenied(123)
    monkeypatch.setattr(run_lgs,'supervise',denied)
    run_lgs.run(tmp_path,dict(accepted_commit='fixture'),'validation')
    rows=read_json(tmp_path/'phase04_validation_results.json')['runs']
    assert len(rows)==6 and all(r['status']=='failed' for r in rows)
    for row in rows:
        manifest=read_json(Path(row['run_dir'])/'manifest.json')
        assert manifest['status']=='failed' and 'AccessDenied' in manifest['reason']
        assert manifest['peak_rss_bytes'] is None


def test_report_preserves_unknown_and_measured_failed_outcomes(tmp_path,monkeypatch):
    import csv
    from common import atomic_json
    from report import report
    def denied(process): raise run_pilot.MonitoringUnavailable('test live RSS denial')
    monkeypatch.setattr(run_pilot,'owned_process_rss',denied)
    unknown=tmp_path/'unknown';unknown.mkdir()
    timing=run_pilot.supervise([sys.executable,'-c','import time;time.sleep(30)'],unknown)
    assert timing['status']=='failed' and timing['peak_rss_bytes'] is None
    atomic_json(unknown/'manifest.json',timing)
    measured=tmp_path/'measured';measured.mkdir()
    numeric=dict(status='resource_limited',reason='memory_limit',peak_rss_bytes=3*1024**3)
    atomic_json(measured/'manifest.json',numeric)
    rows=[dict(graph_id='fixture',method='metric_mds',seed=i,run_dir=str(path),**outcome)
          for i,path,outcome in [(17,unknown,timing),(29,measured,numeric)]]
    atomic_json(tmp_path/'pilot_results.json',dict(commit='fixture',runs=rows))
    atomic_json(tmp_path/'catalog/gallery.json',dict(gallery_count=0))
    atomic_json(tmp_path/'cohort.json',dict(records=[]))
    (tmp_path/'FINDINGS.md').write_text('old report')
    report(tmp_path)
    text=(tmp_path/'FINDINGS.md').read_text()
    assert 'old report' not in text
    assert 'unknown (no complete RSS sample)' in text and '3.000 GiB' in text
    assert timing['reason'] in text and 'memory_limit' in text
    with (tmp_path/'scores.csv').open() as stream: scores=list(csv.DictReader(stream))
    assert [r['status'] for r in scores]==['failed','resource_limited']
    assert scores[0]['peak_rss_bytes']=='' and int(scores[1]['peak_rss_bytes'])==3*1024**3
    failures=read_json(tmp_path/'termination_diagnostics.json')['stopped_runs']
    assert failures[0]['peak_rss_bytes'] is None and failures[0]['reason']==timing['reason']
    assert failures[1]['peak_rss_bytes']==3*1024**3
    assert (tmp_path/'deliverables.json').exists()
