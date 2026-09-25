"""Append-only, serial 30-GiB SFDP cohort, with no elapsed-time cutoff."""
import argparse
from contextlib import contextmanager
import fcntl
from pathlib import Path
import subprocess
import sys
import time

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE.parent/'suitesparse_phase1'))
from common import atomic_json, read_json, sha256, identity
from run_pilot import supervise, environment, allocation_preflight, verified_cached
from adapter import executable

GRAPHS = ['HB/illc1033', 'HB/lock1074', 'HB/lock_700', 'Meszaros/nemscem',
          'HB/sstmodel', 'Bomhof/circuit_2']
SEEDS = [17, 29, 43]
LIMITS = dict(seconds=None, memory_bytes=30*1024**3)
INDEX = 'sfdp_results.json'


@contextmanager
def serial_lock(root):
    with (Path(root)/'.sfdp.lock').open('a') as stream:
        try:
            fcntl.flock(stream, fcntl.LOCK_EX | fcntl.LOCK_NB)
        except BlockingIOError as exc:
            raise RuntimeError('another SFDP runner is active') from exc
        try:
            yield
        finally:
            fcntl.flock(stream, fcntl.LOCK_UN)


def capture_environment():
    env = environment()
    binary = Path(executable())
    env['graphviz'] = dict(executable=str(binary), executable_sha256=sha256(binary),
        version=subprocess.run([str(binary), '-V'], capture_output=True, text=True, check=True).stderr.strip())
    # Homebrew's dot/sfdp share a binary; retain its complete Graphviz installation
    # (including loaded layout plugins) rather than identifying just the launcher.
    install = binary.parent.parent
    env['graphviz']['installation'] = str(install)
    env['graphviz']['installed_files_sha256'] = {
        str(p.relative_to(install)): sha256(p) for p in sorted(install.rglob('*')) if p.is_file()}
    return env


def run(root, graphs=GRAPHS):
    root = Path(root).resolve()
    if not graphs or len(set(graphs)) != len(graphs) or any(g not in GRAPHS for g in graphs):
        raise ValueError('select distinct graphs from the six-graph cohort')
    with serial_lock(root):
        if subprocess.check_output(['git', 'status', '--porcelain'], cwd=HERE, text=True).strip():
            raise RuntimeError('commit source before experiment')
        commit = subprocess.check_output(['git', 'rev-parse', 'HEAD'], cwd=HERE, text=True).strip()
        index = root/INDEX
        rows = read_json(index)['runs'] if index.exists() else []
        if any(r['graph_id'] in graphs for r in rows):
            raise ValueError('requested graph already recorded; preserve existing attempts')
        cohort = read_json(root/'combined_cohort.json')
        identities = {r['graph_id']: r['graph_sha256'] for r in cohort['records'] if r['status']=='completed'}
        env = capture_environment()
        code = {str(p.relative_to(HERE.parent)): sha256(p)
                for directory in [HERE, HERE.parent/'suitesparse_phase1', HERE.parent/'suitesparse_phase5',
                                  HERE.parent/'suitesparse_phase3']
                for p in sorted(directory.iterdir()) if p.suffix in ('.py', '.R', '.txt')}
        for graph in graphs:
            folder = root/'graphs'/graph.replace('/', '__')
            info = read_json(folder/'graph.json')
            if identities.get(graph) != info['graph_sha256']:
                raise ValueError('graph differs from accepted cohort')
            for seed in SEEDS:
                request = dict(schema_version=1, method='sfdp', dimension=3, seed=seed,
                    graph_dir=str(folder), graph_sha256=info['graph_sha256'], landmarks=64,
                    commit=commit, code=code, environment=env, limits=LIMITS,
                    evaluation='suitesparse-uniform-pairs-v1' if graph in GRAPHS[-2:] else 'exact',
                    # Preserve the accepted classical placement of tiny components,
                    # explicitly identified by the common pipeline, including isolates.
                    small_component_cutoff=5, attempt_label='30 GiB; no time limit; serial',
                    resource_admission=dict(memory_bytes=LIMITS['memory_bytes'], seconds=None, serial=True),
                    allocation_preflight=allocation_preflight(info, 64, LIMITS['memory_bytes']))
                key = identity(request)
                dest = root/'runs'/graph.replace('/', '__')/f'sfdp_seed{seed}_{key[:12]}'
                if dest.exists():
                    dest = dest.with_name(dest.name+'_retry_'+str(time.time_ns()))
                dest.mkdir(parents=True)
                request['output'] = str(dest)
                atomic_json(dest/'request.json', request)
                print(graph, seed, 'running', flush=True)
                if not request['allocation_preflight']['admitted']:
                    timing = dict(status='resource_limited', reason='prepared_allocation_preflight',
                        elapsed_seconds=0., peak_rss_bytes=None, limits=LIMITS)
                else:
                    start = time.monotonic()
                    try:
                        timing = supervise([sys.executable, str(HERE/'worker.py'), str(dest/'request.json')],
                            dest, seconds=None, memory=LIMITS['memory_bytes'])
                    except Exception as exc:
                        timing = dict(status='failed', reason=f'{type(exc).__name__}: {exc}',
                            elapsed_seconds=time.monotonic()-start, peak_rss_bytes=None, limits=LIMITS)
                if timing['status']=='completed' and not (dest/'result.json').exists():
                    timing.update(status='failed', reason='missing result')
                if timing['status']=='failed' and not timing.get('reason'):
                    timing['reason'] = f"worker exit {timing.get('exit_code')}; see process.log"
                artifacts = {str(p.relative_to(dest)): sha256(p) for p in sorted(dest.rglob('*'))
                             if p.is_file() and p.name!='manifest.json' and 'numba_cache' not in p.parts}
                atomic_json(dest/'manifest.json', dict(run_key=key, request=request, artifacts=artifacts, **timing))
                if timing['status']=='completed' and not verified_cached(dest, key):
                    raise ValueError('completed SFDP failed artifact validation')
                row = dict(graph_id=graph, method='sfdp', seed=seed, run_dir=str(dest),
                    **{k: timing.get(k) for k in ['status','reason','elapsed_seconds','peak_rss_bytes']})
                if timing['status']=='completed':
                    row['scores'] = read_json(dest/'result.json')['summary']
                rows.append(row)
                atomic_json(index, dict(schema_version=1, commit=commit, limits=LIMITS, serial=True, runs=rows))
                print(graph, seed, timing['status'], round(timing['elapsed_seconds'],2), flush=True)
    return rows


if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('root')
    parser.add_argument('--graphs', nargs='+', choices=GRAPHS, default=GRAPHS)
    args = parser.parse_args()
    run(args.root, args.graphs)
