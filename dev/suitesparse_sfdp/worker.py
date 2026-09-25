"""SFDP adapter through the accepted component preparation/evaluation pipeline."""
from pathlib import Path
import importlib.util
import sys

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE.parent/'suitesparse_phase1'))
from common import read_json
spec = importlib.util.spec_from_file_location('pilot_worker', HERE.parent/'suitesparse_phase1'/'worker.py')
pilot = importlib.util.module_from_spec(spec)
spec.loader.exec_module(pilot)
sys.path.insert(0, str(HERE))
from adapter import embed

if __name__ == '__main__':
    request = read_json(sys.argv[1])
    kwargs = {}
    if request['evaluation'] == 'suitesparse-uniform-pairs-v1':
        sys.path.insert(0, str(HERE.parent/'suitesparse_phase5'))
        from expanded_worker import scorer
        from sampled_metrics import aggregate
        kwargs = dict(scorer=scorer, aggregator=aggregate)
    elif request['evaluation'] != 'exact':
        raise ValueError('unknown evaluation policy')
    pilot.run(request, embedder=embed, input_type='original_graph_edges', **kwargs)
