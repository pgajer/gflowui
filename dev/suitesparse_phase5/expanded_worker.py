"""Existing validated adapters, with a separately versioned shared-pair scorer."""
from pathlib import Path
import hashlib
import sys
sys.path.insert(0,str(Path(__file__).resolve().parents[1]/'suitesparse_phase1'))
import worker as pilot
from common import read_json
sys.path.insert(0,str(Path(__file__).resolve().parents[1]/'suitesparse_phase3'))
from adapters import embed as additional,METHODS
sys.path.insert(0,str(Path(__file__).resolve().parent))
import sampled_metrics as sample

def scorer(z,d,p,edges,ids):
    # Disjoint component IDs give independent streams, shared across every method.
    seed=(sample.SEED+int(hashlib.sha256('\n'.join(ids).encode()).hexdigest()[:8],16))%2**32
    return sample.score_component(z,d,p,edges,ids,seed=seed)

if __name__=='__main__':
    request=read_json(sys.argv[1]);method=request['method']
    pilot.run(request,embedder=additional if method in METHODS else pilot.embed,
        input_type='landmark_distance_features' if method in METHODS and method!='trimap_graph' or method=='lle' else 'original_graph_distances_or_edges',
        scorer=scorer,aggregator=sample.aggregate)
