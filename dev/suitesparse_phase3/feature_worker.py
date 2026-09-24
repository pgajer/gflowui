"""Phase 03 subprocess using the accepted graph preparation and metric contract."""
from pathlib import Path
import sys
import importlib.util
sys.path.insert(0,str(Path(__file__).resolve().parents[1]/'suitesparse_phase1'))
from common import read_json
# Load the accepted component pipeline by its exact file.
spec=importlib.util.spec_from_file_location('pilot_worker',Path(__file__).resolve().parents[1]/'suitesparse_phase1'/'worker.py')
pilot=importlib.util.module_from_spec(spec);spec.loader.exec_module(pilot)
sys.path.insert(0,str(Path(__file__).resolve().parent))
from adapters import embed

if __name__=='__main__':
    pilot.run(read_json(sys.argv[1]),embedder=embed,input_type='landmark_distance_features')
