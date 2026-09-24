"""Main-project metrics around the accepted portable LGS process."""
from pathlib import Path
import sys
sys.path.insert(0,str(Path(__file__).resolve().parents[1]/'suitesparse_phase1'))
from common import read_json
from worker import run
sys.path.insert(0,str(Path(__file__).resolve().parent))
from lgs_adapter import make_embedder

if __name__=='__main__':
    request=read_json(sys.argv[1])
    run(request,embedder=make_embedder(request),input_type='original_unit_graph; experimental_lgs-paper-union-v1')
