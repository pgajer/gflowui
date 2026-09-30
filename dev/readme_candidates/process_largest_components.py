"""Fit and activate extracted components with a fitting-source consistency check."""
from pathlib import Path
import json,hashlib,subprocess,sys
base=Path(__file__).resolve().parent
out=Path('/Users/pgajer/current_projects/suitesparse_embedding_comparison/largest_components_95')
source=Path('/Users/pgajer/current_projects/grip')
snapshot={str(p):hashlib.sha256(p.read_bytes()).hexdigest() for folder in ['R','src'] for p in (source/folder).glob('*') if p.is_file() and p.suffix in ['.R','.cpp','.h']}
(out/'fitting_source_snapshot.json').write_text(json.dumps(snapshot,indent=2))
subprocess.run([sys.executable,str(base/'embed_largest_components.py')],check=True)
assert all(hashlib.sha256(Path(p).read_bytes()).hexdigest()==h for p,h in snapshot.items()),'Fitting sources changed'
subprocess.run([sys.executable,str(base/'activate_largest_components.py')],check=True)
print('RECOVERY COMPLETE',flush=True)
