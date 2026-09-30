"""Acquire, fit, verify, and merge the three next size intervals."""
from pathlib import Path
import subprocess,sys,hashlib,json
base=Path(__file__).resolve().parent
root=Path('/Users/pgajer/current_projects/suitesparse_embedding_comparison')
source=Path('/Users/pgajer/current_projects/grip')
snapshot={str(p):hashlib.sha256(p.read_bytes()).hexdigest() for folder in ['R','src'] for p in (source/folder).glob('*') if p.is_file() and p.suffix in ['.R','.cpp','.h']}
(root/'size_500_800_fitting_sources.json').write_text(json.dumps(snapshot,indent=2))
for lo,hi in [(500,600),(600,700),(700,800)]:
 for step in ['download_size_range.py','embed_size_range.py','activate_size_range.py']:
  print(step,lo,hi,flush=True)
  if step=='activate_size_range.py':
   assert all(hashlib.sha256(Path(p).read_bytes()).hexdigest()==h for p,h in snapshot.items()), 'Fitting source changed during study'
  subprocess.run([sys.executable,str(base/step),str(lo),str(hi)],check=True)
print('ALL THREE COHORTS COMPLETE',flush=True)
