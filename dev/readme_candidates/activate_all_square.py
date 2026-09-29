"""Append candidates to the current viewer without rebuilding or dropping prior assets."""
from pathlib import Path
import sys,datetime
base=Path(__file__).resolve().parents[1]
sys.path.insert(0,str(base/'suitesparse_project'))
import export_viewer as exp
from common import read_json,atomic_json
root=Path('/Users/pgajer/current_projects/suitesparse_embedding_comparison')
original=read_json(root/'viewer_manifest.json')
backup=root/'viewer_manifests'/('before_square_candidates_'+datetime.datetime.now().strftime('%Y%m%d_%H%M%S')+'.json')
atomic_json(backup,original)
save=exp.atomic_json
def staged(path,obj):
 if Path(path).name=='viewer_manifest.json':path=root/'square_400_500/viewer_manifest.json'
 save(path,obj)
exp.atomic_json=staged
exp.build(root,['square_candidate_results.json'],'square_candidate_cohort.json')
new=read_json(root/'square_400_500/viewer_manifest.json')
for field,key in [('graphs','id'),('runs','id'),('indexes','path')]:
 merged={r[key]:r for r in original[field]}
 merged.update({r[key]:r for r in new[field]})
 original[field]=list(merged.values())
original['artifacts'].update(new['artifacts'])
original['notes']+=' Full square 400–500 cohort: connected 400–500 vertex unit-edge graphs; full native SGD with uniform weights, 1000 iterations, seeds 11/29/43; exact diagnostics. No convergence implied.'
atomic_json(root/'viewer_manifest.json',original)
print('Activated',len(original['graphs']),'graphs; preserved prior runs; backup:',backup)
