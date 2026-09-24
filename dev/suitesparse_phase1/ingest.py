"""Import the entire metadata-eligible gallery subset, without substitution."""
import argparse
from pathlib import Path
from common import read_json, atomic_json
from graphs import import_record

if __name__ == '__main__':
    p=argparse.ArgumentParser()
    p.add_argument('root')
    args=p.parse_args()
    root=Path(args.root)
    catalog=read_json(root/'catalog/gallery.json')
    if any(x.get('error') for x in catalog['records']):
        raise RuntimeError('incomplete inventory; resolve metadata errors first')
    results=[]
    for row in catalog['records']:
        if not row['eligible']:
            continue
        try:
            info=import_record(row,root)
            results.append(dict(graph_id=info['graph_id'],status='completed',
                                graph_sha256=info['graph_sha256']))
            print(info['graph_id'],info['n_vertices'],info['n_edges'],info['component_sizes'],flush=True)
        except Exception as exc:
            results.append(dict(graph_id=row['graph_id'],status='failed',error=str(exc)))
            print(results[-1],flush=True)
    atomic_json(root/'cohort.json',dict(schema_version=1,selection='all_metadata_eligible_gallery_members',
         target_count='6–8',actual_count=len(results),records=results,
         limitation=f'{len(results)} gallery matrices meet the frozen limits; no non-gallery substitutes.'))
