"""Publish curated context without rebuilding the gallery or changing its selection.

Usage: python3 publish_graph_annotations.py DATA_ROOT
Edit graph_annotations.json alongside this script to maintain the display text.
The research catalogue and its source evidence are retained separately at DATA_ROOT.
"""
import argparse
import hashlib
import json
from pathlib import Path


def publish(root):
    manifest_path = root / 'viewer_manifest.json'
    original = manifest_path.read_bytes()
    manifest = json.loads(original)
    source = json.loads(Path(__file__).with_name('graph_annotations.json').read_text())
    annotations = {g['id']: g for g in source['graphs']}
    assert len(annotations) == len(source['graphs']), 'Duplicate annotation IDs'
    rows = []
    for graph in manifest['graphs']:
        row = dict(annotations[graph['id']])
        row['graph_sha256'] = graph['graph_sha256']
        rows.append(row)
    payload = dict(schema_version=1, review_date=source['review_date'], graphs=rows)
    content = (json.dumps(payload, indent=2, ensure_ascii=False) + '\n').encode()
    sha = hashlib.sha256(content).hexdigest()
    relative = f'graph_annotations/{sha}.json'
    target = root / relative
    target.parent.mkdir(exist_ok=True)
    target.write_bytes(content)
    manifest.setdefault('artifacts', {})['graph_annotations.json'] = dict(path=relative, sha256=sha)
    backup = root / 'viewer_manifests' / ('before_annotations_' + hashlib.sha256(original).hexdigest() + '.json')
    backup.parent.mkdir(exist_ok=True)
    if not backup.exists():
        backup.write_bytes(original)
    assert manifest_path.read_bytes() == original, 'Gallery changed during publication; retry'
    staged = root / 'viewer_manifest.annotations.tmp'
    staged.write_text(json.dumps(manifest, indent=2, ensure_ascii=False) + '\n')
    staged.replace(manifest_path)
    print(f'Published annotations for {len(rows)} graphs; preserved {len(manifest["runs"])} run records.')


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('data_root', type=Path)
    publish(parser.parse_args().data_root.resolve())
