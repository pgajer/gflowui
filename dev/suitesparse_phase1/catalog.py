"""Metadata-first admission; never fetch graph archives during inventory."""
import argparse
from concurrent.futures import ThreadPoolExecutor
from datetime import datetime, timezone
from pathlib import Path
from urllib.parse import urljoin
import requests
from bs4 import BeautifulSoup
from common import atomic_json, sha256

ABOUT = 'https://sparse.tamu.edu/about'


def bounded_download(url, target, limit=100 * 1024**2):
    target = Path(target)
    target.parent.mkdir(parents=True, exist_ok=True)
    tmp = target.with_suffix(target.suffix + '.partial')
    try:
        with requests.get(url, stream=True, timeout=(15, 60)) as response:
            response.raise_for_status()
            declared = response.headers.get('Content-Length')
            if declared and int(declared) > limit:
                raise ValueError('declared transfer size exceeds cap')
            size = 0
            with tmp.open('wb') as stream:
                for chunk in response.iter_content(65536):
                    size += len(chunk)
                    if size > limit:
                        raise ValueError('streamed transfer exceeds cap')
                    stream.write(chunk)
        tmp.replace(target)
        return size
    finally:
        tmp.unlink(missing_ok=True)


def admission(rows, columns, nonzeros, max_vertices=2999):
    if min(rows, columns) < 1 or nonzeros < 0:
        return False, 'invalid metadata'
    n = rows if rows == columns else rows + columns
    if n > max_vertices:
        return False, 'vertex count is not below 3000' if max_vertices==2999 else f'vertex count exceeds {max_vertices}'
    # Website Nonzeros is full numerical support, not MM triangular storage.
    if nonzeros > 100000:
        return False, 'conservative full-nonzero edge bound exceeds 100000'
    return True, 'metadata eligible; conversion still required'


def parse_metadata(html, url):
    soup = BeautifulSoup(html, 'html.parser')
    fields = {}
    for row in soup.select('tr'):
        key, val = row.find('th'), row.find('td')
        if key and val:
            fields[key.get_text(' ', strip=True)] = val.get_text(' ', strip=True)
    def number(key):
        return int(fields[key].replace(',', ''))
    rows, columns, nnz = number('Num Rows'), number('Num Cols'), number('Nonzeros')
    eligible, reason = admission(rows, columns, nnz)
    links = [urljoin(url, a['href']) for a in soup.select('a[href]')
             if '/MM/' in a['href'] and a['href'].endswith('.tar.gz')]
    return dict(graph_id='/'.join(url.rstrip('/').split('/')[-2:]), url=url,
                rows=rows, columns=columns, nonzeros=nnz,
                n_vertices_bound=rows if rows == columns else rows + columns,
                metadata=fields, archive_url=links[0] if links else None,
                archive_bytes=None, eligible=eligible, admission_reason=reason,
                matrix_license='CC-BY-4.0', metadata_nonzeros_convention='full numerical support')


def inventory(root):
    root = Path(root)
    about = root / 'catalog' / 'about.html'
    bounded_download(ABOUT, about, 5 * 1024**2)
    soup = BeautifulSoup(about.read_text(), 'html.parser')
    gallery = soup.select_one('table.about-images')
    if gallery is None:
        raise ValueError('gallery selector missing; do not inventory unrelated links')
    urls = list(dict.fromkeys(urljoin(ABOUT, a['href']) for a in gallery.select('a[href]')))
    def one(url):
        token = '__'.join(url.split('/')[-2:])
        page = root / 'catalog' / (token + '.html')
        try:
            bounded_download(url, page, 5 * 1024**2)
            row = parse_metadata(page.read_text(), url)
            row.update(metadata_file=str(page), metadata_sha256=sha256(page))
            return row
        except Exception as exc:
            return dict(url=url, eligible=False, admission_reason='metadata unavailable', error=str(exc))
    with ThreadPoolExecutor(max_workers=4) as executor:
        records = list(executor.map(one, urls))
    result = dict(schema_version=1, source=ABOUT, retrieved_at=datetime.now(timezone.utc).isoformat(),
                  about_sha256=sha256(about), gallery_count=len(urls), records=records)
    atomic_json(root / 'catalog' / 'gallery.json', result)
    return result


if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('root')
    args = parser.parse_args()
    result = inventory(args.root)
    print('Gallery:', result['gallery_count'])
    for item in result['records']:
        if item['eligible'] or item.get('error'):
            print({k: item.get(k) for k in ('graph_id', 'rows', 'columns', 'nonzeros', 'error')})
