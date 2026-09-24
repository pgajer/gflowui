"""Build an unchanged upstream Cython kernel plus explicit test entry points.

Generated C, binaries and setup files stay in .cache/oracle. No installation.
"""
from pathlib import Path
import hashlib
import json
import os
import subprocess
import sys

ROOT = Path(__file__).resolve().parents[1]
WRAPPERS = r'''
# Test-only entry points appended by the LGS reproduction project.
# The original algorithm above this marker is unchanged.
def test_seed(unsigned int seed):
    srand(seed)

def test_schedule(ar[double, ndim=2] d, ar[np.int16_t, ndim=2] w,
                  int count, double eps=0.01):
    cdef Pair *pairs = get_pairs(d,w)
    cdef ar[double] result = schedule_convergent(pairs, len(d)*(len(d)-1)//2, 30, eps, count)
    free(pairs)
    return result

def test_run(ar[double, ndim=2] d, ar[np.int16_t, ndim=2] w,
             ar[double] initial, ar[double] steps, float alpha, unsigned int seed):
    cdef Pair *pairs = get_pairs(d,w)
    srand(seed)
    cdef ar[double] result = sgd(initial.copy(), pairs, steps, len(d), len(d)*(len(d)-1)//2, alpha)
    free(pairs)
    return result.reshape((len(d),2))

def test_pair(ar[double] initial, double target, int attractive,
              double step, float alpha):
    cdef Pair pair
    pair.u = 0
    pair.v = 1
    pair.d = target
    pair.w = attractive
    pair.in_neighbor = attractive
    cdef ar[double] steps = np.array([step], dtype=np.float64)
    return sgd(initial.copy(), &pair, steps, 2, 1, alpha).reshape((2,2))

def test_orders(ar[double, ndim=2] d, ar[np.int16_t, ndim=2] w,
                int epochs, unsigned int seed):
    cdef Pair *pairs = get_pairs(d,w)
    cdef int count = len(d)*(len(d)-1)//2
    cdef int t,p
    result = []
    srand(seed)
    for t in range(epochs):
        fisheryates(pairs,count)
        result.append([(pairs[p].u, pairs[p].v) for p in range(count)])
    free(pairs)
    return result
'''


def main():
    build = ROOT / '.cache' / 'oracle'
    build.mkdir(parents=True, exist_ok=True)
    pin = json.loads((ROOT / 'upstream.json').read_text())
    source = ROOT / 'vendor/L2G/modules/cython_l2g.pyx'
    digest = hashlib.sha256(source.read_bytes()).hexdigest()
    assert digest == pin['files_sha256']['modules/cython_l2g.pyx']
    (build / 'lgs_upstream_oracle.pyx').write_text(source.read_text() + WRAPPERS)
    (build / 'setup.py').write_text('''from setuptools import setup, Extension
from Cython.Build import cythonize
import numpy
setup(ext_modules=cythonize(Extension('lgs_upstream_oracle',
 sources=['lgs_upstream_oracle.pyx'], include_dirs=[numpy.get_include()]),
 compiler_directives={'language_level':3}))
''')
    subprocess.run([sys.executable, 'setup.py', 'build_ext', '--inplace'],
                   cwd=build, check=True, env={**os.environ, 'TMPDIR': str(build)})
    print(build)


if __name__ == '__main__':
    main()
