"""Build-only portable recipes in fresh private directories, never system install."""
import argparse
import json
import os
from pathlib import Path
import shutil
import subprocess
import sys
import hashlib

PINS={'ncvis':'d5c8b96e3b3bb131e12cf2d46daa8635ae1ce339',
      'largevis':'feb8121e8eb9652477f7f564903d189ee663796f'}

def replace_once(path,old,new):
    text=path.read_text()
    if text.count(old)!=1: raise ValueError('upstream source does not match expected patch: '+str(path))
    path.write_text(text.replace(old,new))

def build(method,source,destination,gsl=None):
    source=Path(source).resolve();destination=Path(destination).resolve()
    revision=subprocess.check_output(['git','-C',str(source),'rev-parse','HEAD'],text=True).strip()
    if revision!=PINS[method]: raise ValueError('source commit mismatch')
    if subprocess.check_output(['git','-C',str(source),'status','--porcelain'],text=True).strip():
        raise ValueError('source snapshot must be unmodified')
    shutil.copytree(source,destination,ignore=shutil.ignore_patterns('.git'))
    if method=='ncvis':
        file=destination/'setup.py'
        replace_once(file,'"-fopenmp=libiomp5",','"-Xpreprocessor", "-fopenmp", "-I/opt/homebrew/opt/libomp/include",')
        replace_once(file,'libraries = ["m", "iomp5"]','libraries = ["m", "omp"]')
        replace_once(destination/'wrapper'/'ncvis.pyx','cdef cnp.uintp_t[:] n_noise_arr',
                     'cdef size_t[:] n_noise_arr')
        env=os.environ.copy()
        env['LDFLAGS']='-L/opt/homebrew/opt/libomp/lib -Wl,-rpath,/opt/homebrew/opt/libomp/lib'
        subprocess.run([sys.executable,'-m','pip','install','--no-build-isolation','--no-deps','.'],
                       cwd=destination,env=env,check=True)
    else:
        if not gsl: raise ValueError('private GSL prefix required')
        file=destination/'Linux'/'ANNOY'/'annoylib.h'
        replace_once(file,'lseek64(', 'lseek(')
        file=destination/'Linux'/'LargeVis.cpp'
        replace_once(file,'Running propagation %d/%d%c','Running propagation %d/%lld%c')
        replace_once(file,'gsl_rng_set(gsl_r, 314159265);',
            'const char *seed_override = getenv("GFLOWUI_LARGEVIS_SEED");\n'
            '\tgsl_rng_set(gsl_r, seed_override ? strtoul(seed_override, NULL, 10) : 314159265);')
        gsl=Path(gsl).resolve()
        subprocess.run(['c++','LargeVis.cpp','main.cpp','-o','LargeVis','-std=c++11','-O3',
                        '-pthread','-I'+str(gsl/'include'),'-L'+str(gsl/'lib'),
                        '-Wl,-rpath,'+str(gsl/'lib'),'-lgsl','-lgslcblas','-lm'],
                       cwd=destination/'Linux',check=True)
    files={str(p.relative_to(destination)):hashlib.sha256(p.read_bytes()).hexdigest()
           for p in sorted(destination.rglob('*')) if p.is_file()}
    (destination/'build_manifest.json').write_text(json.dumps(dict(method=method,commit=revision,
        recipe='private-native-v1',python=sys.version,files=files),indent=2)+'\n')

if __name__=='__main__':
    p=argparse.ArgumentParser();p.add_argument('method',choices=PINS)
    p.add_argument('source');p.add_argument('destination');p.add_argument('--gsl')
    a=p.parse_args();build(a.method,a.source,a.destination,a.gsl)
