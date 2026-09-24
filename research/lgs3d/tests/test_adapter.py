"""Black-box contract, corruption, resource and process-lifetime regressions."""
import copy
import csv
import io
import json
import os
from pathlib import Path
import signal
import subprocess
import sys
import tempfile
import time
import unittest
import numpy as np
from jsonschema import Draft202012Validator
from lgs_paper.adapter import digest,graph_hash,normalize_request,cache_identity,canonical
from lgs_paper.metrics import evaluate_quality

ROOT=Path(__file__).resolve().parents[1]
PYTHON=str(ROOT/'.venv/bin/python')
REQUEST_SCHEMA=Draft202012Validator(json.loads((ROOT/'schemas/request-v1.json').read_text()))
RESPONSE_SCHEMA=Draft202012Validator(json.loads((ROOT/'schemas/response-v1.json').read_text()))


class AdapterTests(unittest.TestCase):
    def setUp(self):
        (ROOT/'.cache').mkdir(exist_ok=True)
        self.tmp=tempfile.TemporaryDirectory(dir=ROOT/'.cache',prefix='adapter-test-')
        self.p=Path(self.tmp.name)
        fixture=ROOT/'fixtures/adapter_k4'
        for name in ('vertices.csv','edges.csv','request.json'):
            (self.p/name).write_bytes((fixture/name).read_bytes())
        self.req=json.loads((self.p/'request.json').read_text())
        self.req['parameters']['epochs']=4
        self.req['output_directory']='output'

    def tearDown(self):self.tmp.cleanup()

    def write_request(self):
        path=self.p/'request.json';path.write_text(json.dumps(self.req));return path

    def command(self):return [PYTHON,str(ROOT/'run.py'),str(self.write_request())]

    def run_adapter(self,status='completed'):
        run=subprocess.run(self.command(),capture_output=True,text=True,timeout=15)
        self.assertTrue(run.stdout,run.stderr)
        response=json.loads(run.stdout)
        RESPONSE_SCHEMA.validate(response)
        self.assertEqual(response['status'],status,response)
        self.assertEqual(run.returncode,0 if status=='completed' else 1)
        if status!='completed':
            self.assertNotIn('coordinate_path',response);self.assertNotIn('coordinate_sha256',response)
        return response

    def change_file(self,name,data):
        (self.p/name).write_bytes(data)
        self.req['vertex_sha256']=digest((self.p/'vertices.csv').read_bytes())
        self.req['edge_sha256']=digest((self.p/'edges.csv').read_bytes())
        self.req['graph_sha256']=graph_hash(self.req['vertex_sha256'],self.req['edge_sha256'])

    def test_success_cache_and_repeatability(self):
        for dim in (2,3):
            self.req['dimension']=dim
            a=self.run_adapter();b=self.run_adapter()
            self.assertFalse(a['cache_hit']);self.assertTrue(b['cache_hit'])
            self.assertEqual(a['coordinate_sha256'],b['coordinate_sha256'])
            self.assertEqual(a['objective'],b['objective'])
            rows=list(csv.reader(io.StringIO(Path(a['coordinate_path']).read_text())))
            self.assertEqual(rows[0],['vertex_id','x','y']+(['z'] if dim==3 else []))
            self.assertEqual([r[0] for r in rows[1:]],['d','a','c','b'])
            self.assertTrue(np.isfinite(np.array([r[1:] for r in rows[1:]],float)).all())
            directory=Path(a['coordinate_path']).parent
            receipt=json.loads((directory/'COMPLETED.json').read_text())
            self.assertEqual(receipt['result_sha256'],digest((directory/'result.json').read_bytes()))
            self.assertEqual(a['coordinate_sha256'],digest(Path(a['coordinate_path']).read_bytes()))
            self.assertTrue(a['implementation']['commit']);self.assertTrue(a['input_hashes']['vertex_sha256'])
            self.assertTrue(a['environment']['os_release']);self.assertTrue(a['environment']['numpy_blas'])
            self.req['output_directory']='repeat'+str(dim)
            c=self.run_adapter()
            self.assertEqual(a['coordinate_sha256'],c['coordinate_sha256'])
            self.req['output_directory']='output'

    def test_cache_corruption_and_partial_files(self):
        a=self.run_adapter();Path(a['coordinate_path']).write_text('partial')
        b=self.run_adapter();self.assertFalse(b['cache_hit'])
        self.assertIn('invalid_cache_entry_ignored',b['warnings'])
        self.assertEqual(a['coordinate_sha256'],b['coordinate_sha256'])
        (Path(b['coordinate_path']).parent/'result.json').write_text('{}')
        c=self.run_adapter();self.assertFalse(c['cache_hit'])
        self.assertIn('invalid_cache_entry_ignored',c['warnings'])
        partial=self.p/'output/.attempt-unfinished';partial.mkdir();(partial/'coordinates.csv').write_text('partial')
        self.assertTrue(self.run_adapter()['cache_hit']);self.assertTrue(partial.exists())

    def test_key_covers_all_scientific_identity(self):
        req=normalize_request(self.req,self.p)
        identity=cache_identity(req,['d','a','c','b']);key=digest(canonical(identity))
        changes={'dimension':2,'seed':314,'locality_k':2,'initial_coordinate_sha256':'a'*64,
                 'graph_sha256':'b'*64,'vertex_sha256':'c'*64,'edge_sha256':'d'*64,'graph_id':'other'}
        for name,value in changes.items():
            changed={**req,name:value}
            self.assertNotEqual(key,digest(canonical(cache_identity(changed,['d','a','c','b']))),name)
        changed=copy.deepcopy(req);changed['parameters']['repulsion_alpha']=.7
        self.assertNotEqual(key,digest(canonical(cache_identity(changed,['d','a','c','b']))))
        self.assertNotEqual(key,digest(canonical(cache_identity(req,['a','b','c','d']))))
        changed={**req,'job_limits':{'wall_seconds':10,'memory_mib':1024}}
        self.assertEqual(key,digest(canonical(cache_identity(changed,['d','a','c','b']))))

    def test_reject_bad_input(self):
        original=copy.deepcopy(self.req)
        for field,value,status in [('dimension',4,'unsupported'),('dimension',True,'invalid_input'),
                ('locality_k',0,'invalid_input'),('locality_k',4,'invalid_input'),
                ('seed',-1,'invalid_input'),('vertex_sha256','0'*64,'invalid_input'),
                ('graph_sha256','0'*64,'invalid_input')]:
            with self.subTest(field=field,value=value):
                self.req={**original,field:value};self.run_adapter(status)
        self.req=original
        self.change_file('edges.csv',b'source,target,length\nd,a,2\n');self.run_adapter('unsupported')
        for data in (b'source,target,length\nd,a,nan\n',b'source,target,length\nd,a,0\n',
                     b'source,target,length\nd,a,1\na,d,1\n',b'source,target,length\nd,d,1\n',
                     b'source,target,length\nd,unknown,1\n',b'source,target,length\nd,a,1\n'):
            self.change_file('edges.csv',data);self.run_adapter('invalid_input')
        self.change_file('vertices.csv',b'vertex_id\nd\nd\n');self.run_adapter('invalid_input')
        self.change_file('vertices.csv',b'vertex_id\nd\n');self.run_adapter('unsupported')

    def test_initial_coordinates_and_controlled_failure(self):
        self.req['initial_coordinate_file']='initial.csv'
        def initial(data):
            (self.p/'initial.csv').write_bytes(data);self.req['initial_coordinate_sha256']=digest(data)
        initial(b'vertex_id,x,y,z\nd,0,0,0\na,0,0,0\nc,1,0,0\nb,0,1,0\n')
        a=self.run_adapter('failed');self.assertIn('collision',a['termination'])
        initial(b'vertex_id,x,y,z\nd,0,0,0\na,1,0,0\nc,0,1,0\nb,0,0,1\n')
        self.assertFalse(self.run_adapter()['cache_hit']);self.assertTrue(self.run_adapter()['cache_hit'])
        initial(b'vertex_id,x,y,z\nd,nan,0,0\na,1,0,0\nc,0,1,0\nb,0,0,1\n')
        self.run_adapter('invalid_input')
        initial(b'vertex_id,x,y,z\na,0,0,0\nd,1,0,0\nc,0,1,0\nb,0,0,1\n')
        self.run_adapter('invalid_input')
        self.req['initial_coordinate_sha256']='0'*64;self.run_adapter('invalid_input')

    def test_schema_integral_spellings_and_exact_seeds(self):
        REQUEST_SCHEMA.check_schema(REQUEST_SCHEMA.schema)
        RESPONSE_SCHEMA.check_schema(RESPONSE_SCHEMA.schema)
        base=copy.deepcopy(self.req)
        for seed in (17,2**53+1,2**64-1):
            self.req=copy.deepcopy(base);self.req['seed']=seed
            REQUEST_SCHEMA.validate(self.req)
            ordinary=self.run_adapter()
            # Write decimal tokens directly so the test itself never rounds
            # large integer seeds through float before they reach the CLI.
            self.req['schema_version']=1.0;self.req['dimension']=3.0
            self.req['locality_k']=3.0
            self.req['parameters'].update(epochs=4.0,transition_epochs=30.0,
                                         max_backtracks=60.0,walk_depth=10.0,
                                         max_pair_displacement=1,collision_distance=1e-12)
            self.req['job_limits']={'wall_seconds':600.0,'memory_mib':2048.0}
            REQUEST_SCHEMA.validate(self.req)
            path=self.write_request()
            payload=path.read_text().replace(f'"seed": {seed}',f'"seed": {seed}.0')
            path.write_text(payload)
            if seed==17:REQUEST_SCHEMA.validate(json.loads(payload))
            run=subprocess.run([PYTHON,str(ROOT/'run.py'),str(path)],capture_output=True,text=True,timeout=15)
            result=json.loads(run.stdout);RESPONSE_SCHEMA.validate(result)
            self.assertEqual(run.returncode,0,run.stderr)
            self.assertEqual(result['seed'],seed)
            self.assertEqual(result['cache_key'],ordinary['cache_key'])
            self.assertEqual(result['coordinate_sha256'],ordinary['coordinate_sha256'])
            self.assertTrue(result['cache_hit'])
        for field,value in [('dimension',3.5),('dimension',True),('seed',2**64),
                            ('seed',False),('locality_k',1.5),('schema_version',1.5)]:
            self.req={**base,field:value};self.run_adapter('invalid_input')
        for field,value in [('epochs',2.5),('epochs',True),('walk_depth',-1),
                            ('max_backtracks',.5),('transition_epochs',1.0)]:
            self.req=copy.deepcopy(base);self.req['parameters'][field]=value
            self.run_adapter('invalid_input')
        # A truly fractional spelling must not acquire integer validity from
        # rounding to 3.0 in binary float64.
        self.req=copy.deepcopy(base);path=self.write_request()
        path.write_text(path.read_text().replace('"dimension": 3','"dimension": 3.0000000000000000000001'))
        run=subprocess.run([PYTHON,str(ROOT/'run.py'),str(path)],capture_output=True,text=True,timeout=15)
        result=json.loads(run.stdout);RESPONSE_SCHEMA.validate(result)
        self.assertEqual(result['status'],'invalid_input')

    def test_excessive_json_nesting_is_structured_failure(self):
        path=self.p/'deep.json';path.write_text('['*10000+'0'+']'*10000)
        self.assertLess(path.stat().st_size,1024**2)
        run=subprocess.run([PYTHON,str(ROOT/'run.py'),str(path)],capture_output=True,text=True,timeout=15)
        result=json.loads(run.stdout);RESPONSE_SCHEMA.validate(result)
        self.assertEqual(run.returncode,1)
        self.assertEqual(result['status'],'invalid_input')
        self.assertIn('nesting',result['termination'])
        self.assertNotIn('coordinate_path',result);self.assertNotIn('coordinate_sha256',result)
        self.assertNotIn('Traceback',run.stderr)
        self.assertEqual(len(run.stdout.strip().splitlines()),1)

    def test_limits(self):
        self.req['job_limits']={'wall_seconds':.001}
        a=self.run_adapter('resource_limited');self.assertEqual(a['termination'],'wall_timeout')
        self.assertFalse((self.p/'output/cache').exists())
        self.req['job_limits']={'memory_mib':1}
        a=self.run_adapter('resource_limited');self.assertIn('memory_limit',a['termination'])
        self.req['job_limits']={'memory_mib':64}
        self.run_adapter('resource_limited')
        self.req['job_limits']={'wall_seconds':601};self.run_adapter('invalid_input')

    def wait_worker(self,proc):
        deadline=time.monotonic()+10
        while time.monotonic()<deadline:
            files=list((self.p/'output').glob('.attempt-*/worker.pid'))
            if files:return int(files[0].read_text())
            if proc.poll() is not None:self.fail('worker exited before interruption')
            time.sleep(.02)
        self.fail('worker did not start')

    def assert_pid_stopped(self,pid):
        deadline=time.monotonic()+5
        while time.monotonic()<deadline:
            result=subprocess.run(['ps','-o','stat=','-p',str(pid)],capture_output=True,text=True)
            if result.returncode!=0 or result.stdout.strip().startswith('Z'):return
            time.sleep(.05)
        self.fail(f'owned worker {pid} survived')

    def test_cancel_busy_and_no_orphan_on_parent_kill(self):
        self.req['parameters'].update(epochs=1000000,movement_tolerance=0.)
        for sig in (signal.SIGTERM,signal.SIGKILL):
            with self.subTest(signal=sig):
                proc=subprocess.Popen(self.command(),stdout=subprocess.PIPE,stderr=subprocess.PIPE,text=True)
                try:
                    worker=self.wait_worker(proc)
                    busy=subprocess.run(self.command(),capture_output=True,text=True,timeout=5)
                    self.assertEqual(busy.returncode,2);self.assertEqual(json.loads(busy.stdout)['status'],'busy')
                    proc.send_signal(sig);stdout,stderr=proc.communicate(timeout=10)
                    self.assert_pid_stopped(worker)
                    self.assertFalse((self.p/'output/cache').exists())
                    if sig==signal.SIGTERM:
                        result=json.loads(stdout);self.assertEqual(result['status'],'cancelled')
                        self.assertNotIn('coordinate_path',result)
                finally:
                    if proc.poll() is None:proc.kill();proc.communicate()
        self.req['parameters'].update(epochs=4)
        self.assertFalse(self.run_adapter()['cache_hit'])

    def test_input_response_never_overwritten(self):
        data=(self.p/'vertices.csv').read_bytes();(self.p/'response.json').write_bytes(data)
        self.req['vertex_file']='response.json';self.req['output_directory']='.'
        self.run_adapter('invalid_input');self.assertEqual((self.p/'response.json').read_bytes(),data)


class MetricTests(unittest.TestCase):
    def setUp(self):
        self.ids=['a','b','c'];self.d=np.array([[0,1,2],[1,0,1],[2,1,0]])
        self.a=(self.d==1).astype(int)

    def test_straight_and_folded_analytical_paths(self):
        straight=evaluate_quality(self.ids,self.a,self.d,np.array([[0,0],[1,0],[2,0]]))
        for field in ('euclidean_distance_error','relative_distance_stress','edge_length_error_identity',
                      'neighborhood_error_hops_1','neighborhood_error_hops_2'):
            self.assertEqual(straight[field],0.)
        folded=evaluate_quality(self.ids,self.a,self.d,np.array([[0,0],[1,0],[0,0]]))
        self.assertAlmostEqual(folded['euclidean_distance_error'],np.sqrt(2/3))
        self.assertAlmostEqual(folded['relative_distance_stress'],1/3)
        self.assertEqual(folded['edge_length_error_identity'],0.)
        self.assertAlmostEqual(folded['neighborhood_error_hops_1'],2/3)
        self.assertEqual(folded['neighborhood_error_hops_2'],0.)
        self.assertEqual(folded['zero_embedded_pair_count'],1)

    def test_scale_fit_and_collapse(self):
        x=np.array([[0,0],[.7,.2],[2.,1.]])
        a=evaluate_quality(self.ids,self.a,self.d,x);b=evaluate_quality(self.ids,self.a,self.d,7*x)
        for key in ('euclidean_distance_error','relative_distance_stress','neighborhood_error_hops_1'):
            self.assertAlmostEqual(a[key],b[key])
        self.assertNotEqual(a['edge_length_error_identity'],b['edge_length_error_identity'])
        collapsed=evaluate_quality(self.ids,self.a,self.d,np.zeros((3,3)))
        self.assertEqual(collapsed['status'],'unavailable');self.assertEqual(collapsed['reason'],'collapsed_layout')

    def test_scalar_distance_and_neighborhood_oracle(self):
        # Separate pair loops, fitted least squares and explicit set cardinalities.
        x=np.array([[0,0],[.3,1],[2,-.5]])
        result=evaluate_quality(self.ids,self.a,self.d,x)
        pairs=[(i,j) for i in range(3) for j in range(i+1,3)]
        r=[sum((float(x[i,c])-float(x[j,c]))**2 for c in range(2))**.5 for i,j in pairs]
        d=[float(self.d[i,j]) for i,j in pairs]
        s=sum(a*b for a,b in zip(r,d))/sum(a*a for a in r)
        t=sum(a/b for a,b in zip(r,d))/sum((a/b)**2 for a,b in zip(r,d))
        self.assertAlmostEqual(result['euclidean_distance_error'],(sum((s*a-b)**2 for a,b in zip(r,d))/sum(b*b for b in d))**.5)
        self.assertAlmostEqual(result['relative_distance_stress'],sum((t*a/b-1)**2 for a,b in zip(r,d))/3)


if __name__=='__main__':unittest.main()
