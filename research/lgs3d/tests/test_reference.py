import hashlib
import itertools
import json
import math
from pathlib import Path
import unittest
import numpy as np
from numpy.testing import assert_allclose, assert_array_equal
from lgs_reference import reference2d as ref, evaluator2d as ev
from lgs_reference.upstream import ROOT, TinyGraph, load_functions, load_native


def fixtures():
    for case in json.loads((ROOT/'fixtures/small_graphs.json').read_text())['cases']:
        n = len(case['vertex_ids'])
        a = np.zeros((n,n), dtype=np.int64)
        for i,j in case['edges']:
            a[i,j] = a[j,i] = 1
        yield case['name'], a, np.array(case['initial'], dtype=np.float64)


def enumerate_powers(a, depth):
    """Independent integer walk enumeration, no matrix powers."""
    n = len(a)
    current = [[int(i == j) for j in range(n)] for i in range(n)]
    result = []
    for _ in range(depth):
        current = [[sum(current[i][k]*int(a[k,j]) for k in range(n))
                    for j in range(n)] for i in range(n)]
        result.append(np.array(current,dtype=np.float64))
    return result


class ReferenceTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.up = load_functions()
        cls.native = load_native()

    def test_vendor_hashes(self):
        pin = json.loads((ROOT/'upstream.json').read_text())
        for filename,digest in pin['files_sha256'].items():
            self.assertEqual(hashlib.sha256((ROOT/'vendor/L2G'/filename).read_bytes()).hexdigest(),digest)

    def test_neighborhoods_all_k_and_depths(self):
        for name,a,_ in fixtures():
            for depth in (1,2,5,10):
                powers = enumerate_powers(a,depth)
                small_expected = sum(p/p.max() for p in powers)
                large_expected = sum(powers)
                g = TinyGraph(a)
                small = self.up.find_neighbors_small(g,1,depth,0)
                large = self.up.find_neighbors_large(g,1,depth,0)
                assert_allclose(small,small_expected,rtol=1e-13,atol=1e-13)
                assert_allclose(large,large_expected,rtol=1e-10,atol=1e-9)
                assert_allclose(ref.walk_scores(a,depth),small,rtol=0,atol=0)
                assert_allclose(ref.walk_scores(a,depth,'large'),large,rtol=0,atol=0)
                for k in range(1,len(a)):
                    with self.subTest(graph=name,depth=depth,k=k):
                        flags = self.up.find_neighbors(g,k,depth)
                        assert_array_equal(ref.neighbor_flags(small,k),flags)
                        assert_array_equal(flags,flags.T)
                        assert_array_equal(np.diag(flags),0)
                        self.assertTrue(np.isin(flags,[0,1]).all())

    def test_decay_cancels_and_paths_differ(self):
        a = list(fixtures())[1][1]
        powers = enumerate_powers(a,5)
        normalized = sum(p/p.max() for p in powers)
        for decay in (0.1,0.5,0.9):
            assert_allclose(sum((decay**i*p)/(decay**i*p).max()
                               for i,p in enumerate(powers,1)),normalized)
        paper = sum(0.1**i*p for i,p in enumerate(powers,1))
        self.assertGreater(np.max(abs(paper-normalized)),0.1)
        self.assertGreater(np.max(abs(sum(powers)-normalized)),0.1)

    def test_index_zero_and_k_plus_one_counterexamples(self):
        # No ties: center (index 1) selects targets 3 and 2, skips 0.
        scores = np.array([[1,9,2,3],[9,1,8,10],[2,8,1,7],[3,10,7,1]],dtype=float)
        flags = ref.neighbor_flags(scores,1)
        self.assertEqual(flags[1].sum(),3)  # union may exceed k even without defect
        # Two vertices at depth 1: self absent from prefix; code still works here.
        # Directed guard is exhibited by a matrix whose only route to pair (0,2)
        # is selection of 0 by 2. 0's row chooses 1 and 3 instead.
        scores = np.array([[0,10,8,9],[10,0,1,2],[8,1,0,2],[9,2,2,0]],dtype=float)
        self.assertEqual(ref.neighbor_flags(scores,1)[2,0],0)
        # Both non-self top entries are admitted even with k=1.
        self.assertEqual(ref.neighbor_flags(scores,1)[0,1],1)
        self.assertEqual(ref.neighbor_flags(scores,1)[0,3],1)

    def test_all_neighbor_endpoint_and_zero_score_exception(self):
        for _,a,_ in fixtures():
            flags = self.up.find_neighbors(TinyGraph(a),len(a)-1,10)
            assert_array_equal(flags,np.ones_like(a)-np.eye(len(a),dtype=int))
        a = list(fixtures())[1][1]
        flags = self.up.find_neighbors(TinyGraph(a),3,1)
        self.assertEqual(flags[0,3],0)  # unreachable within truncation

    def test_ties_reference_has_no_stable_id_promise(self):
        _,a,_ = list(fixtures())[3]
        base = self.up.find_neighbors(TinyGraph(a),1,2)
        changed = False
        for permutation in itertools.permutations(range(len(a))):
            p = list(permutation)
            result = self.up.find_neighbors(TinyGraph(a[np.ix_(p,p)]),1,2)
            inv = np.argsort(p)
            if not np.array_equal(base,result[np.ix_(inv,inv)]):
                changed = True
                break
        self.assertTrue(changed)

    def test_schedules_including_short_budget_anomaly(self):
        for name,a,_ in fixtures():
            d = ev.shortest_paths(a)
            flags = self.up.find_neighbors(TinyGraph(a),1,5)
            for count in (1,2,5,30,60,200):
                with self.subTest(graph=name,count=count):
                    assert_allclose(ref.schedule(d,count),self.native.test_schedule(d,flags,count),rtol=5e-15,atol=1e-14)
        d = ev.shortest_paths(list(fixtures())[1][1])
        self.assertEqual(ref.schedule(d,1)[0],1)
        self.assertEqual(ref.schedule(d,2)[0],9)

    def test_each_pair_step_capped_and_uncapped(self):
        rng = np.random.default_rng(812)
        for _ in range(60):
            initial = rng.normal(size=(2,2))
            target = float(rng.integers(1,5))
            for attractive,eta,alpha in itertools.product((0,1),(0.03,1.0,12.0),(0.0,0.2,0.6)):
                expected = self.native.test_pair(initial.ravel(),target,attractive,eta,alpha)
                actual = ref.pair_step(initial.copy(),0,1,target,attractive,eta,alpha)
                assert_allclose(actual,expected,rtol=2e-13,atol=2e-13)

    def test_full_kernel_with_shared_schedule_and_initialization(self):
        for name,a,x in fixtures():
            d = ev.shortest_paths(a)
            for k in sorted({1,len(a)-1}):
                w = self.up.find_neighbors(TinyGraph(a),k,5)
                steps = self.native.test_schedule(d,w,60)
                for seed in (17,314,2026):
                    order = self.native.test_orders(d,w,60,seed)
                    expected = self.native.test_run(d,w,x.ravel(),steps,0.6,seed)
                    actual = ref.run(x,d,w,steps,order,0.6)
                    with self.subTest(graph=name,k=k,seed=seed):
                        assert_allclose(actual,expected,rtol=2e-10,atol=2e-10)
                        again = self.native.test_run(d,w,x.ravel(),steps,0.6,seed)
                        assert_array_equal(expected,again)
                        self.assertTrue(np.isfinite(actual).all())

    def test_original_public_entry_matches_internal_wrapper(self):
        for _,a,_ in fixtures():
            d = ev.shortest_paths(a)
            w = self.up.find_neighbors(TinyGraph(a),1,5)
            for count in (2,60,200):
                np.random.seed(421)
                initial = np.random.uniform(-1,1,2*len(a))
                steps = self.native.test_schedule(d,w,count)
                expected = self.native.test_run(d,w,initial,steps,0.6,812)
                np.random.seed(421)
                self.native.test_seed(812)
                actual = self.native.L2G_opt(d,w,n_iter=count)
                assert_array_equal(actual,expected)

    def test_unbiased_shuffle_claim_is_false(self):
        a = list(fixtures())[1][1]
        d = ev.shortest_paths(a)
        w = np.ones(d.shape,dtype=np.int16)
        initial = [(i,j) for i in range(len(a)) for j in range(i)]
        for seed in range(20):
            first = self.native.test_orders(d,w,1,seed)[0]
            self.assertCountEqual(first,initial)
            self.assertTrue(all(a != b for a,b in zip(initial,first)))

    def test_effective_gradient_against_original_small_step(self):
        x = np.array([[0.2,-0.1],[2.1,0.8]],dtype=float)
        d = np.array([[0.,3.],[3.,0.]])
        alpha = float(np.float32(0.6))
        eta = 1e-4
        for flag in (0,1):
            w = np.array([[0,flag],[flag,0]],dtype=np.int16)
            _,gradient = ev.evaluate(x,d,w,alpha)
            result = self.native.test_pair(x.ravel(),3,flag,eta,alpha)
            assert_allclose((x-result)/eta,gradient,rtol=1e-10,atol=1e-11)
            _,paper_gradient = ev.evaluate(x,d,w,alpha,'paper')
            self.assertGreater(np.max(abs(gradient-paper_gradient)),0.01)

    def test_objectives_and_gradients_with_step_sensitivity(self):
        alpha = 0.6
        for _,a,x in fixtures():
            d = ev.shortest_paths(a)
            w = self.up.find_neighbors(TinyGraph(a),1,5)
            value,gradient = ev.upstream_diagnostic(x,d,w,alpha)
            assert_allclose(value,self.up.get_cost(x,d,w,alpha),rtol=1e-12,atol=1e-12)
            functions = [(lambda X: ev.upstream_diagnostic(X,d,w,alpha)[0],gradient)]
            for model in ('code','paper'):
                _,g = ev.evaluate(x,d,w,alpha,model)
                functions.append((lambda X,m=model: ev.evaluate(X,d,w,alpha,m)[0],g))
            for function,g in functions:
                for step in (1e-4,1e-5,1e-6):
                    numeric = ev.finite_difference(function,x,step)
                    assert_allclose(numeric,g,rtol=3e-5,atol=3e-7)

    def test_full_gradient_matches_accumulated_native_pair_forces(self):
        for _,a,x in fixtures():
            d = ev.shortest_paths(a)
            w = self.up.find_neighbors(TinyGraph(a),1,5)
            alpha,eta = float(np.float32(0.6)),1e-4
            _,gradient = ev.evaluate(x,d,w,alpha)
            observed = np.zeros_like(x)
            for i in range(len(x)):
                for j in range(i):
                    pair = x[[i,j]].copy()
                    update = self.native.test_pair(pair.ravel(),d[i,j],int(w[i,j]),eta,alpha)
                    observed[[i,j]] += (pair-update)/eta
            assert_allclose(observed,gradient,rtol=1e-9,atol=1e-10)

    def test_objective_reductions_invariances_and_controlled_descent(self):
        rotation = np.array([[0.6,-0.8],[0.8,0.6]])
        reflection = np.diag([1.,-1.])
        for _,a,x in fixtures():
            d = ev.shortest_paths(a)
            w = np.ones(a.shape,dtype=np.int16)-np.eye(len(a),dtype=np.int16)
            paper = sum((np.linalg.norm(x[i]-x[j])-d[i,j])**2 for i in range(len(x)) for j in range(i))
            code = sum((np.linalg.norm(x[i]-x[j])/d[i,j]-1)**2/4 for i in range(len(x)) for j in range(i))
            assert_allclose(ev.evaluate(x,d,w,0.6,'paper')[0],paper)
            assert_allclose(ev.evaluate(x,d,w,0.6)[0],code)
            w = self.up.find_neighbors(TinyGraph(a),1,5)
            for model in ('paper','code'):
                cost,g = ev.evaluate(x,d,w,0.6,model)
                for transform in (rotation,reflection):
                    value,g2 = ev.evaluate(x@transform+np.array([2.,-3.]),d,w,0.6,model)
                    assert_allclose(cost,value,rtol=1e-13,atol=1e-13)
                    assert_allclose(g@transform,g2,rtol=1e-13,atol=1e-13)
                self.assertLess(ev.evaluate(x-1e-5*g,d,w,0.6,model)[0],cost)

    def test_public_initialization_and_collision_failures(self):
        _,a,x = list(fixtures())[0]
        d = ev.shortest_paths(a)
        w = np.ones(a.shape,dtype=np.int16)
        with self.assertRaisesRegex(ValueError,'truth value'):
            self.native.L2G_opt(d,w,init_pos=x.ravel(),n_iter=1)
        for flag in (0,1):
            result = self.native.test_pair(np.zeros(4),1.,flag,0.1,0.6)
            self.assertFalse(np.isfinite(result).all())
        # Near-zero behavior is faithfully unbounded, not safeguarded.
        tiny = np.array([0.,0.,1e-9,0.])
        result = self.native.test_pair(tiny,1.,0,0.1,0.6)
        self.assertGreater(np.max(abs(result)),1e16)
        with self.assertRaisesRegex(ValueError,'2D'):
            ref.pair_step(np.zeros((2,3)),0,1,1,1,0.1,0.6)

    def test_upstream_metrics_and_analytical_discrepancies(self):
        _,a,x = list(fixtures())[1]
        d = ev.shortest_paths(a)
        pairs = [(np.linalg.norm(x[i]-x[j]),d[i,j]) for i in range(len(x)) for j in range(i)]
        scale = sum(r/t for r,t in pairs)/sum((r/t)**2 for r,t in pairs)
        expected = sum((scale*r/t-1)**2 for r,t in pairs)/len(pairs)
        assert_allclose(self.up.get_stress(x,d),expected,rtol=1e-13,atol=1e-13)
        # A straight 3-vertex path has perfect radius-2 neighborhoods by paper.
        path = np.array([[0,1,0],[1,0,1],[0,1,0]])
        straight = np.array([[0.,0.],[1.,0.],[2.,0.]])
        assert_allclose(self.up.get_neighborhood(TinyGraph(path),straight,2),2/3)
        # 2 clusters on a 4-vertex path: 1 of 3 graph edges crosses -> paper 2/3.
        clusters = [{0,1},{2,3}]
        actual = self.up.get_cluster_distances(TinyGraph(a),clusters)
        self.assertEqual(actual[0,1],0.)
        self.assertNotAlmostEqual(actual[0,1],2/3)


if __name__ == '__main__':
    unittest.main()
