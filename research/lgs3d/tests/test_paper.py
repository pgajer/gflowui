import itertools
import math
import unittest
import numpy as np
from numpy.testing import assert_allclose, assert_array_equal
from lgs_paper import (prepare,Controls,NumericalFailure,objective_gradient,
                       optimize,schedule,safeguarded_pair)
from lgs_paper import reference
from lgs_reference.evaluator2d import evaluate as original_paper_evaluator


def path(n):
    a = np.zeros((n,n),dtype=int)
    for i in range(n-1):
        a[i,i+1] = a[i+1,i] = 1
    return a


def ids(n):
    return [f'vertex_{i:03}' for i in range(n)]


def graphs():
    rng = np.random.default_rng(7307)
    for n in range(2,11):
        a = path(n)
        for i in range(n):
            for j in range(i):
                if rng.random() < 0.25:
                    a[i,j] = a[j,i] = 1
        yield a


class PaperTests(unittest.TestCase):
    def test_walk_scores_against_exact_counts(self):
        for a in graphs():
            for depth,decay in itertools.product((1,2,5,10),(0.1,0.25)):
                p = prepare(ids(len(a)),a,1,depth,decay)
                assert_allclose(p.scores,reference.walk_scores(a,depth,decay),rtol=3e-14,atol=3e-14)
                assert_array_equal(p.directed.sum(axis=1),1)
                assert_array_equal(np.diag(p.directed),False)

    def test_k_union_multiplicity_and_zero_scores(self):
        a = path(6)
        for k in range(1,6):
            p = prepare(ids(6),a,k,1)
            assert_array_equal(p.directed.sum(axis=1),k)
            assert_array_equal(p.attractive,p.directed|p.directed.T)
            self.assertEqual(len(p.pairs),15)
            self.assertEqual(len(set(tuple(sorted(pair)) for pair in p.pairs)),15)
            self.assertEqual(sum(p.attractive[i,j] for i,j in p.pairs),p.attractive.sum()//2)
        p = prepare(ids(6),a,5,1)
        self.assertEqual(p.directed[0,5],True)  # zero score included
        self.assertEqual(p.attractive.sum(),30)

    def test_hand_computable_walk_scores_and_stable_id_ties(self):
        # Path3: s*A+s^2*A^2; leaf row [s^2,s,s^2].
        p = prepare(['z','m','a'],path(3),1,2,0.1)
        assert_allclose(p.scores,[[.01,.1,.01],[.1,.02,.1],[.01,.1,.01]],atol=1e-17)
        self.assertTrue(p.directed[1,2])  # 'a' wins tied leaf scores
        self.assertFalse(p.directed[1,0])
        q = prepare(['z','m','a'],path(3),2,1)
        self.assertTrue(q.directed[0,2])  # zero score accepted at endpoint

    def test_invalid_graph_parameters_and_small_components(self):
        valid = path(3)
        for k in (0,3,-1,True,1.5,float('nan')):
            with self.assertRaises(ValueError): prepare(ids(3),valid,k)
        for depth in (0,-1,False,1.5):
            with self.assertRaises(ValueError): prepare(ids(3),valid,1,depth)
        for decay in (0,1,-1,True,float('inf'),float('nan')):
            with self.assertRaises(ValueError): prepare(ids(3),valid,1,walk_decay=decay)
        for names in (['a','a','b'],['a','', 'b'],['a',1,'b']):
            with self.assertRaises(ValueError): prepare(names,valid,1)
        for a in (np.zeros((3,3)),valid+np.eye(3),valid*.5,valid*2,
                  np.array([[0,1,0],[0,0,1],[0,1,0]]),valid.astype(complex),
                  np.full((3,3),np.nan),np.full((3,3),np.inf)):
            with self.assertRaises(ValueError): prepare(ids(3),a,1)
        with self.assertRaisesRegex(ValueError,'small_component'):
            prepare(['a'],[[0]],1)
        with self.assertRaises(NumericalFailure):
            prepare(ids(8),np.ones((8,8))-np.eye(8),1,400,0.99)

    def test_bfs_distances_against_analytical_path(self):
        p = prepare(ids(10),path(10),2)
        assert_array_equal(p.distances,abs(np.arange(10)[:,None]-np.arange(10)[None,:]))

    def test_2d_and_3d_objective_gradient_step_sensitivity(self):
        rng = np.random.default_rng(162)
        for a in graphs():
            for dimension in (2,3):
                x = rng.normal(size=(len(a),dimension))
                p = prepare(ids(len(a)),a,min(2,len(a)-1))
                cost,g = objective_gradient(p,x,0.2)
                expected,eg = reference.evaluate(x,p.distances,p.attractive,0.2)
                assert_allclose(cost,expected,rtol=2e-13,atol=2e-13)
                assert_allclose(g,eg,rtol=2e-13,atol=2e-13)
                for h in (1e-4,1e-5,1e-6):
                    fd = reference.finite_difference(lambda X: reference.evaluate(X,p.distances,p.attractive,0.2)[0],x,h)
                    assert_allclose(g,fd,rtol=3e-5,atol=3e-7)
                if dimension == 2:
                    old,og = original_paper_evaluator(x,p.distances,p.attractive,0.2,'paper')
                    assert_allclose(cost,old,rtol=2e-13,atol=2e-13)
                    assert_allclose(g,og,rtol=2e-13,atol=2e-13)

    def test_dimension_general_evaluator_and_non_scale_invariance(self):
        p = prepare(ids(4),path(4),1)
        x = np.random.default_rng(313).normal(size=(4,5))
        cost,g = objective_gradient(p,x)
        expected,eg = reference.evaluate(x,p.distances,p.attractive,.2)
        assert_allclose(cost,expected)
        assert_allclose(g,eg)
        self.assertGreater(abs(objective_gradient(p,2*x)[0]-cost),.1)

    def test_translation_orthogonal_invariance_and_gradient_equivariance(self):
        rng = np.random.default_rng(835)
        p = prepare(ids(7),path(7),2)
        for dim in (2,3):
            x = rng.normal(size=(7,dim))
            value,g = objective_gradient(p,x)
            q,_ = np.linalg.qr(rng.normal(size=(dim,dim)))
            for transform in (q,q@np.diag([-1]+[1]*(dim-1))):
                actual,g2 = objective_gradient(p,x@transform+np.arange(dim))
                assert_allclose(actual,value,rtol=1e-13,atol=1e-13)
                assert_allclose(g2,g@transform,rtol=1e-13,atol=1e-13)
            assert_allclose(g.sum(axis=0),0,atol=1e-13)

    def test_vertex_permutation_including_tied_graphs(self):
        rng = np.random.default_rng(345)
        for a in graphs():
            n = len(a)
            names = ids(n)[::-1]
            p = prepare(names,a,min(2,n-1),5)
            perm = rng.permutation(n)
            inverse = np.argsort(perm)
            p2 = prepare([names[i] for i in perm],a[np.ix_(perm,perm)],min(2,n-1),5)
            assert_array_equal(p.scores,p2.scores[np.ix_(inverse,inverse)])
            assert_array_equal(p.directed,p2.directed[np.ix_(inverse,inverse)])
            x = rng.normal(size=(n,3))
            value,g = objective_gradient(p,x)
            value2,g2 = objective_gradient(p2,x[perm])
            self.assertEqual(value,value2)
            assert_array_equal(g,g2[inverse])
            control = Controls(epochs=3)
            first = optimize(p,3,14,controls=control)
            second = optimize(p2,3,14,controls=control)
            assert_array_equal(first.coordinates,second.coordinates[inverse])
            self.assertEqual(first.history,second.history)

    def test_strictly_untied_vertex_permutation(self):
        edges = [(0,1),(0,2),(0,3),(0,4),(1,4),(1,5),(1,7),(2,5),
                 (2,7),(3,4),(3,5),(3,6),(5,6)]
        a = np.zeros((8,8),dtype=int)
        for i,j in edges: a[i,j]=a[j,i]=1
        names = ids(8)
        p = prepare(names,a,2)
        self.assertTrue(all(len(set(np.delete(p.scores[i],i)))==7 for i in range(8)))
        perm = np.array([7,2,4,0,6,5,1,3]); inverse = np.argsort(perm)
        other = prepare([names[i] for i in perm],a[np.ix_(perm,perm)],2)
        assert_array_equal(p.directed,other.directed[np.ix_(inverse,inverse)])
        first = optimize(p,3,18,controls=Controls(epochs=4))
        second = optimize(other,3,18,controls=Controls(epochs=4))
        assert_array_equal(first.coordinates,second.coordinates[inverse])

    def test_all_neighbors_raw_stress_reduction(self):
        for a in graphs():
            p = prepare(ids(len(a)),a,len(a)-1,1)
            x = np.random.default_rng(len(a)).normal(size=(len(a),3))
            expected = sum((math.dist(x[i],x[j])-p.distances[i,j])**2 for i in range(len(a)) for j in range(i))
            for alpha in (0.,.2,20.):
                assert_allclose(objective_gradient(p,x,alpha)[0],expected,rtol=1e-13,atol=1e-13)

    def test_schedule_prefix_consistency_and_exact_definition(self):
        p = prepare(ids(5),path(5),2)
        long = schedule(p,Controls(epochs=200))
        for epochs in (1,2,5,20,60):
            assert_array_equal(schedule(p,Controls(epochs=epochs)),long[:epochs])
        rate = math.log(16/.01)/29
        tau = next(t for t in range(200) if 16*math.exp(-rate*t)<1)
        self.assertEqual(long[0],16)
        self.assertEqual(long[tau],1)
        assert_allclose(long[tau+1],1/(1+rate))
        self.assertTrue(np.all(np.diff(long)<=0))

    def test_safeguarded_pair_matches_small_analytical_step(self):
        for dim in (2,3):
            x = np.arange(2*dim,dtype=float).reshape(2,dim)/3
            for attractive,eta in itertools.product((False,True),(1e-4,0.002)):
                d = math.dist(x[0],x[1])
                q = x[0]-x[1]
                gradient = 2*(d-2)*q/d if attractive else -.2*q/d**2
                expected = x.copy()
                expected[0] -= eta*gradient
                expected[1] += eta*gradient
                actual = x.copy()
                _,step,halves = safeguarded_pair(actual,0,1,2.,attractive,eta,Controls())
                self.assertEqual(halves,0)
                self.assertEqual(step,eta)
                assert_allclose(actual,expected,atol=1e-15,rtol=1e-15)

    def test_pair_descent_large_steps_and_displacement_cap(self):
        for dim in (2,3):
            for attractive in (False,True):
                x = np.zeros((2,dim)); x[1,0] = .07
                control = Controls(max_pair_displacement=.25)
                before = (.07-2)**2 if attractive else -.2*math.log(.07)
                moved,_,_ = safeguarded_pair(x,0,1,2.,attractive,100.,control)
                radius = math.dist(x[0],x[1])
                after = (radius-2)**2 if attractive else -.2*math.log(radius)
                self.assertLess(after,before)
                self.assertLessEqual(moved,.25*(1+1e-14))
                assert_allclose(x.mean(axis=0),np.array([.035]+[0.]*(dim-1)),atol=1e-15)

    def test_pair_guard_avoids_third_vertex_collision(self):
        x = np.array([[0.,0.,0.],[2.,0.,0.],[.5,0.,0.]])
        # Attraction at eta=.25 would put vertex0 exactly on third vertex.
        moved,step,halves = safeguarded_pair(x,0,1,1.,True,.25,Controls())
        self.assertGreater(halves,0)
        self.assertLess(step,.25)
        self.assertGreater(min(math.dist(x[i],x[j]) for i in range(3) for j in range(i)),1e-12)

    def test_controlled_full_gradient_descent(self):
        p = prepare(ids(7),path(7),1)
        x = np.random.default_rng(47).normal(size=(7,3))
        old,g = objective_gradient(p,x)
        new,_ = objective_gradient(p,x-1e-4*g)
        self.assertLess(new,old)

    def test_tetrahedron_nonplanar_3d_and_2d_comparison(self):
        p = prepare(ids(4),np.ones((4,4))-np.eye(4),3)
        initial = np.array([[.2,.1,.3],[1.4,-.2,.1],[-.3,1.2,.4],[.1,.2,1.8]])
        three = optimize(p,3,203,initial,Controls(epochs=200))
        centered = three.coordinates-three.coordinates.mean(axis=0)
        self.assertEqual(np.linalg.matrix_rank(centered,tol=1e-6),3)
        self.assertLess(three.history[-1]['objective'],1e-10)
        distances = [math.dist(three.coordinates[i],three.coordinates[j]) for i,j in p.pairs]
        assert_allclose(distances,1,atol=2e-6,rtol=0)
        two = optimize(p,2,203,initial[:,:2],Controls(epochs=200))
        self.assertGreater(two.history[-1]['objective'],.1)
        # A planar 3D start is not artificially given an out-of-plane component.
        flat = np.c_[initial[:,:2],np.zeros(4)]
        planar = optimize(p,3,203,flat,Controls(epochs=10))
        assert_array_equal(planar.coordinates[:,2],0)

    def test_repeatability_initialization_order_and_no_input_mutation(self):
        p = prepare(ids(6),path(6),2)
        control = Controls(epochs=5)
        a = optimize(p,3,322,controls=control)
        b = optimize(p,3,322,controls=control)
        assert_array_equal(a.coordinates,b.coordinates)
        self.assertEqual(a.history,b.history)
        supplied = a.initial_coordinates.copy()
        c = optimize(p,3,322,supplied,control)
        assert_array_equal(a.coordinates,c.coordinates)
        assert_array_equal(supplied,a.initial_coordinates)
        self.assertEqual(a.coordinates.shape,(6,3))
        self.assertEqual(np.linalg.matrix_rank(a.initial_coordinates-a.initial_coordinates.mean(axis=0)),3)

    def test_coincident_near_zero_nonfinite_and_dimension_inputs(self):
        p = prepare(ids(2),path(2),1)
        for initial in (np.zeros((2,3)),np.array([[0.,0.,0.],[1e-13,0.,0.]])):
            with self.assertRaisesRegex(NumericalFailure,'collision'):
                optimize(p,3,initial=initial)
        for initial in (np.full((2,3),np.nan),np.full((2,3),np.inf),np.zeros((2,2))):
            with self.assertRaises(ValueError): optimize(p,3,initial=initial)
        for dimension in (1,4,True,2.5):
            with self.assertRaises(ValueError): optimize(p,dimension)
        for seed in (-1,True,2.5):
            with self.assertRaises(ValueError): optimize(p,3,seed)
        x = np.array([[0.,0.,0.],[1e-11,0.,0.]])
        # Just above the declared collision floor: a bounded step remains finite.
        moved,_,_ = safeguarded_pair(x,0,1,1.,False,1.,Controls())
        self.assertLessEqual(moved,1.)
        self.assertTrue(np.isfinite(x).all())

    def test_invalid_controls_and_controlled_line_search_failure(self):
        for args in ({'epochs':0},{'epochs':True},{'max_backtracks':-1},{'repulsion_alpha':-1},
                     {'repulsion_alpha':float('nan')},{'schedule_epsilon':1},
                     {'collision_distance':0},{'movement_tolerance':-1},{'armijo':1}):
            with self.assertRaises(ValueError): Controls(**args)
        x = np.array([[0.,0.],[2.,0.],[.5,0.]])
        saved = x.copy()
        with self.assertRaisesRegex(NumericalFailure,'line_search_failed'):
            safeguarded_pair(x,0,1,1.,True,.25,Controls(max_backtracks=0))
        assert_array_equal(x,saved)

    def test_disconnected_attraction_warning_and_unbounded_witness(self):
        a = np.zeros((8,8),dtype=int)
        a[:4,:4]=1; a[4:,4:]=1
        np.fill_diagonal(a,0)
        a[3,4]=a[4,3]=1
        p = prepare(ids(8),a,1)
        self.assertEqual(p.attractive_components,2)
        x = np.random.default_rng(24).normal(size=(8,3))
        separated = x.copy(); separated[4:,0] += 100
        self.assertLess(objective_gradient(p,separated)[0],objective_gradient(p,x)[0])
        result = optimize(p,3,24,x,Controls(epochs=2))
        self.assertTrue(any('unbounded_below' in w for w in result.warnings))

    def test_actual_displacement_cap_with_large_offset_starts(self):
        p = prepare(ids(2),path(2),1)
        # Auditor witness, implementer witness, and a negative-offset version.
        for values in ((1e16+2,1e16+18),(1e16,1e16+10),(-1e16-2,-1e16-18)):
            x = np.array([[values[0],0.,0.],[values[1],0.,0.]])
            saved = x.copy()
            result = optimize(p,3,0,x,Controls(epochs=1))
            actual = np.linalg.norm(result.coordinates-x,axis=1).max()
            self.assertLessEqual(actual,1.)
            self.assertLessEqual(result.history[-1]['max_pair_movement'],1.)
            self.assertEqual(result.termination,'floating_point_stagnation')
            assert_array_equal(x,saved)
            assert_array_equal(result.coordinates,x)
            centered = x-x[0]
            control = optimize(p,3,0,centered,Controls(epochs=1))
            assert_allclose(np.linalg.norm(control.coordinates-centered,axis=1),1.,atol=1e-14,rtol=0)
            self.assertEqual(control.termination,'epoch_budget')
        # A sub-unit cap must not gain an allowance based on coordinate ULP.
        x = np.array([[1e15+.125,0.,0.],[1e15+2.125,0.,0.]])
        result = optimize(p,3,0,x,Controls(epochs=1,max_pair_displacement=.0625))
        self.assertLessEqual(np.linalg.norm(result.coordinates-x,axis=1).max(),.0625)

    def test_roundoff_does_not_masquerade_as_convergence(self):
        p = prepare(ids(2),path(2),1)
        x = np.array([[1e17,0.,0.],[1e17+16,0.,0.]])
        result = optimize(p,3,4,x)
        self.assertEqual(result.termination,'floating_point_stagnation')
        self.assertEqual(result.history[-1]['roundoff_skipped_pairs'],1)
        self.assertGreater(result.history[-1]['gradient_norm'],1)
        assert_array_equal(result.coordinates,x)

    def test_optimizer_orthogonal_equivariance_with_shared_start(self):
        p = prepare(ids(5),path(5),2)
        x = np.random.default_rng(810).normal(size=(5,3))
        q,_ = np.linalg.qr(np.random.default_rng(18).normal(size=(3,3)))
        first = optimize(p,3,412,x,Controls(epochs=5))
        second = optimize(p,3,412,x@q+[1.,2.,3.],Controls(epochs=5))
        assert_allclose(second.coordinates,first.coordinates@q+[1.,2.,3.],rtol=3e-12,atol=3e-12)

    def test_two_vertex_analytical_solution_and_stopping(self):
        p = prepare(ids(2),path(2),1)
        x = np.array([[0.,0.,0.],[1.,0.,0.]])
        result = optimize(p,3,4,x)
        self.assertEqual(result.termination,'movement_tolerance')
        self.assertEqual(result.epochs_completed,1)
        assert_array_equal(result.coordinates,x)
        self.assertEqual(result.history[-1]['objective'],0.)


if __name__ == '__main__':
    unittest.main()
