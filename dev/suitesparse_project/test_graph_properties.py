import unittest
import networkx as nx
import numpy as np
from compute_graph_properties import compute

class PropertiesTest(unittest.TestCase):
    def check_graph(self,g):
        edges=list(g.edges()); p=compute(len(g),edges)
        # Independent explicit enumeration splits credit among all tied geodesics.
        vb=np.zeros(len(g)); eb=dict.fromkeys(map(frozenset,edges),0.)
        for s in g:
            for t in g:
                if s>=t or not nx.has_path(g,s,t): continue
                paths=list(nx.all_shortest_paths(g,s,t))
                for path in paths:
                    for v in path[1:-1]: vb[v]+=1/len(paths)
                    for u,v in zip(path,path[1:]): eb[frozenset((u,v))]+=1/len(paths)
        np.testing.assert_allclose(p['vertex']['betweenness'],vb)
        np.testing.assert_allclose(p['edge']['betweenness'],[eb[frozenset(e)] for e in edges])
        for j,(u,v) in enumerate(edges):
            h=g.copy(); h.remove_edge(u,v)
            d=nx.shortest_path_length(h,u,v) if nx.has_path(h,u,v) else None
            self.assertEqual(p['edge']['detour_ratio'][j],d)
            self.assertAlmostEqual(p['edge']['effective_resistance'][j],nx.resistance_distance(g.subgraph(nx.node_connected_component(g,u)),u,v))
        self.assertEqual(p['vertex']['core_number'],list(nx.core_number(g).values()))
        np.testing.assert_allclose(p['vertex']['clustering'],list(nx.clustering(g).values()))
        return p

    def test_examples(self):
        for g in [nx.path_graph(4),nx.cycle_graph(4),nx.complete_graph(3),nx.complete_graph(4),
                  nx.star_graph(4),nx.complete_bipartite_graph(2,3),nx.disjoint_union(nx.path_graph(3),nx.empty_graph(1)),
                  nx.Graph([(0,1),(1,2),(2,0),(2,3),(3,4),(4,2)])]:
            with self.subTest(edges=list(g.edges())): self.check_graph(g)
        p=self.check_graph(nx.path_graph(4))
        self.assertEqual(p['edge']['bridge_pairs'],[3,4,3])
        self.assertEqual(p['vertex']['articulation'],[0,1,1,0])
        p=self.check_graph(nx.cycle_graph(4))
        np.testing.assert_allclose(p['edge']['effective_resistance'],.75)
        np.testing.assert_allclose(p['vertex']['betweenness'],.5)
        self.assertEqual(p['vertex']['clustering'],[0]*4)
    def test_random_small_graphs(self):
        for seed in range(12): self.check_graph(nx.gnp_random_graph(8,.3,seed=seed))
    def test_isolates(self):
        p=compute(3,[]); self.assertEqual(p['vertex']['degree'],[0]*3)
        self.assertEqual(p['vertex']['betweenness_normalized'],[0]*3)

if __name__=='__main__': unittest.main()
