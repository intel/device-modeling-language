# © 2021-2024 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import unittest

import random
import collections
from dml.topsort import topsort, CycleFound

class Test_topsort(unittest.TestCase):
    def verify_graph(self, g):
        ts = list(topsort(g))
        self.assertEqual(len(g), len(ts))
        index = {val: index for (index, val) in enumerate(ts)}
        for (src, dests) in list(g.items()):
            for dest in dests:
                self.assertGreater(index[src], index[dest], g)

    def generate_dag(self, size):
        dag = {}
        for _ in range(size):
            key = int(random.randrange(1000))
            while key in dag:
                key += 1
            edges = [d for d in dag if random.randrange(2)]
            random.shuffle(edges)
            dag[key] = edges
        return dag

    def test_dags(self):
        self.verify_graph({})
        self.verify_graph({0: []})
        self.verify_graph({0: [1],
                           1: []})
        for _ in range(10):
            self.verify_graph(self.generate_dag(random.randrange(50)))

    def expectCycle(self, graph):
        try:
            with self.assertRaises(CycleFound) as cm:
                topsort(graph)
            cycle = cm.exception.cycle
            # The reported cycle actually forms a cycle in the graph
            for (fr, to) in zip(cycle, cycle[1:] + [cycle[0]]):
                self.assertIn(to, graph[fr])
        except:
            print("Graph:", graph)
            raise

    def test_cyclic(self):
        self.expectCycle({0: [0]})
        self.expectCycle({0: [1], 1: [0]})
        self.expectCycle(collections.OrderedDict(
            [(0, [1]), (1, [2]), (2, [1])]))
        for _ in range(10):
            g = self.generate_dag(random.randrange(50))
            if g:
                # Insert a cycle by walking a random path and adding a
                # back edge
                key = random.choice(list(g.keys()))
                cycle = [key]
                while g[key]:
                    key = random.choice(g[key])
                    cycle.append(key)
                g[key].append(cycle[0])
                self.expectCycle(g)
