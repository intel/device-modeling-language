# © 2021 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

# Utility function for topological sorting

__all__ = (
    'topsort',
    'CycleFound',
)

class CycleFound(Exception):
    def __init__(self, cycle):
        self.cycle = cycle
    def __str__(self):
        return str(self.cycle)

def topsort(graph):
    """Topologically sort a graph. The graph is represented by a dict
    where the keys are the set of nodes, and the values are lists of
    edge targets from the respective node."""
    visited = set()
    result = []
    def traverse(node, path):
        '''Topologically sort the subgraph reachable from node (minus
        already visited nodes): Traverse the graph depth-first,
        appending yet unvisited nodes in 'result'. The deepest nodes
        in the depth-first search are added first.'''
        if node in path:
            raise CycleFound(path[path.index(node):])
        for n in graph[node]:
            if n not in visited:
                traverse(n, path + [node])
        visited.add(node)
        result.append(node)
    for node in graph:
        if node not in visited:
            traverse(node, [])
    return result
