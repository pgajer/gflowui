"""Independent slow scalar paper evaluator; no imports from optimized core."""
import math
import numpy as np


def evaluate(points, distances, attractive, alpha):
    n,dimension = np.shape(points)
    cost = 0.
    gradient = [[0. for _ in range(dimension)] for _ in range(n)]
    for i in range(n):
        for j in range(i):
            q = [float(points[i][axis])-float(points[j][axis]) for axis in range(dimension)]
            radius = math.sqrt(sum(v*v for v in q))
            if not radius > 0:
                raise ValueError('singular reference configuration')
            if attractive[i][j]:
                cost += (radius-float(distances[i][j]))**2
                derivative = 2*(radius-float(distances[i][j]))
            else:
                cost -= alpha*math.log(radius)
                derivative = -alpha/radius
            for axis in range(dimension):
                partial = derivative*q[axis]/radius
                gradient[i][axis] += partial
                gradient[j][axis] -= partial
    return cost,np.array(gradient)


def finite_difference(function,points,h):
    result = np.zeros_like(points,dtype=float)
    for i in range(len(points)):
        for axis in range(points.shape[1]):
            plus,minus = points.copy(),points.copy()
            plus[i,axis] += h
            minus[i,axis] -= h
            result[i,axis] = (function(plus)-function(minus))/(2*h)
    return result


def walk_scores(adjacency,depth,decay):
    """Python integer walk counting, weighted only after each exact power."""
    n = len(adjacency)
    counts = [[int(i == j) for j in range(n)] for i in range(n)]
    scores = [[0.]*n for _ in range(n)]
    for p in range(1,depth+1):
        counts = [[sum(counts[i][v]*int(adjacency[v][j]) for v in range(n))
                   for j in range(n)] for i in range(n)]
        for i in range(n):
            for j in range(n):
                scores[i][j] += decay**p*counts[i][j]
    return np.array(scores)
