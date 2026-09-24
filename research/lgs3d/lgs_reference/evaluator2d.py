"""Independent scalar evaluators, derived from the mathematics in METHOD.md.

These routines do not call the translated optimizer or upstream objective.
Only 2D is implemented in phase 02.
"""
import math
import numpy as np


def evaluate(points, distances, flags, alpha, model='code'):
    X = np.asarray(points, dtype=np.float64)
    if X.ndim != 2 or X.shape[1] != 2 or not np.isfinite(X).all():
        raise ValueError('finite 2D coordinates required')
    value = 0.0
    gradient = np.zeros_like(X)
    for i in range(len(X)):
        for j in range(i):
            dx, dy = float(X[i,0]-X[j,0]), float(X[i,1]-X[j,1])
            radius = math.hypot(dx,dy)
            if radius == 0:
                raise ValueError('singular pair')
            target = float(distances[i,j])
            if flags[i,j]:
                coefficient = 1/(4*target**2) if model == 'code' else 1
                cost = coefficient*(radius-target)**2
                derivative = 2*coefficient*(radius-target)
            elif model == 'code':
                cost, derivative = alpha/radius, -alpha/radius**2
            elif model == 'paper':
                cost, derivative = -alpha*math.log(radius), -alpha/radius
            else:
                raise ValueError('unknown model')
            value += cost
            for axis, displacement in enumerate((dx,dy)):
                g = derivative*displacement/radius
                gradient[i,axis] += g
                gradient[j,axis] -= g
    return value, gradient


def upstream_diagnostic(points, distances, flags, alpha):
    """Ordered entries, including diagonal floor; returns value and gradient."""
    n = len(points)
    cost, gradient = 0.0, np.zeros_like(points, dtype=np.float64)
    eps = 1e-13
    for i in range(n):
        for j in range(n):
            dx, dy = float(points[i,0]-points[j,0]), float(points[i,1]-points[j,1])
            squared = dx*dx+dy*dy
            radius = math.sqrt(max(squared,eps))
            cost += flags[i,j]*(distances[i,j]-radius)**2-alpha*math.log(radius+2*eps)
            if squared > eps:
                derivative = 2*flags[i,j]*(radius-distances[i,j])-alpha/(radius+2*eps)
                for axis, displacement in enumerate((dx,dy)):
                    g = derivative*displacement/radius
                    gradient[i,axis] += g
                    gradient[j,axis] -= g
    denominator = (1+alpha)*n*n
    return cost/denominator, gradient/denominator


def finite_difference(function, points, step):
    result = np.zeros_like(points, dtype=np.float64)
    for i in range(len(points)):
        for axis in range(2):
            upper, lower = points.copy(), points.copy()
            upper[i,axis] += step
            lower[i,axis] -= step
            result[i,axis] = (function(upper)-function(lower))/(2*step)
    return result


def shortest_paths(adjacency):
    """Tiny independent Floyd-Warshall evaluator, unit edge distances."""
    n = len(adjacency)
    d = [[0 if i == j else (1 if adjacency[i][j] else math.inf)
          for j in range(n)] for i in range(n)]
    for k in range(n):
        for i in range(n):
            for j in range(n):
                d[i][j] = min(d[i][j], d[i][k]+d[k][j])
    return np.asarray(d, dtype=np.float64)
