"""Attributed Python translation of Jacob Miller's pinned 2D L2G behavior.

BSD-3-Clause terms: ../vendor/L2G/LICENSE. Deliberate anomalies are retained.
This is a research oracle translation, not a validated external-input API.
"""
import math
import numpy as np


def walk_scores(adjacency, depth, mode='small'):
    A = np.asarray(adjacency)
    if mode == 'small':
        result = np.zeros(A.shape, dtype=np.float64)
        for p in range(1, depth + 1):
            power = 0.1**p * np.linalg.matrix_power(A, p)
            result += power / np.max(power)
        return result
    if mode == 'large':
        eigval, eigvec = np.linalg.eigh(A)
        powered = sum(eigval**p for p in range(1, depth + 1))
        return (eigvec @ np.diag(powered)) @ eigvec.T
    raise ValueError('unknown reference path')


def neighbor_flags(scores, k):
    flags = np.zeros(scores.shape, dtype=np.int16)
    for i, ordering in enumerate(np.argsort(scores, axis=1)):
        for v in ordering[::-1][:k+1]:
            if scores[i, v] == 0:
                break
            if i != v and v != 0:
                flags[i, v] = flags[v, i] = 1
    return flags


def schedule(distances, count, epsilon=0.01):
    values = [1/float(distances[i,j])**2 for i in range(len(distances)) for j in range(i)]
    eta_max, switch = 1/min(values), 1/max(values)
    rate = math.log(eta_max/(epsilon/max(values)))/29
    result = np.zeros(count)
    for t in range(count):
        eta = eta_max * math.exp(-rate*t)
        if eta < switch:
            break
        result[t] = eta
    tau = t  # Preserve upstream's last-step overwrite when no switch occurs.
    for t in range(tau, count):
        result[t] = switch / (1+rate*(t-tau))
    return result


def pair_step(points, i, j, target, attractive, eta, alpha):
    if np.asarray(points).ndim != 2 or points.shape[1] != 2:
        raise ValueError('reference optimizer supports 2D only')
    alpha = float(np.float32(alpha))  # Cython sgd uses C float.
    dx, dy = map(float, points[i] - points[j])
    mag = math.sqrt(dx*dx + dy*dy)
    attraction = min(eta*attractive/(target*target), 1)*(mag-target)/(2*mag)
    repulsion = -alpha*min(eta, 1)*(1-attractive)/(mag**3)
    delta = (attraction+repulsion)*np.array([dx,dy])
    points[i] -= delta
    points[j] += delta
    return points


def run(initial, distances, flags, steps, orders, alpha):
    points = np.asarray(initial, dtype=np.float64).copy()
    if len(steps) != len(orders):
        raise ValueError('one explicit order per epoch required')
    for eta, order in zip(steps, orders):
        for i, j in order:
            pair_step(points, i, j, distances[i,j], flags[i,j], eta, alpha)
    return points
