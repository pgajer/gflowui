"""Explicit paper-form LGS variant; standalone experimental numerical code."""
from .core import (VARIANT, Problem, Controls, Result, NumericalFailure,
                   prepare, objective_gradient, optimize, schedule, safeguarded_pair)
