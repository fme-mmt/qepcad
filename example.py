import sympy
from sympy import *
from ProjectionPhase import *

x1 = var('x1')
x2 = var('x2')
x = var('x')
y = var('y')

# Example 1

poly1 = poly(x1 - x2)
poly2 = poly(x1**2 + x2**2 - 1)
polys = [poly1, poly2]

PROJ(polys, x2) # [x1 + 1, x1 - 1, 2*x1**2 - 1]

# Example 2

poly1 = poly(x1 - 1)
poly2 = poly(x2 - 1)
poly3 = poly(x**2 + y**2 - 2)
polys = [poly1, poly2, poly3]

PROJ(polys, x2) # 

# Example 3

poly1 = poly(144*y**2 + 96*x**2*y + 9*x**4 + 105*x**2 + 70*x - 98)
poly2 = poly(x*y**2 + 6*x*y + x**3 + 9*x)
polys = [poly1, poly2]

PROJ(polys, y) # [x**4 - 15*x**2 - 10*x + 14, x]

# Example 4

poly1 = poly(x1**2 * (2*x1+3) - 2*x2**2)
poly2 = poly(x1**2 + x2**2 - 1)
polys = [poly1, poly2]

PROJ(polys, y) # [2*x1 + 3, x1, x1 + 1, x1 - 1, 2*x1**3 + 5*x1**2 - 2]

# Example 5

poly1 = poly(y**4 + - 2*y**3 + y**2 - 3*x**2*y + 2*x**4)
polys = [poly1]

PROJ(polys, y) # [2048*x**12 - 4608*x**10 + 37*x**8 + 12*x**6]

