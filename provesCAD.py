from CAD import *

x, y, z = symbols('x y z')

poly1 = poly(x ** 2 + y ** 2 - 4 * x)
poly2 = poly(x ** 2 + y ** 2 - 4 * z)
poly3 = poly(z)
polys = [poly1, poly2, poly3]

CAD(polys)
