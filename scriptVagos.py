from lifting import *
x = symbols('x')
polin = Poly(x**2-1)
stack = Stack(None, [polin])

for c in stack.Cells:
    print(c.sample)
