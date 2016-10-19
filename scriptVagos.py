from lifting import *
x = symbols('x')
polin1 = Poly(x**2-1)
polin2 = Poly((x-1)**2-1)
stack = Stack(None, [polin1, polin2])

for c in stack.cells:
    print(c.dimension)

print("\n")

for c in stack.cells:
    print(c.sample)
