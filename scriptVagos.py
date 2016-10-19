from lifting import *
x = symbols('x')
polin1 = Poly(x**2-1)
print(polin1)
polin2 = Poly((x-1)**2-1)
print(polin2)
stack = Stack(None, [polin1, polin2])

for c in stack.cells:
    print(c.dimension)

print("")

for c in stack.cells:
    print(c.sample)


print("\n\n.....\n\n")


polin3 = Poly(x**2+1)
print(polin3)
stack2 = Stack(None, [polin3])

for c in stack2.cells:
    print(c.dimension)

print("")

for c in stack2.cells:
    print(c.sample)
