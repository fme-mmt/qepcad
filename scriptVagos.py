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


#test para varias variables


x1 = symbols('x1')
x2 = symbols('x2')

# Example 1

poly1 = Poly(x1 + 1)
poly2 = Poly(x1 - 1)
poly3 = Poly(2*x1**2 - 1)
polys = [poly1, poly2, poly3]

stack3 = Stack(None, polys)

print("EJEMPLO3")

#for c in stack3.cells:
    # print(c.sample)


cadMuestra = baseCad(polys)
print("c sample:")
for c in cadMuestra.cells:
    print(c.sample)
