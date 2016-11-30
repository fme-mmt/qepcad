from lifting import *

x, y, z = symbols('x y z')


def printSamples(cad):
    print('Samples')
    i = 0
    for stackList in cad.stackList:
        print('stackList: ' + str(i))
        j = 0
        for stack in stackList:
            print('stack: ' + str(j))
            cells = stack.cells
            print('cells:')
            for cell in cells:
                print(cell.sample)
            j += 1
        i += 1

"""Ejemplo de las Slides 2F, pag 4: Represent x - y < 0 ^ x^2+y^2 - 1 <= 0

(x = -1∧y = 0)∨(-1<x<-1/sqrt(2)∧-sqrt(1-x^2)<=y<=sqrt(1-x^2))∨(x = -1/sqrt(2)∧-sqrt(1-x^2)<y<=sqrt(1-x^2))
∨(-1/sqrt(2)<x<1/sqrt(2)∧x<y<=sqrt(1-x^2))

"""
projectionFactorSet = ([
                        {Poly(x + 1), Poly(x - 1), Poly(2 * x ** 2 - 1)},
                        {Poly(x - y), Poly(x ** 2 + y ** 2 - 1)},
])
cad = Cad()
cad.construct(projectionFactorSet)
#printSamples(cad)
# FUNCIONA


print('-------------------')

# TODO 2: cambiar solve per l'algoritme que toqui del llibre (explicar que pasa la mailing list)
""" Al hacer este ejemplo peta al intentar hacer solve de:
Poly(4*y**4 - (sqrt(6)*sqrt(-2**(1/3)*(194 + 57*sqrt(114))**(2/3) + 55*2**(2/3) + 20*(194 + 57*sqrt(114))**(1/3)) +
12*(194 + 57*sqrt(114))**(1/6))/(3*(194 + 57*sqrt(114))**(1/6))*y**3 - (-4*sqrt(6)*(194 +
57*sqrt(114))**(1/6)*sqrt(-2**(1/3)*(194 + 57*sqrt(114))**(2/3) + 55*2**(2/3) + 20*(194 + 57*sqrt(114))**(1/3)) -
55*2**(2/3) + 4*(194 + 57*sqrt(114))**(1/3) + 2**(1/3)*(194 + 57*sqrt(114))**(2/3))/(12*(194 + 57*sqrt(114))**(1/3))*y**2 +
 (-1980*2**(2/3)*(194 + 57*sqrt(114))**(1/6) - 110*2**(1/6)*sqrt(3)*sqrt(-2**(1/3)*(194 + 57*sqrt(114))**(2/3) + 55*2**(2/3) +
 20*(194 + 57*sqrt(114))**(1/3)) + 4*sqrt(6)*(194 + 57*sqrt(114))**(1/3)*sqrt(-2**(1/3)*(194 + 57*sqrt(114))**(2/3) + 55*2**(2/3) +
 20*(194 + 57*sqrt(114))**(1/3)) + 2**(5/6)*sqrt(3)*(194 + 57*sqrt(114))**(2/3)*sqrt(-2**(1/3)*(194 + 57*sqrt(114))**(2/3) + 55*2**(2/3)
  + 20*(194 + 57*sqrt(114))**(1/3)) + 144*sqrt(194 + 57*sqrt(114)) + 36*2**(1/3)*(194 + 57*sqrt(114))**(5/6))/(288*sqrt(194 + 57*sqrt(114)))*y
  + (-4*2**(5/6)*sqrt(3)*(194 + 57*sqrt(114))**(5/6)*sqrt(-2**(1/3)*(194 + 57*sqrt(114))**(2/3) + 55*2**(2/3) + 20*(194 + 57*sqrt(114))**(1/3))
   - 2508*2**(5/6)*sqrt(57) - 16*sqrt(6)*sqrt(194 + 57*sqrt(114))*sqrt(-2**(1/3)*(194 + 57*sqrt(114))**(2/3) + 55*2**(2/3) +
   20*(194 + 57*sqrt(114))**(1/3)) - 5511*2**(1/3) + 57*2**(1/6)*sqrt(57)*(194 + 57*sqrt(114))**(1/3) + 440*2**(1/6)*sqrt(3)*(194 +
   57*sqrt(114))**(1/6)*sqrt(-2**(1/3)*(194 + 57*sqrt(114))**(2/3) + 55*2**(2/3) + 20*(194 + 57*sqrt(114))**(1/3)) + 2517*2**(2/3)*(194 +
   57*sqrt(114))**(1/3) + 858*(194 + 57*sqrt(114))**(2/3))/(1152*(194 + 57*sqrt(114))**(2/3)), y, domain='EX')
"""
projectionFactorSet2 = ([
                        {Poly(x + 1), Poly(x - 1), Poly(x),
                         Poly(32 * x ** 6 - 80 * x ** 4 + 85 * x ** 2 - 32),
                         Poly(2 * x ** 2 - 1)},

                        {Poly(4 * y ** 4 + 8 * x * y ** 3 + 8 * x ** 2 * y ** 2 - 4 * y ** 2 + 8 * x ** 3 * y - 8 * x * y + 4 * x ** 4 - 4 * x ** 2 + 1),
                        Poly(x ** 2 + y ** 2 - 1), Poly(y + x)},

                        {Poly(x ** 2 + y ** 2 + z ** 2 - 1), Poly(2 * (x + y) * z - 1), Poly(y)}
                        ])
cad2 = Cad()
# cad2.construct(projectionFactorSet2)
# printSamples(cad2)
print('-----------')

"""
Test 3: Descomposicion de la esfera:
"""

projectionFactorSet3 = ([{Poly(x + 2), Poly(x - 2)}, {Poly(x**2 + y**2 - 4)}, {Poly(x**2 + y**2 + z**2-4)}])
cad3 = Cad()
cad3.construct(projectionFactorSet3)
#printSamples(cad3)
# FUNCIONA

print('----------')

"""
Test 4: Descomposicion del problema de clase:
x^2+y^2 <= z <= 2
x^2+y^2 , 2 - z
"""
print('Ejemplo 4')
projectionFactorSet4 = ([{Poly(x + sqrt(2)), Poly(x - sqrt(2))}, {Poly(x**2 + y**2 - 2)},
                         {Poly(x**2 + y**2 - z), Poly(2 - z)}])


cad4 = Cad()
cad4.construct(projectionFactorSet4)
#printSamples(cad4)
# FUNCIONA


"""
Calculeu el volum del recinte limitat lateralment pel cilindre $x^2+y^2 = 4x$,
 superiorment pel paraboloide $x^2 + y^2 = 4z$ i inferiorment pel pla $z=0$.
"""

print('Ejemplo 5')
projectionFactorSet5 = ([{Poly(x+4), Poly(x-4), Poly(x**2-4*x)},
                         {Poly(x**2+y**2), Poly(x**2+y**2-4*x)},
                         {Poly(x**2+y**2-4*x), Poly(x**2+y**2-4*z), Poly(z)}
                         ])

cad5 = Cad()
cad5.construct(projectionFactorSet5)
printSamples(cad5)

"""
cosas que he usado para testear :
"""

allSymbols = []
for setp in projectionFactorSet4:
    for p in setp:
        for symb in p.atoms(Symbol):
            if symb not in allSymbols:
                allSymbols.append(symb)


def eval_better(p, sample):
    n = len(sample)
    psyms = p.atoms(Symbol)
    m = len(psyms)
    if m == 1:
        return p

    for i in range(0, n):
        if allSymbols[i] in psyms:
            p = p.eval(allSymbols[i], sample[i])
    return p


print('----------')
