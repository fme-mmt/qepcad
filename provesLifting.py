from lifting import *
x, y, z = symbols('x y z')
projectionFactorSet = ([
                        {Poly(x + 1), Poly(x - 1), Poly(2 * x ** 2 - 1)},
                        {Poly(x - y), Poly(x ** 2 + y ** 2 - 1)},
])
cad = Cad()
cad.construct(projectionFactorSet)

# TODO 1: comprovar que aquest exemple dona un cad correcte


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
projectionFactorSet = ([
                        {Poly(x + 1), Poly(x - 1), Poly(x),
                         Poly(32 * x ** 6 - 80 * x ** 4 + 85 * x ** 2 - 32),
                         Poly(2 * x ** 2 - 1)},

                        {Poly(4 * y ** 4 + 8 * x * y ** 3 + 8 * x ** 2 * y ** 2 - 4 * y ** 2 + 8 * x ** 3 * y - 8 * x * y + 4 * x ** 4 - 4 * x ** 2 + 1),
                        Poly(x ** 2 + y ** 2 - 1), Poly(y + x)},

                        {Poly(x ** 2 + y ** 2 + z ** 2 - 1), Poly(2 * (x + y) * z - 1), Poly(y)}
                        ])
cad = Cad()
cad.construct(projectionFactorSet)

print('-----------')