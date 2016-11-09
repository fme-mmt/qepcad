from lifting import *
x, y, z = symbols('x y z')
projectionFactorSet = ([
                        {Poly(x + 1), Poly(x - 1), Poly(2 * x ** 2 - 1)},
                        {Poly(x - y), Poly(x ** 2 + y ** 2 - 1)},
])
cad = Cad()
cad.construct(projectionFactorSet)


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
