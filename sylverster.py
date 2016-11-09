from sympy import Matrix, sign
from sympy.polys.polytools import LC
from functools import partial

coefs = lambda n, v, p: [p.as_expr().coeff(v,k) for k in range(n)][::-1]

def SyHa(P,Q, j,v):
    p = P.degree(v)
    q = Q.degree(v)
    assert P.gens == Q.gens
    coef = partial(coefs, p+q-j, v)
    return Matrix([coef(P*v**k) for k in range(q-j-1,-1,-1)] + [coef(Q*v**k) for k in range(0,p-j,1)])


def sRes(P, Q, j=-1, v=0):
    if not v:
        v = P.gens[-1]
    p = P.degree(v)
    q = Q.degree(v)
    if p <= q:
        raise ValueError, "P must have a greater degree than Q"

    def sres(j):
        n = p + q -2*j
        if j==p:
            return sign(LC(P, v))
        return (SyHa(P,Q,j,v)[:,:n]).det()

    if j==-1:
        return [sres(j) for j in range(p,-1,-1)]

    return sres(j)


if __name__ == '__main__':
    from sympy import Poly
    P = Poly('x*(x^3 - x)')
    Q = Poly('x^3 + x')

    print sRes(P,Q)

    P = Poly('x^2 + y^2 + z^2 -1')
    x,y,z = P.gens
    P1 = P.diff(z)
    print SyHa(P, P1, 0, z)  # from [SPR, example 5.17]
    for j in range(2):
        print j, sRes(P, P1, j).simplify()

    Q = Poly('x^2 + y^2 -1')
    x,y = Q.gens
    Q1 = Q.diff(y)
    print SyHa(Q, Q1, 0, y)
    for j in range(2):
        print j, sRes(Q, Q1, j).simplify()
