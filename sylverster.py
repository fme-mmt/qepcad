from sympy import Matrix, sign
from sympy.polys.polytools import LC
from functools import partial

coefs = lambda n, v, p: [p.as_expr().coeff(v, k) for k in range(n)][::-1]


def SyHa(P, Q, j, v):
    p = P.degree(v)
    q = Q.degree(v)
    assert P.gens == Q.gens
    coef = partial(coefs, p + q - j, v)
    return Matrix([coef(P * v ** k) for k in range(q - j - 1, -1, -1)] + [coef(Q * v ** k) for k in range(0, p - j, 1)])


def sRes(P, Q, j=-1, v=0):
    if not v:
        v = P.gens[-1]
    p = P.degree(v)
    q = Q.degree(v)
    if p <= q:
        raise ValueError("P must have a greater degree than Q")

    def sres(j):
        n = p + q - 2 * j
        if j == p:
            return sign(LC(P, v))
        return (SyHa(P, Q, j, v)[:, :n]).det()

    if j == -1:
        return [sres(j) for j in range(p, -1, -1)]

    return sres(j)

def trunc(Q, v):
    from sympy.polys.polyerrors import PolynomialError
    try:
        q = Q.degree(v)
    except PolynomialError:
        q = 0
    if not Q:
        return []
    elif q == 0:
        return [Q]
    else:
        ts = [Q]
        ts.extend(trunc(truncd(q-1, v, Q), v))
        return ts

def truncd(d, v, Q):
    assert d <= Q.degree(v)
    from sympy import Poly
    cs = coefs(d+1, v, Q)
    cs.reverse()
    p = sum(c*v**k for k,c in enumerate(cs))
    return Poly(p) if p else Poly(p, v)


def elim(pols, v):
    deg = lambda p: p.degree(v)
    els = []
    for p in pols:
        if deg(p) < 2: continue
        for r in trunc(p, v):
            for j in range(deg(r) - 1):
                els.append(sRes(r, r.diff(v), j))

    tps = truncs(pols)
    for r in tps:
        for s in tps:
            if deg(r) > deg(s):
                for j in range(deg(s)):
                    els.append(sRes(r, s, j))
            elif deg(s) > deg(r):
                for j in range(deg(r)):
                    els.append(sRes(s, r, j))
            else:
                rb = LC(s, v) * r - LC(R, v) * s
                for j in range(deg(rb)):
                    els.append(sRes(s, rb, j))

    for r in tps:
        els.append(LC(r, v))

    return els


if __name__ == '__main__':
    from sympy import Poly

    P = Poly('x*(x^3 - x)')
    Q = Poly('x^3 + x')

    print(sRes(P, Q))

    P = Poly('x^2 + y^2 + z^2 -1')
    x, y, z = P.gens
    P1 = P.diff(z)
    print(SyHa(P, P1, 0, z))  # from [SPR, example 5.17]
    for j in range(2):
        print(j, sRes(P, P1, j).simplify())

    Q = Poly('x^2 + y^2 -1')
    x, y = Q.gens
    Q1 = Q.diff(y)
    print(SyHa(Q, Q1, 0, y))

    P = Poly('x^2 + y^2 + z^2 -1')
    x, y, z = P.gens

    for j in range(2):
        print(j, sRes(Q, Q1, j).simplify())

    print(elim([P], z))
