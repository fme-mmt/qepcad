# from sympy import *
from sympy import poly
from sympy import degree, diff
from sympy.abc import *
from sympy import LT
from sympy import subresultants
from sympy import Matrix, sign
from sympy.polys.polytools import LC
from functools import partial

coefs = lambda n, v, p: [p.as_expr().coeff(v, k) for k in range(n)][::-1]


def SyHa(P, Q, j, v):
    P = poly(P)
    Q = poly(Q)

    p = P.degree(v)
    q = Q.degree(v)
    # assert P.gens == Q.gens
    coef = partial(coefs, p + q - j, v)
    return Matrix([coef(P * v ** k) for k in range(q - j - 1, -1, -1)] + [coef(Q * v ** k) for k in range(0, p - j, 1)])


def sRes(P, Q, j=-1, v=0):
    if not v:
        v = P.gens[-1]

    P = poly(P)
    Q = poly(Q)

    p = P.degree(v)
    q = Q.degree(v)

    def sres(j):
        n = p + q - 2 * j
        if j == p:
            return sign(LC(P, v))
        elif p > j > q:
            return 0
        sh = SyHa(P, Q, j, v)
        assert sh.shape[0] == n
        return (sh[:, :n]).det()

    if j == -1:
        return [sres(j) for j in range(p, -1, -1)]

    return sres(j)


def PSC(F, G, x):
    n = min(degree(F, x), degree(G, x))
    if n < 0:
        return []
    subs = sRes(F, G, -1, x)[2:]  # subresultants PRS
    s = []
    for p in reversed(subs):
        s.append(LC(p, x))
    return s


# computes de reductum of the polynomial F,
# as a polynomial of I_r[x]

def red(F, x):
    p = F - LT(F, x)
    return p


# computes de reducta (RED) of the polynomial F,
# as a polynomial of I_r[x]
def reducta(F, x):
    res = []
    aux = F
    while aux != 0:
        res.append(aux)
        aux = red(aux, x)
    return res


def proj1(poly_set, x):
    p_out = []
    # F is a polynomial in A
    for F in poly_set:
        if degree(F, x) > 1:
            R = reducta(F, x)
            for G in R:
                if degree(G, x) > 0:
                    H = diff(G, x)
                    psc_1 = PSC(G, H, x)
                    if len(psc_1):
                        if degree(G) == 2:
                            p_out.append(poly(psc_1[0]))
                        else:
                            for aux in psc_1[0:degree(G) - 2]:
                                p_out.append(poly(aux))
    return p_out


def proj2(poly_set, x):
    p_out = []
    # F is a polynomial in A
    for i in range(len(poly_set)):
        F = poly_set[i]
        R = reducta(F, x)
        for j in range(i + 1, len(poly_set)):
            G = poly_set[j]
            S = reducta(G, x)
            for H in R:
                if degree(H, x) > 0:
                    for I in S:
                        if degree(I, x) > 0:
                            if degree(H, x) > degree(I, x):
                                psc = PSC(H, I, x)
                                if len(psc) > 0:
                                    if degree(I, x) == 1:
                                        p_out.append(poly(psc[0]))
                                    else:
                                        for aux in psc[0:degree(I, x) - 1]:
                                            p_out.append(poly(aux))

                            elif degree(H, x) < degree(I, x):
                                psc = PSC(I, H, x)
                                if len(psc) > 0:
                                    if degree(H, x) == 1:
                                        p_out.append(poly(psc[0]))
                                    else:
                                        for aux in psc[0:degree(H, x) - 1]:
                                            p_out.append(poly(aux))

                            elif degree(H, x) == degree(I, x):
                                HH = H.mul_ground(LC(I)).add(-I.mul_ground(LC(H)))
                                psc = PSC(I, HH, x)
                                if len(psc) > 0:
                                    if degree(HH, x) == 1:
                                        p_out.append(poly(psc[0]))
                                    else:
                                        for aux in psc[0:degree(HH, x) - 1]:
                                            p_out.append(poly(aux))

    return p_out


def iter_proj(proj_set, x):
    p_out = []
    for aux in proj1(proj_set, x):
        p_out.append(poly(aux))

    for aux in proj2(proj_set, x):
        p_out.append(poly(aux))

    print('p_out: ', p_out)
    return p_out


def proj(proj_set):
    p_out = []
    var_set = set()
    for p in proj_set:
        p = poly(p)
        q = p.gens
        for var in q:
            var_set.add(var)

    p_out.append(proj_set)
    removed_var = []
    for i, var in enumerate(var_set):
        if i < len(var_set) - 1:
            proj_set = iter_proj(proj_set, var)
            p_out.append(proj_set)
            removed_var.append(var)

    output = {'projection': p_out, 'variables': removed_var}
    print('output = ', output)
    return output



# poly1 = poly(x**2 + y**2 - 4*x)
# poly2 = poly(x**2 + y**2 - 4*z)
# poly3 = poly(z)
# polys = [poly1, poly2, poly3]
#
# proj(polys)

