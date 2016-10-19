
from sympy import *
from sympy import poly
from sympy.abc import x, y, z
from sympy import LC
from sympy import LT
from sympy import subresultants

# computes de reductum of the polynomial F, as a polynomial of I_r[x]
def red(F, x):
    p = F - LT(F, x)
    return p

# computes de reducta (RED) of the polynomial F, as a polynomial of I_r[x]
def reducta(F, x):
    L = []
    aux = F
    while(aux != 0):
        L.append(aux)
        aux = red(aux, x)
    return L

# exemple del llibre (o l'article)
# p = poly((x**2 +y**2 - 1)*z**3 + (x-1)*z**2 + (x-1)**2 + y**2)
# p
# red(p, z)
# reducta(p, z)

# no esta gens refinat a nivell de cost
# especialment tota la part de fer dos RED, esta fatal en aquest sentit

# computes the operation PROJ (not all the projection fase)
# projection of the polynomials in A, as polynomials of I_r[x]
def PROJ(A, x):
    P = []   # this will be the output set
    # estic usant les operacions de list, pero al final es fa list(set(P)) i elimina els duplicats
    # F is a polynomial in A
    for F in A:
        R = reducta(F, x)
        for G in R:
            P.append( LC(G, x))
            H = diff(G, x)
            PSC = []

            # n = min(deg(G), deg(H))
            # for i in range(n):
            # psc_j(G,H)
            n = min(degree(G,x), degree(H,x))
            print("n : ", n)
            p = subresultants(G,H)
            print("subresultants: ", p)
            p.reverse()
            #import pdb; pdb.set_trace()
            p = p[:n + 1]
            for i, cp in enumerate(p):
                # psc_j(G,H)
                if cp != 0:
                    #cp = cp.collect(cp.as_expr(), x)
                    print("cp: ", cp)
                    c = cp.as_expr().coeff(x, i)

                    P.append(c)
                    print("c i : ", c, i)

            for I in A:
                if(I == F):
                    break
                else:
                    S = reducta(I, x)
                    for J in S:
                        m = min(degree(G,x), degree(H,x))
                        q = subresultants(G,H)
                        for i in range(max(m,0)):
                                # psc_j(G,H)
                                if q[i] != 0:
                                    d = q[i].as_expr().coeff(x**(m-i))
                                    P.append(d)
                                    print("d i : ", d, i)
    return list(set(P))
# de moment fem totes les possibles combinacions sense refinar. Bastaria amb associar un booleà de usat o no a cada polinomi

A = [poly(144*y**2 + 96*x**2*y + 9*x**4 + 105*x**2 + 70*x - 98), poly(x*y**2 + 6*x*y + x**3 + 9*x)]
PROJ(A, y)
