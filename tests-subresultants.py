import sympy
from sympy import var

import subresultants
from subresultants import PSC

x = var('x')
y = var('y')

assert(PSC(0,0,y) == set())

assert(PSC(0,2,y) == set())

assert(PSC(2,3,y) == {1})

assert(PSC(2,3*x,y) == {1})

assert(PSC(2*y,3*x,y) == {1}) # WolframAlpha says the last item is 3*x

assert(PSC(2*y,3*x+5*y**2,y) == {12*x, 1}) # WolframAlpha says the last item is 2

assert(PSC(y**3,y**3,y) == {1}) # The psc are [0, 0, 0, 1]

assert(PSC(y**3,y**3+y,y) == {1}) # The psc are [0, 1, 0, 1]

assert(
    PSC(
        x*y**2 + 6*x*y + x**3 + 9*x,
        x*y**2 + 6*x*y + x**3 + 9*x,
        y
    ) == {1}
) # The psc are [0, 0, 1]

assert(
    PSC(
        144*y**2 + 96*x**2*y + 9*x**4 + 105*x**2 + 70*x - 98,
        288*y + 96*x**2,
        y
    ) == {
        -580608*x**4 + 8709120*x**2 + 5806080*x - 8128512,
        1 # WolframAlpha says 288
    }
)

assert(
    PSC(
        x*y**2 + 6*x*y + x**3 + 9*x,
        2*x*y + 6*x,
        y
    ) == {
        4*x**5,
        1 # WolframAlpha says 2x
    }
)

assert(
    PSC(
        144*y**2 + 96*x**2*y + 9*x**4 + 105*x**2 + 70*x - 98,
        x*y**2 + 6*x*y + x**3 + 9*x,
        y
    ) == {
        81*x**10 + 3330*x**8 + 1260*x**7 - 37395*x**6 - 45780*x**5 - 32096*x**4 + 167720*x**3 + 1435204*x**2,
        -96*x**3 + 864*x,
        1
    }
)

assert(
    PSC(
        96*x**2*y + 9*x**4 + 105*x**2 + 70*x - 98,
        x*y**2 + 6*x*y + x**3 + 9*x,
        y
    ) == {
        81*x**9 + 5922*x**7 + 1260*x**6 + 31725*x**5 - 25620*x**4 + 40768*x**3 - 13720*x**2 + 9604*x,
        1 # WolframAlpha says 96*x**2
    }
)

assert(
    PSC(
        144*y**2 + 96*x**2*y + 9*x**4 + 105*x**2 + 70*x - 98,
        6*x*y + x**3 + 9*x,
        y
    ) == {
        -108*x**6 + 1188*x**4 + 2520*x**3 + 8136*x**2,
        1 # WolframAlpha says 6*x
    }
)

assert(
    PSC(
        144*y**2 + 96*x**2*y + 9*x**4 + 105*x**2 + 70*x - 98 + 5*y**3*x**2,
        x*y**2 + 6*x*y + x**3 + 9*x + 13*y**4*x,
        y
    ) == {
        14414517*x**19 + 687793900*x**17 + 448451640*x**16 + 11694974205*x**15 + 15844701600*x**14 + 83044945098*x**13 + 171966372480*x**12 + 121914266472*x**11 + 346814252400*x**10 - 337255476500*x**9 - 1379936299840*x**8 + 698835050936*x**7 + 1974720551440*x**6 - 844086248304*x**5 - 1497378034880*x**4 + 1241148924496*x**3,
        68445*x**12 + 1593025*x**10 + 1064700*x**9 + 113972316*x**8 + 12148500*x**7 - 488676520*x**6 - 338669240*x**5 + 479769524*x**4 - 232906752*x**2,
        -6215*x**5 + 269568*x,
        1 # WolframAlpha says 5*x**2
    }
)
