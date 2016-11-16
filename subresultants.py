"""
This is the "subresultants" module. It supplies one function, PSC().
Eventually it should probably be integrated into ProjectionPhase.py
"""

import sympy
from sympy import subresultants, LC, degree

def PSC(F, G, x):
    """PSC(F, G, x) returns the list of the non-zero principal subresultant
    coefficients (psc) of the two polynomials F and G with respect to the
    variable x.
    
    The n-th psc, where n is the minimum of the degrees of F and G with
    respect to x, is defined as 1. Extended psc beyond the n-th are not
    considered.

    >>> PSC(0, 0, var('y'))
    []
    >>> PSC(poly('2*y'), poly('3*x'), var('y'))
    [1]
    >>> PSC(poly('2*y'), poly('3*x + 5*y**2'), var('y'))
    [12*x, 1]
    >>> PSC(poly('y**3'), poly('y**3 + y'), var('y'))
    [1, 1]
    """
    n = min( degree(F,x), degree(G,x) )
    if n < 0:
        return []
    subs = subresultants(F, G, x)[2:]
    s = []
    for p in reversed(subs):
        s.append( LC(p, x) )
    s.append(1); # SIAM defines it as 1 but WolframAlpha sometimes says otherwise
    return s


# Execute doctest when run from the command line
if __name__ == "__main__":
    import doctest
    from sympy import poly, var
    doctest.testmod()
