"""
This is the "subresultants" module. It supplies one function, PSC().
Eventually it should probably be integrated into ProjectionPhase.py
"""

import sympy
from sympy import subresultants, LC, degree

def PSC(F, G, x):
    """PSC(F, G, x) returns a set with the non-zero principal subresultant
    coefficients (psc) of the two polynomials F and G with respect to the
    variable x.
    
    The n-th psc, where n is the minimum of the degrees of F and G with
    respect to x, is defined as 1 when F and G have the same degree.
    Extended psc beyond the n-th are not considered.
    
    If the degree of the polynomial F is strictly less than the degree of
    the polynomial G, then F and G are interchanged.

    >>> PSC(0, 0, var('x'))
    set()
    >>> PSC(poly('2*x'), poly('3*y'), var('x'))
    {3*y}
    >>> PSC(poly('2*x'), poly('3*y + 5*x**2'), var('x')) == {12*var('y'), 2}
    True
    >>> PSC(poly('x**3'), poly('x**3 + x'), var('x'))
    {1}
    """
    degF = degree(F, x)
    degG = degree(G, x)
    if degF < degG:
        [F, G] = [G, F]
        [degF, degG] = [degG, degF]
    subs = subresultants(F, G, x)
    s = set()
    i = len(subs) - 1
    while i > 1:
        coef = LC(subs[i], x)
        degS = degree(subs[i], x)
        s.add( coef**(degG-i+2-degS) )
        i -= 1
    if i > 0:
        s.add( LC(subs[i], x)**(degF-degG) )
    return s


# Execute doctest when run from the command line
if __name__ == "__main__":
    import doctest
    from sympy import poly, var
    doctest.testmod()
