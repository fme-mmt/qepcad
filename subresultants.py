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

    >>> PSC(0, 0, var('y'))
    set()
    >>> PSC(poly('2*y'), poly('3*x'), var('y'))
    {3*x}
    >>> PSC(poly('2*y'), poly('3*x + 5*y**2'), var('y'))
    {12*x, 2}
    >>> PSC(poly('y**3'), poly('y**3 + y'), var('y'))
    {1}
    """
    s = set()
    subs = subresultants(F, G, x)
    deg1 = degree(F, x)
    deg2 = degree(G, x)
    if deg1 < deg2:
        [F, G] = [G, F]
        [deg1, deg2] = [deg2, deg1]
    i = len(subs) - 1
    while i > 1:
        coef = LC(subs[i], x)
        deg = degree(subs[i], x)
        s.add( coef**(deg2-i+2-deg) )
        i -= 1
    if i > 0:
        s.add( LC(subs[i], x)**(deg1-deg2) )
    return s


# Execute doctest when run from the command line
if __name__ == "__main__":
    import doctest
    from sympy import poly, var
    doctest.testmod()
