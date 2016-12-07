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
    
    If the degree of the polynomial F is strictly less than the degree of
    the polynomial G, then F and G are interchanged.
    
    Extended psc beyond the n-th are not considered, where n is the minimum
    of the degrees of F and G with respect to x.

    >>> PSC(0, 0, var('x'))
    set()
    >>> PSC(poly('2*x'), poly('3*y'), var('x'))
    {3*y}
    >>> PSC(poly('2*x'), poly('3*y + 5*x**2'), var('x')) == {12*var('y'), 2}
    True
    >>> PSC(poly('x**3'), poly('x**3 + x'), var('x'))
    {1}
    """
    subs = subresultants(F, G, x)
    s = set()
    i = len(subs) - 1
    if i < 0:
        return s
    currDeg = degree(subs[i], x)
    while i > 0:
        nextDeg = degree(subs[i-1], x)
        s.add( LC(subs[i], x)**(nextDeg-currDeg) )
        currDeg = nextDeg
        i -= 1
    return s


# Execute doctest when run from the command line
if __name__ == "__main__":
    import doctest
    from sympy import poly, var
    doctest.testmod()
