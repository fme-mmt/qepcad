from sympy import *
from ProjectionPhase import *
from lifting import *

# Input: list of polys
def CAD(polys):
    outputProj = proj(polys)
    cad = Cad()
    # TODO: handle variable projection order
    cad.construct(outputProj['projection'])
