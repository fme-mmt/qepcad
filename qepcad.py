from sympy import *
from sympy.geometry import *

class Cell:
    def __init__(self):
    # Public instance variables
        self.dimension = 0
    # Point does not handle 1D. Should we provide our own CellPoint class or interface?
        self.sample = Point(0, 0)

    def containsPoint(self, point: Point) -> bool:
        # point should be of class Point from sympy geometry package
        # Do some magic here
        pass