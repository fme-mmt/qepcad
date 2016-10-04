from sympy import *
from sympy.geometry import *

class Cell:
    """
    CAD Cell
    
    [Insert long description here]
    
    Obviously, the sample point belongs to the cell:
    >>> c = Cell()
    >>> c.containsPoint(c.sample);
    True
    """

    def __init__(self):
    # Public instance variables
        self.dimension = 0
    # Point does not handle 1D. Should we provide our own CellPoint class or interface?
        self.sample = Point(0, 0)

    def containsPoint(self, point: Point) -> bool:
        # point should be of class Point from sympy geometry package
        # Do some magic here
        return False;


class Stack:
    """
    CAD Stack
    
    [Insert long description here]
    
    """

    def __init__(self):
    # Public instance variables
        self.length = 0
        self.dimension = 0
        
    # Private instance variables
        self.cells = []
        
    def getCell(self, i: int) -> Cell:
        return cells[i]


# Execute doctest when run from the command line
if __name__ == "__main__":
    import doctest
    doctest.testmod()