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
        # Can we test against instance variables? Properties? Better write a getter method?
        # Add test to check dimension of sample against self.dimension
        self.sample = Point(0, 0)

    # Should we add an exception if point.dimension != self.dimension?
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
        if (i >= self.length):
            raise StackCellOutOfBoundsError(self, i);
        return self.cells[i]

class QepCadError:
    def __str__(self):
        return "QepCadError"

class StackCellOutOfBoundsError:
    def __init__(self, stack, index):
        self.stack = stack;
        self.index = index;
        self.msg = ("Tried to get cell "+ index 
                    + " for Stack " + repr(self.stack)
                    + " of length " + str(self.cell.length));
    def __str__(self):
        return self.msg;


# Execute doctest when run from the command line
if __name__ == "__main__":
    import doctest
    doctest.testmod()