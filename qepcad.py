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
        self.belongsToRegionProjection = 0;
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
    
    getCell raises a StackCellOutOfBoundsError exception when index is below 0 or above length:
    >>> from qepcad import *
    >>> s = Stack()
    >>> s.getCell(8)
    Traceback (most recent call last):
        ...
    qepcad.StackCellOutOfBoundsError: Tried to get cell with index 8 for Stack of length 0
    >>> s.getCell(-1)
    Traceback (most recent call last):
        ...
    qepcad.StackCellOutOfBoundsError: Tried to get cell with index -1 for Stack of length 0
    """

    def __init__(self):
    # Public instance variables
        self.length = 0
        self.dimension = 0
        self.originalCell = Cell()
    # Private instance variables
        self.cells = []
        
    def getCell(self, i: int) -> Cell:
        if (i >= self.length or i < 0):
            raise StackCellOutOfBoundsError(self, i);
        return self.cells[i]

class QepCadError:
    def __str__(self):
        return "QepCadError"

class StackCellOutOfBoundsError(Exception):
    def __init__(self, stack, index):
        self.stack = stack;
        self.index = index;
        # We should use standard unit testing for testing exceptions. 
        # doctest checks if the correct exception is raise by comparing the message it prints.
        # So if the excpetion message changes, the tests break. Not good :(
        self.msg = ("Tried to get cell with index "+ str(index)
                    + " for Stack of length " + str(self.stack.length));
    def __str__(self):
        return self.msg;

class Cad:
    def __init__(self):
        pass;
    
    def containsPoint(self, point: Point) -> bool:
        pass;
    
    """ si index tiene n coordenadas, nos devuelve la celda n-dimensional correspondiente """
    def getCell(self, index: list):

# Execute doctest when run from the command line
if __name__ == "__main__":
    import doctest
    doctest.testmod()