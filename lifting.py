from sympy import *
from sympy.geometry import *

class Cell:
#     """
#     CAD Cell
#
#     [Insert long description here]
#
#     Obviously, the sample point belongs to the cell:
#     >>> c = Cell()
#     >>> c.containsPoint(c.sample);
#     True
#     """

    def __init__(self, dimension, sample, stack):
    # Public instance variables
        self.dimension = dimension
        self.sample = sample
        self.stack = stack
        # self.positionInStack = 0
        self.cad = stack.cad

        # self.belongsToRegionProjection = 0;

        # Point does not handle 1D. Should we provide our own CellPoint class or interface?
        # Can we test against instance variables? Properties? Better write a getter method?
        # Add test to check dimension of sample against self.dimension

    def getDimension(self):
        return self.dimension

    def getSamplePoint(self):
        return self.sample
#         if self.sample: # check syntax here, how to tell if variable null in python?
#             pass
#         else
#             # here we need the projection factor set, which should be a member of Cad
#             # se we need a pointer to the Cad the cell belongs to

    # Should we add an exception if point.dimension != self.dimension?
    def containsPoint(self, point: Point) -> bool:
        # point should be of class Point from sympy geometry package
        # Do some magic here
        return False;


class Stack:
#     """
#     CAD Stack
#
#     [Insert long description here]
#
#     getCell raises a StackCellOutOfBoundsError exception when index is below 0 or above length:
#     >>> from qepcad import *
#     >>> s = Stack()
#     >>> s.getCell(8)
#     Traceback (most recent call last):
#         ...
#     qepcad.StackCellOutOfBoundsError: Tried to get cell with index 8 for Stack of length 0
#     >>> s.getCell(-1)
#     Traceback (most recent call last):
#         ...
#     qepcad.StackCellOutOfBoundsError: Tried to get cell with index -1 for Stack of length 0
#     """
    """ Constructs a stack over baseCell from a projection factor set """
    def __init__(self, baseCell, projectionFactorSet):
        self.baseCell = baseCell
        self.cad = baseCell.cad  # hot to handle the first stack?
                                # Somehow we'll have to set this directly. Subclass? Another init?
        roots = []
        for p in projectionFactorSet:
            q = p.eval(baseCell.getSamplePoint())
            roots.extend(q.all_roots())
        self.cells = self.constructStackCells(roots)


    # Private method
    def constructStackCells(self, roots):
        cells = [];

        # First cell
        firstCell = Cell(self.baseCell.dimension + 1,
                         self.fbaseCell.sample.append(roots[0] + eps), self)
        cells.append(firstCell)

        for i in range(1, roots.length - 1):
            cellRoot = Cell(self.baseCell.dimension,
                            self.baseCell.sample.append(roots[i]), self)
            cellNext = Cell(self.baseCell.dimension + 1,
                            self.baseCell.sample.append((roots[i] + roots[i + 1] / 2)), self)
            cells.extend([cellRoot, cellNext])

        cellLastRoot = Cell(self.baseCell.dimension,
                            self.baseCell.sample.append(roots[i]), self)
        cells.append(cellLastRoot)

        lastCell = Cell(self.baseCell.dimension + 1,
                        self.baseCell.sample.append(roots[-1] + eps), self)
        cells.append(lastCell)

        return cells


    def getCad(self):
        return self.cad;

    def getBaseCell(self):
        return self.baseCell

    def getCell(self, i: int) -> Cell:
        return self.cells[i]

    def getNumberOfCells(self):
        return len(self.cells)

    def getDimension(self):
        return self.baseCell.dimension + 1


class Cad:
    def __init__(self):
        self.dimension = 0
        self.cells = []

    def containsPoint(self, point: Point) -> bool:
        pass;

    def cellExists(self, index: list):
        pass;

    """ si index tiene n coordenadas, nos devuelve la celda n-dimensional correspondiente """
    def getCell(self, index: list):
        pass;

    # metodo para añadir celdas (se usa en cada iteración para añadir las nuevas celdas)

    def addCell(self, cell: Cell):
        self.cells.append(cell)
        if self.dimension < cell.dimension:
            self.dimension = cell.dimension


def cadExtension(self, cad, projectionFactorSet):
    # para stack
    #     para celda del stack
    #         me construyo el nuevo stack sobre esta celda
    #         y añado el nuevo stack en este mismo cad

    # igual esto sería al reves
    # para cada celda del cad
    for cell in cad.cells:
        # construyo el nuevo stack sobre esta celda y añado el nuevo stack en este mismo cad
        cad.addCell(stackConstruction(self, cell, projectionFactorSet))



 # Hay que escribir funcion que construia cad caso base
def baseCad(projectionFactorSet):
    """
    Constructing a base cad for a polinomial gives us a cad of dimension 1
    (intervals and points)
    >>> cad = baseCad([Poly(x**2-1)])
    >>> cad.dimension
    1
    """
    # Tenemos n polinomios de una variable. A partir de sus r raíces
    # obtenemos las primeras r+1 celdas
    roots = []
    baseCad = Cad()
    for p in projectionFactorSet:
        # añado las raíces a el conjunto de raíces.
        roots.append(solveset(p))  # # una vez tenga el formato de las cosas lo
                                    # # escribiré como toca

    # ordeno las raíces para crear mi conjunto de indices.
    roots.sort()

    stack = 0 # this is a hack, which stack shall we put here?
    eps = 0.1
    j = 0
    for i in range(0, 2 * len(roots) + 1):
         # Añadimos los puntos muestra(que tienen solo una coordenada)
        if i == 0:
            cell = Cell(1, roots[0] - eps, stack)
            j += 1

        if i == 2 * len(roots) + 1 :
            cell = Cell(1, roots[-1] + eps, stack)
            continue

        # si estamos en una celda par añade el punto medio, si no añade la raíz como punto muestra.
        if i % 2 == 0 :
            cell = Cell(1, (roots[j] + roots[j + 1]) / 2))
            j += 1
        else:
            cell = Cell(0, roots[j])

        baseCad.addCell(cell)

    return baseCad





# Execute doctest when run from the command line
if __name__ == "__main__":
    import sympy.polys
    import doctest
    x, y, z = symbols('x y z')
    doctest.testmod()
