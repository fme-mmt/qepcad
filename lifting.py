from sympy import *

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
        # self.cad = stack.cad

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
    def containsPoint(self, point) -> bool:
        # point should be of class Point from sympy geometry package
        # Do some magic here
        return False


class Stack:
    """
    Stack class

    Init method constructs a stack over baseCell from a projection factor set

    When the projection factor set is a set of univariate polynomials with some roots,
    the result is a stack with a cell of dimension 0 for each root
    and a cell of dimension 1 for each interval between roots.
    >>> stack = Stack(None, [Poly(x**2-1), Poly((x-1)**2-1)])
    >>> for c in stack.cells:
    ...     print(c.dimension)
    1
    0
    1
    0
    1
    0
    1
    0
    1
    >>> for c in stack.cells:
    ...     print(c.sample)
    [-1.10000000000000]
    [-1]
    [-1/2]
    [0]
    [1/2]
    [1]
    [3/2]
    [2]
    [2.10000000000000]

    When the projection factor set is a set of univariate polynomials with no roots,
    the result is a single cell containing the whole real line with an arbitrary sample point
    that is set to be 0:
    >>> stack = Stack(None, [Poly(x**2+1)])
    >>> for c in stack.cells:
    ...     print(c.dimension)
    1
    >>> for c in stack.cells:
    ...     print(c.sample)
    [0]

    When the projection factor set is a set of multivariate polynomials with roots,
    the result is similar to the univariate case, but substitution of the baseCell
    sample point is performed.
    >>> stack = Stack(Cell(2, [0, 0], None), [Poly(x**2+y**2+z**2-9), Poly(x**2+y**2+(z-1)**2-9)])
    >>> for c in stack.cells:
    ...     print(c.dimension)
    3
    2
    3
    2
    3
    2
    3
    2
    3
    >>> for c in stack.cells:
    ...     print(c.sample)
    [0, 0, -3.10000000000000]
    [0, 0, -3]
    [0, 0, -5/2]
    [0, 0, -2]
    [0, 0, 1/2]
    [0, 0, 3]
    [0, 0, 7/2]
    [0, 0, 4]
    [0, 0, 4.10000000000000]

    When the projection factor set is a set of multivariate polynomials with no roots,
    the result is similar to the univariate case, but substitution of the baseCell
    sample point is performed.
    >>> stack = Stack(Cell(2, [0, 0], None), [Poly(x**2+y**2+z**2+9)])
    >>> for c in stack.cells:
    ...     print(c.dimension)
    3
    >>> for c in stack.cells:
    ...     print(c.sample)
    [0, 0, 0]
    """
    def __init__(self, baseCell, projectionFactor):
        self.baseCell = baseCell
#         self.cad = baseCell.cad  # how to handle the first stack?
                                # Somehow we'll have to set this directly. Subclass? Another init?
        self.roots = []
        #p is a set of polynomials of the projectionFactor
        for p in projectionFactor:
            q = p
            if baseCell:
                q = q.subs(baseCell.getSamplePoint())
            newRoots = solve(q)
            for purgedRoots in newRoots:
                if im(purgedRoots.evalf()) == 0:
                    self.roots.append(purgedRoots)

            # TODO: purgar las raíces no reales
            # TODO: decirle que tome las racíces para una variable concreta(?)

        self.roots.sort()
        self.cells = self.constructStackCells(self.roots)


    # Private method
    def constructStackCells(self, roots):
        baseCell = Cell(0, [], self)
        if self.baseCell:
            baseCell = self.baseCell

        # If no roots -> cell is R^n
        if len(roots) == 0:
            dim = baseCell.dimension + 1
            return [Cell(dim, [0] * dim, self)]

        cells = []
        # declaro eps con un valor arbitrario para que compile
        eps = 0.1

        # First cell
        firstCell = Cell(baseCell.dimension + 1,
                         baseCell.sample + [roots[0] - eps], self)
        cells.append(firstCell)
        for i in range(0, len(roots) - 1):
            cellRoot = Cell(baseCell.dimension,
                            baseCell.sample + [roots[i]], self)
            cellNext = Cell(baseCell.dimension + 1,
                            baseCell.sample + [(roots[i] + roots[i + 1]) / 2], self)
            cells.extend([cellRoot, cellNext])

        cellLastRoot = Cell(baseCell.dimension,
                            baseCell.sample + [roots[-1]], self)
        cells.append(cellLastRoot)

        lastCell = Cell(baseCell.dimension + 1,
                        baseCell.sample + [roots[-1] + eps], self)
        cells.append(lastCell)

        return cells


#     def getCad(self):
#         return self.cad;

    def getBaseCell(self):
        return self.baseCell

    def getCell(self, i: int) -> Cell:
        return self.cells[i]

    def getNumberOfCells(self):
        return len(self.cells)

    def getDimension(self):
        if self.baseCell:
            return self.baseCell.dimension + 1
        return -1

""" Cad.construct(projectionFactorSet) returns a list stackList of lists stacks were the i stacks element
  is all the stacks of the i phase """
class Cad:
    def __init__(self):
        self.dimension = 0
        self.stackList = []

    def construct(self, projectionFactorSet):
        projectionFactor = projectionFactorSet[0]
        stack = Stack(Cell(0, [], None), projectionFactor)
        self.stackList.append([stack])
        for i in range(1, len(projectionFactorSet)):
            self.stackList.append([])
            projectionFactor = projectionFactorSet[i]
            for previosPhaseStack in self.stackList[i - 1]:
                for cell in previosPhaseStack.cells:
                    stack = Stack(cell, projectionFactor)
                    self.stackList[i].append(stack)


    def containsPoint(self, point) -> bool:
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


def cadExtension(cad, projectionFactorSet):
    # para stack
    #     para celda del stack
    #         me construyo el nuevo stack sobre esta celda
    #         y añado el nuevo stack en este mismo cad
    for stack in cad.stacks:
        for cell in stack:
            newStack = Stack(cell, projectionFactorSet)
            cad.stacks.append(newStack)
    return cad


# Hay que escribir funcion que construia cad caso base
def baseCad(projectionFactorSet):
#     """
#     Constructing a base cad for a polinomial gives us a cad of dimension 1
#     (intervals and points)
#     >>> cad = baseCad([Poly(x**2-1)])
#     >>> cad.dimension
#     1
#     """
    # Tenemos n polinomios de una variable. A partir de sus r raíces
    # obtenemos las primeras r+1 celdas
    roots = []
    baseCad = Cad()
    for p in projectionFactorSet:
        # añado las raíces a el conjunto de raíces.
        for r in solve(p):
            roots.append(r)

    # ordeno las raíces para crear mi conjunto de indices.
    roots.sort()

    stack = (None, projectionFactorSet)  # this is a hack, which stack shall we put here?
    eps = 0.1
    j = 0

    for i in range(0, 2 * len(roots) + 1):
        # Añadimos los puntos muestra(que tienen solo una coordenada)
        if i == 0:
            cell = Cell(1, [roots[0] - eps], stack)
            baseCad.addCell(cell)
            cell = Cell(1, [roots[0]], stack)
            baseCad.addCell(cell)
            j += 1

        elif i == 2 * len(roots):
            cell = Cell(1, [roots[-1] + eps], stack)
            baseCad.addCell(cell)

        # si estamos en una celda par añade el punto medio, si no añade la raíz como punto muestra.
        elif i % 2 == 0:
            if (j + 1) < len(roots):
                cell = Cell(1, [(roots[j] + roots[j + 1]) / 2], stack)
                baseCad.addCell(cell)
                j += 1
        else:
            if j < len(roots) and i < 2 * len(roots) - 1:
                cell = Cell(0, roots[j], stack)
                baseCad.addCell(cell)
    return baseCad





# Execute doctest when run from the command line
if __name__ == "__main__":
    import sympy.polys
    import doctest
    x, y, z = symbols('x y z')
    doctest.testmod()
