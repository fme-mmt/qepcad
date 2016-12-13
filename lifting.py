from sympy import *

allSymbols = []


class Cell:
    """
    CAD Cell: the class that we use to save the Cell related data.

    Attributes:
        dimension: the dimension of the cell
        sample: the sample point of the cell
        stack: the stack where that cell belongs.
    """
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
        # self.belongsToRegionProjection = 0;

    def get_dimension(self):
        return self.dimension

    def get_sample_point(self):
        return self.sample
        # TODO: Add an exception if the sample is not right
#         if self.sample: # check syntax here, how to tell if variable null in python?
#             pass
#         else
#             # here we need the projection factor set, which should be a member of Cad
#             # se we need a pointer to the Cad the cell belongs to

    # Should we add an exception if point.dimension != self.dimension?
    def contains_point(self, point) -> bool:
        # point should be of class Point from sympy geometry package
        # Do some magic here
        return False


class Stack:
    """
    Stack class

    Init method constructs a stack over base_cell from a projection factor set taking the real roots and sorting them.

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
    the result is similar to the univariate case, but substitution of the base_cell
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
    the result is similar to the univariate case, but substitution of the base_cell
    sample point is performed.
    >>> stack = Stack(Cell(2, [0, 0], None), [Poly(x**2+y**2+z**2+9)])
    >>> for c in stack.cells:
    ...     print(c.dimension)
    3
    >>> for c in stack.cells:
    ...     print(c.sample)
    [0, 0, 0]
    """
    def __init__(self, base_cell, projection_factor):
        self.base_cell = base_cell
        self.roots = []
        # p is a set of polynomials of the projection_factor
        for p in projection_factor:
            q = p
            if base_cell:
                q = eval_better(p, base_cell.get_sample_point())

            new_roots = solve(q)
            for purgedRoots in new_roots:
                if im(purgedRoots.evalf()) == 0:
                    self.roots.append(purgedRoots)

        self.roots.sort()
        self.cells = self.construct_stack_cells(self.roots)

    # Private method
    def construct_stack_cells(self, roots):
        base_cell = Cell(0, [], self)
        if self.base_cell:
            base_cell = self.base_cell

        # If no roots -> cell is R^n -> We take an arbitrary number (0)
        if not roots:
            dim = base_cell.dimension + 1
            return [Cell(dim, base_cell.sample + [0], self)]

        cells = []
        # We use an arbitrary number to define the cells of the boundary of our roots list.
        eps = 0.1
        # We need to delete the repeated roots to avoid errors.
        # TODO: Make it more efficient.
        original_roots = roots
        roots = []
        for i in original_roots:
            if i not in roots:
                roots.append(i)

        # First cell
        first_cell = Cell(base_cell.dimension + 1, 
                          base_cell.sample + [roots[0] - eps], self)
        
        cells.append(first_cell)
        for i in range(0, len(roots) - 1):
            cell_root = Cell(base_cell.dimension,
                             base_cell.sample + [roots[i]], self)
            cell_next = Cell(base_cell.dimension + 1,
                             base_cell.sample + [(roots[i] + roots[i + 1]) / 2], self)
            cells.extend([cell_root, cell_next])

        cell_last_root = Cell(base_cell.dimension,
                              base_cell.sample + [roots[-1]], self)
        cells.append(cell_last_root)

        last_cell = Cell(base_cell.dimension + 1,
                         base_cell.sample + [roots[-1] + eps], self)
        cells.append(last_cell)

        return cells


#     def getCad(self):
#         return self.cad;

    def get_base_cell(self):
        return self.base_cell

    def get_cell(self, i: int) -> Cell:
        return self.cells[i]

    def get_number_of_cells(self):
        return len(self.cells)

    def get_dimension(self):
        if self.base_cell:
            return self.base_cell.dimension + 1
        return -1

""" Cad.construct(projection_factor_set) returns a list stackList of lists stacks were the i stacks element
  is all the stacks of the i phase """


class Cad:
    """
    This is the base class of the Cad.

    Attributes:
        dimension: the maximum dimension of the cad.
        stackList: a list of stacks that makes the cad.

    Methods:
        construct: we construct the cad using the projection factor set.
        contains_point: return true if the point is in the cad and false else
        get_cell: return a n-cell
    """
    def __init__(self):
        self.dimension = 0
        self.stackList = []

    def construct(self, projection_factor_set):
        # We put all the symbols in a list to use it latter in the eval_better function
        for setp in projection_factor_set:
            for p in setp:
                for symb in p.atoms(Symbol):
                    if symb not in allSymbols:
                        allSymbols.append(symb)

        projection_factor = projection_factor_set[0]
        stack = Stack(Cell(0, [], None), projection_factor)
        self.stackList.append([stack])
        for i in range(1, len(projection_factor_set)):
            self.stackList.append([])
            projection_factor = projection_factor_set[i]
            for previousPhaseStack in self.stackList[i - 1]:
                for cell in previousPhaseStack.cells:
                    stack = Stack(cell, projection_factor)
                    self.stackList[i].append(stack)

    def contains_point(self, point) -> bool:
        pass

    def cell_exists(self, index: list):
        pass

    def get_cell(self, index: list):
        """
        :param index: a list of n coordinates.
        :return: The n-dimensional corresponding cell.
        """
        pass


def cad_extension(cad, projection_factor_set):
    """
    :param cad: the cad that we want to extend
    :param projection_factor_set: the projection factor set that of one dimension more than the cad that we will use to
        extend
    :return: the extended cad

    This method does:
    For each stack in cad
        for each cell in stack
            we build a new stack in that cell and add the new stack to the original cad
    """

    for stack in cad.stacks:
        for cell in stack:
            new_stack = Stack(cell, projection_factor_set)
            cad.stacks.append(new_stack)
    return cad


def eval_better(p, sample):
    """
    :param p: a polynomial of n variables.
    :param sample: a sample point of n-1 variables.
    :return: the polynomial evaluated in the n-1 variables (if they're present)
    """
    n = len(sample)
    psyms = p.atoms(Symbol)
    m = len(psyms)
    if m == 1:
        return p

    for i in range(0, n):
        if allSymbols[i] in psyms:
            p = p.eval(allSymbols[i], sample[i])
    return p

# Execute doctest when run from the command line
if __name__ == "__main__":
    import sympy.polys
    import doctest
    x, y, z = symbols('x y z')
    doctest.testmod()
