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

    def __init__(self, stack, positionInStack):
    # Public instance variables
        self.stack = stack
        self.positionInStack = positionInStack
        self.cad = stack.cad
        
        #self.belongsToRegionProjection = 0;
        
        # Point does not handle 1D. Should we provide our own CellPoint class or interface?
        # Can we test against instance variables? Properties? Better write a getter method?
        # Add test to check dimension of sample against self.dimension

    def dimension(self):
        pass
    
    def samplePoint(self):
        pass
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

    def __init__(self, baseCell, stackCells):
    # Public instance variables
        self.length = 0
        self.dimension = 0
        self.baseCell = baseCell
        self.cad = baseCell.cad # hot to handle the first stack? 
                                # Somehow we'll have to set this directly. Subclass? Another init?
    # Private instance variables
        self.cells = stackCells
        
    def getCell(self, i: int) -> Cell:
        if i >= self.length or i < 0:
            raise StackCellOutOfBoundsError(self, i);
        return self.cells[i]
    


class QepCadError:
    def __str__(self):
        return "QepCadError"

class StackCellOutOfBoundsError(Exception):
    def __init__(self, stack, index):
        self.stack = stack
        self.index = index
        # We should use standard unit testing for testing exceptions. 
        # doctest checks if the correct exception is raise by comparing the message it prints.
        # So if the excpetion message changes, the tests break. Not good :(
        self.msg = ("Tried to get cell with index "+ str(index)
                    + " for Stack of length " + str(self.stack.length))
    def __str__(self):
        return self.msg
        


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

    def addCell(self,cell: Cell):
        self.cells.append(cell)
        if self.dimension < cell.dimension:
            self.dimension = cell.dimension

        
def cadExtension(self, cad, projectionFactorSet):
    # para stack
    #     para celda del stack
    #         me construyo el nuevo stack sobre esta celda
    #         y añado el nuevo stack en este mismo cad

    #igual esto sería al reves
    #para cada celda del cad
    for cell in cad.cells:
        #construyo el nuevo stack sobre esta celda y añado el nuevo stack en este mismo cad
        cad.addCell(stackConstruction(self,cell,projectionFactorSet))


def stackConstruction(self, cell, projectionFactorSet):
    
    # construimos un nuevo conjunto P de polinomios del siguiente modo:
    # para cada polinomio substituimos las k-1 variables por el punto muestra,
    # de modo que obtenemos un polinomio de 1 variable.
    # 
    # para cada polinomio de P
    roots = []
    for p in projectionFactorSet:
        q = p.eval(cell.samplePoint())
    #    buscamos las raíces de P. si obtenemos r raíces,
    #    significa que construimos r+1 nuevas celdas de dimension cell.dimension+1 
    #    i r celdas (las correspondientes a las raíces) de dimension cell.dimension
        roots.extend(q.all_roots())
    
    stackCells = constructStackCells(cell, roots)
    return Stack(cell, stackCells)

def constructStackCells(baseCell, roots):
        cells = [];

        # First cell
        firstCell = Cell(baseCell.dimension + 1, baseCell.sample.append(roots[0] + eps))
        cells.append(firstCell)
        
        for i in range(1, roots.length-1):
            cellRoot = Cell(baseCell.dimension, baseCell.sample.append(roots[i]))
            cellNext = Cell(baseCell.dimension + 1, baseCell.sample.append((roots[i]+roots[i+1]/2)))
            cells.extend([cellRoot, cellNext])
            
        cellLastRoot = Cell(baseCell.dimension, baseCell.sample.append(roots[i]))
        cells.append(cellLastRoot)
        
        lastCell = Cell(baseCell.dimension + 1, baseCell.sample.append(roots[-1] + eps))
        cells.append(lastCell)
        
        return cells

 # Hay que escribir funcion que construia cad caso base
def baseCad(self, projectionFactorSet):
    """
    Constructing a base cad for a polinomial gives us a cad of dimension 1
    (intervals and points)
    >>> cad = baseCad([Poly(x**2-1)])
    >>> cad.dimension
    1
    """
    #Tenemos n polinomios de una variable. A partir de sus r raíces
    #obtenemos las primeras r+1 celdas
    roots = []
    baseCad = Cad()
    for p in projectionFactorSet:
        #añado las raíces a el conjunto de raíces.
        roots.append(solveset(p)) ## una vez tenga el formato de las cosas lo
                                    ## escribiré como toca
    
    #ordeno las raíces para crear mi conjunto de indices.
    roots.sort()
    
    eps = 0.1
    j = 0
    for i in range(0,2*len(roots)+1):
        cell = Cell()
         #Añadimos los puntos muestra(que tienen solo una coordenada)
        if i == 0:
            cell.sample=roots[0]-eps
            cell.dimension = 1
            j += 1
        
        if i == 2*len(roots)+1 :
            cell.sample = roots[-1]+eps
            cell.dimension = 1
            continue
                
        #si estamos en una celda par añade el punto medio, si no añade la raíz como punto muestra.
        if i%2==0 :
            cell.dimension = 1
            cell.sample = (roots[j]+roots[j+1])/2
            j+=1
        else:
            cell.dimension = 0
            cell.sample = roots[j]
        
        baseCad.addCell(cell)

    return baseCad
       
        
            
    
         
# Execute doctest when run from the command line
if __name__ == "__main__":
    import sympy.polys
    import doctest
    doctest.testmod()
