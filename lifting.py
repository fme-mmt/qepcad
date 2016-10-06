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
        
    def addSamplePoint(self,point : Point):
        self.sample = point;
    
    def addDimension(self,dimension: int):
        self.dimension = dimension;

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
        self.dimension = 0;
    
    def containsPoint(self, point: Point) -> bool:
        pass;
    
    def cellExists(self, index: list):
        pass;
    
    """ si index tiene n coordenadas, nos devuelve la celda n-dimensional correspondiente """
    def getCell(self, index: list):
        pass;
    
    # metodo para añadir celdas (se usa en cada iteración para añadir las nuevas celdas)
    #En un principio pondría la cabecera así, la implementación dependerá 
    #de la estructura de datos que usemos.  
    def addCell(self,cell: Cell):
        pass;
        
def cadExtension(self, cad, projectionFactorSet):
    # para stack
    #     para celda del stack
    #         me construyo el nuevo stack sobre esta celda
    #         y añado el nuevo stack en este mismo cad

def stackConstruction(self, Cell, projectionFactorSet):
    
    # construimos un nuevo conjunto P de polinomios del siguiente modo:
    # para cada polinomio substituimos las k-1 variables por el punto muestra,
    # de modo que obtenemos un polinomio de 1 variable.
    # 
    # para cada polinomio de P
    #    buscamos las raíces de P. si obtenemos r raíces,
    #    significa que construimos r+1 nuevas celdas de dimension cell.dimension+1 
    #    i r celdas (las correspondientes a las raíces) de dimension cell.dimension
    #    
    #    para construir cada celda de dimension cell.dimension
    #        c = Cell()
    #        c.dimension = cell.dimension
    #        c.sample = cell.sample | raiz ("concatencion de coordenada)
    #
    #    para construir cada celda de dimension cell.dimension+1
    #        si primera o ultima celda
    #            c = Cell()
    #            c.dimension = cell.dimension+1
    #            c.sample = cell.sample | raiz +- eps
    #        si no
    #            c = Cell()
    #            c.dimension = cell.dimension+1
    #            c.sample = cell.sample | punto medio entre raizes
    #
    #        
    
 # Hay que escribir funcion que construia cad caso base
def baseCad:
    #Tenemos n polinomios de una variable. A partir de sus r raíces
    #obtenemos las primeras r+1 celdas
    roots = [];
    baseStack = Stack();
    for p in polinomialSet:
        #añado las raíces a el conjunto de raíces.
        roots.append(solveset(p)); ## una vez tenga el formato de las cosas lo 
                                    ## escribiré como toca
    
    #ordeno las raíces para crear mi conjunto de indices.
    roots.sort();
    eps = 0.1;
    j = 0;
    for i in range(0,2*len(roots)+1):
        cell = Cell();
         #Añadimos los puntos muestra(que tienen solo una coordenada)
        if(i == 0):
            cell.addSamplePoint(roots[0]-eps);
            cell.addDimension(1);
            ++j;
        
        if(i == 2*len(roots)+1):
            cell.addSamplePoint(roots[-1]+eps);
            cell.addDimension(1);
            continue;
                
        #si estamos en una celda par añade el punto medio, s no añade la raíz como punto muestra.
        if(i%2==0):
            cell.addDimension(1);
            cell.addSamplePoint(roots[j]+roots[j+1])/2)
            ++j;
        else:
            cell.addDimension(0);
            cell.addSamplePoint(roots[j]);
        
       #ahora dentro de este mismo for tengo que añadir las celdas al stack
        #base, que he olvidado declarar e indexarlas como toca.
        
            
    
         
# Execute doctest when run from the command line
if __name__ == "__main__":
    import doctest
    doctest.testmod()
