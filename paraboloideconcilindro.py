from mpl_toolkits.mplot3d import Axes3D
from mpl_toolkits.mplot3d import axes3d
from matplotlib import cm
import matplotlib.pyplot as plt
import numpy as np

fig = plt.figure()
ax = fig.add_subplot(111, projection='3d')

u = np.linspace(0, 2 * np.pi, 100)
v = np.linspace(0, np.pi, 100)

# plane
x = np.arange(-5, 5, 0.25)
y = np.arange(-5, 5, 0.25)
x, y = np.meshgrid(x, y)
z = 0
ax.plot_surface(x, y, z, color='b',alpha=0.5)


#paraboloid

u = np.linspace(0, 2 * np.pi, 100)
v = np.linspace(0, 8, 100)

x = 2 * np.sqrt(v) * np.outer(np.cos(u), np.ones(np.size(u)))
y = 2 * np.sqrt(v) * np.outer(np.sin(u), np.ones(np.size(u)))
z = 2 * np.outer(np.ones(np.size(u)), v)
ax.plot_surface(x, y, z, rstride=4, cstride=4, color='m', alpha=0.3)



# cylinder
u = np.linspace(0, 2 * np.pi, 100)
v = np.linspace(0, 8, 100)

x =2 + 2 * np.outer(np.cos(u), np.ones(np.size(u)))
y = 2 * np.outer(np.sin(u), np.ones(np.size(u)))
z = 2 * np.outer(np.ones(np.size(u)), v)
ax.plot_surface(x, y, z, rstride=4, cstride=4, color='c')

plt.show()



plt.show()
