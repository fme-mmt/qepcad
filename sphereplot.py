from mpl_toolkits.mplot3d import Axes3D
from mpl_toolkits.mplot3d import axes3d
from matplotlib import cm
import matplotlib.pyplot as plt
import numpy as np

fig = plt.figure()
ax = fig.add_subplot(111, projection='3d')

u = np.linspace(0, 2 * np.pi, 100)
v = np.linspace(0, np.pi, 100)

x = 4 * np.outer(np.cos(u), np.sin(v))
y = 4 * np.outer(np.sin(u), np.sin(v))
z = 4 * np.outer(np.ones(np.size(u)), np.cos(v))
ax.plot_surface(x, y, z, rstride=4, cstride=4, color='c')

#plt.show()

#mpl.rcParams['legend.fontsize'] = 10

#fig = plt.figure()
ax = fig.gca(projection='3d')
theta = np.linspace(-4 * np.pi, 4 * np.pi, 100)
z = np.linspace(-4, -4, 100)
r = 4
x = r * np.sin(theta)
y = r * np.cos(theta)
ax.plot(x, y, z, label='R2 Projection')
ax.legend()

plt.show()
