from mpl_toolkits.mplot3d import Axes3D
from mpl_toolkits.mplot3d import axes3d
from matplotlib import cm
import matplotlib.pyplot as plt
import numpy as np

fig = plt.figure()
ax = fig.add_subplot(111, projection='3d')

u = np.linspace(0, 2 * np.pi, 100)
v = np.linspace(0, np.pi, 100)

x = 2 * np.outer(np.cos(u), np.sin(v))
y = 2 * np.outer(np.sin(u), np.sin(v))
z = 2 * np.outer(np.ones(np.size(u)), np.cos(v))
ax.plot_surface(x, y, z, rstride=4, cstride=4, color='c')

ax = fig.gca(projection='3d')
theta = np.linspace(-4 * np.pi, 4 * np.pi, 100)
z = -2
r = 2
x = r * np.sin(theta)
y = r * np.cos(theta)
ax.plot(x, y, z, label='R2 Projection')
ax.legend()

# rallita vertical 1
x = np.linspace(-2.5, -2.5, 100)
y = np.linspace(-2, 2, 100)
z = -2
ax.plot(x, y, z, color='g')

# rallita vertical 2
x = np.linspace(-2, -2, 100)
y = np.linspace(-2, 2, 100)
z = -2
ax.plot(x, y, z, color='g')

# rallita vertical 3
x = np.linspace(0, 0, 100)
y = np.linspace(-2, 2, 100)
z = -2
ax.plot(x, y, z, color='g')

# rallita vertical 4
x = np.linspace(2, 2, 100)
y = np.linspace(-2, 2, 100)
z = -2
ax.plot(x, y, z, label='R Projection', color='g')

# rallita vertical 2
x = np.linspace(2.5, 2.5, 100)
y = np.linspace(-2, 2, 100)
z = -2
ax.plot(x, y, z, color='g')



# rallita horizontal
y = np.linspace(-2, -2, 100)
x = np.linspace(-2.5, 2.5, 100)
z = -2
ax.plot(x, y, z, color='g')


ys = [-2, -2, -2, -2, -2]
xs = [-2.5, -2, 0, 2, 2.5]
zs = [-2, -2, -2, -2, -2]
ax.scatter(xs, ys, zs, label='R Projection')

plt.show()



plt.show()
