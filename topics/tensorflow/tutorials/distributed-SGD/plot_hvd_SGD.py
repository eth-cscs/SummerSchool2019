import numpy as np
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt


def loss_function_field(m, n, xref, yref):
    return np.mean(np.square(yref - m * xref - n))


# Load data for visualization
slope_hist0 = np.load('slope_hist_0.npy')
offset_hist0 = np.load('offset_hist_0.npy')
slope_hist1 = np.load('slope_hist_1.npy')
offset_hist1 = np.load('offset_hist_1.npy')
x_train = np.load('x_train.npy')
y_train = np.load('y_train.npy')

# The reference
ref_slope = 2.0
ref_offset = 0.0

# Create [slope x offset] grid for contour plot
_m = np.arange(-0, 4.01, 0.1)
_n = np.arange(-0.5, 0.51, 0.1)
M, N = np.meshgrid(_m, _n)

Z = np.zeros(M.shape)
for i in range(M.shape[0]):
    for j in range(M.shape[1]):
        Z[i, j] = loss_function_field(M[i, j], N[i, j],
                                      x_train, y_train)

# matplotlib.rcParams['figure.figsize'] = (10.0, 10.0)

cp = plt.contour(M, N, Z, 50, vmin=Z.min(), vmax=Z.max(), alpha=0.4)
plt.clabel(cp, cp.levels[:6])
plt.colorbar()
m = slope_hist0[-1]
n = offset_hist0[-1]
plt.plot(slope_hist0, offset_hist0, '.-', lw=1, label='rank0')
plt.plot(slope_hist1, offset_hist1, 'x-', lw=1, label='rank1')
plt.plot([ref_slope], [ref_offset], 'rx', ms=10)
plt.xlim([_m.min(), _m.max()])
plt.ylim([_n.min(), _n.max()])
plt.xlabel('Slope')
plt.ylabel('Offset')
plt.legend()
# plt.show()
plt.savefig('training.png')

# matplotlib.rcParams['figure.figsize'] = (6.0, 4.0)
