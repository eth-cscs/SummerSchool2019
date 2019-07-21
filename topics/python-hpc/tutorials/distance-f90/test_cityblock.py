import time
import numpy as np
from scipy.spatial.distance import cdist
from metrics.cbdm import cityblock_distance_matrix


def cityblock_numpy(x, y):
    """Cityblock distance matrix.

    Inputs:
    x: (N,) numpy array
    y: (N,) numpy array

    Ouput:
    (N, N) Cityblock distance matrix:
    r_ij = abs(x_ij - y_ij)
    """

    return cdist(x, y, 'cityblock')


nsamples, nfeat = (5000, 500)
x = 10. * np.random.random([nsamples, nfeat])
cbdm_f90 = np.empty([nsamples, nsamples], order='F')

start = time.time()
# euclidean_distance_matrix(x.T, x.T, dist_matrix)
cityblock_distance_matrix(x.T, x.T, nsamples, nfeat, cbdm_f90)
print("f90:   %.2f seconds" % (time.time() - start))

start = time.time()
cbdm_np = cityblock_numpy(x, x)
print("numpy: %.2f seconds" % (time.time() - start))

print(np.abs(cbdm_f90 - cbdm_np).max())
