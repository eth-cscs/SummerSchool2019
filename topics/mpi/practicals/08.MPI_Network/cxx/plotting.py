#
# Plotting program, to use to visualize the output of the mini-app
# CSCS - Summer School 2015
# Written by Jean M. Favre
#
# Tested on daint, with python/2.7.6
# Date: Thu May 21 14:11:38 CEST 2015
#

import pylab as pl
import numpy as np

# change filename below as appropriate
headerFilename =  "output.bov"
headerfile = open(headerFilename, "r")
header = headerfile.readlines()
headerfile.close()

rawFilename = header[1].split()[1]

res_x = int(header[2].split()[1][:-1]) # remove the ',' at end of string
res_y = int(header[2].split()[2][:-1]) # remove the ',' at end of string
print 'xdim %s' % res_x
print 'ydim %s' % res_y

data = np.fromfile(rawFilename, dtype=np.double, count=-1, sep='')
assert data.shape[0] == res_x * res_y, "raw data array does not match the resolution in the header"

x = np.linspace(0., 1., res_x)
y = np.linspace(0., 1.*res_y/res_x, res_y)
X, Y = np.meshgrid(x, y)
# number of contours we wish to see
N = 12
pl.contourf(X, Y, data.reshape(res_y, res_x), N, alpha=.75, cmap='jet')
C=pl.contour(X, Y, data.reshape(res_y, res_x), N, colors='black', linewidth=.1)

#pl.clabel(C, inline=1)
pl.axes().set_aspect('equal')
pl.savefig("output.png", dpi=72)
pl.show()
