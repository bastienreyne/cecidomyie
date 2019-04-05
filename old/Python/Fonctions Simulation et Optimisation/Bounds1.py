"""
    Ce fichier contient les definitions des bornes pour l'algorithme de 
    scipy.optimize.basinhoping pour le modele1 (cecidomyies)
"""

###############################################################################

import numpy as np

###############################################################################

class MyBounds1(object):
     def __init__(self, xmax=[1,1,0.5,1,1], xmin=[0,0.5,0,0,0]):
         self.xmax = np.array(xmax)
         self.xmin = np.array(xmin)
     def __call__(self, **kwargs):
         x = kwargs["x_new"]
         tmax = bool(np.all(x <= self.xmax))
         tmin = bool(np.all(x >= self.xmin))
         return tmax and tmin
     
class MyBounds2(object):
     def __init__(self, xmax=[1,1,1,1,1], xmin=[0,0,0,0,0]):
         self.xmax = np.array(xmax)
         self.xmin = np.array(xmin)
     def __call__(self, **kwargs):
         x = kwargs["x_new"]
         tmax = bool(np.all(x <= self.xmax))
         tmin = bool(np.all(x >= self.xmin))
         return tmax and tmin
     
###############################################################################
     
mybounds1_1 = MyBounds1()
mybounds1_2 = MyBounds2()