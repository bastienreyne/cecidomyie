"""
    Ce fichier contient les definitions des bornes pour l'algorithme de 
    scipy.optimize.basinhoping pour le modele2 (inflorescences)
"""

###############################################################################

import numpy as np

###############################################################################

class MyBounds1(object):
     def __init__(self, xmax=[np.inf], xmin=[0]):
         self.xmax = np.array(xmax)
         self.xmin = np.array(xmin)
     def __call__(self, **kwargs):
         x = kwargs["x_new"]
         tmax = bool(np.all(x <= self.xmax))
         tmin = bool(np.all(x >= self.xmin))
         return tmax and tmin
     
###############################################################################
     
mybounds2_1 = MyBounds1()