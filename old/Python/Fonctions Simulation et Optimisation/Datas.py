"""
    Ce fichier permet de recuperer les donnees (nombre de larves, nombre 
    d'inflorescences vivantes, nombre d'inflorescences mortes, nombre de 
    nouvelles inflorescences) pour chaque parcelle a chaque date ; et de 
    declarer les parametres fixes en variables globales en appelant ce 
    fichier lors des simulations. 
"""

###############################################################################

import FunDatas as dt
import numpy as np

###############################################################################

""" PARAMETRES FIXES DU MODELE """

d_l   = 7
E     = 150
mu    = 0.04
p_pup = 0.77
d_p   = 5
beta  = 1
T     = 50
TotJours,xJours = dt.totJours()

xJoursA = np.copy(xJours)
xJoursB = np.copy(xJours)
xJoursC = np.copy(xJours)

for i in range(len(xJours)):
    xJoursA[i] = xJours[i] + 29
    xJoursB[i] = xJours[i] + 8
    xJoursC[i] = xJours[i] + 20
    
xJoursA = list(xJoursA)
xJoursB = list(xJoursB)
xJoursC = list(xJoursC)

###############################################################################

""" DONNEES DE CHAQUE PARCELLE DU BLOC1 """


L_A, new_A, I_A_pie, I_A_flo, dead_A_pie, dead_A_flo = dt.bloc ("Bloc1_enh_ras_jour")
L_B, new_B, I_B_pie, I_B_flo, dead_B_pie, dead_B_flo = dt.bloc ("Bloc1_bache_jour")
L_C, new_C, I_C_pie, I_C_flo, dead_C_pie, dead_C_flo = dt.bloc ("Bloc1_enh_haut_jour")

###############################################################################

