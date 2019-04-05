# Importation des librairies 

from scipy.optimize import least_squares
import numpy as np
import matplotlib.pyplot as plt
from pandas import *
from itertools import *

# Fonctions 

## Pour recuperer les donnees

def fichier_piege (Nom) :
    f = read_excel('../../Fichiers de donnees/Donnees re organisees/Fichier piege/'+Nom+'.xls')
    df = f.groupby('date').sum()
    nb_larves = df["larves"]
    nb_inflo = df["inflos_vivantes"]
    nb_inflo_morte = df["inflos_mortes"]
    return nb_larves, nb_inflo, nb_inflo_morte


def fichier_donnees_floraison (Nom) :
    f = read_excel('../../Fichiers de donnees/Donnees re organisees/Fichier donnees floraison/Avec lissage/'+Nom+'.xls')
    df = f.groupby('date').sum()
    nouvelles = df["nouvelles"]
    vivantes = df["alive"]
    mortes = df["dead"]
    return nouvelles, vivantes, mortes

## Pour avoir les memes dates entre les donnees du fichier floraison et celles du fichier piege

def debut_fin (donnees_foraison, donnees_piege):
    date_1 = donnees_foraison.index
    date_2 = donnees_piege.index
    debut = date_1[0]
    if (debut.replace(year=2017)<=date_2[0]) :
        debut = date_2[0]
    fin = date_1[np.shape(date_1)[0]-1]
    if (fin.replace(year=2017)>=date_2[np.shape(date_2)[0]-1]) :
        fin = date_2[np.shape(date_2)[0]-1]
    for i in range (np.shape(date_1)[0]) :
        if (debut.replace(year=2017)==date_1[i].replace(year=2017)) :
            ii = i
        if (fin.replace(year=2017)==date_1[i].replace(year=2017)) :
            jj = i
    for i in range (np.shape(date_2)[0]) :
        if (debut.replace(year=2017)==date_2[i].replace(year=2017)) :
            kk = i
        if (fin.replace(year=2017)==date_2[i].replace(year=2017)) :
            ll = i
    date = date_2[kk:ll]
    return (date, ii,jj,kk,ll)


# Pour l'optimisation

def integrate (a, inflos_donnees_floraison) :
    inflos_interpo = np.zeros(len(inflos_donnees_floraison))
    for i in range (len(inflos_donnees_floraison)) :
        inflos_interpo[i] = a*inflos_donnees_floraison[i]
    return inflos_interpo

def distance (Simu,Obs) :
    return np.sqrt(sum((Simu-Obs)**2))

def objectif (params, inflos_donnees_floraison, inflos_donnees_piege) :
    val = distance(integrate(params, inflos_donnees_floraison), inflos_donnees_piege)
    return val

def visual_optimization (date, inflos_donnees_floraison, inflos_donnees_piege, p0) :  
    min_ = 99999999999999999999999
    N = len(p0)
    for cpt in range (N):
        i  = p0[cpt]
        b = least_squares(objectif,i,args=(inflos_donnees_floraison, inflos_donnees_piege)).cost
        if (b<min_):
            min_ = b
            min_init = i
    res = least_squares(objectif,min_init,args=(inflos_donnees_floraison, inflos_donnees_piege))
    x_ = res.x
    y = integrate(x_, inflos_donnees_floraison)
    plt.plot(date, y, label="Inflos_floraison * a")
    plt.plot(date, inflos_donnees_piege, label="Inflos_piege")
    plt.legend()
    plt.xticks(rotation=60)
    plt.show()
    return x_

# BLOC 1

piege_larves_bloc1_j, piege_inflo_bloc1_j, piege_inflo_morte_bloc1_j = fichier_piege("Bloc1_jour")
donnees_floraison_nouvelles_bloc1_j, donnees_floraison_vivantes_bloc1_j, donnees_floraison_mortes_bloc1_j = fichier_donnees_floraison("Bloc1_jour")
(date_1_j,ii,jj,kk,ll) = debut_fin (donnees_floraison_nouvelles_bloc1_j, piege_larves_bloc1_j)
donnees_floraison_nouvelles_bloc1_j = list(donnees_floraison_nouvelles_bloc1_j[ii:jj])
donnees_floraison_vivantes_bloc1_j = list(donnees_floraison_vivantes_bloc1_j[ii:jj])
donnees_floraison_mortes_bloc1_j = list(donnees_floraison_mortes_bloc1_j[ii:jj])
piege_larves_bloc1_j = list(piege_larves_bloc1_j[kk:ll])
piege_inflo_bloc1_j = list(piege_inflo_bloc1_j[kk:ll])
piege_inflo_morte_bloc1_j = list(piege_inflo_morte_bloc1_j[kk:ll])

piege_larves_bloc1_bache_j, piege_inflo_bloc1_bache_j, piege_inflo_morte_bloc1_bache_j = fichier_piege("Bloc1_bache_jour")
donnees_floraison_nouvelles_bloc1_bache_j, donnees_floraison_vivantes_bloc1_bache_j, donnees_floraison_mortes_bloc1_bache_j = fichier_donnees_floraison("Bloc1_bache_jour")
(date_1_bache_j,ii,jj,kk,ll) = debut_fin (donnees_floraison_nouvelles_bloc1_bache_j, piege_larves_bloc1_bache_j)
donnees_floraison_nouvelles_bloc1_bache_j = list(donnees_floraison_nouvelles_bloc1_bache_j[ii:jj])
donnees_floraison_vivantes_bloc1_bache_j = list(donnees_floraison_vivantes_bloc1_bache_j[ii:jj])
donnees_floraison_mortes_bloc1_bache_j = list(donnees_floraison_mortes_bloc1_bache_j[ii:jj])
piege_larves_bloc1_bache_j = list(piege_larves_bloc1_bache_j[kk:ll])
piege_inflo_bloc1_bache_j = list(piege_inflo_bloc1_bache_j[kk:ll])
piege_inflo_morte_bloc1_bache_j = list(piege_inflo_morte_bloc1_bache_j[kk:ll])

piege_larves_bloc1_enh_ras_j, piege_inflo_bloc1_enh_ras_j, piege_inflo_morte_bloc1_enh_ras_j = fichier_piege("Bloc1_enh_ras_jour")
donnees_floraison_nouvelles_bloc1_enh_ras_j, donnees_floraison_vivantes_bloc1_enh_ras_j, donnees_floraison_mortes_bloc1_enh_ras_j = fichier_donnees_floraison("Bloc1_enh_ras_jour")
(date_1_enh_ras_j,ii,jj,kk,ll) = debut_fin (donnees_floraison_nouvelles_bloc1_enh_ras_j, piege_larves_bloc1_enh_ras_j)
donnees_floraison_nouvelles_bloc1_enh_ras_j = list(donnees_floraison_nouvelles_bloc1_enh_ras_j[ii:jj])
donnees_floraison_vivantes_bloc1_enh_ras_j = list(donnees_floraison_vivantes_bloc1_enh_ras_j[ii:jj])
donnees_floraison_mortes_bloc1_enh_ras_j = list(donnees_floraison_mortes_bloc1_enh_ras_j[ii:jj])
piege_larves_bloc1_enh_ras_j = list(piege_larves_bloc1_enh_ras_j[kk:ll])
piege_inflo_bloc1_enh_ras_j = list(piege_inflo_bloc1_enh_ras_j[kk:ll])
piege_inflo_morte_bloc1_enh_ras_j = list(piege_inflo_morte_bloc1_enh_ras_j[kk:ll])

piege_larves_bloc1_enh_haut_j, piege_inflo_bloc1_enh_haut_j, piege_inflo_morte_bloc1_enh_haut_j = fichier_piege("Bloc1_enh_haut_jour")
donnees_floraison_nouvelles_bloc1_enh_haut_j, donnees_floraison_vivantes_bloc1_enh_haut_j, donnees_floraison_mortes_bloc1_enh_haut_j = fichier_donnees_floraison("Bloc1_enh_haut_jour")
(date_1_enh_haut_j,ii,jj,kk,ll) = debut_fin (donnees_floraison_nouvelles_bloc1_enh_haut_j, piege_larves_bloc1_enh_haut_j)
donnees_floraison_nouvelles_bloc1_enh_haut_j = list(donnees_floraison_nouvelles_bloc1_enh_haut_j[ii:jj])
donnees_floraison_vivantes_bloc1_enh_haut_j = list(donnees_floraison_vivantes_bloc1_enh_haut_j[ii:jj])
donnees_floraison_mortes_bloc1_enh_haut_j = list(donnees_floraison_mortes_bloc1_enh_haut_j[ii:jj])
piege_larves_bloc1_enh_haut_j = list(piege_larves_bloc1_enh_haut_j[kk:ll])
piege_inflo_bloc1_enh_haut_j = list(piege_inflo_bloc1_enh_haut_j[kk:ll])
piege_inflo_morte_bloc1_enh_haut_j = list(piege_inflo_morte_bloc1_enh_haut_j[kk:ll])


# DONNEES DEUX FOIS PAR SEMAINE _ Fichiers donnees floraison et piege


# BLOC1

piege_larves_bloc1_s, piege_inflo_bloc1_s, piege_inflo_morte_bloc1_s = fichier_piege("Bloc1_semaine")
donnees_floraison_nouvelles_bloc1_s, donnees_floraison_vivantes_bloc1_s, donnees_floraison_mortes_bloc1_s = fichier_donnees_floraison("Bloc1_semaine")
(date_1_s,ii,jj,kk,ll) = debut_fin (donnees_floraison_nouvelles_bloc1_s, piege_larves_bloc1_s)
donnees_floraison_nouvelles_bloc1_s = list(donnees_floraison_nouvelles_bloc1_s[ii:jj])
donnees_floraison_vivantes_bloc1_s = list(donnees_floraison_vivantes_bloc1_s[ii:jj])
donnees_floraison_mortes_bloc1_s = list(donnees_floraison_mortes_bloc1_s[ii:jj])
piege_larves_bloc1_s = list(piege_larves_bloc1_s[kk:ll])
piege_inflo_bloc1_s = list(piege_inflo_bloc1_s[kk:ll])
piege_inflo_morte_bloc1_s = list(piege_inflo_morte_bloc1_s[kk:ll])

piege_larves_bloc1_bache_s, piege_inflo_bloc1_bache_s, piege_inflo_morte_bloc1_bache_s = fichier_piege("Bloc1_bache_semaine")
donnees_floraison_nouvelles_bloc1_bache_s, donnees_floraison_vivantes_bloc1_bache_s, donnees_floraison_mortes_bloc1_bache_s = fichier_donnees_floraison("Bloc1_bache_semaine")
(date_1_bache_s,ii,jj,kk,ll) = debut_fin (donnees_floraison_nouvelles_bloc1_bache_s, piege_larves_bloc1_bache_s)
donnees_floraison_nouvelles_bloc1_bache_s = list(donnees_floraison_nouvelles_bloc1_bache_s[ii:jj])
donnees_floraison_vivantes_bloc1_bache_s = list(donnees_floraison_vivantes_bloc1_bache_s[ii:jj])
donnees_floraison_mortes_bloc1_bache_s = list(donnees_floraison_mortes_bloc1_bache_s[ii:jj])
piege_larves_bloc1_bache_s = list(piege_larves_bloc1_bache_s[kk:ll])
piege_inflo_bloc1_bache_s = list(piege_inflo_bloc1_bache_s[kk:ll])
piege_inflo_morte_bloc1_bache_s = list(piege_inflo_morte_bloc1_bache_s[kk:ll])

piege_larves_bloc1_enh_ras_s, piege_inflo_bloc1_enh_ras_s, piege_inflo_morte_bloc1_enh_ras_s = fichier_piege("Bloc1_enh_ras_semaine")
donnees_floraison_nouvelles_bloc1_enh_ras_s, donnees_floraison_vivantes_bloc1_enh_ras_s, donnees_floraison_mortes_bloc1_enh_ras_s = fichier_donnees_floraison("Bloc1_enh_ras_semaine")
(date_1_enh_ras_s,ii,jj,kk,ll) = debut_fin (donnees_floraison_nouvelles_bloc1_enh_ras_s, piege_larves_bloc1_enh_ras_s)
donnees_floraison_nouvelles_bloc1_enh_ras_s = list(donnees_floraison_nouvelles_bloc1_enh_ras_s[ii:jj])
donnees_floraison_vivantes_bloc1_enh_ras_s = list(donnees_floraison_vivantes_bloc1_enh_ras_s[ii:jj])
donnees_floraison_mortes_bloc1_enh_ras_s = list(donnees_floraison_mortes_bloc1_enh_ras_s[ii:jj])
piege_larves_bloc1_enh_ras_s = list(piege_larves_bloc1_enh_ras_s[kk:ll])
piege_inflo_bloc1_enh_ras_s = list(piege_inflo_bloc1_enh_ras_s[kk:ll])
piege_inflo_morte_bloc1_enh_ras_s = list(piege_inflo_morte_bloc1_enh_ras_s[kk:ll])

piege_larves_bloc1_enh_haut_s, piege_inflo_bloc1_enh_haut_s, piege_inflo_morte_bloc1_enh_haut_s = fichier_piege("Bloc1_enh_haut_semaine")
donnees_floraison_nouvelles_bloc1_enh_haut_s, donnees_floraison_vivantes_bloc1_enh_haut_s, donnees_floraison_mortes_bloc1_enh_haut_s = fichier_donnees_floraison("Bloc1_enh_haut_semaine")
(date_1_enh_haut_s,ii,jj,kk,ll) = debut_fin (donnees_floraison_nouvelles_bloc1_enh_haut_s, piege_larves_bloc1_enh_haut_s)
donnees_floraison_nouvelles_bloc1_enh_haut_s = list(donnees_floraison_nouvelles_bloc1_enh_haut_s[ii:jj])
donnees_floraison_vivantes_bloc1_enh_haut_s = list(donnees_floraison_vivantes_bloc1_enh_haut_s[ii:jj])
donnees_floraison_mortes_bloc1_enh_haut_s = list(donnees_floraison_mortes_bloc1_enh_haut_s[ii:jj])
piege_larves_bloc1_enh_haut_s = list(piege_larves_bloc1_enh_haut_s[kk:ll])
piege_inflo_bloc1_enh_haut_s = list(piege_inflo_bloc1_enh_haut_s[kk:ll])
piege_inflo_morte_bloc1_enh_haut_s = list(piege_inflo_morte_bloc1_enh_haut_s[kk:ll])
