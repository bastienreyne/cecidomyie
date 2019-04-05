"""
    Ce fichier contient les fonctions pour importer les donnees ; et celles 
    pour calculer les dates. 
"""

###############################################################################

import numpy as np
from datetime import date
import pandas as pd 

###############################################################################

def daynb (day, refday) :
    
    """
        Renvoie la difference de jours entre day et refday
        
        Args
        ----
        day : int
            Date 2 
        refday : Date
            Date 1 
            
        Returns
        -------
        diff : int 
            Numero de la date day en fonction de la date de reference refday.
            Autrement dit, la difference, en jours de ces deux dates. 
    """
    
    diff = (date(2017,day[1], day[0]) - date(2017,refday[1], refday[0])).days
    
    return diff

###############################################################################

def totJours() : 
    
    """
        Renvoie le nombre de jours entre le premier et le dernier releve sur 
        les donnees du fichier piege.
        
        Returns
        -------
        TotJours : int
            Nombre de jours entre le premier et le dernier releve
    """
    
    listeJours = np.array(((18,7),(21,7),(25,7),(28,7),
                       (1,8),(4,8),(8,8),(11,8),(15,8),(18,8),(22,8),(25,8),(29,8),
                       (1,9),(5,9),(8,9),(12,9),(15,9),(19,9),(22,9),(26,9),(29,9),
                       (3,10),(6,10)))
    xJours = list (map(lambda d : daynb(d,(18,7)), listeJours))
    TotJours = xJours[-1]
    
    return TotJours,xJours

###############################################################################

def bloc (Nom) :
    
    """
        Renvoie trois colonnes d'un fichiers excel.
        
        Args
        ----
        Nom : string
            Le nom du fichier piege dont on recupere les donnees
            "NumeroBloc_Taitement_Frequence"
            NumeroBloc = Bloc1 ou Bloc2
            Traitement = bache, enh_haut, enh_ras
            Frequence  = jour ou semaine
        groupe : string
            Colonne index
        Sheet : string
            Feuille du fichier excel
        un : string
            Nom de la premiere colonne dont on recupere les donnees
        deux : string
            Nom de la deuxieme colonne dont on recupere les donnees
        trois : string
            Nom de la troisieme colonne dont on recupere les donnees
        quatre : string
            Nom de la quatrieme colonne dont on recupere les donnees
            
        Returns
        -------
        nb_larves : ndarray
            Nombre de larves
        nb_nouvelles : ndarray
            Nombre de nouvelles inflorescences
        nb_inflo : ndarray
            Nombre d'inflorescences vivantes
        nb_inflo_morte : ndarray
            Nombre d'inflorescences mortes
    """
    
    f = pd.read_excel("../../Fichiers de donnees/Donnees re organisees/Simulation/"+Nom+".xls")
    liste = f.groupby("date").sum()
    nb_larves = np.array(liste["larves_piege"])
    nb_nouvelles = np.array(liste["nouvelles_floraison"])
    nb_inflo_piege = np.array(liste["inflos_vivantes_piege"])
    nb_inflo_floraison = np.array(liste["inflos_vivantes_floraison"])
    nb_inflo_morte_piege = np.array(liste["inflos_mortes_piege"])
    nb_inflo_morte_floraison = np.array(liste["inflos_mortes_floraison"])
    
    return nb_larves, nb_nouvelles, nb_inflo_piege, nb_inflo_floraison, nb_inflo_morte_piege, nb_inflo_morte_floraison

###############################################################################