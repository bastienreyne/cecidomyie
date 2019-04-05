"""
    Ce fichier contient des fonctions de normalisation.
"""
###############################################################################

import numpy as np

###############################################################################

def norm1 (X) :
    
    """
        Normalise le vecteur X.
        
        Args
        ----
        X : ndarray
            Vecteur non normalise
        
        Returns
        -------
        Y : ndarray
            Vecteur normalise
    """
    
    Y = (X-min(X))/float(max(X)-min(X))
    
    return Y

###############################################################################

def norm2 (Obs,Simu) :
    
    """
        Normalise le vecteur Obs et ajuste Simu en fonction.
        
        Args
        ----
        Obs : ndarray
            Vecteur non normalise
        Simu : ndarray
            Vecteur non normalise
        
        Returns
        -------
        Obs_n : ndarray
            Vecteur normalise
        Simu_n : ndarray 
            Vecteur normalise
    """
    
    min_ = min(Obs)
    max_ = max(Obs)
    Obs_n  = (Obs-min_)/float(max_-min_)
    Simu_n = (Simu-min_)/float(max_-min_)
    
    return Obs_n,Simu_n

###############################################################################