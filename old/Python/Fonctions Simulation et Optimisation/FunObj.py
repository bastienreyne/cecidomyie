"""
    Ce fichier contient les differentes fonctions objectif trouvees dans la 
    litterature (rapport de Imem Soula, 2015) que nous allons tester pour notre
    probleme.
"""

###############################################################################

import FunNorm as nrm
import numpy as np

###############################################################################
    # MAE : Mean Absolute Error
###############################################################################

def MAE (Simu,Obs) :
    
    """
        Renvoie l'erreur moyenne absolue entre les vecteurs Simu et Obs
        
        Args
        ----
        Simu : ndarray
            Vecteur des individus simules
        Obs : ndarray
            Vecteur des individus observes
        
        Returns
        -------
        d : float
            MAE entre Simu et Obs
    """
    
    N = np.size(Simu)
    d = np.sqrt(sum(abs(Simu-Obs)))/N

    return d

###############################################################################

def MAE_1 (Simu,Obs) :
    
    """
        Renvoie l'erreur moyenne abolue entre les vecteurs Simu et Obs 
        normalises avec nrm.norm1
        
        Args
        ----
        Simu : ndarray
            Vecteur des individus simules
        Obs : ndarray
            Vecteur des individus observes
        
        Returns
        -------
        d : float
            MAE entre Simu et Obs normalises avec nrm.norm1
    """
    
    N = np.size(Simu)
    Simu = nrm.norm1(Simu)
    Obs  = nrm.norm1(Obs)
    d = np.sqrt(sum(abs(Simu-Obs)))/N

    return d

###############################################################################

def MAE_2 (Simu,Obs) :
    
    """
        Renvoie l'erreur moyenne abolue entre les vecteurs Simu et Obs 
        normalises avec nrm.norm2
        
        Args
        ----
        Simu : ndarray
            Vecteur des individus simules
        Obs : ndarray
            Vecteur des individus observes
        
        Returns
        -------
        d : float
            MAE entre Simu et Obs normalises avec nrm.norm2
    """

    N = np.size(Simu)
    Simu,Obs = nrm.norm2(Simu,Obs)
    d = np.sqrt(sum(abs(Simu-Obs)))/N

    return d

###############################################################################
    # MSE : Mean Square Error
###############################################################################

def MSE (Simu,Obs) :
    
    """
        Renvoie l'erreur moyenne quadratique entre les vecteurs Simu et Obs 
        
        Args
        ----
        Simu : ndarray
            Vecteur des individus simules
        Obs : ndarray
            Vecteur des individus observes
        
        Returns
        -------
        d : float
            MSE entre Simu et Obs
    """
    
    N = np.size(Simu)
    d = sum((Simu-Obs)**2)/N

    return d

###############################################################################

def MSE_1 (Simu,Obs) :
    
    """
        Renvoie l'erreur moyenne quadratique entre les vecteurs Simu et Obs 
        normalises avec nrm.norm1
        
        Args
        ----
        Simu : ndarray
            Vecteur des individus simules
        Obs : ndarray
            Vecteur des individus observes
        
        Returns
        -------
        d : float
            MSE entre Simu et Obs normalises avec nrm.norm1
    """
    
    N = np.size(Simu)
    Simu = nrm.norm1(Simu)
    Obs  = nrm.norm1(Obs)
    d = sum((Simu-Obs)**2)/N

    return d

###############################################################################

def MSE_2 (Simu,Obs) :
    
    """
        Renvoie l'erreur moyenne quadratique entre les vecteurs Simu et Obs 
        normalises avec nrm.norm2
        
        Args
        ----
        Simu : ndarray
            Vecteur des individus simules
        Obs : ndarray
            Vecteur des individus observes
        
        Returns
        -------
        d : float
            MSE entre Simu et Obs normalises avec nrm.norm2
    """

    N = np.size(Simu)
    Simu,Obs = nrm.norm2(Simu,Obs)
    d = sum((Simu-Obs)**2)/N

    return d

###############################################################################
    # RMSE : Root Mean Square Error
###############################################################################

def RMSE (Simu,Obs) :
    
    """
        Renvoie la racine de l'erreur moyenne quadratique entre les vecteurs 
        Simu et Obs 
        
        Args
        ----
        Simu : ndarray
            Vecteur des individus simules
        Obs : ndarray
            Vecteur des individus observes
        
        Returns
        -------
        d : float
            RMSE entre Simu et Obs
    """
    
    N = np.size(Simu)
    d = np.sqrt(sum((Simu-Obs)**2))/N

    return d

###############################################################################

def RMSE_1 (Simu,Obs) :
    
    """
        Renvoie la racine de l'erreur moyenne quadratique entre les vecteurs 
        Simu et Obs normalises avec nrm.norm1
        
        Args
        ----
        Simu : ndarray
            Vecteur des individus simules
        Obs : ndarray
            Vecteur des individus observes
        
        Returns
        -------
        d : float
            RMSE entre Simu et Obs normalises avec nrm.norm1
    """
    
    N = np.size(Simu)
    Simu = nrm.norm1(Simu)
    Obs  = nrm.norm1(Obs)
    d = np.sqrt(sum((Simu-Obs)**2))/N

    return d

###############################################################################

def RMSE_2 (Simu,Obs) :
    
    """
        Renvoie la racine de l'erreur moyenne quadratique entre les vecteurs 
        Simu et Obs normalises avec nrm.norm2
        
        Args
        ----
        Simu : ndarray
            Vecteur des individus simules
        Obs : ndarray
            Vecteur des individus observes
        
        Returns
        -------
        d : float
            RMSE entre Simu et Obs normalises avec nrm.norm2
    """

    N = np.size(Simu)
    Simu,Obs = nrm.norm2(Simu,Obs)
    d = np.sqrt(sum((Simu-Obs)**2))/N

    return d

###############################################################################
    # RRMSE : Relative Root Squared Error
###############################################################################

def RRMSE (Simu,Obs) :
    
    """
        Renvoie la racine de l'erreur moyenne quadratique relative entre les 
        vecteurs Simu et Obs 
        
        Args
        ----
        Simu : ndarray
            Vecteur des individus simules
        Obs : ndarray
            Vecteur des individus observes
        
        Returns
        -------
        d : float
            RRMSE entre Simu et Obs
    """
    
    N = np.size(Simu)
    d = np.sqrt(sum((Simu-Obs)**2))/N
    d = d/np.mean(Simu)

    return d

###############################################################################
    # PCC : Pearson Correlation Coefficient
###############################################################################

def PCC (Simu,Obs) :
    
    """
        Renvoie le coefficient de correlation de Pearson entre les vecteurs 
        Simu et Obs 
        
        Args
        ----
        Simu : ndarray
            Vecteur des individus simules
        Obs : ndarray
            Vecteur des individus observes
        
        Returns
        -------
        d : float
            PCC entre Simu et Obs
    """
    
    mean1 = np.mean(Simu)
    mean2 = np.mean(Obs)
    d1 = sum((Obs-mean2)*(Simu-mean1))
    d2 = np.sqrt(sum((Simu-mean2)**2)*sum((Obs-mean1)**2))
    d = -abs(d1)/float(d2)

    return d

###############################################################################
    # D : Agreement Index 
###############################################################################

def D (Simu,Obs) :
    
    """
        Renvoie l'indice d'agregation entre les vecteurs Simu et Obs 
        
        Args
        ----
        Simu : ndarray
            Vecteur des individus simules
        Obs : ndarray
            Vecteur des individus observes
        
        Returns
        -------
        d : float
            D entre Simu et Obs
    """
    
    mean_ = np.mean(Obs)
    d1 = sum((Obs-Simu)**2)
    d2 = sum(abs(Simu-mean_)+abs(Obs-mean_))
    d = d1/float(d2)

    return -(1-d)

###############################################################################
    # NSE : Nash Sutcliffe Efficiency Coefficient
###############################################################################

def NSE (Simu,Obs) :
    
    """
        Renvoie le critere de Nash Sutcliffe entre les vecteurs Simu et Obs 
        
        Args
        ----
        Simu : ndarray
            Vecteur des individus simules
        Obs : ndarray
            Vecteur des individus observes
        
        Returns
        -------
        d : float
            NSE entre Simu et Obs
    """
    
    mean_ = np.mean(Obs)
    d1 = sum((Obs-Simu)**2)
    d2 = sum((Obs-mean_)**2)
    d = d1/float(d2)

    return -d