"""
    Ce fichier contient les fonctions pour faire les optimisations et
    estimations de parametres pour le modele 1.
"""

###############################################################################

import numpy as np
from scipy.optimize import least_squares, basinhopping
import matplotlib.pyplot as plt
from itertools import *

import FunModele1 as mdl1
import Datas as d

###############################################################################

def objectif(params, args_, distance, L_A, L_B, L_C, obs, cumul) : 

    """
        Calcule et renvoie la valeur de la fonction objectif en fonction des 
        parametres mis en entree. 
    
        Args
        ----
        params : list
            Contient les parametres fixes du modele de la fonction 
            mdl1.integrateCecidoEchange
        args : list
            Contient les parametres estimes du modele de la fonction 
            mdl1.integrateCecidoEchange           
        distance : fonction 
            Fonction objectif choisie
        L_ABC : ndarray
            Nombre de larves au cours du temps dans chaque sous parcelle
        obs : bool
            Vaut 0 si on fait l'optimisation sur toutes les valeurs
            journalieres, 1 si sur les valeurs observees
        cumul : bool
            Vaut 1 si on fait l'optimisation sur les valeurs cumulees au cours
            du temps. 0 sinon. 
            
        Returns 
        -------
        val : float
            Valeur de la fonction objectif
    """
    
    allparams = list(params)+list(args_)
    Lt_A, Lt_p_A, Nt_A, Lt_B, Lt_p_B, Nt_B, Lt_C, Lt_p_C, Nt_C = mdl1.integrateCecidoEchange(*allparams)
    if (cumul==1):
        if (obs==1):
            val = distance(np.cumsum(Lt_A[d.xJoursA]),np.cumsum(L_A[d.xJoursA]))/3 + distance(np.cumsum(Lt_B[d.xJoursB]),np.cumsum(L_B[d.xJoursB]))/3 + distance(np.cumsum(Lt_C[d.xJoursC]),np.cumsum(L_C[d.xJoursC]))/3
        else : 
            val = distance(np.cumsum(Lt_A),np.cumsum(L_A))/3 + distance(np.cumsum(Lt_B),np.cumsum(L_B))/3 + distance(np.cumsum(Lt_C),np.cumsum(L_C))/3
    else : 
        if (obs==1):
            val = (distance(Lt_A[d.xJoursA],L_A[d.xJoursA])+distance(Lt_B[d.xJoursB],L_B[d.xJoursB])+distance(Lt_C[d.xJoursC],L_C[d.xJoursC]))/3
        else : 
            val = (distance(Lt_A,L_A)+distance(Lt_B,L_B)+distance(Lt_C,L_C))/3

    return val

###############################################################################

def optimize(params, args_, distance, L_A, L_B, L_C, bounds, obs, cumul):
    
    """
        Optimise sur la fonction objectif avec l'algorithme simple de descente
        de gradient et renvoie le resultat de cette opt. 
    
        Args
        ----
        params : list
            Contient les parametres fixes du modele de la fonction 
            mdl1.integrateCecidoEchange
        args_ : list
            Contient une initialisation des parametres a estimer du modele de 
            la fonction mdl1.integrateCecidoEchange           
        distance : fonction 
            Fonction objectif choisie
        L_ABC : ndarray
            Nombre de larves au cours du temps dans chaque sous parcelle
        bounds : tuple
            Bornes des parametres a estimer
        obs : bool
            Vaut 0 si on fait l'optimisation sur toutes les valeurs
            journalieres, 1 si sur les valeurs observees
        cumul : bool
            Vaut 1 si on fait l'optimisation sur les valeurs cumulees au cours
            du temps. 0 sinon. 
            
        Returns 
        -------
        res : tuple
            Resultat de l'optimisation
    """    

    res = least_squares(objectif,params,bounds=bounds,args=(args_,distance, L_A, L_B, L_C, obs, cumul))
    
    return res

###############################################################################
    
def visual_optimization(params,args_,distance,L_A,L_B,L_C,bounds,obs,cumul):
    
    """
        Optimise sur la fonction objectif avec plusieurs points de depart pour
        l'algorithme de descente de gradient. Renvoie le resultat de la 
        meilleure optimisation et les valeurs du systeme Lt, Lt_p et Nt pour
        chaque sous parcelle. Affiche les larves estimees et observees pour
        chaque sous parcelle.
    
        Args
        ----
        params : list
            Contient les parametres fixes du modele de la fonction 
            mdl1.integrateCecidoEchange
        args_ : list
            Contient plusieurs initialisations des parametres a estimer du 
            modele de la fonction mdl1.integrateCecidoEchange           
        distance : fonction 
            Fonction objectif choisie
        L_ABC : ndarray
            Nombre de larves au cours du temps dans chaque sous parcelle
        bounds : tuple
            Bornes des parametres a estimer
        obs : bool
            Vaut 0 si on fait l'optimisation sur toutes les valeurs
            journalieres, 1 si sur les valeurs observees
        cumul : bool
            Vaut 1 si on fait l'optimisation sur les valeurs cumulees au cours
            du temps. 0 sinon. 
            
        Returns 
        -------
        res : tuple
            Resultat de l'optimisation
        Lt_ABC : ndarray
            Nombre de larves estimees dans chaque sous parcelle a chaque temps
        Lt_p_ABC : ndarray
            Nombre de larves piegees estimees dans chaque sous parcelle a 
            chaque temps
        Nt_ABC : ndarray
            Nombre d'adultes estimes dans chaque sous parcelle a chaque temps
    """    
    
    global distvalues
    distvalues = []
    
    params_ = list(product(*params))
    N = len(params_)
    for cpt in range (N):
        p0 = params_[cpt]
        res = optimize(p0,args_,distance,L_A,L_B,L_C,bounds,obs,cumul)
        distvalues.append(res.fun)
    k = np.argmin(distvalues)

    res = optimize(params_[k],args_,distance,L_A,L_B,L_C,bounds,obs,cumul)
    
    allparams = list(res.x)+list(args_)
    Lt_A, Lt_p_A, Nt_A, Lt_B, Lt_p_B, Nt_B, Lt_C, Lt_p_C, Nt_C = mdl1.integrateCecidoEchange(*allparams)
    
    plt.plot(Lt_A[d.xJoursA],label="Larves simulees")
    plt.plot(L_A[d.xJoursA],label="Larves observees")
    plt.legend()
    plt.title("Enherbement ras")
    plt.show()
    
    plt.plot(Lt_B[d.xJoursB],label="Larves simulees")
    plt.plot(L_B[d.xJoursB],label="Larves observees")
    plt.legend()
    plt.title("Bachage")
    plt.show()
    
    plt.plot(Lt_C[d.xJoursC],label="Larves simulees")
    plt.plot(L_C[d.xJoursC],label="Larves observees")
    plt.legend()
    plt.title("Enherbement haut")
    plt.show()
    
    return res, Lt_A, Lt_p_A, Nt_A, Lt_B, Lt_p_B, Nt_B, Lt_C, Lt_p_C, Nt_C

###############################################################################

def visual_opt(params,args_,distance,L_A,L_B,L_C,bounds,obs,cumul):
    
    """
        Optimise sur la fonction objectif un seul point de depart pour
        l'algorithme de descente de gradient. Renvoie le resultat de 
        l'optimisation et les valeurs du systeme Lt, Lt_p et Nt pour chaque 
        sous parcelle. Affiche les larves estimees et observees pour chaque
        sous parcelle.
    
        Args
        ----
        params : list
            Contient les parametres fixes du modele de la fonction 
            mdl1.integrateCecidoEchange
        args_ : list
            Contient une seule initialisation des parametres a estimer du 
            modele de la fonction mdl.integrateCecidoEchange           
        distance : fonction 
            Fonction objectif choisie
        L_ABC : ndarray
            Nombre de larves au cours du temps dans chaque sous parcelle
        bounds : tuple
            Bornes des parametres a estimer
        obs : bool
            Vaut 0 si on fait l'optimisation sur toutes les valeurs
            journalieres, 1 si sur les valeurs observees
        cumul : bool
            Vaut 1 si on fait l'optimisation sur les valeurs cumulees au cours
            du temps. 0 sinon. 
            
        Returns 
        -------
        res : tuple
            Resultat de l'optimisation
        Lt_ABC : ndarray
            Nombre de larves estimees dans chaque sous parcelle a chaque temps
        Lt_p_ABC : ndarray
            Nombre de larves piegees estimees dans chaque sous parcelle a 
            chaque temps
        Nt_ABC : ndarray
            Nombre d'adultes estimes dans chaque sous parcelle a chaque temps
    """    

    res = optimize(params,args_,distance,L_A,L_B,L_C,bounds,obs,cumul)
    
    allparams = list(res.x)+list(args_)
    Lt_A, Lt_p_A, Nt_A, Lt_B, Lt_p_B, Nt_B, Lt_C, Lt_p_C, Nt_C = mdl1.integrateCecidoEchange(*allparams)
    
    plt.plot(Lt_A[d.xJoursA],label="Larves simulees")
    plt.plot(L_A[d.xJoursA],label="Larves observees")
    plt.legend()
    plt.title("Enherbement ras")
    plt.show()
    
    plt.plot(Lt_B[d.xJoursB],label="Larves simulees")
    plt.plot(L_B[d.xJoursB],label="Larves observees")
    plt.legend()
    plt.title("Bachage")
    plt.show()
    
    plt.plot(Lt_C[d.xJoursC],label="Larves simulees")
    plt.plot(L_C[d.xJoursC],label="Larves observees")
    plt.legend()
    plt.title("Enherbement haut")
    plt.show()
    
    return res, Lt_A, Lt_p_A, Nt_A, Lt_B, Lt_p_B, Nt_B, Lt_C, Lt_p_C, Nt_C

###############################################################################

def optimize_bis(params, args_, distance, L_A, L_B, L_C, mybounds, obs, cumul):
    
    """
        Optimise sur la fonction objectif avec l'algorithme du recuit simule
        de gradient et renvoie le resultat de cette opt. 
    
        Args
        ----
        params : list
            Contient les parametres fixes du modele de la fonction 
            mdl1.integrateCecidoEchange
        args_ : list
            Contient une initialisation des parametres a estimer du modele de 
            la fonction mdl1.integrateCecidoEchange           
        distance : fonction 
            Fonction objectif choisie
        L_ABC : ndarray
            Nombre de larves au cours du temps dans chaque sous parcelle
        mybounds :
            Bornes des parametres a estimer
        obs : bool
            Vaut 0 si on fait l'optimisation sur toutes les valeurs
            journalieres, 1 si sur les valeurs observees
        cumul : bool
            Vaut 1 si on fait l'optimisation sur les valeurs cumulees au cours
            du temps. 0 sinon. 
            
        Returns 
        -------
        res : float
            Resultat de l'optimisation
    """   
    
    res = basinhopping(objectif,params,minimizer_kwargs={"args" : (args_,distance, L_A, L_B, L_C, obs, cumul)}, accept_test=mybounds)
    
    return res

###############################################################################

def visual_opt_bis(params,args_,distance,L_A,L_B,L_C,mybounds,obs,cumul):

    """
        Optimise sur la fonction objectif un seul point de depart pour
        l'algorithme du recuit simule. Renvoie le resultat de 
        l'optimisation et les valeurs du systeme Lt, Lt_p et Nt pour chaque 
        sous parcelle. Affiche les larves estimees et observees pour chaque
        sous parcelle.
    
        Args
        ----
        params : list
            Contient les parametres fixes du modele de la fonction 
            mdl1.integrateCecidoEchange
        args_ : list
            Contient une seule initialisation des parametres a estimer du 
            modele de la fonction mdl1.integrateCecidoEchange           
        distance : fonction 
            Fonction objectif choisie
        L_ABC : ndarray
            Nombre de larves au cours du temps dans chaque sous parcelle
        bounds : tuple
            Bornes des parametres a estimer
        obs : bool
            Vaut 0 si on fait l'optimisation sur toutes les valeurs
            journalieres, 1 si sur les valeurs observees
        cumul : bool
            Vaut 1 si on fait l'optimisation sur les valeurs cumulees au cours
            du temps. 0 sinon. 
            
        Returns 
        -------
        res : tuple
            Resultat de l'optimisation
        Lt_ABC : ndarray
            Nombre de larves estimees dans chaque sous parcelle a chaque temps
        Lt_p_ABC : ndarray
            Nombre de larves piegees estimees dans chaque sous parcelle a 
            chaque temps
        Nt_ABC : ndarray
            Nombre d'adultes estimes dans chaque sous parcelle a chaque temps
    """ 
    
    res = optimize_bis(params,args_,distance,L_A,L_B,L_C,mybounds,obs,cumul)
    allparams = list(res.x)+list(args_)
    Lt_A, Lt_p_A, Nt_A, Lt_B, Lt_p_B, Nt_B, Lt_C, Lt_p_C, Nt_C = mdl1.integrateCecidoEchange(*allparams)
    
    plt.plot(Lt_A[d.xJoursA],label="Larves simulees")
    plt.plot(L_A[d.xJoursA],label="Larves observees")
    plt.legend()
    plt.title("Enherbement ras")
    plt.show()
    
    plt.plot(Lt_B[d.xJoursB],label="Larves simulees")
    plt.plot(L_B[d.xJoursB],label="Larves observees")
    plt.legend()
    plt.title("Bachage")
    plt.show()
    
    plt.plot(Lt_C[d.xJoursC],label="Larves simulees")
    plt.plot(L_C[d.xJoursC],label="Larves observees")
    plt.legend()
    plt.title("Enherbement haut")
    plt.show()
    
    return res, Lt_A, Lt_p_A, Nt_A, Lt_B, Lt_p_B, Nt_B, Lt_C, Lt_p_C, Nt_C

###############################################################################
        
def visual_optimization_bis(params,args_,distance,L_A,L_B,L_C,bounds,obs,cumul):
    
    """
        Optimise sur la fonction objectif avec plusieurs points de depart pour
        l'algorithme du recuit simule. Renvoie le resultat de la 
        meilleure optimisation et les valeurs du systeme Lt, Lt_p et Nt pour
        chaque sous parcelle. Affiche les larves estimees et observees pour
        chaque sous parcelle.
    
        Args
        ----
        params : list
            Contient les parametres fixes du modele de la fonction 
            mdl1.integrateCecidoEchange
        args_ : list
            Contient plusieurs initialisations des parametres a estimer du 
            modele de la fonction mdl1.integrateCecidoEchange           
        distance : fonction 
            Fonction objectif choisie
        L_ABC : ndarray
            Nombre de larves au cours du temps dans chaque sous parcelle
        bounds : tuple
            Bornes des parametres a estimer
        obs : bool
            Vaut 0 si on fait l'optimisation sur toutes les valeurs
            journalieres, 1 si sur les valeurs observees
        cumul : bool
            Vaut 1 si on fait l'optimisation sur les valeurs cumulees au cours
            du temps. 0 sinon. 
            
        Returns 
        -------
        res : tuple
            Resultat de l'optimisation
        Lt_ABC : ndarray
            Nombre de larves estimees dans chaque sous parcelle a chaque temps
        Lt_p_ABC : ndarray
            Nombre de larves piegees estimees dans chaque sous parcelle a 
            chaque temps
        Nt_ABC : ndarray
            Nombre d'adultes estimes dans chaque sous parcelle a chaque temps
    """    
    
    global distvalues
    distvalues = []
    
    params_ = list(product(*params))
    N = len(params_)
    for cpt in range (N):
        p0 = params_[cpt]
        res = optimize_bis(p0,args_,distance,L_A,L_B,L_C,bounds,obs,cumul)
        distvalues.append(res.fun)
    k = np.argmin(distvalues)

    res = optimize(params_[k],args_,distance,L_A,L_B,L_C,bounds,obs,cumul)
    
    allparams = list(res.x)+list(args_)
    Lt_A, Lt_p_A, Nt_A, Lt_B, Lt_p_B, Nt_B, Lt_C, Lt_p_C, Nt_C = mdl1.integrateCecidoEchange(*allparams)
    
    plt.plot(Lt_A[d.xJoursA],label="Larves simulees")
    plt.plot(L_A[d.xJoursA],label="Larves observees")
    plt.legend()
    plt.title("Enherbement ras")
    plt.show()
    
    plt.plot(Lt_B[d.xJoursB],label="Larves simulees")
    plt.plot(L_B[d.xJoursB],label="Larves observees")
    plt.legend()
    plt.title("Bachage")
    plt.show()
    
    plt.plot(Lt_C[d.xJoursC],label="Larves simulees")
    plt.plot(L_C[d.xJoursC],label="Larves observees")
    plt.legend()
    plt.title("Enherbement haut")
    plt.show()
    
    return res, Lt_A, Lt_p_A, Nt_A, Lt_B, Lt_p_B, Nt_B, Lt_C, Lt_p_C, Nt_C

###############################################################################