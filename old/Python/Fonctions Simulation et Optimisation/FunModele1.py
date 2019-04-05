"""
    Ce fichier contient les fonctions pour construire le modele manguier_CdF, 
    plus particulierement le modele calculant la population de cecidomyies.
"""

###############################################################################

import numpy as np
import matplotlib.pyplot as plt
from Datas import *

###############################################################################

def stepCecido (bache, t, Nt, Lt, It, d_l, E, mu, p_pup, d_p, mu_MS, pl, k) :
    
    """
        Calcule et renvoie le nombre d'adultes, de larves et de larves piegees
        a la date t+1. 
        
        Args
        ----
        bache : bool
            Vaut 1 si la parcelle est bachee, 0 sinon
        t : int
            Date du systeme
        Nt : ndarray
            Nombre d'adultes dans le verger des dates 0 a t
        Lt : ndarray
            Nombre de larves dans le verger des dates 0 a t
        It : ndarray
            Nombre d'inflorescences dans le verger a  chaque date
        d_l : int
            Duree de developpement des larves et des oeufs
        E : int
            Nombre moyen d'oeufs pondus par une femelle
        mu : float
            Taux de survie des oeufs jusqu'au troisieme stade larvaire
        p_pup : float
            Probabilite, pour une larve, d'entrer en pupaison
        d_p : int
            Duree de pupaison
        mu_MS : float 
            Probabilite de survie a  la modalite du sol
        pl : float
            Probabilite pour la population endogene de rester
        k : float
            Disponibilite quotidienne sur une ressource
            
        Returns
        -------
        Nt_plus_1 : int
            Nombre d'adultes emergeant restant dans le verger a  la date t+1
        Lt_plus_1 : int
            Nombre de larves dans le verger a  la date t+1
        Nt_plus_1_out : int
            Nombre d'adultes emergeant quittant le verger a  la date t+1
    """
    
    if (t>=d_l) : 
        if (Nt[t-d_l] < (k*It[t-d_l])):
            R = 1
        else :
            R = k*It[t-d_l]/Nt[t-d_l]

        Lt_plus_1 = Nt[t-d_l] * R * E * mu / 2.0
    else : 
        Lt_plus_1 = 0.0
    
    if (bache==1):
        return Lt_plus_1
    
    if (t >= (d_p+d_l)) :
        emerge        = Lt[t-d_p-d_l] * mu_MS * p_pup
        Nt_plus_1     = emerge * pl
        
        Nt_plus_1_out = emerge - Nt_plus_1
    else :
        Nt_plus_1     = 0.0
        Nt_plus_1_out = 0.0
    
    return Nt_plus_1, Lt_plus_1, Nt_plus_1_out

###############################################################################
    
def integrateCecidoEchange (lambda_, mu_MS_A, mu_MS_C, pl, k, TotJours, It_A, It_B, It_C, d_l, E, mu, p_pup, d_p, beta, popExo, out) :
    
    """
        Calcule et renvoie les variables du systeme Lt, Lt_p et Nt.
        
        Args
        ----
        lambda_ : float
            Pour calculer la population exogene des trois sous-parcelles.
        mu_MS_A : float
            Taux de survie a la modalite du sol enherbement ras
        mu_MS_C : float
            Taux de survie a la modalite du sol enherbement haut
        pl : float
            Probabilite pour la population endogene de rester
        k : float
            Disponibilite quotidienne sur une ressource
        TotJours : int
            Nombre de jours 
        It_A : ndarray
            Nombre d'inflorescences dans la parcelle A (EnhRas) a chaque date
        It_B : ndarray
            Nombre d'inflorescences dans la parcelle B (Bache) a chaque date
        It_C : ndarray
            Nombre d'inflorescences dans la parcelle C (EnhHaut) a chaque date
        d_l : int
            Duree de developpement des larves et des oeufs
        E : int
            Nombre moyen d'oeufs pondus par une femelle
        mu : float
            Taux de survie des oeufs jusqu'au troisieme stade larvaire
        p_pup : float
            Probabilite, pour une larve, d'entrer en pupaison
        d_p : int
            Duree de pupaison
        beta : float
            Taux d'efficacite du piege
        pl : float
            Probabilite pour la population endogene de rester
        k : float
            Disponibilite quotidienne sur une ressource
        popExo : function
            Calcule la population exogene a chaque date
        out : function
            Redistribue les adultes qui partent d'un bloc vers les deux autres
        L : ndarray
            Nombre de larves observees dans le verger a chaque date
            
        Returns
        -------
        Nt_A : ndarray
            Nombre d'adultes dans la sous parcelle enh.ras a chaque date
        Lt_A : ndarray
            Nombre de larves dans la sous parcelle enh.ras a chaque date
        Lt_p_A : ndarray
            Nombre de larves piegees la sous parcelle enh.ras a chaque date
        Nt_B : ndarray
            Nombre d'adultes dans la sous parcelle bache a chaque date
        Lt_B : ndarray
            Nombre de larves dans la sous parcelle bache a chaque date
        Lt_p_B : ndarray
            Nombre de larves piegees la sous parcelle bache a chaque date
        Nt_C : ndarray
            Nombre d'adultes dans la sous parcelle enh.haut a chaque date
        Lt_C : ndarray
            Nombre de larves dans la sous parcelle enh.haut a chaque date
        Lt_p_C : ndarray
            Nombre de larves piegees la sous parcelle enh.haut a chaque date
    """

    Nt_A   = popExo(lambda_, It_A, TotJours+1)
    Nt_B   = popExo(lambda_, It_B, TotJours+1)
    Nt_C   = popExo(lambda_, It_C, TotJours+1)
    
    Lt_A   = np.zeros(TotJours+1)
    Lt_p_A = np.zeros(TotJours+1)
    Lt_B   = np.zeros(TotJours+1)
    Lt_p_B = np.zeros(TotJours+1)
    Lt_C   = np.zeros(TotJours+1)
    Lt_p_C = np.zeros(TotJours+1)
    
    for t in range (1,TotJours+1) :

        Nt_plus_1_A, Lt_plus_1_A, Nt_plus_1_out_A = stepCecido (0, t, Nt_A, Lt_A, It_A, d_l, E, mu, p_pup, d_p, mu_MS_A, pl, k)
        Lt_plus_1_B = stepCecido (1, t, Nt_B, Lt_B, It_B, d_l, E, mu, p_pup, d_p, 0.0, pl, k)
        Nt_plus_1_C, Lt_plus_1_C, Nt_plus_1_out_C = stepCecido (0, t, Nt_C, Lt_C, It_C, d_l, E, mu, p_pup, d_p, mu_MS_C, pl, k)
        
        N_AB, N_AC = out(Nt_plus_1_out_A, It_B[t], It_C[t])
        N_CA, N_CB = out(Nt_plus_1_out_C, It_A[t], It_B[t])
        
        Nt_A[t]   = Nt_plus_1_A + Nt_A[t] + N_CA
        Lt_A[t]   = Lt_plus_1_A
        Lt_p_A[t] = Lt_plus_1_A*beta
        
        Nt_B[t]   = Nt_B[t] + N_AB + N_CB
        Lt_B[t]   = Lt_plus_1_B
        Lt_p_B[t] = Lt_plus_1_B*beta
        
        Nt_C[t]   = Nt_plus_1_C + Nt_C[t] + N_AC
        Lt_C[t]   = Lt_plus_1_C
        Lt_p_C[t] = Lt_plus_1_C*beta

    return Lt_A, Lt_p_A, Nt_A, Lt_B, Lt_p_B, Nt_B, Lt_C, Lt_p_C, Nt_C 

###############################################################################

def visualIntegrateCecidoEchange (lambda_, mu_MS_A, mu_MS_C, pl, k, TotJours, It_A, It_B, It_C, d_l, E, mu, p_pup, d_p, beta, popExo, out, L_A, L_B, L_C) :
        
    """
        Calcule et renvoie les variables du systeme Lt, Lt_p et Nt.
        Affiche les courbes des larves simulees et observees.
        
        Args
        ----
        lambda_ : float/int ou ndarray
            C'est un float/int si on calcule la population exogene de la meme
            maniere pour les trois sous-parcelle, sinon c'est un ndarray.
        mu_MS_A : float
            Taux de survie a la modalite du sol enherbement ras
        mu_MS_C : float
            Taux de survie a la modalite du sol enherbement haut
        pl : float
            Probabilite pour la population endogene de rester
        k : float
            Disponibilite quotidienne sur une ressource
        TotJours : int
            Nombre de jours 
        It_A : ndarray
            Nombre d'inflorescences dans la parcelle A (EnhRas) a chaque date
        It_B : ndarray
            Nombre d'inflorescences dans la parcelle B (Bache) a chaque date
        It_C : ndarray
            Nombre d'inflorescences dans la parcelle C (EnhHaut) a chaque date
        d_l : int
            Duree de developpement des larves et des oeufs
        E : int
            Nombre moyen d'oeufs pondus par une femelle
        mu : float
            Taux de survie des oeufs jusqu'au troisieme stade larvaire
        p_pup : float
            Probabilite, pour une larve, d'entrer en pupaison
        d_p : int
            Duree de pupaison
        beta : float
            Taux d'efficacite du piege
        pl : float
            Probabilite pour la population endogene de rester
        k : float
            Disponibilite quotidienne sur une ressource
        popExo : function
            Calcule la population exogene a chaque date
        out : function
            Redistribue les adultes qui partent d'un bloc vers les deux autres
        L_A : ndarray
            Nombre de larves observees dans la parcelle enh.ras a chaque date
        L_B : ndarray
            Nombre de larves observees dans la parcelle bachage a chaque date
        L_C : ndarray
            Nombre de larves observees dans la parcelle enh.haut a chaque date
            
        Returns
        -------
        Nt_A : ndarray
            Nombre d'adultes dans la sous parcelle enh.ras a chaque date
        Lt_A : ndarray
            Nombre de larves dans la sous parcelle enh.ras a chaque date
        Lt_p_A : ndarray
            Nombre de larves piegees la sous parcelle enh.ras a chaque date
        Nt_B : ndarray
            Nombre d'adultes dans la sous parcelle bache a chaque date
        Lt_B : ndarray
            Nombre de larves dans la sous parcelle bache a chaque date
        Lt_p_B : ndarray
            Nombre de larves piegees la sous parcelle bache a chaque date
        Nt_C : ndarray
            Nombre d'adultes dans la sous parcelle enh.haut a chaque date
        Lt_C : ndarray
            Nombre de larves dans la sous parcelle enh.haut a chaque date
        Lt_p_C : ndarray
            Nombre de larves piegees la sous parcelle enh.haut a chaque date
    """

    Lt_A, Lt_p_A, Nt_A, Lt_B, Lt_p_B, Nt_B, Lt_C, Lt_p_C, Nt_C  = integrateCecidoEchange (lambda_, mu_MS_A, mu_MS_C, pl, k, TotJours, It_A, It_B, It_C, d_l, E, mu, p_pup, d_p, beta, popExo, out)
    
    plt.plot(Lt_p_A, label="Larves simulees")
    plt.plot(L_A, label="Larves observees")
    plt.legend()
    plt.title("Enherbement ras")
    plt.show()
    
    plt.plot(Lt_p_B, label="Larves simulees")
    plt.plot(L_B, label="Larves observees")
    plt.legend()
    plt.title("Bachage")
    plt.show()
    
    plt.plot(Lt_p_C, label="Larves simulees")
    plt.plot(L_C, label="Larves observees")
    plt.legend()
    plt.title("Enherbement haut")
    plt.show()
        
    return Lt_A, Lt_p_A, Nt_A, Lt_B, Lt_p_B, Nt_B, Lt_C, Lt_p_C, Nt_C

###############################################################################
    
def integrateCecidoEchangeAdultes (lambda_, mu_MS_A, mu_MS_C, pl, k, TotJours, It_A, It_B, It_C, d_l, E, mu, p_pup, d_p, beta, popExo, out) :
    
    """
        Calcule, renvoie et affiche les variables du systeme Lt, Lt_p et Nt .
        
        Args
        ----
        lambda_ : float
            Pour calculer la population exogene des trois sous-parcelles.
        mu_MS_A : float
            Taux de survie a la modalite du sol enherbement ras
        mu_MS_C : float
            Taux de survie a la modalite du sol enherbement haut
        pl : float
            Probabilite pour la population endogene de rester
        k : float
            Disponibilite quotidienne sur une ressource
        TotJours : int
            Nombre de jours 
        It_A : ndarray
            Nombre d'inflorescences dans la parcelle A (EnhRas) a chaque date
        It_B : ndarray
            Nombre d'inflorescences dans la parcelle B (Bache) a chaque date
        It_C : ndarray
            Nombre d'inflorescences dans la parcelle C (EnhHaut) a chaque date
        d_l : int
            Duree de developpement des larves et des oeufs
        E : int
            Nombre moyen d'oeufs pondus par une femelle
        mu : float
            Taux de survie des oeufs jusqu'au troisieme stade larvaire
        p_pup : float
            Probabilite, pour une larve, d'entrer en pupaison
        d_p : int
            Duree de pupaison
        beta : float
            Taux d'efficacite du piege
        pl : float
            Probabilite pour la population endogene de rester
        k : float
            Disponibilite quotidienne sur une ressource
        popExo : function
            Calcule la population exogene a chaque date
        out : function
            Redistribue les adultes qui partent d'un bloc vers les deux autres
        L : ndarray
            Nombre de larves observees dans le verger a chaque date
            
        Returns
        -------
        Nt_A : ndarray
            Nombre d'adultes dans la sous parcelle enh.ras a chaque date
        Lt_A : ndarray
            Nombre de larves dans la sous parcelle enh.ras a chaque date
        Lt_p_A : ndarray
            Nombre de larves piegees la sous parcelle enh.ras a chaque date
        Nt_B : ndarray
            Nombre d'adultes dans la sous parcelle bache a chaque date
        Lt_B : ndarray
            Nombre de larves dans la sous parcelle bache a chaque date
        Lt_p_B : ndarray
            Nombre de larves piegees la sous parcelle bache a chaque date
        Nt_C : ndarray
            Nombre d'adultes dans la sous parcelle enh.haut a chaque date
        Lt_C : ndarray
            Nombre de larves dans la sous parcelle enh.haut a chaque date
        Lt_p_C : ndarray
            Nombre de larves piegees la sous parcelle enh.haut a chaque date
    """

    Nt_A   = popExo(lambda_, It_A, TotJours+1)
    Nt_B   = popExo(lambda_, It_B, TotJours+1)
    Nt_C   = popExo(lambda_, It_C, TotJours+1)
    
    Lt_A   = np.zeros(TotJours+1)
    Lt_p_A = np.zeros(TotJours+1)
    Lt_B   = np.zeros(TotJours+1)
    Lt_p_B = np.zeros(TotJours+1)
    Lt_C   = np.zeros(TotJours+1)
    Lt_p_C = np.zeros(TotJours+1)
    
    Nt_CA = np.zeros(TotJours+1)
    Nt_CB = np.zeros(TotJours+1)
    Nt_AB = np.zeros(TotJours+1)
    Nt_AC = np.zeros(TotJours+1)
    
    for t in range (1,TotJours+1) :

        Nt_plus_1_A, Lt_plus_1_A, Nt_plus_1_out_A = stepCecido (0, t, Nt_A, Lt_A, It_A, d_l, E, mu, p_pup, d_p, mu_MS_A, pl, k)
        Lt_plus_1_B = stepCecido (1, t, Nt_B, Lt_B, It_B, d_l, E, mu, p_pup, d_p, 0.0, pl, k)
        Nt_plus_1_C, Lt_plus_1_C, Nt_plus_1_out_C = stepCecido (0, t, Nt_C, Lt_C, It_C, d_l, E, mu, p_pup, d_p, mu_MS_C, pl, k)
        
        N_AB, N_AC = out(Nt_plus_1_out_A, It_B[t], It_C[t])
        N_CA, N_CB = out(Nt_plus_1_out_C, It_A[t], It_B[t])
        
        Nt_A[t]   = Nt_plus_1_A + Nt_A[t] + N_CA
        Lt_A[t]   = Lt_plus_1_A
        Lt_p_A[t] = Lt_plus_1_A*beta
        
        Nt_B[t]   = Nt_B[t] + N_AB + N_CB
        Lt_B[t]   = Lt_plus_1_B
        Lt_p_B[t] = Lt_plus_1_B*beta
        
        Nt_C[t]   = Nt_plus_1_C + Nt_C[t] + N_AC
        Lt_C[t]   = Lt_plus_1_C
        Lt_p_C[t] = Lt_plus_1_C*beta
        
        Nt_CA[t] = N_CA
        Nt_CB[t] = N_CB
        Nt_AB[t] = N_AB
        Nt_AC[t] = N_AC
        
    return Lt_A, Lt_p_A, Nt_A, Lt_B, Lt_p_B, Nt_B, Lt_C, Lt_p_C, Nt_C, Nt_CA, Nt_CB, Nt_AB, Nt_AC

###############################################################################

def popExo1 (lambda_, It, TotJours) : 
    
    """
        Lorsque la population exogene prend une valeur differente a chaque
        date. Renvoie le meme vecteur que lambda_t. 
        
        Args
        ----
        lambda_ : ndarray
        It  : ndarray
            Nombre d'inflorescences a  chaque date (INUTILE ICI)
        TotJours : int
            Nombre de jours (INUTILE ICI)
            
        Returns
        -------
        lambda_t : ndarray
            Population exogene a chaque date
    """
    
    return lambda_

###############################################################################

def popExo2 (lambda_, It, TotJours) :
    
    """        
        Lorsque la population exogene est proportionnel aux inflorescences. 
        Renvoie un vecteur proportionnel a It. 
        
        Args
        ----
        lambda_ : float
            Coefficient multiplicatif
        It  : ndarray
            Nombre d'inflorescences a  chaque date
        TotJours : int
            Nombre de jours (INUTILE ICI)
            
        Returns
        -------
        lambda_t : ndarray
            Population exogene a chaque date. lambda_t = lambda_*It
    """
    
    lambda_t = np.zeros(TotJours)
    for i in range (TotJours):
        lambda_t[i] = It[i]*lambda_
    
    return lambda_t

###############################################################################
    
def popExo3 (lambda_, It, TotJours) : 
    
    """
        Lorsque la population exogene est constante au cours du temps. Renvoie 
        un vecteur constant egal a lambda_ pour chaque date. 
        
        Args
        ----
        lambda_ : int
        It  : ndarray
            Nombre d'inflorescences a  chaque date
        TotJours : int
            Nombre de jours (INUTILE ICI)
            
        Returns
        -------
        lambda_t : ndarray
            Population exogene a chaque date.
    """
    
    lambda_t = np.ones(TotJours)*lambda_
    
    return lambda_t

###############################################################################

def out1(Nt_plus_1_out_1, It_2, It_3) :
    
    """
        Renvoie le nombre d'adultes qui partent de la sous parcelle 1 
        redistribues aux blocs 2 et 3 en quantite egale.
    
        Args
        ----
        Nt_plus_1_out_1 : int
            Nombre d'adultes quittant le bloc 1
        It_2 : int
            Nombre d'inflorescences du bloc 2 (INUTILE ICI)
        It_3 : int
            Nombre d'inflorescences du bloc 3 (INUTILE ICI)
            
        Returns
        -------
        N_12 : int
            Nombre d'adultes allant du bloc 1 au 2
        N_13 : int
            Nombre d'adultes allant du bloc 1 au 3
    """
    
    N_12 = Nt_plus_1_out_1/2.0
    N_13 = Nt_plus_1_out_1/2.0
    
    return N_12, N_13

###############################################################################

def out2(Nt_plus_1_out_1, It_2, It_3) :
    
    """
        Renvoie le nombre d'adultes qui partent de la sous parcelle 1 
        redistribues aux blocs 2 et 3 proportionnellement a  la quantite de 
        ressources dans chaque bloc.
    
        Args
        ----
        Nt_plus_1_out_1 : int
            Nombre d'adultes quittant le bloc 1
        It_2 : int
            Nombre d'inflorescences du bloc 2
        It_3 : int
            Nombre d'inflorescences du bloc 3
            
        Returns
        -------
        N_12 : int
            Nombre d'adultes allant du bloc 1 au 2
        N_13 : int
            Nombre d'adultes allant du bloc 1 au 3
    """
    
    a2   = It_2/float(It_2+It_3)
    N_12 = Nt_plus_1_out_1 * a2
    N_13 = Nt_plus_1_out_1 - N_12
    
    return N_12, N_13

###############################################################################