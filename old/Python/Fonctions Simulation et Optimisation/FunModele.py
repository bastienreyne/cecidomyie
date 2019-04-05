"""
    Ce fichier permet de simuller les larves de cecido et les inflos en 
    mettant en entree les nouvelles inflos. (modele complet)
"""
###############################################################################

import numpy as np
import Datas as d
from math import *
import matplotlib.pyplot as plt

###############################################################################

gamma = 0.59
mu_MS_A = 0.98
mu_MS_C = 0.44
pl = 0.59
k = 0.84
psi = 102

It_plus_A = d.new_A
It_plus_B = d.new_B
It_plus_C = d.new_C

###############################################################################

def integrateCecidoInflo (psi, It_plus_A, It_plus_B, It_plus_C) :
    
    """
        Calcule et renvoie les variables du systeme It, Lt, Lt_p et Nt pour
        chaque sous-parcelle.
        
        Args
        ----
        psi : int 
            Nombre de larves cumulees sur une inflorescence qui garantit sa
            mort
        It_plus_A/B/C : ndarray
            Nombre de nouvelles inflorescences dans la sous-parcelle A 
            (enherbement ras) ou B (bache) ou C (enherbement haut) a chaque 
            date
            
        Returns
        -------
        It_A/B/C : ndarray
            Nombre d'inflorescences vivantes dans la sous-parcelle A 
            (enherbement ras) ou B (bache) ou C (enherbement haut) a chaque
            date
        Lt_A/B/C : ndarray
            Nombre de larves qui s'ejectent des inflorescences dans la sous-
            parcelle A (enherbement ras) ou B (bache) ou C (enherbement haut) 
            a chaque date
        Lt_p_A/B/C : ndarray
            Nombre de larves piegees dans la sous-parcelle A (enherbement ras)
            ou B (bache) ou C (enherbement haut) a chaque date
        Nt_A/B/C : ndarray
            Nombre d'adultes dans la sous- parcelle A (enherbement ras) ou B 
            (bache) ou C (enherbement haut) a chaque date
    """
    dtA = len(It_plus_A)
    dtB = len(It_plus_B)
    dtC = len(It_plus_C)
    dt = max(dtA,dtB,dtC)

    DegatA = np.zeros(dt+1)
    It_A   = np.zeros(dt+1)
    It_A[dt-dtA:dt] = It_plus_A
    Lt_A   = np.zeros(dt)
    Lt_p_A = np.zeros(dt)
    Nt_A   = np.zeros(dt)
    
    DegatB = np.zeros(dt+1)
    It_B   = np.zeros(dt+1)
    It_B[dt-dtB:dt] = It_plus_B
    Lt_B   = np.zeros(dt)
    Lt_p_B = np.zeros(dt)
    Nt_B   = np.zeros(dt)
    
    DegatC = np.zeros(dt+1)
    It_C   = np.zeros(dt+1)
    It_C[dt-dtC:dt] = It_plus_C
    Lt_C   = np.zeros(dt)
    Lt_p_C = np.zeros(dt)
    Nt_C   = np.zeros(dt)
    
    debA = np.where(It_A!=0)[0][0]
    debB = np.where(It_B!=0)[0][0]
    debC = np.where(It_C!=0)[0][0]    
    
    II_A = np.diag(It_A)   
    II_B = np.diag(It_B)
    II_C = np.diag(It_C)

    for t in range (1,dt) :
        
        Nt_plus_1_A, Lt_plus_1_A, Nt_plus_1_out_A = stepCecido (0, t, debA, Nt_A, Lt_A, It_A, mu_MS_A)
        Lt_plus_1_B = stepCecido (1, t, debB, Nt_B, Lt_B, It_B, 0.0)
        Nt_plus_1_C, Lt_plus_1_C, Nt_plus_1_out_C = stepCecido (0, t, debC, Nt_C, Lt_C, It_C, mu_MS_C)
        
        DegatA, II_A = stepInflo(psi,t,DegatA,Lt_plus_1_A,II_A)
        DegatB, II_B = stepInflo(psi,t,DegatB,Lt_plus_1_B,II_B)
        DegatC, II_C = stepInflo(psi,t,DegatC,Lt_plus_1_C,II_C)
        
        if ((It_B[t]+It_C[t])!=0):
            N_AB, N_AC = out(Nt_plus_1_out_A, It_B[t], It_C[t])
        else:
            N_AB = 0
            N_AC = 0
        
        if ((It_A[t]+It_B[t])!=0):
            N_CA, N_CB = out(Nt_plus_1_out_C, It_A[t], It_B[t])
        else:
            N_CA = 0
            N_CB = 0
        
        Nt_A[t]   = gamma*It_A[t] + Nt_plus_1_A + N_CA
        Lt_A[t]   = Lt_plus_1_A
        Lt_p_A[t] = Lt_plus_1_A*d.beta
        
        Nt_B[t]   = gamma*It_B[t] + N_AB + N_CB
        Lt_B[t]   = Lt_plus_1_B
        Lt_p_B[t] = Lt_plus_1_B*d.beta
        
        Nt_C[t]   = gamma*It_C[t] + Nt_plus_1_C + N_AC
        Lt_C[t]   = Lt_plus_1_C
        Lt_p_C[t] = Lt_plus_1_C*d.beta
        
        It_A[t+1] = np.sum(II_A[:,t+1])
        It_B[t+1] = np.sum(II_B[:,t+1])
        It_C[t+1] = np.sum(II_C[:,t+1])

    return It_A[dt-dtA:dt], Lt_A[dt-dtA:dt], Lt_p_A[dt-dtA:dt], Nt_A[dt-dtA:dt], It_B[dt-dtB:dt], Lt_B[dt-dtB:dt], Lt_p_B[dt-dtB:dt], Nt_B[dt-dtB:dt], It_C[dt-dtC:dt], Lt_C[dt-dtC:dt], Lt_p_C[dt-dtC:dt], Nt_C[dt-dtC:dt] 

    
###############################################################################

def stepCecido (bache, t, dt, Nt, Lt, It, mu_MS) :
    
    """
        Calcule et renvoie le nombre d'adultes, de larves et de larves piegees
        a la date t+1. 
    """
    
    if (t>=d.d_l+dt) : 
        if (Nt[t-d.d_l] <= (k*It[t-d.d_l])):
            R = 1
        else :
            R = k*It[t-d.d_l]/Nt[t-d.d_l]
        Lt_plus_1 = Nt[t-d.d_l] * R * d.E * d.mu
    else : 
        Lt_plus_1 = 0.0
    
    if (bache==1):
        return Lt_plus_1
    
    if (t >= (d.d_p+d.d_l+dt)) :
        emerge        = Lt[t-d.d_p] * mu_MS * d.p_pup / 2.0
        Nt_plus_1     = emerge * pl
        
        Nt_plus_1_out = emerge - Nt_plus_1
    else :
        Nt_plus_1     = 0.0
        Nt_plus_1_out = 0.0
    
    return Nt_plus_1, Lt_plus_1, Nt_plus_1_out

###############################################################################
    
def stepInflo(psi,t,Degat,Lt,II):
    
    id_ = list(np.where(II[:,t]!=0)[0])
    
    if (Lt==0):
        for i in id_:
            II[i,t+1] = II[i,t]
            
    else:
        nbInflo = np.sum(II[:,t])
        for i in id_:
            alpha = floor(Degat[i]/psi)
            II[i,t+1] = II[i,t] - min(alpha,II[i,t])
            Degat[i] = Degat[i] + II[i,t]/float(nbInflo)*Lt - alpha*psi
    
    return Degat,II

###############################################################################

def out(Nt_plus_1_out_1, It_2, It_3) :
    
    """
        Renvoie le nombre d'adultes qui partent de la sous parcelle 1 
        redistribues aux blocs 2 et 3 proportionnellement a  la quantite de 
        ressources dans chaque bloc.
    """
    
    a2   = It_2/float(It_2+It_3)
    N_12 = round(Nt_plus_1_out_1 * a2)
    N_13 = Nt_plus_1_out_1 - N_12
    
    return N_12, N_13


###############################################################################