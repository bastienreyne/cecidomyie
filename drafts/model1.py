#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Mar 27 10:31:01 2019

@author: bastien
"""

### Sous-modèle 1

## Packages
import numpy as np
import pandas as pd
#from pyOpt import Optimization, NSGA2

## Paramètres fixes
sex_ratio = 0.5
mu_B = 0.0
proba_pupaison = 0.77
duree_larvation = 7
duree_pupaison = 5
eggs = 150
mu_larvation = 0.04
nb_jours = 80

## Paramètres à calibrer
gamma = 1.5
mu_ER = 0.8
mu_EH = 0.4
proba_migration = 0.5
capacite_inflo = 4.5

## Import data nécéssaire
inflosER = np.array(pd.read_csv("r1.csv").inflos_vivantes)
inflosB = np.array(pd.read_csv("b1.csv").inflos_vivantes)
inflosEH = np.array(pd.read_csv("h1.csv").inflos_vivantes)
larvesER = np.array(pd.read_csv("r1.csv").larves)
larvesB = np.array(pd.read_csv("b1.csv").larves)
larvesEH = np.array(pd.read_csv("h1.csv").larves)

inflos = [inflosER, inflosB, inflosEH]
larvess = [larvesER, larvesB, larvesEH]
## Indices
ER, B, EH = range(3)

## Fonctions
def exogene(gamma, inflos):
    ## Renvoie les individus exogenes lambda_t,i
    femelles_exogene = [gamma * inflos[bloc] for bloc in [ER, B, EH]]
    return femelles_exogene

def echangeDansBloc(proba_migration, inflos):
    ## Renvoie les coefs alpha
    alphaER, alphaB, alphaEH = [np.zeros((nb_jours, 3)) for bloc in [ER,B,EH]]
    alphaER[:,ER], alphaEH[:, EH] = [1-proba_migration for bloc in [ER,EH]]
    
    ## alphaER
    somme_inflosERB = inflos[B] + inflos[ER]
    alphaER[:,EH] = proba_migration * (inflos[ER] / somme_inflosERB)
    
    ## alphaEH
    somme_inflosEHB = inflos[B] + inflos[EH]
    alphaEH[:,ER] = proba_migration * (inflos[EH] / somme_inflosEHB)
    
    ## alphaB
    alphaB[:,ER] = proba_migration * (inflos[B] / somme_inflosEHB)
    alphaB[:,EH] = proba_migration * (inflos[B] / somme_inflosERB)
    
    return [alphaER, alphaB, alphaEH]

def dispo_ressource(day, capacite_inflo, inflos, femelles):
    ## Renvoie l'indicateur de disponibilité des ressources R
    if (femelles[day] <= capacite_inflo * inflos[day]):
        return 1
    else :
        return capacite_inflo * inflos[day] / femelles[day]
    
def larves(day, capacite_inflo, inflos, femelles):
    ## Revoie le nombre de larves
    if (day - duree_larvation <= -1):
        return 0
    else:
        R = dispo_ressource(day-duree_larvation, capacite_inflo, inflos, femelles)
        return femelles[day - duree_larvation] * R * eggs * mu_larvation

def femelles_endogenes(day, larvess, mu_sol):
    ## Renvoie le nombre de femelles endogènes
    if (day - duree_pupaison <= -1):
        return 0
    else:
        return larvess[day-duree_pupaison] * mu_sol * proba_pupaison * sex_ratio

def femelles_total(day, alpha, femelles_exo, femelles_endo):
    ## Renvoie le nombre de femelles total
    return femelles_exo[day] + np.dot(alpha[day,:], femelles_endo)

def dynamiques(gamma, proba_migration, mu_ER, mu_EH, capacite_inflo ,inflos):
    femelles_exogene = exogene(gamma, inflos)
    larve = [np.zeros(nb_jours) for bloc in [ER, B, EH]]
    femelles_endogene = [np.zeros(nb_jours) for bloc in [ER, B, EH]]
    femelles = [np.zeros(nb_jours) for bloc in [ER, B, EH]]
    alpha = echangeDansBloc(proba_migration, inflos)
    mu_sol = [mu_ER, mu_B, mu_EH]
    for day in range(nb_jours):
        for bloc in [ER,B,EH]:
            femelles[bloc][day] = femelles_exogene[bloc][day]
            larve[bloc][day] = larves(day, capacite_inflo, inflos[bloc], femelles[bloc])
            femelles_endogene[bloc][day] = femelles_endogenes(day, larve[bloc], mu_sol[bloc])
        femelle_endo = np.array([femelles_endogene[ER][day], femelles_endogene[B][day], femelles_endogene[EH][day]])
        for bloc in [ER,B, EH]:
            femelles[bloc][day] = femelles_total(day, alpha[bloc], femelles_exogene[bloc], femelle_endo)
    
    return larve





























