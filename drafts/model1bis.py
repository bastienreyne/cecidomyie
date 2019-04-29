#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Mar 25 11:51:31 2019

@author: bastien
"""

### Sous-modèle 1

## Packages
import numpy as np
import pandas as pd
from pyOpt import Optimization, NSGA2

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
inflosER = np.array(pd.read_csv("/home/bastien/Stage/Moi/r1.csv").inflos_vivantes)
inflosB = np.array(pd.read_csv("/home/bastien/Stage/Moi/b1.csv").inflos_vivantes)
inflosEH = np.array(pd.read_csv("/home/bastien/Stage/Moi/h1.csv").inflos_vivantes)
larvesER = np.array(pd.read_csv("/home/bastien/Stage/Moi/r1.csv").larves)
larvesB = np.array(pd.read_csv("/home/bastien/Stage/Moi/b1.csv").larves)
larvesEH = np.array(pd.read_csv("/home/bastien/Stage/Moi/h1.csv").larves)

## Fonctions
def exogene(gamma, inflosER, inflosB, inflosEH):
    ## Renvoie les individus exogene au bloc
    exogeneER = gamma * inflosER
    exogeneB = gamma * inflosB
    exogeneEH = gamma * inflosEH
    return exogeneER, exogeneB, exogeneEH

def alpha(proba_migration, inflosER, inflosB, inflosEH):
    ## Renvoie les alpha_t,i,j
    alphaER, alphaB, alphaEH = [np.zeros((nb_jours, 3)) for i in range(3)]
    alphaER[:,0], alphaB[:,1], alphaEH[:,2] = 1 - proba_migration, 1 - proba_migration, 1 - proba_migration
    
    somme_inflosERB = inflosB + inflosER
    alphaER[:,2] = proba_migration * (inflosER / somme_inflosERB)
    
    somme_inflosEHB = inflosB + inflosEH
    alphaEH[:,0] = proba_migration * (inflosEH / somme_inflosEHB)
    
    return alphaER, alphaB, alphaEH

def dispo_ressource(day, capacite_inflo, inflos, femelles):
    ## Renvoie l'indicateur de disponibilité des ressources R
    if (femelles[day] <= capacite_inflo * inflos[day]):
        return 1
    else :
        return capacite_inflo * inflos[day] / femelles[day]
    
def larves(day, capacite_inflo, inflos, femelles):
    ## Revoie le nombre de larves
    if (day - duree_larvation <= 0):
        return 0
    else:
        R = dispo_ressource(day-duree_larvation, capacite_inflo, inflos, femelles)
        return femelles[day - duree_larvation] * R * eggs * mu_larvation

def femelles_endogenes(day, larves, mu_sol):
    ## Renvoie le nombre de femelles endogènes
    if (day - duree_larvation - duree_pupaison <= 0):
        return 0
    else:
        return larves[day-duree_pupaison] * mu_sol * proba_pupaison

def femelles_total(day, alpha, femelles_exo, femelles_endo):
    ## Renvoie le nombre de femelles total
    return femelles_exo[day] + np.dot(alpha[day,:], femelles_endo)


def dynamiques(gamma, proba_migration, mu_ER, mu_EH, capacite_inflo ,inflosER, inflosB, inflosEH):
    ## Renvoie les dynamiques
    femelles_exoER, femelles_exoB, femelles_exoEH = exogene(gamma, inflosER, inflosB, inflosEH)
    larvesER, larvesB, larvesEH = np.zeros(nb_jours), np.zeros(nb_jours), np.zeros(nb_jours)
    femelles_endoER, femelles_endoB, femelles_endoEH = np.zeros(nb_jours), np.zeros(nb_jours), np.zeros(nb_jours)
    alphaER, alphaB, alphaEH = alpha(proba_migration, inflosER, inflosB, inflosEH)
    for day in range(nb_jours):
        larvesER[day], larvesB[day], larvesEH[day] = larves(day, capacite_inflo, inflosER, femellesER), larves(day, capacite_inflo, inflosB, femellesB), larves(day, capacite_inflo, inflosEH, femellesEH)
        femelles_endoER[day], femelles_endoB[day], femelles_endoEH[day] = femelles_endogenes(day, larvesER, mu_ER), femelles_endogenes(day, larvesB, mu_B), femelles_endogenes(day, larvesEH, mu_EH)
        femelles_endo = np.array([femelles_endoER[day], femelles_endoB[day], femelles_endoEH[day]])
        femellesER[day], femellesB[day], femellesEH[day] = femelles_total(day, alphaER, femelles_exoER, femelles_endo), femelles_total(day, alphaB, femelles_exoB, femelles_endo), femelles_total(day, alphaEH, femelles_exoEH, femelles_endo)
    return larvesER, larvesB, larvesEH

ER,B,EH = range(3)

def dynamiques(gamma, proba_migration, mu, capacite_inflo ,inflos):
    femelles_exo = [ exogene(gamma, inflos[block]) for block in [ER,B,EH] ]
    for day in range(nb_jours):
        for block in [ER,B,EH]:
            larves[block][day] = larves(day, inflos[block], femelles[block])
        femelles_endo = np.array([femelles_endoER[day], femelles_endoB[day], femelles_endoEH[day]])
        for block in [ER,B,EH]:
        

def objectif(args):
    larves_estER, larves_estB, larves_estEH = dynamiques(args[0], args[1], args[2], args[3], args[4] ,inflosER, inflosB, inflosEH)
    f = [np.sqrt((larves_estER-larvesER)**2), np.sqrt((larves_estB-larvesB)**2), np.sqrt((larves_estEH-larvesEH)**2)]
    
    fail = 0
    return f, fail

opt_prob = Optimization('NSGA2', objectif)
opt_prob.addObj('f')
opt_prob.addVar('gamma', 'c', lower=0.0, upper=150.0)
opt_prob.addVar('proba_migration', 'c', lower=0.0, upper=1.0)
opt_prob.addVar('mu_ER', 'c', lower=0.0, upper=1.0)
opt_prob.addVar('mu_EH', 'c', lower=0.0, upper=1.0)
opt_prob.addVar('capacite_inflo', 'c', lower=0.0, upper=150.0)

nsga = NSGA2()
nsga.setOption('PrintOut',0)
nsga(opt_prob)
#print(opt_prob.solution(0))