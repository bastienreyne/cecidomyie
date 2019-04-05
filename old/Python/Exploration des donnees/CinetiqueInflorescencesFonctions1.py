# Importation des librairies

import matplotlib.pyplot as plt
from pandas import *

# Pour la lecture des fichiers 

def bloc (Nom, groupe, Sheet, un, deux, trois, quatre, cinq) :
    f = read_excel('../../Fichiers de donnees/Donnees re organisees/Fichier donnees floraison/'+Nom+'.xls')
    liste = f.groupby(groupe).sum()
    nouvelles = liste[un]
    vivantes = liste[deux]
    mortes = liste[trois]
    vivantes_theo = liste[quatre]
    mortes_theo = liste[cinq]  
    return nouvelles, vivantes, mortes, vivantes_theo, mortes_theo


def fichier_donnees_floraison (Nom) :
    f = read_excel('../../Fichiers de donnees/Donnees re organisees/Fichier donnees floraison/'+Nom+'.xls')
    df = f.groupby('date').sum()
    nouvelles = df["nouvelles"]
    vivantes = df["vivantes"]
    mortes = df["mortes"]
    return nouvelles, vivantes, mortes


def plot_ (Jours, nouvelles, vivantes, mortes, vivantes_theo, mortes_theo) :
    plt.plot(Jours,vivantes,label="Vivantes")
    plt.plot(Jours,nouvelles,label="Nouvelles")
    plt.plot(Jours,mortes,label="Mortes")
    plt.legend()
    plt.xticks(rotation=60)
    plt.title("Donnees relevees")
    plt.show()
    plt.plot(Jours,vivantes_theo,label="Vivantes")
    plt.plot(Jours,nouvelles,label="Nouvelles")
    plt.plot(Jours,mortes_theo,label="Mortes")
    plt.legend()
    plt.xticks(rotation=60)
    plt.title("Donnees theoriques")
    plt.show()
    plt.plot(Jours,vivantes_theo,label="Vivantes theoriques")
    plt.plot(Jours,vivantes,label="Vivantes observees")
    plt.plot(Jours,vivantes_theo-vivantes,label="Difference")
    plt.xticks(rotation=60)
    plt.legend()
    plt.title("Comparaison")
    plt.show()

# DONNEES REELLES _ Fichier donnees floraison
    
## BLOC 1

Bloc1_nouvelles, Bloc1_vivantes, Bloc1_mortes, Bloc1_vivantes_theo, Bloc1_mortes_theo = bloc("Bloc1", "date", "1", "nouvelles", "vivantes", "mortes", "vivantes_theo", "mortes_theo")
Bloc1_Jours = Bloc1_nouvelles.index

Bloc1_bache_nouvelles, Bloc1_bache_vivantes, Bloc1_bache_mortes, Bloc1_bache_vivantes_theo, Bloc1_bache_mortes_theo = bloc("Bloc1_bache", "date", "1", "nouvelles", "vivantes", "mortes", "vivantes_theo", "mortes_theo")
Bloc1_bache_Jours = Bloc1_bache_nouvelles.index

Bloc1_enh_ras_nouvelles, Bloc1_enh_ras_vivantes, Bloc1_enh_ras_mortes, Bloc1_enh_ras_vivantes_theo, Bloc1_enh_ras_mortes_theo = bloc("Bloc1_enh_ras", "date", "1", "nouvelles", "vivantes", "mortes", "vivantes_theo", "mortes_theo")
Bloc1_enh_ras_Jours = Bloc1_enh_ras_nouvelles.index

Bloc1_enh_haut_nouvelles, Bloc1_enh_haut_vivantes, Bloc1_enh_haut_mortes, Bloc1_enh_haut_vivantes_theo, Bloc1_enh_haut_mortes_theo = bloc("Bloc1_enh_haut", "date", "1", "nouvelles", "vivantes", "mortes", "vivantes_theo", "mortes_theo")
Bloc1_enh_haut_Jours = Bloc1_enh_haut_nouvelles.index