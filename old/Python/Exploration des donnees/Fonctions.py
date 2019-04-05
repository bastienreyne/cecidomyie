import numpy as np
import csv
import matplotlib.pyplot as plt
from datetime import date, timedelta
from pandas import * # pour read_excel
import os


# Pour recuperer les donnees du fichier 

def donnees (Nom, groupe, Sheet, un, deux, trois) :
    
    f = read_excel(Nom+'.xls', Sheet)
    liste = f.groupby(groupe).sum()
    nb_larves = liste[un]
    nb_inflo = liste[deux]
    nb_inflo_morte = liste[trois]

    return nb_larves, nb_inflo, nb_inflo_morte

# Pour afficher trois courbes sur un meme graphe

def triplot (x, y1, y2, y3, y1_nom, y2_nom, y3_nom, titre, show=1) :
    
    plt.plot (x, y1, label=y1_nom)
    plt.plot (x, y2, label=y2_nom)
    plt.plot (x, y3, label=y3_nom)
    plt.legend ()
    plt.title (titre)
    plt.xticks (rotation=60)
    if (show==1) :
        plt.show ()
        
# Pour afficher trois subplots

def trisubplot (x, y1, y2, y3, y1_nom, y2_nom, y3_nom, titre, xx, yy, yy_nom, xxx, yyy, yyy_nom) :
    
    plt.subplot(311)
    triplot(x, y1, y2, y3, y1_nom, y2_nom, y3_nom, titre, 0)
    plt.subplot(312)
    plt.plot(xx, yy, label=yy_nom)
    plt.subplot(313)
    plt.plot(xxx, yyy, label=yyy_nom)
    plt.show()
    
# Pour afficher deux subplots

def deuxsubplot (x, y1, y2, y3, y1_nom, y2_nom, y3_nom, titre) :

    plt.subplot(211)
    plt.plot(x, y1, label=y1_nom)
    plt.plot(x, y2, label=y2_nom)
    plt.title(titre)
    plt.legend()
    plt.subplot(212)
    plt.plot(x, y3, label=y3_nom)
    plt.plot(x,np.zeros(len(x)),'k:')
    plt.legend()
    plt.show()