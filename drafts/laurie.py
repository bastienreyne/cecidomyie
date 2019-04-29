import numpy as np

def norm_minmax(X):
	Y = (X-min(X))/float(max(X)-min(X))
	return(Y)

norm_minmax(np.array([5, 2, 4]))
## Pourquoi cette norme ?

from datetime import date
import pandas

def diff_jours(day, refday):
	diff = (date(2017, day[1], day[0]) - date(2017, refday[1], refday[0])).days
	return diff

data = pandas.read_csv("/home/bastien/Stage/Moi/Data/2017_flo_B1_.csv", ";")
a = data.new.shape
type(a)

dtA = 12
b = np.zeros(dtA)
b[15-12:]=3
b
c = np.diag(b)
c
0.1+0.2-0.3 == 0.
