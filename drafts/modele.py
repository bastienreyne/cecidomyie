# coding=utf-8
import numpy as np
import pandas
#from datetime import date
from sklearn.preprocessing import StandardScaler
from scipy.optimize import basinhopping, differential_evolution, least_squares

## Data
floraison = pandas.read_csv("/home/bastien/Stage/Moi/2017_floraison.csv", ";")
piege = pandas.read_csv("/home/bastien/Stage/Moi/2017_piege.csv", ";")


## Paramètres fixes du modèle
d_l   = 7
E     = 150
mu    = 0.04
p_pup = 0.77
d_p   = 5
beta  = 1
T     = 50
mu_B = 0
days = pandas.to_datetime(piege.date)
nb_jours = pandas.to_timedelta(days.unique()[-1] - days.unique()[0]).days

## ENLEVER LES VARIABLES DE LA FONCTION QUI SONT FIXES !
def stepCecido(time, Nt, Lt, It, mu_MS, pl, k):
	## Calcule au temps time+1 : N(t+1), L(t+1) et I(t+1)
	if (time >= d_l):
		if (Nt[time - d_l] < k * It[time - d_l]):
			R = 1
		else:
			R = k * It[time - d_l] / Nt[time - d_l]
		newLt = Nt[time-d_l] * R * E * mu / 2.0
	else:
		newLt = 0.0

	if (time >= d_p + d_l):
		emerge = Lt[time - d_p - d_l] * mu_MS * p_pup
		newNt = emerge * pl
		newNtOut = emerge * (1 - pl)
	else:
		newNt = 0.0
		newNtOut = 0.0

	return newNt, newLt, newNtOut


def cecidoExo(lamda, It):
    ## Renvoie les popoulations qui viennent d'ailleurs
    lamda_t = np.zeros(nb_jours)
    for i in range(nb_jours):
        lamda_t[i] = It[i] * lamda

    return lamda_t

def transfert(newNtOut, It2, It3):
	## Renvoie la migration vers un autre sous-bloc
	alpha12 = It2 / float(It2 + It3)
	N12 = newNtOut * alpha12
	N13 = newNtOut * (1 - alpha12)

	return N12, N13

def integrateCecidoEchange(lamda, mu_A, mu_C, pl, k):
	## Renvoie Lt, Ltp, Nt
	NtA = cecidoExo(lamda, ItA)
	NtB = cecidoExo(lamda, ItB)
	NtC = cecidoExo(lamda, ItC)
	LtA = np.zeros(nb_jours)
	LtB = np.zeros(nb_jours)
	LtC = np.zeros(nb_jours)
	LtpA = np.zeros(nb_jours)
	LtpB = np.zeros(nb_jours)
	LtpC = np.zeros(nb_jours)
	NtCA = np.zeros(nb_jours)
	NtCB = np.zeros(nb_jours)
	NtAB = np.zeros(nb_jours)
	NtAC = np.zeros(nb_jours)

	for time in range(1, nb_jours):
		newNtA, newLtA, newNtAOut = stepCecido(time, NtA, LtA, ItA, mu_A, pl, k)
		newNtB, newLtB, newNtBOut = stepCecido(time, NtB, LtB, ItB, mu_B, pl, k)
		newNtC, newLtC, newNtCOut = stepCecido(time, NtC, LtC, ItC, mu_C, pl, k)

		NAB, NAC = transfert(newNtAOut, ItB[time], ItC[time])
		NCA, NCB = transfert(newNtCOut, ItA[time], ItB[time])

		NtA[time] = newNtA + NtA[time] + NCA
		NtB[time] = newNtB + NtB[time] + NAB + NCB
		NtC[time] = newNtC + NtC[time] + NAC

		LtA[time] = newLtA
		LtB[time] = newLtB
		LtC[time] = newLtC

		LtpA[time] = newLtA * beta
		LtpB[time] = newLtB * beta
		LtpC[time] = newLtC * beta

		NtCA[time] = NCA
		NtCB[time] = NCB
		NtAB[time] = NAB
		NtAC[time] = NAC

	return LtA, LtpA, NtA, LtB, LtpB, NtB, LtC, LtpC, NtC, NtCA, NtCB, NtAB, NtAC


scaler = StandardScaler()
def objectif(params, LA, LB, LC):
	LtA, LtpA, NtA, LtB, LtpB, NtB, LtC, LtpC, NtC, NtCA, NtCB, NtAB, NtAC = integrateCecidoEchange(*params)
	LA_norm = scaler.fit_transform(LA)
	LB_norm = scaler.fit_transform(LB)
	LC_norm = scaler.fit_transform(LC)
	LtA_norm = scaler.fit_transform(LtA.reshape(-1,1))
	LtB_norm = scaler.fit_transform(LtB.reshape(-1,1))
	LtC_norm = scaler.fit_transform(LtC.reshape(-1,1))

	N = np.size(LA)
	ans = np.sqrt(sum((LA_norm - LtA_norm)**2))/N + np.sqrt(sum((LB_norm - LtB_norm)**2))/N + np.sqrt(sum((LC_norm - LtC_norm)**2))/N

	return ans

## SOUS-MODÈLE 1


ItA = pandas.read_csv("/home/bastien/Stage/Moi/r1.csv").inflos_vivantes
ItB = pandas.read_csv("/home/bastien/Stage/Moi/b1.csv").inflos_vivantes
ItC= pandas.read_csv("/home/bastien/Stage/Moi/h1.csv").inflos_vivantes
x0 = [0.82, 1, 0.33, 0.36, 0.13]
LA = np.array(pandas.read_csv("/home/bastien/Stage/Moi/r1.csv").larves).reshape(-1,1)
LB = np.array(pandas.read_csv("/home/bastien/Stage/Moi/b1.csv").larves).reshape(-1,1)
LC = np.array(pandas.read_csv("/home/bastien/Stage/Moi/h1.csv").larves).reshape(-1,1)

class MyBounds(object):
    def __init__(self, xmax=[100.0, 1.0, 1.0, 1.0, 100.0], xmin=[0.0, 0.0, 0.0, 0.0, 0.0] ):
        self.xmax = np.array(xmax)
        self.xmin = np.array(xmin)
    def __call__(self, **kwargs):
        x = kwargs["x_new"]
        tmax = bool(np.all(x <= self.xmax))
        tmin = bool(np.all(x >= self.xmin))
        return tmax and tmin

mybounds = MyBounds()
   
#print('100 ITERATIONS')
#mod1 = basinhopping(objectif, x0, minimizer_kwargs={"args" : (LA, LB, LC)}, niter = 100, accept_test = mybounds)
#print(mod1)
#mod2 = basinhopping(objectif, x0, minimizer_kwargs={"args" : (LA, LB, LC)}, niter = 100, accept_test = mybounds)
#print(mod2)
#
#print('1000 ITERATIONS')
#mod3 = basinhopping(objectif, x0, minimizer_kwargs={"args" : (LA, LB, LC)}, niter = 1000, accept_test = mybounds)
#print(mod3)
#mod4 = basinhopping(objectif, x0, minimizer_kwargs={"args" : (LA, LB, LC)}, niter = 1000, accept_test = mybounds)
#print(mod4)
#
#print('ITERATIONS ILLIMITES')
#mod5 = basinhopping(objectif, x0, minimizer_kwargs={"args" : (LA, LB, LC)}, accept_test = mybounds)
#print(mod5)
#mod6 = basinhopping(objectif, x0, minimizer_kwargs={"args" : (LA, LB, LC)}, accept_test = mybounds)
#print(mod6)

## 100 iter
## array([1.73812746, 0.21824423, 0.24246656, 0.56341689, 0.45395005])
## array([1.90999194, 0.07816468, 0.00346807, 0.8467694 , 0.45395005])
## array([1.2392772 , 0.61052257, 0.7368315 , 0.913993  , 0.45395005])
## array([2.01500331, 0.84950122, 0.9757249 , 0.56736659, 2.58898609]) #NORM
## array([0.80849886, 0.61734386, 0.48316423, 0.59642683, 0.97925857]) #NORM
#bounds = [(0.0,100.0),(0.0,1.0),(0.0,1.0),(0.0,1.0),(0.0,100.0)]
#mod7 = differential_evolution(objectif, bounds, args=(LA, LB, LC))
#print(mod7)
#
#bounds2 = [(0.0,1000.0),(0.0,1.0),(0.0,1.0),(0.0,1.0),(0.0,1000.0)]
#mod8 = differential_evolution(objectif, bounds2, args=(LA, LB, LC))
#print(mod8)
bounds3 = ([0.0,1000.0], [0.0,1.0],[0.0,1.0],[0.0,1.0],[0.0,1000.0])
mod9 = least_squares(objectif, x0, bounds = bounds3, args = (LA,LB, LC))


## SOUS-MODÈLE 2
#gamma = 0.5
#mu_A = 0.5
#mu_C = 0.5
#pl = 0.5
#k = 0.5
#psi = 0.5
#newItA = 0#vecteur
#newItB = 0#vecteur
#newItC = 0#vecteur
#
#
#def stepInflo(time, psi, damage, Lt, Itd):
#	id = list(np.where(Itd[:,time] != 0)[0])
#
#	if (Lt == 0):
#		for i in id:
#			Itd[i, time +1] = Itd[i, time]
#	else:
#		nbInflo = np.sum(Itd[:, time])
#		for i in id:
#			alpha = damage[i] / psi
#			Itd[i, time + 1] = Itd[i, time] - min(alpha, Itd[i, time])
#			damage[i] = damage[i] + Itd[i, time] / float(nbInflo) * Lt - alpha * psi
#
#	return damage, Itd
#
#def stepCecido2(time, deb, Nt, Lt, It, mu_MS):
#	## Calcule au temps time+1 : N(t+1), L(t+1) et I(t+1)
#	if (time >= d_l + deb):
#		if (Nt[time - d_l] < k * It[time - d_l]):
#			R = 1
#		else:
#			R = k * It[time - d_l] / Nt[time - d_l]
#		newLt = Nt[time-d_l] * R * E * mu / 2.0
#	else:
#		newLt = 0.0
#
#	if (time >= d_p + d_l + deb):
#		emerge = Lt[time - d_p - d_l] * mu_MS * p_pup
#		newNt = emerge * pl
#		newNtOut = emerge * (1 - pl)
#	else:
#		newNt = 0.0
#		newNtOut = 0.0
#
#	return newNt, newLt, newNtOut
#
#
#def integrateCecidoInflo(psi):
#	lengthA = len(newItA)
#	lengthB = len(newItB)
#	lengthC = len(newItC)
#	length = max(lengthA, lengthB, lengthC)
#
#	damageA = np.zeros(length + 1)
#	damageB = np.zeros(length + 1)
#	damageC = np.zeros(length + 1)
#	ItA = np.zeros(length + 1)
#	ItB = np.zeros(length + 1)
#	ItC = np.zeros(length + 1)
#
#	LtA = np.zeros(length)
#	LtB = np.zeros(length)
#	LtC = np.zeros(length)
#	LtpA = np.zeros(length)
#	LtpB = np.zeros(length)
#	LtpC = np.zeros(length)
#	NtA = np.zeros(length)
#	NtB = np.zeros(length)
#	NtC = np.zeros(length)
#
#	ItA[length - lengthA:length] = newItA
#	ItB[length - lengthB:length] = newItB
#	ItC[length - lengthC:length] = newItC
#
#	firstA = np.where(ItA != 0)[0][0]
#	firstB = np.where(ItB != 0)[0][0]
#	firstC = np.where(ItC != 0)[0][0]
#
#	ItdA = np.diag(ItA)
#	ItdB = np.diag(ItB)
#	ItdC = np.diag(ItC)
#
#	for time in range(1, length):
#		newNtA, newLtA, newLtAOut = stepCecido2(time, firstA, NtA, LtA, ItA, mu_A)
#		newNtB, newLtB, newLtBOut = stepCecido2(time, firstB, NtB, LtB, ItB, mu_B)
#		newNtC, newLtC, newLtCOut = stepCecido2(time, firstC, NtC, LtC, ItC, mu_C)
#
#		damageA, ItdA = stepInflo(time, psi, damageA, newLtA, ItdA)
#		damageB, ItdB = stepInflo(time, psi, damageB, newLtB, ItdB)
#		damageC, ItdC = stepInflo(time, psi, damageC, newLtC, ItdC)
#
#		if (ItB[time] + ItC[time] != 0):
#			NAB, NAC = transfert(newNtA, ItB[time], ItC[time])
#		else:
#			NAB = 0
#			NAC = 0
#
#		if (ItA[time] + ItB[time] != 0):
#			NCA, NCB = transfert(newNtC, ItA[time], ItB[time])
#		else:
#			NCA = 0
#			NCB = 0
#
#		NtA[time] = gamma * ItA[time] + newNtA + NCA
#		NtB[time] = gamma * ItB[time] + newNtB + NCB + NAB
#		NtC[time] = gamma * ItC[time] + newNtC + NAC
#
#		LtA[time] = newLtA
#		LtB[time] = newLtB
#		LtC[time] = newLtC
#
#		LtpA[time] = newLtA * beta
#		LtpB[time] = newLtB * beta
#		LtpC[time] = newLtC * beta
#
#		ItA[time + 1] = np.sum(ItdA[:, time + 1])
#		ItB[time + 1] = np.sum(ItdB[:, time + 1])
#		ItC[time + 1] = np.sum(ItdC[:, time + 1])
#
#	return ItA[length - lengthA:length], LtA[length - lengthA:length], LtpA[length - lengthA:length], NtA[length - lengthA:length], ItB[length - lengthA:length], LtB[length - lengthA:length], LtpB[length - lengthA:length], NtB[length - lengthA:length], ItC[length - lengthA:length], LtC[length - lengthA:length], LtpC[length - lengthA:length], NtC[length - lengthA:length]
