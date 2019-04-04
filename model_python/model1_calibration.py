#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Mar 28 15:29:30 2019

@author: bastien
"""
import numpy as np
import model1 as mod
from scipy.optimize import basinhopping, differential_evolution

ER, B, EH = range(3)
larves_obs = mod.larvess


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
x0 = [0.82, 1, 0.33, 0.36, 0.13]
## RMSE
def rmse(params):
	larves_est = mod.dynamiques(*params, mod.inflos)
	N = len(larves_est[ER])
    
	ans = np.sqrt(sum((larves_est[ER] - larves_obs[ER])**2))/N + np.sqrt(sum((larves_est[B] - larves_obs[B])**2))/N + np.sqrt(sum((larves_est[EH] - larves_obs[EH])**2))/N

	return ans

test_rmse = basinhopping(rmse, x0, niter = 10000, accept_test = mybounds)
print(test_rmse)

## MSE
def mse(params):
	larves_est = mod.dynamiques(*params, mod.inflos)
	N = len(larves_est[ER])
    
	ans = sum((larves_est[ER] - larves_obs[ER])**2)/N + sum((larves_est[B] - larves_obs[B])**2)/N + sum((larves_est[EH] - larves_obs[EH])**2)/N

	return ans

test_mse = basinhopping(mse, x0, niter = 10000, accept_test = mybounds)
print(test_mse)

## MAE
def mae(params):
	larves_est = mod.dynamiques(*params, mod.inflos)
	N = len(larves_est[ER])
    
	ans = sum(abs(larves_est[ER] - larves_obs[ER]))/N + sum(abs(larves_est[B] - larves_obs[B]))/N + sum(abs(larves_est[EH] - larves_obs[EH]))/N

	return ans

test_mae = basinhopping(mae, x0, niter = 10000, accept_test = mybounds)
print(test_mae)


### DIFFERENTIAL EVOLUTION
bounds2 = [(0.0,1000.0),(0.0,1.0),(0.0,1.0),(0.0,1.0),(0.0,1000.0)]

test_rmse_de = differential_evolution(rmse, bounds2)
print(test_rmse_de)

test_mse_de = differential_evolution(mse, bounds2)
print(test_mse_de)

test_mae_de = differential_evolution(mae, bounds2)
print(test_mae_de)
