#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Apr  3 15:48:46 2019

@author: bastien
"""

import numpy as np
import model1_calibration as mod1
from SALib.sample import saltelli
from SALib.analyze import sobol

problem = {'num_vars' : 5,
           'names' : ['gamma', 'pm', 'muer', 'mueh', 'k'],
           'bounds' : [[0.,0.1],
                       [0.,0.5],
                       [0.75,1.],
                       [0.,0.1],
                       [0.,0.2]]}
           
model_input = saltelli.sample(problem, 3)
model_output = np.array([mod1.mae(model_input[iter]) for iter in range(36)])
SA = sobol.analyze(problem, model_output)
print(SA)