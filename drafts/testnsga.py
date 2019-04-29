 
from pyOpt import Optimization, NSGA2

def objfunc(x):
    
    f = -x[0]*x[1]*x[2]
    h = -x[0]*x[2]
    g = [0.0]*2
    g[0] = x[0] + 2.*x[1] + 2.*x[2] - 72.0
    g[1] = -x[0] - 2.*x[1] - 2.*x[2]
    
    fail = 0
    return f,h,g, fail

# Instantiate Optimization Problem 
opt_prob = Optimization('TP37 Constrained Problem',objfunc)
opt_prob.addVar('x1','c',lower=0.0,upper=42.0,value=10.0)
opt_prob.addVar('x2','c',lower=0.0,upper=42.0,value=10.0)
opt_prob.addVar('x3','c',lower=0.0,upper=42.0,value=10.0)
opt_prob.addObj('f')
opt_prob.addObj('h')
opt_prob.addCon('g1','i')
opt_prob.addCon('g2','i')
print( opt_prob )

nsga2 = NSGA2()
nsga2.setOption('PrintOut',0)
nsga2(opt_prob)
print( opt_prob.solution(0))
