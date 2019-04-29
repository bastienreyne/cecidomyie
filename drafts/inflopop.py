import numpy as np

v = [142, 181, 219, 258, 297, 336, 374, 413, 428, 443, 458, 474, 489, 504, 519, 542, 564, 587, 610, 633, 655, 678, 655, 632, 610, 587, 564, 542, 519, 496, 770, 1044, 1033, 1023, 1012, 1002, 991, 947, 903, 1154, 1404, 1655, 1906, 2199, 2492, 2785, 2921, 3056, 3192, 3327, 3463, 4044, 4626, 4901, 5176, 5450, 5725, 6000, 5547, 5094, 5143, 5192, 5242, 5291, 5340, 4886, 4431, 4589, 4746, 4904, 5062, 4563, 4063, 3564, 3291, 3018, 2745, 2472, 2170, 1869, 1567]

def estimate(J,alpha):
    I = list(J)
    for i, ival in enumerate(J) :
        for j in xrange(i-1,max(0,i-50),-1):
            oi = J[j]
            for d in xrange(j+1,i+1):
                oi *= (1-alpha[d])
            I[i] += oi
    return I

def estimate_death(J,alpha):
    I  = estimate(J,alpha)
    return [(I[i-1] if i >0 else 0) + J[i] -vi for i,vi in enumerate(I)]

def estimate_death2(J,alpha):
    D = [0 for j in J]
    for i, ival in enumerate(J) :
        for j in xrange(i-1,max(0,i-50),-1):
            oi = J[j]
            for d in xrange(j+1,i):
                oi *= (1-alpha[d])
            D[i] += oi * alpha[i]
    return D


def get_args(args):
    alpha = args[len(v):]
    J = args[:len(v)]
    return J, alpha

def opt(args):
    J, alpha = get_args(args)
    I = estimate(J,alpha)
    I = np.array(I)
    dI = pow(I - v,2)
    return sum(dI) + sum(alpha)

J = [1 for i in v]
alpha = [.1 for i in v]

from scipy.optimize import *

#print estimate(J,alpha)


res = minimize(opt, J+alpha, bounds = [(0,np.inf) for i in v]+[(0,1) for i in v])
print list(res.x)

optarg = list(res.x)
print opt(optarg)

def plot_estimate(args):
    import matplotlib.pyplot as plt
    J, alpha = get_args(args)
    I = estimate(J,alpha)
    D = estimate_death(J,alpha)
    #D2 = estimate_death2(J,alpha)
    fig, ax = plt.subplots()
    ax.plot(v,label='observed', linewidth=3)
    ax.plot(I,label='simulated')
    ax.plot(J,label='sim. burst')
    #ax.plot(D2,label='sim. dead', color='b', linewidth=3)
    ax.plot(D,label='sim. dead', color='r')
    ax.set_ylabel('population')
    ax.legend()
    ax2 = ax.twinx()
    ax2.plot(np.array(alpha),label='sim. death rate', color='black')
    ax2.set_ylabel('proba', color='black')
    ax2.set_ylim(0,1)
    plt.legend()
    plt.show()

plot_estimate(optarg)
