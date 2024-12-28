# -*- coding: utf-8 -*-
import numpy as np
import matplotlib.pyplot as plt

global tau
global mu_0

X=np.zeros(91)
Y=np.zeros(91)
count=1

X[0]=0.0
Y[0]=0.0
X[1]=1.0
X[2]=0.0

tau=1.2
Req=2.3148

def Simpson(func,low,hi,N):
    h=(hi-low)/N
    intgrl=func(low)+func(hi)
    for i in range (1,N-1):
        if (i%2==0):
            intgrl+=2*func(low+i*h)
        else:
            intgrl+=4*func(low+i*h)
    intgrl*=h/3.0
    return(intgrl)

def func(x):
    return((x**0.5)/(np.exp((x-mu_0)/tau))-1.0)


while(tau<=10.0):
    count+=1
    X[count]=tau
    mu_0=-10.0
    for i in range (1,1000):
        Intgrl=Simpson(func,0.0,10.0,10000)
        print(Intgrl)
        if(Intgrl>2.3148):
            Y[count]=mu_0
            break
        else:
            mu_0+=25.0/1000.0
    tau+=0.1
    
plt.plot(X,Y)