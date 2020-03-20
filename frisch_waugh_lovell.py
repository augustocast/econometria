import numpy as np

#We set seed for get fixed random numbers
np.random.seed(1)

#N is the large of the vector and K the number of explicative variables, additionaly
# we have n-dimentional intercept vector of ones. Ideally, we should check if
#x matrix columns are linearly indepent. 
N = 100
K = 2
intercept = np.array(np.ones((N, 1)))
y = np.random.rand(N, 1)
x = np.random.rand(N, K)
xin = np.append(intercept, x, axis=1)
xint = xin.T 

def ols (xin, xint, y):
    '''Returns b1, given  y and x'
    The specificated model is y = xb + u
    if x'x is invertible, te solution for beta_ols(Kx1) is [(x'x)^-1]x'y
    '''
    beta_ols = np.dot(np.dot(np.linalg.inv(np.dot(xint, xin)), xint), y)
    return beta_ols[1,]

b1 = ols(xin, xint, y)

def two_stages(y, x):
    '''
    '''
    #Aca defini a px en vez de a px2, hace falta extraer el vector, lo mismo para xstar
    Px2= np.dot(xin, np.dot(np.linalg.inv(np.dot(xint, xin)), xint))
    Mx2= np.identity(N) - Px2
    y_star = np.dot(Mx2, y)
    x_star = np.dot(Mx2, xin)
     
    