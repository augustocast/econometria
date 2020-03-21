import numpy as np

#We set seed for get fixed random numbers
np.random.seed(100)

#N is the large of the vector and K the number of explicative variables, additionaly
# we have n-dimentional intercept vector of ones. Ideally, we should check if
#x matrix columns are linearly indepent. 
N = 100
K = 2

y = np.random.randint(-100, high=100, size=(N, 1))
x = np.random.randint(-100, high=100, size=(N, K))

def add_intercept (x, N):
    intercept = np.array(np.ones((N, 1)))
    xin = np.append(intercept, x, axis=1)
    return xin

def b1_ols (x, y):
    '''Returns b1ols, given  y and x
    The specificated model is y = xb + u1 with intercept 
    if x'x is invertible, te solution for beta_ols(Kx1) is [(x'x)^-1]x'y
    '''
    xin = add_intercept(x, N)
    xint = xin.T
    beta_ols = np.dot(np.dot(np.linalg.inv(np.dot(xint, xin)), xint), y)
    return beta_ols[1,]

b1_ols = b1_ols(x, y)

   

def two_stages(uncorr_i, interest_b):
    '''
    Returns b1ts, given y and x.
    The specificated model is y* = x1*b1 + u2
    Where y* is the part of y that it's uncorrelated with x2. Analogously, 
    x1* is the part of y that has 0 correlation with x2.
    For estimating this isolated effect, we compute the ortogonal projection of 
    each variable and x2.
    '''
    def var_star(uncorr_i, var_star_i):
        var_star_i= 1
        var_star = y if var_star_i == 'y' else x[:, var_star_i-1].reshape(-1,1)
        uncorr = x[:, uncorr_i-1].reshape(-1,1)
        uncorr_t = uncorr.T
        px2 = np.dot(uncorr, np.dot(np.linalg.inv(np.dot(uncorr_t, uncorr)), uncorr_t))
        mx2 = np.identity(N) - px2
        return np.dot(mx2, var_star)
    y_star = var_star(uncorr_i, 'y')
    interest_var = x[:, interest_b-1].reshape(-1,1)
    x_star = var_star(uncorr_i, interest_var)
    b1_two_stages = b1_ols(x_star, y_star)
    return b1_two_stages
   
b1_two_stages = two_stages(1,2)

def two_stages(uncorr_i, interest_b):
    '''
    Returns b1ts, given y and x.
    The specificated model is y* = x1*b1 + u2
    Where y* is the part of y that it's uncorrelated with x2. Analogously, 
    x1* is the part of y that has 0 correlation with x2.
    For estimating this isolated effect, we compute the ortogonal projection of 
    each variable and x2.
    '''
    def var_star(uncorr_i, var_star_i):
        var_star_i= 1
        var_star = y if var_star_i == 'y' else x[:, var_star_i-1].reshape(-1,1)
        uncorr = x[:, uncorr_i-1].reshape(-1,1)
        uncorr_t = uncorr.T
        px2 = np.dot(uncorr, np.dot(np.linalg.inv(np.dot(uncorr_t, uncorr)), uncorr_t))
        mx2 = np.identity(N) - px2
        return np.dot(mx2, var_star)
    y_star = var_star(uncorr_i, 'y')
    interest_var = x[:, interest_b-1].reshape(-1,1)
    x_star = var_star(uncorr_i, interest_var)
    b1_two_stages = b1_ols(x_star, y_star)
    return b1_two_stages
   
b1_two_stages = two_stages(1,2)
    
    

