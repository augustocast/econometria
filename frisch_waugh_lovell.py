import numpy as np

#We set seed for get fixed random numbers
np.random.seed(42)

#N is the large of the vector and K the number of explicative variables, additionaly
# we have n-dimentional intercept vector of ones. Ideally, we should check if
#x matrix columns are linearly indepent. 
#Check what's happens with k =! 2
N = 10000
K = 2

#If we allow other sources, the script must be able tocheck sizes and data types
y = np.random.randint(-100, high=100, size=(N, 1))
x = np.random.randint(-100, high=100, size=(N, K))


def add_intercept (x):
    '''Appends a column of ones as first column of matrix X '''
    intercept = np.array(np.ones((N, 1)))
    xin = np.append(intercept, x, axis=1)
    return xin


def ortogonal_matrix(uncorr_i):
    '''Returns the Mxi matrix, given variable index'''
    uncorr = x[:, uncorr_i-1].reshape(-1,1)
    uncorr_t = uncorr.T
    pxi = np.dot(uncorr, np.dot(np.linalg.inv(np.dot(uncorr_t, uncorr)), uncorr_t))
    mxi = np.identity(N) - pxi
    return mxi

def var_star(uncorr_i, var_star_i):
    '''computes orthogonal projection of var_star_i onto uncorr_i
    '''
    var_star = y if var_star_i == 'y' else x[:, var_star_i-1].reshape(-1,1)
    mxi = ortogonal_matrix(uncorr_i)
    return np.dot(mxi, var_star)

def b1_ols (x, y):
    '''Returns b1ols, given  y and x
    The specificated model is y = xb + u1 with intercept 
    if x'x is invertible, te solution for beta_ols(Kx1) is [(x'x)^-1]x'y
    '''
    xin = add_intercept(x)
    xint = xin.T
    beta_ols = np.dot(np.dot(np.linalg.inv(np.dot(xint, xin)), xint), y)
    return beta_ols[1,]

def two_stages(uncorr_i, interest_b):
    '''
    Returns b1ts, given y and x.
    The specificated model is y* = x1*b1 + u2
    Where y* is the part of y that it's uncorrelated with x2. Analogously, 
    x1* is the part of y that has 0 correlation with x2.
    uncorr_i = control variable index
    interest_b = interest variable index
    '''
    y_star = var_star(uncorr_i, 'y')
    x_star = var_star(uncorr_i, interest_b) 
    b_two_stages = b1_ols(x_star, y_star)
    return b_two_stages
   
b1 = b1_ols(x, y)    
b1_two_stages = two_stages(2,1)


test = b1/b1_two_stages
print(test)
    
    
    
    
    
    
    
 