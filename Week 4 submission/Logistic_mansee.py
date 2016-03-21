
# coding: utf-8

# In[ ]:




# In[ ]:




# In[2]:


import pandas as pd
import statsmodels.api as sm
import statsmodels.formula.api as smf
import numpy as np
from scipy import linalg

def myqr(A):
    n, m = A.shape
    R = A.copy()
    Q = np.eye(n)

    for k in range(m-1):
        x = np.zeros((n, 1))
        x[k:, 0] = R[k:, k]
        v = x
        v[k] = x[k] + np.sign(x[k,0]) * np.linalg.norm(x)

        s = np.linalg.norm(v)
        if s != 0:
            u = v / s
            R -= 2 * np.dot(u, np.dot(u.T, R))
            Q -= 2 * np.dot(u, np.dot(u.T, Q))
    Q = Q.T
    return Q, R

def mySweep(B, m):
    A = np.copy(B)
    n, c = A.shape
    for k in range(m):
        for i in range(n):
            for j in range(n):
                if i != k and j != k:
                    A[i,j] = A[i,j]- A[i,k]*A[k,j]/A[k,k]

        for i in range(n):
            if i != k:
                A[i,k] = A[i,k]/A[k,k]

        for j in range(n):
            if j != k:
                A[k,j] = A[k,j]/A[k,k]

        A[k,k] = -1/A[k,k]
    return A




def mylm(X,Y):

    value_ii= np.dot(X.T, X)  
    value_ij= np.dot(X.T, Y)
    value_ji= np.dot(Y.T, X)
    value_jj= np.dot(Y.T, Y)
    
    rb_for_sweep_one=np.vstack((value_ii,value_ji))
    rb_for_sweep_two=np.vstack((value_ij,value_jj))
    input_forsweep= np.hstack((rb_for_sweep_one,rb_for_sweep_two))

    for i in range(0,input_forsweep.shape[1]):
        mean_value = np.mean(input_forsweep[:,i]);
        std_value = np.std(input_forsweep[:,i])
        input_forsweep[:,i] = (input_forsweep[:,i] - mean_value)/std_value
        print input_forsweep[:,i]



    
    ans=mySweep(input_forsweep,8)
    
    beta_hat=ans[:,9]
    
    return beta_hat

def mylogistic(_x, _y):
    x = _x.copy()
    y = _y.copy()
    r, c = x.shape

    beta = np.zeros((c, 1))
    epsilon = 1e-6

    while True:
        eta = np.dot(x, beta)
        pr = exp_it(eta)
        w = pr * (1 - pr)
        z = eta + (y - pr) / w
        sw = np.sqrt(w)
        mw = np.repeat(sw, c, axis=1)

        x_work = mw * x
        y_work = sw * z
        
       
        beta_new, _, _, _ = np.linalg.lstsq(x_work, y_work)
        err = np.sum(np.abs(beta_new - beta))
        beta = beta_new
        if err < epsilon:
            break
    
    model = smf.glm('admit ~ gre + gpa + rank', df, family=sm.families.Binomial()).fit()
    print model.summary()

    return model


def exp_it(_x):
    x = _x.copy()
    y = 1 / (1 + np.exp(-x))
    return y


if __name__ == '__main__':
    n = 400
    p = 6
    df = pd.read_csv("http://www.ats.ucla.edu/stat/data/binary.csv")
    X = np.random.normal(0, 1, (n, p))
    #beta = np.arange(p) + 1
    beta = np.ones((p, 1))
    print beta

    Y = np.random.uniform(0, 1, (n, 1)) < exp_it(np.dot(X, beta)).reshape((n, 1))

    logistic_beta = mylogistic(X, Y)
    print logistic_beta


# In[ ]:



