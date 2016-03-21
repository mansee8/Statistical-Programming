
# coding: utf-8

# In[25]:

import numpy as np

def mySweep(X, m):
    A = np.copy(X)
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

def mylm_sweep(X,Y):
    n = X.shape[0]
    X1  = np.hstack((np.ones((n, 1)), X))

    value_ii = np.dot(np.transpose(X1),X1)
    value_ij = np.dot(np.transpose(X1),Y)
    value_ji = np.dot(np.transpose(Y),X1)
    value_jj = np.dot(np.transpose(Y), Y)
    rb_for_sweep_one=np.vstack((value_ii,value_ji))
    rb_for_sweep_two=np.vstack(( value_ij,value_jj))
    input_forsweep= np.hstack((rb_for_sweep_one,rb_for_sweep_two))

    beta = mySweep(input_forsweep,8)
    beta_hat = beta[0:8,9]
    return beta_hat


def mylm_qr(X, Y):
    n = X.shape[0]
    p = X.shape[1]
    X1  = np.hstack((np.ones((n, 1)), X, Y))
    Q, R = myqr(X1)
    R1 = R[0:p, 0:p]
    Y1 = R[0:p, p+1]
    #defining mysolve function to get inverse which is (RT*R)-1 * RT*Y
    
    ans_inv = -np.dot(np.dot(mySweep(np.dot(np.transpose(R1),R1), p), np.transpose(R1)), Y1)

    return ans_inv



# read from text for the state.x77 data
input_data = np.loadtxt(open('state_x77.csv','rb'), delimiter=',', skiprows=1, usecols=range(1,10))
X = np.hstack((input_data[:,0:3], input_data[:,4:10]))
Y = data[:,3:4]
print "MY_lm_sweep results"
print(mylm_sweep(X, Y))
print "My_lm_QR results"
print(mylm_qr(X, Y))


# In[ ]:




# In[ ]:



