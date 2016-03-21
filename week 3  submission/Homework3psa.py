
# coding: utf-8

# In[12]:

import numpy as np

def gram_schmidt_qr(Aq):
    n,c = Aq.shape
    Q = np.zeros((c,c))
    R = np.zeros((c,c))

    for j in range(c):
        V = Aq[:,j]
        if (j > 0):
            for i in range(j):
                R[i,j] = np.dot(np.transpose(Q[:,i]),Aq[:,j])
                V = V - np.multiply(R[i,j],Q[:,i])

        R[j,j] = np.linalg.norm(V)
        Q[:,j] = V/R[j,j]

    return Q,R

def my_pca(A):
    iterations = 1000;
    n,c = A.shape
    V = np.random.randn(n,n)

    for i in range(iterations):
        q_i,r_i = gram_schmidt_qr(A)
        V = np.dot(A,q_i)
        
    Bq,Br = gram_schmidt_qr(V)
    diagonal_Br = np.diagonal(Br)
    return Bq, diagonal_Br

input_data = np.loadtxt(open('iris1.csv','rb'), delimiter=',', usecols=range(0,4))
n,c = input_data.shape

for i in range(c):
    col_mean = np.mean(input_data[:,i])
    input_data[:,i] = input_data[:,i] - col_mean

A = np.dot(np.transpose(input_data),input_data)/n

eigen_vectors, eigen_values = my_pca(A)

standard_deviation = np.sqrt(eigen_values)
print("Standard deviation")
print(standard_deviation)
proportion_of_variance = eigen_values/np.sum(eigen_values)
print("Proportion of Variance")
print(proportion_of_variance)


# In[ ]:




# In[ ]:



