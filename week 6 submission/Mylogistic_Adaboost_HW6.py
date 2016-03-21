
# coding: utf-8

# In[31]:

import numpy as np
import pandas as pd
import math

def accuracy(p, y):
    """
    Calculate the accuracy of predictions against truth labels.

        Accuracy = # of correct predictions / # of data.

    Args:
        p: predictions of shape (n, 1)
        y: true labels of shape (n, 1)

    Returns:
        accuracy: The ratio of correct predictions to the size of data.
    """
    return np.mean((p > 0.5) == (y == 1))

def myAdaboost(X, Y, Xtest, Ytest, m=100, num_iterations=4000,
                  learning_rate=1e-1, lamda=0.0001):
    n, p = X.shape
    
    n = X.shape[0] 
    p = X.shape[1]+1
    #ntest = dim(Xtest)[1]
    ntest, ptest = Xtest.shape
    X1 = np.hstack((np.ones((n,1)),X))
    X1test = np.hstack((np.ones((ntest,1)),Xtest))
    th = 0.8
    X1 = X1 > th
    X1test = X1test > th
    
    X1 = 2*X1-1
    Y = 2*Y - 1
    X1test = 2*X1test - 1
    Ytest = 2*Ytest - 1
    beta = np.zeros((p,1))
    acc_train = None
    acc_test = None

    for it in xrange(int(num_iterations)):
        S = np.dot(X1,beta)
        Stest = np.dot(X1test,beta)
        W = np.exp((np.multiply(-Y,S)))
        a=np.dot(np.ones((1,n)),np.multiply(np.multiply(W,Y),X1))/n
        e=(1-a)/2
        j = np.argmin(e)
        
        beta[j]=beta[j]+ math.log((1-e[j])/e[j])/2
        
        acc_train = np.mean(np.multiply(np.sign(S),Y))
        acc_test = np.mean(np.multiply(np.sign(Stest),Ytest))
    return beta, acc_train, acc_test


# load data
def load_digits(subset=None, normalize=True):
    """
    Load digits and labels from digits.csv.

    Args:
        subset: A subset of digit from 0 to 9 to return.
                If not specified, all digits will be returned.
        normalize: Whether to normalize data values to between 0 and 1.

    Returns:
        digits: Digits data matrix of the subset specified.
                The shape is (n, p), where
                    n is the number of examples,
                    p is the dimension of features.
        labels: Labels of the digits in an (n, ) array.
                Each of label[i] is the label for data[i, :]
    """
    # load digits.csv, adopted from sklearn.
    import pandas as pd
    df = pd.read_csv('Downloads/digits.csv')

    # only keep the numbers we want.
    if subset is not None:
        df = df[df.iloc[:,-1].isin(subset)]

    # convert to numpy arrays.
    digits = df.iloc[:,:-1].values.astype('float')
    labels = df.iloc[:,-1].values.astype('int')

    # Normalize digit values to 0 and 1.
    if normalize:
        digits -= digits.min()
        digits /= digits.max()

    # Change the labels to 0 and 1.
    for i in xrange(len(subset)):
        labels[labels == subset[i]] = i

    labels = labels.reshape((labels.shape[0], 1))
    return digits, labels


def display_samples(digits, labels, n_samples=5):
    """
    Display random samples from the training set for each label.
    """
    distinct_label = np.unique(labels)

    fig_rows = len(distinct_label)
    fig_cols = n_samples
    fig_num = 1

    fig = plt.figure(1)
    fig.suptitle('Random samples of training data')
    for label in distinct_label:
        # random choose samples to display
        choice = np.random.choice(np.ix_(labels == label)[0], n_samples)
        for idx in choice:
            ax = fig.add_subplot(fig_rows, fig_cols, fig_num)
            fig.subplots_adjust(wspace=0, hspace=0)
            ax.set_title(labels[idx])
            ax.imshow(digits[idx].reshape(8,8), cmap=plt.cm.gray_r)
            ax.axis('off')
            fig_num += 1
    plt.show()


def split_samples(digits, labels):
    """Split the data into a training set (70%) and a testing set (30%)."""
    num_samples = digits.shape[0]
    num_training = round(num_samples * 0.7)
    indices = np.random.permutation(num_samples)
    training_idx, testing_idx = indices[:num_training], indices[num_training:]
    return (digits[training_idx], labels[training_idx],
            digits[testing_idx], labels[testing_idx])


#====================================
# Load digits and labels.
digits, labels = load_digits(subset=[3, 5], normalize=True)
training_digits, training_labels, testing_digits, testing_labels = split_samples(digits, labels)
print '# training', training_digits.shape[0]
print '# testing', testing_digits.shape[0]

# Train a net and display training accuracy.
beta, train_accuracy, testing_accuracy = myAdaboost(training_digits, training_labels, testing_digits, testing_labels)

print 'Beta Values'
print beta

print 'Accuracy on training data:'
print train_accuracy

print 'Accuracy on testing data:'
print testing_accuracy


# In[ ]:



