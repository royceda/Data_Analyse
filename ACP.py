#we have a matrix Z (n x p) with n people and p character

import numpy as np
import matplotlib as mp
from numpy import linalg as la

"Create the scalars which enable to centerize Z "
def Center(Z):
    n = len(Z);
    A = Z;
    p = 1.0/n;
    X = np.zeros(n);
    for j in range(0, n):
        sum = 0.0;
        for i in range(0, n):
            sum += p * Z[i][j];
        X[j] = sum;
    return X;

"compute variance"
def Variance(Z):
    n = len(Z);
    X = Center(Z);
    S = np.zeros(n);
    for j in range (0, n):
        sum = 0.0;
        for i in range(0,n):
            sum += (Z[i][j] - X[j])
        S[j] = np.sqrt(sum);
    return S


def Centerize(Z):
    A = Z;
    n = len(Z);
    X = Center(Z);

    for j in range (0, n):
        for i in range (0, n):
            A[i][j] -= X[j]
    return A


def Reduce(Z):
    A = Z;
    return A;


def ACP(Z):
    tmp = Centerize(Z);
    tmp = Reduce(tmp);

    size = np.shape(tmp);

    #SVD decomposition
    U, A, V = np.linalg.svd(tmp);

    S = np.zeros(size[O], size[1])
    mini = min(size[0], size[1]);
    S[:mini, :mini ]= np.diag(A);

    #Factorial coord of peoples (Scores)
    Xi = np.dot(U, S);

    #Factorial coord of variables (loadings)
    Phi = np.dot(V, np.transpose(A));

    return Xi, Phi;







#Test

Z = [[1,2,3,4,5],[1,2,3,4,5],[6,7,8,9,10],[2,4,3,5,8]];
print Z;
print "Center(Z): "
print Center(Z);
print "Variance(Z): "
print Variance(Z);
print "Centerize(Z): "
print Centerize(Z);


ACP(Z)
