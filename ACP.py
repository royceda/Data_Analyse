#we have a matrix Z (n x p) with n people and p character

import numpy as np
import matplotlib as mp


"Create the scalars which enable to centerize Z "
def Center(Z):
    n = len(Z);
    A = Z;
    p = 1.0/n;
    for j in range(0, n):
        sum = 0.0;
        for i in range(0, n):
            sum += p * Z[i][j];
        X[j] = sum;
    return X;

"compute variance"
def Variance(Z):
    n = len(Z);
    X = center(Z);

    for j in range (0, n):
        sum = 0.0;
        for i in range(0,n):
            sum += (Z[i][j] - centerize(Z))
        S[j] = np.sqrt(sum);
    return S


def Centerize(Z):
    A = Z;
    n = len(Z);
    X = center(Z);

    for j in range (0, n):
        for i in range (0, n):
            A[i][j] -= X[j]
    return A






#Test

Z = [[1,2,3,4,5],[1,2,3,4,5],[6,7,8,9,10],[2,4,3,5,8]];
print Z;
print "Center(Z): " center(Z);
print "Variance(Z): " Variance(Z);
print "Centerize(Z): " Centerize(Z);
