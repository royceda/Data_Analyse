#we have a matrix Z (n x p) with n people and p character

import numpy as np
import matplotlib as mp


def centerize(Z):
    n = len(Z);
    A = Z;
    p = 1.0/n;

    for i in range(0, n):
        for j in range(0, n):
            X[i] = p * Z[i][j];
