# Méthodes de Monte Carlo pour le calcul d'intégrales 

# Imports 
import matplotlib.pyplot as plt
import math as mat
import numpy as np
import pandas as pd
from numpy import polynomial
from numpy.random import *
from scipy.misc import *
from scipy.stats import kstest
from scipy.special import factorial
from scipy import optimize
from scipy import stats

# Dataset 
seller = pd.read_csv('Data/dataset_vraissemblance.csv')
X = ['type_libre','sexe_homme','sexe_femme','idf','etranger','une_tete','dec1','dec2','dec3']
columns = ['type_libre','sexe_homme','sexe_femme','idf','etranger','une_tete','dec1','dec2',
           'dec3','tau_birth','tau_contract','Td','Ts','Td_clone','Ts_clone','tau_begin','tau_end']
seller['tau_birth'] = seller['tau_birth'] / seller['tau_birth'].mean()
seller['tau_contract'] = seller['tau_contract'] / seller['tau_contract'].mean()
seller['Ts'] = seller['Ts'] / seller['Ts'].mean()
seller['Td'] = seller['Td'] / seller['Td'].mean()
seller['Td_clone'] = seller['Td_clone'] / seller['Td_clone'].mean()
seller['Ts_clone'] = seller['Ts_clone'] / seller['Ts_clone'].mean()

# Fonctions que nous utiliserons 
def phiD(beta_d): # beta_d est un vecteur de taille 9
    x_i = seller[X].values 
    phi = np.exp(np.dot(x_i,beta_d))
    return phi 

def phiS(beta_s): # beta_d est un vecteur de taille 9
    x_i = seller[X].values 
    phi = np.exp(np.dot(x_i,beta_s))
    return phi 

def IDD(delta,lambda_d,t_1,t_2): 
    if t_1 <= t_2:
        I = lambda_d * t_1
    else: 
        I = lambda_d * (t_2 + delta * (t_1 - t_2))
    return I 

def ID(lambda_d,t):
    I = lambda_d * t
    return I 

def IS(lambda_s,t):
    I = lambda_s * t
    return I 

# Simulation de la loi exponentielle 
def simul_expo(lamb, n):    #lamb = paramètre ; n = nombre de simulations
    U = rand(n)
    return - np.log(1-U) / lamb

# Monte Carlo sur R+
def MC1_Rplus(f,n): 
  return np.mean(f(simul_expo(1,n)))    #estimateur de Monte Carlo

# Vérifions que cela approxime correctement les intégrales 
# Pour cela, nous avons déjà calculé l'intégrale de vraissemblance_code.py 

# Valeurs 
parameters = np.random.uniform(0, 1, size=21)
parameters = list(parameters)
lambda_d = parameters[0]
lambda_s = parameters[1]
beta_d = list(parameters[2:11])
beta_s = list(parameters[11:20])
delta = parameters[-1]
i = 12
phi_d = phiD(beta_d)
phi_s = phiS(beta_s)

# Calcul à la main 
s = lambda_s * phi_s[i] 
d = lambda_d * phi_d[i] 
t_end = (seller['tau_end'][i] - seller['tau_birth'][i]) * 10 ** -6
t_begin = (seller['tau_begin'][i] - seller['tau_birth'][i]) * 10 ** -6
deno = d * (1 - delta) + s
membre_1 = - (s * (np.exp( - d * (delta * t_end - (1 - delta) * t_begin )) - np.exp(- t_end * (d - s)))) / deno
membre_2 = np.exp( - t_end * (d + s)) - np.exp( - d * t_begin - s * t_end)
membre_3 = np.exp(- d * t_begin - s * t_end) - np.exp( - t_end * (d + s)) - s * (np.exp(- d * (delta * t_end - (1 - delta) * t_begin ) - s * t_begin ) - np.exp( - t_end * (s + d))) / deno
denominateur = membre_1 + membre_2 + membre_3

# Calcul numérique
def g(t):
    deno1 = np.exp(- phi_d[i] * IDD(delta,lambda_d, t_begin, t)) - np.exp(- phi_d[i] * IDD(delta, lambda_d, t_end, t))
    #print(np.exp(- phi_d[i] * IDD(delta, lambda_d, t_end, t)))
    deno2 = lambda_s * phi_s[i] * np.exp(- phi_s[i] * IS(lambda_s, t))
    return deno1 * deno2 / np.exp(- t)

print("Valeur numérique : ", denominateur)
print("Valeur approchée de l'intégrale: ", MC1_Rplus(g, 10**4))