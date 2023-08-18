# fonctions.py

import numpy as np 
import pandas as pd 
from scipy.optimize import minimize
from scipy.integrate import quad
import random
from tqdm import tqdm

seller = pd.read_csv('Data/df_vraissemblance1.csv')
seller['tau_birth'] = seller['tau_birth'] / seller['tau_birth'].mean()
seller['tau_contract'] = seller['tau_contract'] / seller['tau_contract'].mean()
seller['Ts'] = seller['Ts'] / seller['Ts'].mean()
seller['Td'] = seller['Td'] / seller['Td'].mean()
seller['Td_clone'] = seller['Td_clone'] / seller['Td_clone'].mean()
seller['Ts_clone'] = seller['Ts_clone'] / seller['Ts_clone'].mean()

X = ['type_libre','sexe_homme','idf','etranger','une_tete','dec1','dec2','dec3']
columns = ['type_libre','sexe_homme','idf','etranger','une_tete','dec1','dec2',
           'dec3','tau_birth','tau_contract','Td','Ts','Td_clone','Ts_clone',
           'tau_begin','tau_end']

# Cas le plus simple 
    
# Quelques fonctions utiles 
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

def LSeller_i(lambda_d, lambda_s, phi_d, phi_s,delta, i):
    # numérateur
    numerateur_d_exp =  np.exp(-(phi_d[i] * IDD(delta,lambda_d,seller['Td'][i],seller['Ts'][i])))
    numerateur_d = lambda_d * phi_d[i] * delta * numerateur_d_exp
    numerateur_s_exp = np.exp(-(phi_s[i] * IS(lambda_s,seller['Ts'][i])))
    numerateur_s = lambda_s * phi_s[i] * numerateur_s_exp
    numerateur = numerateur_d * numerateur_s
    # dénominateur 
    s = lambda_s * phi_s[i] 
    d = lambda_d * phi_d[i] 
    t_end = seller['tau_end'][i] - seller['tau_birth'][i]
    t_begin = seller['tau_begin'][i] - seller['tau_birth'][i]
    deno = d * (1 - delta) + s
    membre_1 = - (s * (np.exp( - d * (delta * t_end - (1 - delta) * t_begin )) - np.exp(- t_end * (d - s)))) / deno
    membre_2 = np.exp( - t_end * (d + s)) - np.exp( - d * t_begin - s * t_end)
    membre_3 = np.exp(- d * t_begin - s * t_end) - np.exp( - t_end * (d + s)) - s * (np.exp(- d * (delta * t_end - (1 - delta) * t_begin ) - s * t_begin ) - np.exp( - t_end * (s + d))) / deno    
    denominateur = membre_1 + membre_2 + membre_3
    resultat = numerateur / denominateur
    
    return resultat


def LClone_i(lambda_d, lambda_s, phi_d, phi_s,delta, i):
    # numérateur
    numerateur_d_exp =  np.exp(-(phi_d[i] * IDD(delta,lambda_d,seller['Td_clone'][i],seller['Ts_clone'][i])))
    numerateur_d = lambda_d * phi_d[i] * delta * numerateur_d_exp
    numerateur_s_exp = np.exp(-(phi_s[i] * IS(lambda_s,seller['Ts_clone'][i])))
    numerateur_s = lambda_s * phi_s[i] * numerateur_s_exp
    numerateur = numerateur_d * numerateur_s
    
    # dénominateur 
    s = lambda_s * phi_s[i] 
    d = lambda_d * phi_d[i] 
    t_end = seller['tau_end'][i] - seller['tau_birth'][i]
    t_begin = seller['tau_begin'][i] - seller['tau_birth'][i]
    deno = d * (1 - delta) + s
    membre_1 = - (s * (np.exp( - d * (delta * t_end - (1 - delta) * t_begin )) - np.exp(- t_end * (d - s)))) / deno
    membre_2 = np.exp( - t_end * (d + s)) - np.exp( - d * t_begin - s * t_end)
    membre_3 = np.exp(- d * t_begin - s * t_end) - np.exp( - t_end * (d + s)) - s * (np.exp(- d * (delta * t_end - (1 - delta) * t_begin ) - s * t_begin ) - np.exp( - t_end * (s + d))) / deno
    denominateur = membre_1 + membre_2 + membre_3
    
    resultat = numerateur / denominateur
    return resultat

def log_negatif(x):
    if x > 0: return np.log(x)
    elif x <= 0: return -np.log(-x)
vlog_negatif = np.vectorize(log_negatif)

# end 