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

def lambdaD(t): 
        return np.exp(alpha_d * t)
    
def lambdaS(t):
    return alpha_s * (t ** (alpha_s - 1))

# Après calculs à la main, nous obtenons des valeurs pour les intégrales, les voici :
def IDD(t1,t2):
    if t1 > t2: I = ( (1 - delta) * np.exp(alpha_d * t2) + delta * np.exp(alpha_d * t1) - 1 ) / alpha_d
    else: I = ( np.exp(alpha_d * t1 ) - 1 ) / alpha_d
    return I

def ID(t):
    return ( np.exp(alpha_d * t) - 1 ) / alpha_d

def IS(t):
    return t ** alpha_s

def phiD(beta_d): # beta_d est un vecteur de taille 8
    x_i = seller[X].values 
    phi = np.exp(np.dot(x_i,beta_d))
    return phi 

def phiS(beta_s): # beta_d est un vecteur de taille 8
    x_i = seller[X].values 
    phi = np.exp(np.dot(x_i,beta_s))
    return phi 

phi_d = phiD(beta_d)
phi_s = phiS(beta_s)

def L_seller(i):
    numerateur1 = (1 + sigma_d2 * phi_d[i] * IDD(seller['Td'][i],seller['Ts'][i])) ** (- sigma_d2 - 1) 
    numerateur2 = delta * phi_d[i] * lambdaD(seller['Td'][i]) 
    numerateur3 = (1 + sigma_s2 * phi_s[i] * IS(seller['Ts'][i])) ** (- sigma_s2 - 1)
    numerateur4 = phi_s[i] * lambdaS(seller['Ts'][i])
    numerateur = numerateur1 * numerateur2 * numerateur3 * numerateur4
    
    def int_denominateur(t):
        deno1 = 1 - (1 + sigma_d2 * phi_d[i] * IDD((seller['tau_end'][i] - seller['tau_birth'][i]), t)) ** ( - sigma_d2 - 1)
        deno2 = phi_s[i] * lambdaS(t) * (1 + sigma_s2 * phi_s[i] * IS(t)) ** ( - sigma_d2 - 1)
        return deno1 * deno2
    denominateur = quad(int_denominateur, 0, 10000)[0]
    return numerateur / denominateur

def L_clone(i):
    numerateur1 = (1 + sigma_d2 * phi_d[i] * ID(seller['Td_clone'][i])) ** (- sigma_d2 - 1) 
    numerateur2 = delta * phi_d[i] * lambdaD(seller['Td'][i]) 
    numerateur3 = (1 + sigma_s2 * phi_s[i] * IS(seller['Td_clone'][i])) ** (- sigma_s2 - 1)
    numerateur = numerateur1 * numerateur2 * numerateur3 

    def int_denominateur(t):
        deno1 = 1 - (1 + sigma_d2 * phi_d[i] * IDD((seller['tau_end'][i] - seller['tau_birth'][i]), t)) ** ( - sigma_d2 - 1)
        deno2 = phi_s[i] * lambdaS(t) * (1 + sigma_s2 * phi_s[i] * IS(t)) ** ( - sigma_d2 - 1)
        return deno1 * deno2
    denominateur = quad(int_denominateur, 0, 10000)[0]

    return numerateur / denominateur

def log_negatif(x):
    if x > 0: return np.log(x)
    elif x <= 0: return -np.log(-x)
vlog_negatif = np.vectorize(log_negatif)

