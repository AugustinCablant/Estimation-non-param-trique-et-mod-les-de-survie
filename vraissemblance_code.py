import numpy as np 
import pandas as pd 
from scipy.optimize import minimize
from scipy.integrate import quad
import random
from tqdm import tqdm

seller = pd.read_csv('Data/df_vraissemblance1.csv')
X = ['type_libre','sexe_homme','idf','etranger','une_tete','dec1','dec2','dec3']
columns = ['type_libre','sexe_homme','idf','etranger','une_tete','dec1','dec2','dec3','tau_birth','tau_contract','Td','Ts','Td_clone','Ts_clone','tau_begin','tau_end']

### Quelques fonctions utiles ###
def phiD(beta_d): # beta_d est un vecteur de taille 8
    x_i = seller[X].values 
    phi = np.exp(np.dot(x_i,beta_d))
    return phi

def phiS(beta_s): # beta_d est un vecteur de taille 8
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
###


### Fonction de vraissemblance ### 
def likelihood(parameters):

    # Paramètres à trouver
    # lambda_d, lambda_s et delta des réels 
    # beta_d et beta_s des vecteurs de taille 8

    parameters = list(parameters)
    lambda_d = parameters[0]
    lambda_s = parameters[1]
    beta_d = list(parameters[2:10])
    beta_s = list(parameters[10:18])
    delta = parameters[-1]

    phi_d = phiD(beta_d)
    phi_s = phiS(beta_s)
    L_seller_sum = 0
    L_clone_sum = 0

    for i in tqdm(seller.index.to_list()): 
        L_seller_sum = L_seller_sum + np.log(LSeller_i(lambda_d, lambda_s, phi_d, phi_s,delta, i))
        L_clone_sum = L_clone_sum + np.log(LClone_i(lambda_d, lambda_s, phi_d, phi_s,delta, i))

    
    # Log_vraisemblance
    L_1 = np.sum(L_seller_sum)
    L_2 = np.sum(L_clone_sum)

    Likelihood = L_1 + L_2
    return -Likelihood
###


print(LSeller_i(np.random.uniform(1,5,size=1),
               np.random.uniform(1,5,size=1),
               phiD(np.random.uniform(1,5,size=8)),
                phiS(np.random.uniform(1,5,size=8)),
                np.random.uniform(1,5,size=1),
                1))
print(LClone_i(np.random.uniform(1,5,size=1),
               np.random.uniform(1,5,size=1),
               phiD(np.random.uniform(1,5,size=8)),
                phiS(np.random.uniform(1,5,size=8)),
                np.random.uniform(1,5,size=1),
                1))


### Estimation ### 
initial_params = np.random.uniform(2,15,size=19)

result = minimize(likelihood, initial_params, method='L-BFGS-B', options={'maxiter':1000, 'disp': True, 'ftol': 1e-1})

# Résultats
estimated_params = result.x
print("Estimated Parameters:", estimated_params)
print("success ? ", result.success)
print(result.message)
### 