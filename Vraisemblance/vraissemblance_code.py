import numpy as np 
import pandas as pd 
import matplotlib.pyplot as plt
from scipy.optimize import minimize
from scipy.integrate import quad
import random
from tqdm import tqdm
from prettytable import PrettyTable

seller = pd.read_csv('Data/dataset_vraissemblance.csv')
facteur_de_normalisation = 10 ** (-6)

X = ['sexe_femme','idf','etranger','dec1','dec2','dec3']
columns = ['sexe_femme','idf','etranger','dec1','dec2','dec3','tau_birth','tau_contract','Td','Ts','Td_clone','Ts_clone','tau_begin','tau_end']

### Quelques fonctions utiles ###
def phiD(beta_d): 
    x_i = seller[X].values 
    phi = np.exp(np.dot(x_i,beta_d))
    return phi 

def phiS(beta_s): 
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
    t_end = (seller['tau_end'][i] - seller['tau_birth'][i]) * facteur_de_normalisation
    t_begin = (seller['tau_begin'][i] - seller['tau_birth'][i]) * facteur_de_normalisation
    deno = d * (1 - delta) + s

    # on multiplie par -1
    membre_1 =  s * (np.exp(- delta * d * t_end) - 
                  np.exp(- delta * d * t_begin) + 
                  np.exp(- t_begin * (d + s)) -
                  np.exp(- delta * d * t_end - t_begin * ((1 - delta) * d + s))) / deno
    membre_2 = np.exp( - t_end * (d + s)) - np.exp( - d * t_begin - s * t_end)
    membre_3_1 = np.exp(- d * t_begin - s * t_end) - np.exp( - t_end * (d + s)) 
    membre_3_2 = s * (np.exp(- d * (delta * t_end - (1 - delta) * t_begin ) - s * t_begin ) - np.exp( - t_end * (s + d))) / deno
    membre_3 = membre_3_1 + membre_3_2
                                               
    denominateur = membre_1 + membre_2 + membre_3
    resultat = numerateur / denominateur
    
    return resultat


def LClone_i(lambda_d, lambda_s, phi_d, phi_s,delta, i):
    # numérateur
    numerateur_d_exp =  np.exp(-(phi_d[i] * ID(lambda_d, seller['Td_clone'][i])))
    numerateur_d = lambda_d * phi_d[i] * numerateur_d_exp
    numerateur_s_exp = np.exp(-(phi_s[i] * IS(lambda_s,seller['Td_clone'][i])))
    numerateur = numerateur_d * numerateur_s_exp
    
    # dénominateur 
    s = lambda_s * phi_s[i] 
    d = lambda_d * phi_d[i] 
    t_end = (seller['tau_end'][i] - seller['tau_birth'][i]) * facteur_de_normalisation
    t_begin = (seller['tau_contract'][i] - seller['tau_birth'][i]) * facteur_de_normalisation
    deno = d * (1 - delta) + s

    #on multiplie aussi par -1
    membre_1 = - s * (np.exp(- delta * d * t_end) - 
                  np.exp(- delta * d * t_begin) + 
                  np.exp(- t_begin * (d + s)) -
                  np.exp(- delta * d * t_end - t_begin * ((1 - delta) * d + s))) / deno
    membre_2 = np.exp( - t_end * (d + s)) - np.exp( - d * t_begin - s * t_end)
    membre_3_1 = np.exp(- d * t_begin - s * t_end) - np.exp( - t_end * (d + s)) 
    membre_3_2 = s * (np.exp(- d * (delta * t_end - (1 - delta) * t_begin ) - s * t_begin ) - np.exp( - t_end * (s + d))) / deno
    membre_3 = membre_3_1 + membre_3_2
    denominateur = membre_1 + membre_2 + membre_3
    
    resultat = numerateur / denominateur
    return resultat

# À supprimer in fine
"""
def log_negatif(x):
    if x > 0: return np.log(x)
    elif x <= 0: return -np.log(-x)
vlog_negatif = np.vectorize(log_negatif)
"""

# Fonction de vraissemblance
def likelihood(parameters):

    # Paramètres à trouver
    # lambda_d, lambda_s et delta des réels 
    # beta_d et beta_s des vecteurs de taille 6

    parameters = list(parameters)
    lambda_d = parameters[0]
    lambda_s = parameters[1]
    beta_d = list(parameters[2:8])
    beta_s = list(parameters[8:14])
    delta = parameters[-1]

    phi_d = phiD(beta_d)
    phi_s = phiS(beta_s)
    L_seller_sum = 0
    L_clone_sum = 0
    # Il faut 2000 observations 
    for i in tqdm(seller.index.to_list()): 
        Log_seller = LSeller_i(lambda_d, lambda_s, phi_d, phi_s,delta, i)
        Log_clone = LClone_i(lambda_d, lambda_s, phi_d, phi_s,delta, i)
        L_seller_sum = L_seller_sum + Log_seller
        L_clone_sum = L_clone_sum + Log_clone
    # Log_vraisemblance
    L_1 = np.sum(L_seller_sum)
    L_2 = np.sum(L_clone_sum)

    Likelihood = L_1 + L_2
    return Likelihood

# Réduire l'ordre de grandeur des variables
seller['Ts'] = seller['Ts'] * facteur_de_normalisation
seller['Td'] = seller['Td'] * facteur_de_normalisation
seller['Td_clone'] = seller['Td_clone'] * facteur_de_normalisation
seller['Ts_clone'] = seller['Ts_clone'] * facteur_de_normalisation

parameters_list = [
    "lambda_d", "lambda_s", "delta",
    *["beta_d" + str(i) for i in range(6)],
    *["beta_s" + str(i) for i in range(6)]
]

# Répéter le calcul de la minimisation
initial_params = np.random.uniform(1, 5, size=15)
result = minimize(likelihood, initial_params, method='BFGS', options={'disp': True})

# Résultats de l'itération actuelle
estimated_params = result.x
success = result.success
message = result.message
#result['std'] = param_stds
hessian = result.hess_inv
#print(estimated_params)

parameters_list = [
    "lambda_d", "lambda_s", "delta",
    *["beta_d" + str(i) for i in range(6)],
    *["beta_s" + str(i) for i in range(6)]
    ]

# Calculer les écarts types des estimateurs (racine carrée des variances diagonales)
covariance_matrix = np.linalg.inv(hessian)
std_deviations = np.sqrt(np.diag(covariance_matrix))
#print("Écarts types des estimateurs :", std_deviations)
print("Paramètres initiaux : ", initial_params)
print(success)
print(message)

for i, param in enumerate(estimated_params):
    print(parameters_list[i], " : ", param, "     std : ", std_deviations[i])
