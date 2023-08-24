import numpy as np 
import pandas as pd 
import matplotlib.pyplot as plt
from scipy.optimize import minimize
from scipy.integrate import quad
import random
from tqdm import tqdm
from prettytable import PrettyTable

seller = pd.read_csv('Data/dataset_vraissemblance.csv')


X = ['sexe_femme','idf','etranger','dec1','dec2','dec3']
columns = ['sexe_femme','idf','etranger','dec1','dec2','dec3','tau_birth','tau_contract','Td','Ts','Td_clone','Ts_clone','tau_begin','tau_end']

### Quelques fonctions utiles ###
def phiD(beta_d): 
    x_i = seller[X].values 
    phi = np.exp(np.dot(x_i,beta_d))
    return phi / phi.mean()

def phiS(beta_s): 
    x_i = seller[X].values 
    phi = np.exp(np.dot(x_i,beta_s))
    return phi / phi.mean()

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
    t_end = (seller['tau_end'][i] - seller['tau_birth'][i]) * 10 ** -6
    t_begin = (seller['tau_begin'][i] - seller['tau_birth'][i]) * 10 ** -6
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
    t_end = (seller['tau_end'][i] - seller['tau_birth'][i]) * 10 ** -6
    t_begin = (seller['tau_begin'][i] - seller['tau_birth'][i]) * 10 ** -6
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
    for i in tqdm(seller.index.to_list()): 
        Log_seller = vlog_negatif(LSeller_i(lambda_d, lambda_s, phi_d, phi_s,delta, i))
        Log_clone = vlog_negatif(LClone_i(lambda_d, lambda_s, phi_d, phi_s,delta, i))
        if Log_seller != np.inf and Log_clone != np.inf and Log_seller != - np.inf and Log_clone != - np.inf: 
            if Log_seller != np.nan and Log_clone != np.nan and Log_seller != None and Log_clone != None:
                L_seller_sum = L_seller_sum + Log_seller
                L_clone_sum = L_clone_sum + Log_clone
    # Log_vraisemblance
    L_1 = np.sum(L_seller_sum)
    L_2 = np.sum(L_clone_sum)

    Likelihood = L_1 + L_2
    return -Likelihood

# Réduire l'ordre de grandeur des variables
seller['tau_birth'] = seller['tau_birth'] / seller['tau_birth'].mean()
seller['tau_contract'] = seller['tau_contract'] / seller['tau_contract'].mean()
seller['Ts'] = seller['Ts'] / seller['Ts'].mean()
seller['Td'] = seller['Td'] / seller['Td'].mean()
seller['Td_clone'] = seller['Td_clone'] / seller['Td_clone'].mean()
seller['Ts_clone'] = seller['Ts_clone'] / seller['Ts_clone'].mean()

parameters_list = [
    "lambda_d", "lambda_s", "delta",
    *["beta_d" + str(i) for i in range(8)],
    *["beta_s" + str(i) for i in range(8)]
]

# Répéter le calcul de la minimisation
initial_params = np.random.uniform(1, 5, size=15)
result = minimize(likelihood, initial_params, method='BFGS', options={'maxiter': 10000, 'disp': True})

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
