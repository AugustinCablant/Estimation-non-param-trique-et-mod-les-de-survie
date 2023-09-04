#Imports
import numpy as np 
import pandas as pd 
from scipy.optimize import minimize
from tqdm import tqdm

#Charger les données
seller = pd.read_csv('Data/dataset_vraissemblance.csv')
facteur_de_normalisation = 10 ** (-4)

#Colonnes que l'on utilise
X = ['sexe_femme','idf','etranger','dec1','dec2','dec3']
columns = ['sexe_femme','idf','etranger','dec1','dec2','dec3','tau_birth','tau_contract',
           'Td','Ts','Td_clone','Ts_clone','tau_begin','tau_end']

#Fonctions pour la contribution 
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

def log_negatif(x):
    if x > 0: return np.log(x)
    elif x <= 0: return -np.log(-x)

#Contribution du vendeur 
def LSeller_i(lambda_d, lambda_s, phi_d, phi_s,delta, i):
    """
    lambda_d / lambda_s / delta des scalaires 
    phi_d / phi_s des vecteurs
    i correspond au ième vendeur  
    cette fonction retourne la contribution du ième vendeur à la fonction de vraisemblance  
    """
    #quelques variables pour rendre visible les calculs
    d = lambda_d * phi_d[i]
    s = lambda_s * phi_s[i]
    deno = (1 - delta) * d + s
    t_end = (seller['tau_end'][i] - seller['tau_birth'][i]) * facteur_de_normalisation
    t_begin = (seller['tau_begin'][i] - seller['tau_birth'][i]) * facteur_de_normalisation

    numerateur = delta * d * np.exp(- phi_d[i] * IDD(delta, lambda_d, seller['Td'][i], seller['Ts'][i])) * s * np.exp(
        - phi_s[i] * IS(lambda_s, seller['Ts'][i]))
    denominateur = s * (np.exp(- delta * d * t_begin) - np.exp(- delta * d * t_end) + np.exp(
        - delta * d * t_end - t_begin * ((1 - delta) * d + s)) - np.exp( - t_begin * (d + s))  #(1)
        - (np.exp(- d * (delta * t_end + (1 - delta) * t_begin) - s * t_begin) + np.exp(- t_end * (d + s)))  #début de (2)
        ) / deno + np.exp(- t_begin * (d + s)) - np.exp(- t_begin * d - t_end * s) + np.exp(
        - t_begin * d - t_end * s) - np.exp(- t_end * (d + s))  #(3)
    return numerateur #/ denominateur

#contribution des clones 
def LClone_i(lambda_d, lambda_s, phi_d, phi_s,delta, i):
    """
    lambda_d / lambda_s / delta des scalaires 
    phi_d / phi_s des vecteurs
    i correspond au ième vendeur  
    cette fonction retourne la contribution du ième clone à la fonction de vraisemblance  
    """
    #quelques variables pour rendre visible les calculs
    d = lambda_d * phi_d[i]
    s = lambda_s * phi_s[i]
    deno = (1 - delta) * d + s
    t_end = (seller['tau_end'][i] - seller['tau_birth'][i]) * facteur_de_normalisation
    t_begin = (seller['tau_contract'][i] - seller['tau_birth'][i]) * facteur_de_normalisation

    numerateur = d * np.exp(- phi_d[i] * ID(lambda_d, seller['Td_clone'][i])) * np.exp(
    - phi_s[i] * IS(lambda_s, seller['Td_clone'][i]))
    denominateur = s * (np.exp(- delta * d * t_begin) - np.exp(- delta * d * t_end) + np.exp(
        - delta * d * t_end - t_begin * ((1 - delta) * d + s)) - np.exp( - t_begin * (d + s))  #(1)
        - (np.exp(- d * (delta * t_end + (1 - delta) * t_begin) - s * t_begin) + np.exp(- t_end * (d + s)))  #début de (2)
        ) / deno + np.exp(- t_begin * (d + s)) - np.exp(- t_begin * d - t_end * s) + np.exp(
        - t_begin * d - t_end * s) - np.exp(- t_end * (d + s))  #(3)
    return numerateur #/ denominateur

#fonction de vraissemblance
def likelihood(parameters):
    # Paramètres à trouver
    # lambda_d, lambda_s et delta des réels 
    # beta_d et beta_s des vecteurs de taille 6
    lambda_d = parameters[0]
    lambda_s = parameters[1]
    delta = parameters[2]
    delta = parameters[3]
    beta_d = list(parameters[3:9])
    beta_s = list(parameters[9:15])
    phi_d = phiD(beta_d) * facteur_de_normalisation
    phi_s = phiS(beta_s) * facteur_de_normalisation
    L_seller_sum = 0
    L_clone_sum = 0
    for i in tqdm(seller.index.to_list()): 
        Log_seller_i = log_negatif(LSeller_i(lambda_d, lambda_s, phi_d, phi_s, delta, i))
        Log_clone_i = log_negatif(LClone_i(lambda_d, lambda_s, phi_d, phi_s, delta, i))
        L_seller_sum = L_seller_sum + Log_seller_i
        L_clone_sum = L_clone_sum + Log_clone_i
    L_1 = np.sum(L_seller_sum)
    L_2 = np.sum(L_clone_sum)
    Likelihood = L_1 + L_2
    return - Likelihood

# Réduire l'ordre de grandeur des variables
td_mean = (seller['Td'].mean() + seller['Td_clone'].mean()) / 2
ts_mean = seller['Ts'].mean()
seller['Ts'] = seller['Ts'] * facteur_de_normalisation
seller['Td'] = seller['Td'] * facteur_de_normalisation
seller['Td_clone'] = seller['Td_clone'] * facteur_de_normalisation
seller['Ts_clone'] = seller['Ts_clone'] * facteur_de_normalisation

#minimisation de l'opposé de la log-vraisemblance
initial_params = np.array([1 / td_mean, 1 / ts_mean, 1,
                           -0.5, 0, 0, 0, 0, 0,
                           -0.5, 0, 0, 0, 0, 0])
result = minimize(likelihood, initial_params, method='L-BFGS-B', options={'disp': True, 'tol': 1e-2})
estimated_params = result.x
success = result.success
message = result.message
hessian = result.hess_inv

print("Paramètres initiaux : ", initial_params)
print(success)
print(message)

#afficher les paramètres
parameters_list = [
    "lambda_d", "lambda_s", "delta",
    *["beta_d" + str(i) for i in range(6)],
    *["beta_s" + str(i) for i in range(6)]
    ]
for i, param in enumerate(estimated_params):
    print(parameters_list[i], " : ", param) #, "     std : ", std_deviations[i])

# Calculer les écarts types des estimateurs (racine carrée des variances diagonales)
covariance_matrix = np.linalg.inv(hessian)
std_deviations = np.sqrt(np.diag(covariance_matrix))
print(std_deviations)
