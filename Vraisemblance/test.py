import numpy as np 
import pandas as pd 
from scipy.optimize import minimize
from scipy.integrate import quad

#Charger les données
seller = pd.read_csv('Data/dataset_vraissemblance.csv')
facteur_de_normalisation = 10 ** (-4)

#Colonnes que l'on utilise
X = ['sexe_femme','idf','etranger','dec1','dec2','dec3']
columns = ['sexe_femme','idf','etranger','dec1','dec2','dec3','tau_birth','tau_contract',
           'Td','Ts','Td_clone','Ts_clone','tau_begin','tau_end']

# Réduire l'ordre de grandeur des variables
seller['Ts'] = seller['Ts'] * facteur_de_normalisation
seller['Td'] = seller['Td'] * facteur_de_normalisation
seller['Td_clone'] = seller['Td_clone'] * facteur_de_normalisation
seller['Ts_clone'] = seller['Ts_clone'] * facteur_de_normalisation

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

"""
#Contribution du vendeur 
def LSeller_i(lambda_d, lambda_s, phi_d, phi_s,delta, i):
    """
"""
    lambda_d / lambda_s / delta des scalaires 
    phi_d / phi_s des vecteurs
    i correspond au ième vendeur  
    cette fonction retourne la contribution du ième vendeur à la fonction de vraisemblance  
    """
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
    print(numerateur, denominateur)
    return numerateur / denominateur

#contribution des clones 
def LClone_i(lambda_d, lambda_s, phi_d, phi_s,delta, i):
    """
"""
    lambda_d / lambda_s / delta des scalaires 
    phi_d / phi_s des vecteurs
    i correspond au ième vendeur  
    cette fonction retourne la contribution du ième clone à la fonction de vraisemblance  
    """
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
    return numerateur / denominateur
"""
td_mean = (seller['Td'].mean() + seller['Td_clone'].mean()) / 2
ts_mean = seller['Ts'].mean()
seller['Ts'] = seller['Ts'] * facteur_de_normalisation
seller['Td'] = seller['Td'] * facteur_de_normalisation
seller['Td_clone'] = seller['Td_clone'] * facteur_de_normalisation
seller['Ts_clone'] = seller['Ts_clone'] * facteur_de_normalisation

initial_params = np.array([1 / td_mean, 1 / ts_mean, 1,
                           -0.5, 0, 0, 0, 0, 0,
                           -0.5, 0, 0, 0, 0, 0])
phi_d = phiD(initial_params[3:9])
phi_s = phiD(initial_params[9:15])

for i in seller.index.to_list():
    d = initial_params[0] * phi_d[i]
    s = initial_params[1] * phi_s[i]
    t_begin = (seller['tau_begin'][i] - seller['tau_birth'][i]) * facteur_de_normalisation
    t_end = (seller['tau_end'][i] - seller['tau_birth'][i]) * facteur_de_normalisation
    def integ(t):
        integrande = (np.exp(- phi_d[i] * IDD(initial_params[2], initial_params[0], t_begin, t)) - np.exp(
            - phi_d[i] * IDD(initial_params[2], initial_params[0], t_end, t))) * s * np.exp(
                - phi_s[i] * IS(initial_params[1], t))
        return integrande
    print(quad(integ, 0, 10000)[0])