import numpy as np 
import pandas as pd 
from scipy.optimize import minimize
from scipy.integrate import quad
import random
from tqdm import tqdm


seller = pd.read_csv('Data/dataset_vraissemblance.csv')

""""
seller['tau_birth'] *= 10**-3
seller['tau_contract'] *= 10**-3
seller['Td'] = *= 10**-3
seller['Ts'] *= 10**-3
seller['Td_clone'] *= 10**-3
seller['Ts_clone'] *= 10**-3
seller['tau_begin'] *= 10**-3
seller['tau_end'] *= 10**-3
"""
seller['tau_birth'] = seller['tau_birth'] / seller['tau_birth'].mean()
seller['tau_contract'] = seller['tau_contract'] / seller['tau_contract'].mean()
seller['Ts'] = seller['Ts'] / seller['Ts'].mean()
seller['Td'] = seller['Td'] / seller['Td'].mean()
seller['Td_clone'] = seller['Td_clone'] / seller['Td_clone'].mean()
seller['Ts_clone'] = seller['Ts_clone'] / seller['Ts_clone'].mean()

X = ['type_libre','sexe_homme','sexe_femme','idf','etranger','une_tete','dec1','dec2','dec3']
columns = ['type_libre','sexe_homme','sexe_femme','idf','etranger','une_tete','dec1','dec2',
           'dec3','tau_birth','tau_contract','Td','Ts','Td_clone','Ts_clone','tau_begin','tau_end']



def log_vraissemblance(params): 
    sigma_d2 = params[0]
    sigma_s2 = params[1]
    alpha_d = params[2]
    alpha_s = params[3]
    beta_d = params[4:13]
    beta_s = params[13:22]
    delta = params[22] 
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
        return phi / phi.mean()

    def phiS(beta_s): # beta_d est un vecteur de taille 8
        x_i = seller[X].values 
        phi = np.exp(np.dot(x_i,beta_s))
        return phi /phi.mean()
    
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

    # Retourner la fonction log_vraissemblance 
    L_seller_sum = 0 
    L_clone_sum = 0
    compteur = 0 
    for i in seller.index.to_list(): 
        Log_seller = vlog_negatif(L_seller(i))
        Log_clone = vlog_negatif(L_clone(i))
        if Log_seller != np.inf and Log_clone != np.inf and Log_seller != - np.inf and Log_clone != - np.inf: 
            if Log_seller != np.nan and Log_clone != np.nan and Log_seller != None and Log_clone != None:
                compteur +=1
                L_seller_sum = L_seller_sum + Log_seller
                L_clone_sum = L_clone_sum + Log_clone
    L_1 = np.sum(L_seller_sum)
    L_2 = np.sum(L_clone_sum)
    Likelihood = L_1 + L_2
    return -Likelihood

print(log_vraissemblance(np.random.uniform(1,3, size = 23)))

"""
liste_compteur = []
initial_params = np.random.uniform(1,3, size = 23)
result = minimize(log_vraissemblance, initial_params, method='L-BFGS-B', options={'maxiter': 1000, 'disp': True})
estimated_params = result.x
success = result.success
message = result.message
print(estimated_params)
print(liste_compteur)



num_repeats = 1

parameters_list = [
    "sigma_d2", "sigma_s2", "alpha_d", "alpha_s",
    *["beta_d" + str(i) for i in range(9)],
    *["beta_s" + str(i) for i in range(9)],
    "delta"
            ]

# Créer un DataFrame avec la colonne "parameters" et la colonne "valeurs"
data = {"parameters": parameters_list, "valeurs": [0] * len(parameters_list)}
all_estimations = []
liste_compteur = []
liste_compteur_true = []


# Répéter le calcul de la minimisation
for _ in tqdm(range(num_repeats)):
    initial_params = np.random.uniform(1,3, size = 23)
    #np.random.uniform(1,3), np.random.uniform(-10,10), np.random.uniform(-10,10), np.random.uniform(-10,10, size=9), np.random.uniform(-10,10, size=9), np.random.uniform(-10,10)
    result = minimize(log_vraissemblance, initial_params, method='L-BFGS-B', options={'maxiter': 1000, 'disp': True, 'ftol': 1e-1})
    
    # Résultats de l'itération actuelle
    estimated_params = result.x
    success = result.success
    message = result.message
    if liste_compteur[-1] > 900:
        liste_compteur_true.append(liste_compteur[-1])
        all_estimations.append(estimated_params)
    # Ajouter les résultats de l'itération actuelle au dictionnaire
    for i, param in enumerate(estimated_params):
            if i<=20:
                data["valeurs"][i] += param
            else:
                pass
all_estimations = np.array(all_estimations)
param_means = np.mean(all_estimations, axis=0)
param_stds = np.std(all_estimations, axis=0)
result = pd.DataFrame(data)
result['valeurs'] /= len(all_estimations)
result['std'] = param_stds
print(result)
print(liste_compteur_true)
"""