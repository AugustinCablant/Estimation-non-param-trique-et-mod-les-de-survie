import numpy as np 
import pandas as pd 
from scipy.optimize import minimize
from scipy.integrate import quad
import random
from tqdm import tqdm


seller = pd.read_csv('Data/df_vraissemblance1.csv')

X = ['type_libre','sexe_homme','idf','etranger','une_tete','dec1','dec2','dec3']
columns = ['type_libre','sexe_homme','idf','etranger','une_tete','dec1','dec2',
           'dec3','tau_birth','tau_contract','Td','Ts','Td_clone','Ts_clone',
           'tau_begin','tau_end']


def log_vraissemblance(sigma_d2, sigma_s2, alpha_d, alpha_s, beta_d, beta_s, delta): 
    def lambdaD(t): 
        return np.exp(alpha_d * t)
    
    def lambdaS(t):
        return alpha_s * (t ** (alpha_s - 1))

    # Après calculs à la main, nous obtenons des valeurs pour les intégrales, les voici :
    def IDD(t1,t2):
        if t1 > t2: I = ( (1 - delta) * np.exp(alpha_d * t2) + delta * np.exp(alpha_d * t1) - 1 ) / alpha_d
        else: I = ( np.exp(alpha_d * t1) - 1 ) / alpha_d
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
        return phi / phi.mean()
    
    phi_d = phiD(beta_d)
    phi_s = phiS(beta_s)

    def L_seller(i):
        numerateur1 = (1 + sigma_d2 * phi_d[i] * IDD(seller['Td'][i],seller['Ts'][i])) ** (- sigma_d2 - 1) 
        numerateur2 = delta * phi_d[i] * lambdaD(seller['Td'][i]) 
        numerateur3 = (1 + sigma_s2 * phi_s[i] * IS(seller['Ts'][i])) ** (- sigma_s2 - 1)
        numerateur4 = phi_s[i] * lambdaS(seller['Ts'][i])
        numerateur = numerateur1 * numerateur2 * numerateur3 * numerateur4
        print('new')
        print("Td", seller['Td'][i])
        print("sigma", sigma_d2)
        print("phi", phi_d[i]) 
        print("IDD", IDD(seller['Td'][i],seller['Ts'][i]))
        print("lambdaD", lambdaD(seller['Td'][i]))
        def int_denominateur(t):
            deno1 = 1 - (1 + sigma_d2 * phi_d[i] * IDD(seller['tau_end'][i] - seller['tau_birth'][i], t)) ** ( - sigma_d2 - 1)
            deno2 = phi_s[i] * lambdaS(t) * (1 + sigma_s2 * phi_s[i] * IS(t)) ** ( - sigma_d2 - 1)
            return deno1 * deno2
        denominateur = quad(int_denominateur, 0, np.inf)[0]
        return numerateur / denominateur
    
    def L_clone(i):
        numerateur1 = (1 + sigma_d2 * phi_d[i] * ID(seller['Td_clone'][i])) ** (- sigma_d2 - 1) 
        numerateur2 = delta * phi_d[i] * lambdaD(seller['Td'][i]) 
        numerateur3 = (1 + sigma_s2 * phi_s[i] * IS(seller['Td_clone'][i])) ** (- sigma_s2 - 1)
        numerateur = numerateur1 * numerateur2 * numerateur3 

        def int_denominateur(t):
            deno1 = 1 - (1 + sigma_d2 * phi_d[i] * IDD(seller['tau_end'][i] - seller['tau_birth'][i], t)) ** ( - sigma_d2 - 1)
            deno2 = phi_s[i] * lambdaS(t) * (1 + sigma_s2 * phi_s[i] * IS(t)) ** ( - sigma_d2 - 1)
            return deno1 * deno2
        denominateur = quad(int_denominateur, 0, np.inf)[0]

        return numerateur / denominateur

    def log_negatif(x):
        if x > 0: return np.log(x)
        elif x <= 0: return -np.log(-x)
    vlog_negatif = np.vectorize(log_negatif)

    # Retourner la fonction log_vraissemblance 
    for i in tqdm(seller.index.to_list()): 
        Log_seller = vlog_negatif(L_seller(i))
        Log_clone = vlog_negatif(L_clone(i))
        if Log_seller != np.inf and Log_clone != np.inf and Log_seller != - np.inf and Log_clone != - np.inf: 
            if Log_seller != np.nan and Log_clone != np.nan and Log_seller != None and Log_clone != None:
                L_seller_sum = L_seller_sum + Log_seller
                L_clone_sum = L_clone_sum + Log_clone
    # Log_vraisemblance
    L_1 = np.sum(L_seller_sum)
    L_2 = np.sum(L_clone_sum)
    Likelihood = L_1 + L_2
    return -Likelihood

log_vraissemblance(np.random.uniform(-50,50), np.random.uniform(-50,50), np.random.uniform(-50,50), np.random.uniform(-50,50), 
                   np.random.uniform(-50,50, size=8), np.random.uniform(-50,50, size=8), np.random.uniform(-50,50))
