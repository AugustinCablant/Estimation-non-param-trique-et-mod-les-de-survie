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

    def IDD(t1,t2):
        if t1 > t2: 
            I = quad(lambdaD, 0, t2)[0] + delta * quad(lambdaD, t1, t2)[0]
        else:
            I = quad(lambdaS, 0, t1)[0]
        return I
    
    def ID(t):
        return quad(lambdaD,0,t)[0]

    def IS(t):
        return quad(lambdaS,0,t)[0]
    
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

        def int_denominateur(t):
            deno1 = 1 - (1 + sigma_d2 * phi_d[i] * IDD(seller['tau_end'][i] - seller['tau_birth'][i], t)) ** ( - sigma_d2 - 1)
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
            deno1 = 1 - (1 + sigma_d2 * phi_d[i] * IDD(seller['tau_end'][i] - seller['tau_birth'][i], t)) ** ( - sigma_d2 - 1)
            deno2 = phi_s[i] * lambdaS(t) * (1 + sigma_s2 * phi_s[i] * IS(t)) ** ( - sigma_d2 - 1)
            return deno1 * deno2
        denominateur = quad(int_denominateur, 0, 10000)[0]

        return numerateur / denominateur
    
    # Retourner la fonction log_vraissemblance 
    log_seller_sum = 0
    log_clone_sum = 0
    for i in seller.index.to_list():
        log_seller_sum += np.log(L_seller(i))
        log_clone_sum += np.log(L_clone(i))
    
    return log_seller_sum + log_clone_sum


log_vraissemblance(np.random.uniform(0,1), np.random.uniform(0,1), np.random.uniform(0,1), np.random.uniform(0,1), 
                   np.random.uniform(0,1, size=8), np.random.uniform(0,1, size=8), np.random.uniform(0,1))