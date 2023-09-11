"""
Hypothèses : 
- Var(V_d) = Var(V_s) = 0
- delta ne dépend pas de t, t_s et x
- lambda_d et lambda_s ne dépendent pas de t
"""
#Imports
import numpy as np 
import pandas as pd 
from scipy.optimize import minimize
from scipy.integrate import quad
from tqdm import tqdm

#Charger les données
seller = pd.read_csv('Data/dataset_vraissemblance.csv')
facteur_de_normalisation = 10 ** (-3)
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
    #calcul à la main
    
    denominateur = s * (np.exp(- delta * d * t_begin) - np.exp(- delta * d * t_end) + np.exp(
        - delta * d * t_end - t_begin * ((1 - delta) * d + s)) - np.exp( - t_begin * (d + s))  #(1)
        - (np.exp(- d * (delta * t_end + (1 - delta) * t_begin) - s * t_begin) + np.exp(- t_end * (d + s)))  #début de (2)
        ) / deno + np.exp(- t_begin * (d + s)) - np.exp(- t_begin * d - t_end * s) + np.exp(
        - t_begin * d - t_end * s) - np.exp(- t_end * (d + s))  #(3)
    """
    #calcul numérique 
    def integ(t):
        integrande = (np.exp(- phi_d[i] * IDD(initial_params[2], initial_params[0], t_begin, t)) - np.exp(
            - phi_d[i] * IDD(initial_params[2], initial_params[0], t_end, t))) * s * np.exp(
                - phi_s[i] * IS(initial_params[1], t))
        return integrande
    denominateur2 = quad(integ, 0, 10000)[0]
    """
    return numerateur / denominateur

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

    #calcul à la main
    
    denominateur = s * (np.exp(- delta * d * t_begin) - np.exp(- delta * d * t_end) + np.exp(
        - delta * d * t_end - t_begin * ((1 - delta) * d + s)) - np.exp( - t_begin * (d + s))  #(1)
        - (np.exp(- d * (delta * t_end + (1 - delta) * t_begin) - s * t_begin) + np.exp(- t_end * (d + s)))  #début de (2)
        ) / deno + np.exp(- t_begin * (d + s)) - np.exp(- t_begin * d - t_end * s) + np.exp(
        - t_begin * d - t_end * s) - np.exp(- t_end * (d + s))  #(3)
    """
    #calcul numérique 
    def integ(t):
        integrande = (np.exp(- phi_d[i] * IDD(initial_params[2], initial_params[0], t_begin, t)) - np.exp(
            - phi_d[i] * IDD(initial_params[2], initial_params[0], t_end, t))) * s * np.exp(
                - phi_s[i] * IS(initial_params[1], t))
        return integrande
    denominateur2 = quad(integ, 0, 10000)[0]
    
    """
    return numerateur / denominateur

#fonction de vraissemblance
def likelihood(parameters):
    # Paramètres à trouver
    # lambda_d, lambda_s et delta des réels 
    # beta_d et beta_s des vecteurs de taille 6
    lambda_d = parameters[0]
    lambda_s = parameters[1]
    delta = parameters[2]
    beta_d = list(parameters[3:9])
    beta_s = list(parameters[9:15])
    phi_d = phiD(beta_d) 
    phi_s = phiS(beta_s) 
    L_seller_sum = 0
    L_clone_sum = 0
    for i in tqdm(seller.index.to_list()): 
        Log_seller_i = np.log(LSeller_i(lambda_d, lambda_s, phi_d, phi_s, delta, i))
        Log_clone_i = np.log(LClone_i(lambda_d, lambda_s, phi_d, phi_s, delta, i))
        L_seller_sum = L_seller_sum + Log_seller_i
        L_clone_sum = L_clone_sum + Log_clone_i
    Likelihood = L_seller_sum + L_clone_sum 
    print(- Likelihood)
    return - Likelihood

# Réduire l'ordre de grandeur des variables
td_mean = (seller['Td'].mean() + seller['Td_clone'].mean()) / 2
ts_mean = seller['Ts'].mean()
seller['Ts'] = seller['Ts'] * facteur_de_normalisation
seller['Td'] = seller['Td'] * facteur_de_normalisation
seller['Td_clone'] = seller['Td_clone'] * facteur_de_normalisation
seller['Ts_clone'] = seller['Ts_clone'] * facteur_de_normalisation

#minimisation de l'opposé de la log-vraisemblance
"""
#paramètres initiaux du cas sans troncature
np.array([1 / td_mean, 1 / ts_mean, 1,
                           -0.5, 0, 0, 0, 0, 0,
                           -0.5, 0, 0, 0, 0, 0])

#paramètres initiaux : les paramètres optimaux dans le cas sans troncature
initial_params = [6.04346613e-03, 6.31093146e-03, 2.05136335e+01,
                9.45523846e-01,-5.76752473e-01, -4.21429986e-01, 3.89043616e-01, -3.58258201e-01,
                -1.62128708e-01, 5.91413099e-01, 1.78948759e-01, -1.47719028e+00,
                5.22530594e-01, 7.99081591e-01, 3.06774052e-02]
#optim 1
initial_params = [0.0257828, 0.0142333, 10.79241345, -0.2711224, 0.0436154, 0.04625169, 0.57186483, 
                  0.1971254, 0.07792981, -0.38274347, 0.04600457, 0.02380623, 1.05678997,  0.45055338, 0.17921091]  
"""
initial_params = [6.04346613e-03, 6.31093146e-03, 10.79241345, 9.45523846e-01,-5.76752473e-01, -4.21429986e-01, 3.89043616e-01, 
                  -3.58258201e-01, -1.62128708e-01, 5.91413099e-01, 1.78948759e-01, -1.47719028e+00, 5.22530594e-01, 
                  7.99081591e-01, 3.06774052e-02]



result = minimize(likelihood, initial_params, method='Nelder-Mead', options={'disp': True, 'tol': 1e-6, 'maxiter': 50000})
estimated_params = result.x
success = result.success
message = result.message
#calcul des écarts-types
# Calculer la matrice de covariance des paramètres en utilisant une approximation numérique
epsilon = 1e-5  # Petit epsilon pour calculer les dérivées numériques
num_params = len(initial_params)
covariance_matrix = np.zeros((num_params, num_params))

for i in range(num_params):
    for j in range(num_params):
        # Calculez les dérivées partielles numériques
        params_plus_epsilon = np.array(estimated_params)
        params_plus_epsilon[i] += epsilon
        params_plus_epsilon[j] += epsilon

        params_minus_epsilon = np.array(estimated_params)
        params_minus_epsilon[i] -= epsilon
        params_minus_epsilon[j] -= epsilon

        # Calculez les gradients numériques
        grad_plus = (likelihood(params_plus_epsilon) - result.fun) / epsilon
        grad_minus = (likelihood(params_minus_epsilon) - result.fun) / epsilon

        # Remplissez la matrice de covariance
        covariance_matrix[i, j] = (grad_plus * grad_minus)

# Calculez les écarts-types des paramètres à partir de la matrice de covariance
parameter_std_devs = np.sqrt(np.diag(covariance_matrix))

#affichons les résultats:
print("Paramètres initiaux : ", initial_params)
print(success)
print(message)
parameters_list = [
    "lambda_d", "lambda_s", "delta",
    *["beta_d" + str(i) for i in range(6)],
    *["beta_s" + str(i) for i in range(6)]
    ]
for i, param in enumerate(estimated_params):
    print(parameters_list[i], " : ", param, "  std :", parameter_std_devs[i])
