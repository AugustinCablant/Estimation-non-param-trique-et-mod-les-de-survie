import numpy as np 
import pandas as pd 
import matplotlib.pyplot as plt
from scipy.optimize import minimize
from scipy.integrate import quad
import random
from tqdm import tqdm
from prettytable import PrettyTable

seller = pd.read_csv('Data/dataset_vraissemblance.csv')


X = ['type_libre','sexe_homme','sexe_femme','idf','etranger','une_tete','dec1','dec2','dec3']
columns = ['type_libre','sexe_homme','sexe_femme','idf','etranger','une_tete','dec1','dec2','dec3','tau_birth','tau_contract','Td','Ts','Td_clone','Ts_clone','tau_begin','tau_end']

seller['Td_mean'] = seller['Td'] / np.mean(seller['Td'])
print(np.mean(seller['Td_mean']))