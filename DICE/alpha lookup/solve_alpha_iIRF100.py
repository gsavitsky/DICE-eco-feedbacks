import numpy as np
import matplotlib.pyplot as plt
from scipy.optimize import brentq

# DFAIR Parameters from DICE-2023 
shares = [0.2173, 0.2240, 0.2824, 0.2763]
taus = [1000000.0, 394.4, 36.53, 4.304]

def calculate_iirf_residue(alpha, target_iirf):
    # Standard DFAIR Equation (11) 
    calc_iirf = sum(s * alpha * t * (1. - np.exp(-100./(alpha * t))) 
                    for s, t in zip(shares, taus))
    return calc_iirf - target_iirf

iIRF100_array = np.linspace(10, 95, 500) # Centered on your 40-60 range
alphas = []

for target in iIRF100_array:
    # Use brentq for guaranteed convergence in a specific range
    sol = brentq(calculate_iirf_residue, 0.000001, 100.0, args=(target,))
    alphas.append(sol)

# Visualizing for Stella Graphical Function
plt.plot(iIRF100_array, alphas, label='DFAIR Alpha Solution')
plt.xlabel('iIRF100 (years)')
plt.ylabel('Alpha Scaling Factor')
plt.grid(True)
plt.show()


# Combine the two arrays into a single 2D array (columns)
data_to_save = np.column_stack((iIRF100_array, alphas))

# Save with a header
np.savetxt('dfair_alpha_lookup.csv', data_to_save, 
           delimiter=',', 
           header='iIRF100,alpha', 
           comments='')