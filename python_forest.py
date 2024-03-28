# code adapted from https://github.com/microsoft/EconML
import matplotlib.pyplot as plt
import shap
from econml.dml import CausalForestDML
import pandas as pd
from sklearn.ensemble import RandomForestRegressor
from random import randrange, seed
import subprocess

seed(14)

# Load in data
# subprocess.call ("Rscript data_setup.r", shell=True)
X = pd.read_csv('X.csv')
Y = pd.read_csv('Y.csv')
W = pd.read_csv('W.csv')

# fit causal forest with default parameters 
causal_forest = CausalForestDML(n_estimators = 1000)
causal_forest.fit(Y, W, X=X)
# X = X.rename(columns=dict(zip(labs["name"], labs["label"])))

# calculate shap values of causal forest model 
shap_values = causal_forest.shap_values(X)
# plot shap values 
shap.summary_plot(shap_values['income']['treat'], show = False)
plt.savefig("figures/overall_waterfall.png", bbox_inches='tight', dpi=300)
plt.show()

# Waterfall
for i in range(5):
    shap.plots.waterfall(shap_values['income']['treat'][randrange(len(shap_values['income']['treat']))], max_display=20, show = False)
    plt.savefig(f"figures/idwaterfall_{i}.png", bbox_inches='tight', dpi=300)
    plt.show()

# estimates
ate = causal_forest.ate_inference(X=X)
inference = causal_forest.effect_inference(X)
