# code adapted from https://github.com/microsoft/EconML
import matplotlib.pyplot as plt
import shap
from econml.dml import CausalForestDML
import pandas as pd
from sklearn.ensemble import RandomForestRegressor
import random

# Load in data
X = pd.read_csv('X.csv')
Y = pd.read_csv('Y.csv')
W = pd.read_csv('W.csv')
X_id = pd.read_csv('X_id.csv')
labs = pd.read_csv('labels_select.csv')

id_select = labs.label.loc[labs.include]

# Estimate nuisance models
w_model = RandomForestRegressor(n_estimators = 1000)
w_model.fit(X_id, W)

y_model = RandomForestRegressor(n_estimators = 1000)
y_model.fit(X_id, Y)

# fit causal forest with default parameters 
causal_forest = CausalForestDML(n_estimators = 1000)
causal_forest.fit(y_model.predict(X), w_model.predict(X), X=X)
X = X.rename(columns=dict(zip(labs["name"], labs["label"])))

# calculate shap values of causal forest model 
shap_values = causal_forest.shap_values(X)
# plot shap values 
shap.summary_plot(shap_values['Y0']['T0'], show = False)
plt.savefig("test1.png", bbox_inches='tight', dpi=300)
plt.show()

# Waterfall
for i in range(4):
    shap.plots.waterfall(shap_values['Y0']['T0'][random.randrange(len(shap_values['Y0']['T0']))], max_display=20, show = False)
    plt.savefig(f"idwaterfall_{i}.png", bbox_inches='tight', dpi=300)
    plt.show()

# estimates
ate = causal_forest.ate_inference(X=X)
inference = causal_forest.effect_inference(X)