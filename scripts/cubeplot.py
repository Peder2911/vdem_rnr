
import numpy as np
import pandas as pd
from matplotlib import pyplot as plt
from mpl_toolkits.mplot3d import Axes3D

logitToProb = lambda x: np.exp(x) / (1+np.exp(x))

d = pd.read_csv("Cache/simulations.csv")
d.sim_mean  = logitToProb(d.sim_mean)
d.sim_upper = logitToProb(d.sim_upper)
d.sim_lower = logitToProb(d.sim_lower)

values = {
    "X": d.lfree_fair_elections,
    "Y": d.lhorizontal_constraint_narrow,
    "Z": d.sim_mean,
}

dim = int(np.sqrt(values["X"].shape[0]))

values = {k:v.to_numpy().reshape(dim,dim) for k,v in values.items()}

plt3d = plt.figure().gca(projection = "3d")
plt3d.plot_wireframe(**values)
ax = plt.gca()
ax.view_init(25,50)

for a in range(1,100):
    ax.view_init(25,270 + 45*(np.sin(a / 3.14)))
    plt.savefig(f"/tmp/cube/{a}frame.png")

print("done!")
