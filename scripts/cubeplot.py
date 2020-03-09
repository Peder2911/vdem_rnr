
import numpy as np
import pandas as pd
from matplotlib import pyplot as plt
from mpl_toolkits.mplot3d import Axes3D
from matplotlib.colors import LightSource

plt.clf()

logitToProb = lambda x: np.exp(x) / (1+np.exp(x))

#d = pd.read_csv("Cache/t3_model_7.rds.simulations.csv")
#d.sim_mean = logitToProb(d.sim_mean) 


def castAsMatrix(series):
    dim = int(np.sqrt(series.shape[0]))
    return series.to_numpy().reshape(dim,dim)

def makeCubePlot(data,out):
    plt.clf()

    variables = (
        #("sim_lower","gray",0.6),
        ("sim_mean","red",1),
        #("sim_upper","gray",0.6)
    )

    plt3d = plt.figure().gca(projection = "3d")

    for vname, color, alpha in variables:
        values = {
            "X": data.lfree_fair_elections,
            "Y": data.lhorizontal_constraint_narrow,
            "Z": data[vname],
        }

        values = {k:castAsMatrix(v) for k,v in values.items()}

        ls = LightSource(0,0)
        rgb = ls.shade(values["Z"], plt.get_cmap("cividis"),
            blend_mode="soft",vert_exag = -6)

        plt3d.plot_surface(**values,alpha=alpha,
            facecolors = rgb,linewidth =.3,shade = False)

    ax = plt.gca()
    ax.set_xlabel("Vertical Constraints")
    ax.set_ylabel("Horizontal Constraints")
    ax.set_zlabel("P (Conflict outbreak)")
    ax.set_xlim(values["X"].max(),0)
    #ax.set_ylim(values["Y"].max(),0)
    f = plt.gcf()
    f.set_size_inches(6,6)
    angles = [
        (25,25),
        (25,45),
        (25,65)
    ]
    for vert,hor in angles:
        ax.view_init(vert,hor)
        plt.savefig(out.format(angle=hor))


if __name__ == "__main__":
    tasks = [
        ("Cache/t3_model_7.rds.simulations.csv","Cache/t3_model_7_cube_{angle}.svg"),
        ("Cache/t3_model_6.rds.simulations.csv","Cache/t3_model_6_cube_{angle}.svg"),
    ]
    for infile,outfile in tasks:
        print(f"Doing {infile}")
        makeCubePlot(pd.read_csv(infile),outfile)
