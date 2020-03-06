
import pandas as pd
from matplotlib import pyplot as plt
import subprocess
from io import StringIO
import os

DIR = "/tmp/"
EXT = ".pdf"
plt.style.use("ggplot")

data = pd.read_csv(
    StringIO(
        subprocess.run(["Rscript","-e","write.csv(readRDS('Cache/prepped_data.rds'))"],
                        stdout = subprocess.PIPE).stdout.decode()
        )
    )

def completerow(row):
    variables = [
        "free_fair_elections",
        "horizontal_constraint_narrow",
        "c2_onset",
        "llnpop200",
        "llnGDPPerCapita200",
        "ethfrac",
        "lmtnest",
        "nbConflict",
        "timesince",
        "timesince_sq",
        "timesince_cb",
        "gwno",
        "year"
    ]
    nn = [pd.notnull(row[v]) for v in variables]
    return all(nn)
#complete = data.apply(completerow,1)
data = data[data.apply(completerow,1)]
data = data[data.year > 1945]
print(data.shape)

hist = lambda dat: plt.hist(dat, width = 0.01, bins = 100)
cathist = lambda dat: plt.hist(dat, bins = 2)

def makehist(dat,name,fn = hist):
    plt.clf()
    fn(dat)
    ax = plt.gca()
    ax.set_xlim(0,1)
    ax.set_xlabel(name.title())
    plt.savefig(os.path.join(DIR,name+EXT))
    

makehist(data.horizontal_constraint_narrow,"horizontal")
makehist(data.free_fair_narrow,"vertical")
makehist(data.c2_onset,"c2_onset",fn=cathist)
