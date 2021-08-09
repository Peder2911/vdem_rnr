# VDEM institutions RNR

Replication code for Fjelde et. al 2021 (https://doi.org/10.1093/isq/sqaa076)

Depends on packages found in `requirements.txt` and `r_packages.txt`.  The
packages [timelib](https://github.com/peder2911/timelib) and
[evallib](https://github.com/peder2911/evallib) are not on CRAN, but can easily
be installed from github using `devtools::install_github`.

Requirements are installed by running "findDeps.R".

Run `makeEverything.sh` after installing packages. This script runs everything
else. `tables.sh` creates all tables, while `plots.sh` creates all plots.
To generate figure 3, run STATA code included in Figure3/

Use runDocker.sh to replicate figures / tables in Docker.
