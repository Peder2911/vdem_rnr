# VDEM institutions RNR

Reproduction and update of analysis from an upcoming article.

Depends on packages found in `requirements.txt` and `r_packages.txt`.  The
packages [timelib](https://github.com/peder2911/timelib) and
[evallib](https://github.com/peder2911/evallib) are not on CRAN, but can easily
be installed from github using `devtools::install_github`.

First, install python(3.6+) requirements (requirements.txt), and R requirements
(r_packages.txt)

Run `makeEverything.sh` after installing packages. This script runs everything else.
`tables.sh` creates all tables, while `plots.sh` creates all plots.
