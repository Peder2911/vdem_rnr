#!/bin/bash
shopt -s extglob
set -e

Rscript -e "saveRDS(read.csv('Data/institutions.csv'),'Cache/prepped_data.rds')"
Rscript -e "saveRDS(read.csv('Data/ged.csv'),'Cache/ged.rds')"

bash tables.sh
bash plots.sh
