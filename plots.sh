#!/bin/bash
shopt -s extglob
set -e

Rscript -e "saveRDS(read.csv('Data/institutions.csv'),'Cache/prepped_data.rds')"
Rscript -e "saveRDS(read.csv('Data/ged.csv'),'Cache/ged.rds')"

echo "Running \"Regular\" table"
Rscript scripts/tables/regular.R
echo "Running table_2"
Rscript scripts/tables/table_2.R
echo "Running table_3"
Rscript scripts/tables/table_3.R

for F in scripts/plots/!(preparation|regular|table_2|table_3).R
   do
      echo "Running $F"
      Rscript $F
   done

for F in scripts/plots/*.py
   do
      echo "Running $F"
      python3 $F
   done
