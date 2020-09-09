#!/bin/bash

Rscript -e "saveRDS(read.csv('Data/institutions.csv'),'Cache/prepped_data.rds')"
shopt -s extglob
set -e

echo "Running \"Regular\" table"
Rscript scripts/tables/regular.R
echo "Running table_2"
Rscript scripts/tables/table_2.R
echo "Running table_3"
Rscript scripts/tables/table_3.R

for F in scripts/tables/!(preparation|regular|table_2|table_3).R
   do
      echo "Running $F"
      Rscript $F
   done
