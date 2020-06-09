#!/bin/bash
if [[ $* = "--rmcache" ]] ;  then
   echo "Truncating cache..."
   rm Cache/*
else 
   echo "Using existing cache"
fi

shopt -s extglob

echo "Running data preparation"
Rscript scripts/preparation.R
echo "Running \"Regular\" table"
Rscript scripts/regular.R

echo "Running table_2"
Rscript scripts/table_2.R
echo "Running table_3"
Rscript scripts/table_3.R

for F in scripts/!(preparation|regular|table_2|table_3).R
   do
      echo "Running $F"
      Rscript $F
   done

for F in scripts/*.py
   do
      echo "Running $F"
      python3 $F
   done
