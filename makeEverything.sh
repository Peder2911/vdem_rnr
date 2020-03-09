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

for F in scripts/!(preparation|regular).R
   do
      echo "Running $F"
      Rscript $F
   done

for F in scripts/*.py
   do
      echo "Running $F"
      python3 $F
   done
