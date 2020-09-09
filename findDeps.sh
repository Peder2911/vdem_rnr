set -e 

pip3 install -r requirements.txt

Rscript -e "install.packages('devtools')"

for pkg in evallib imlib timelib
do 
   Rscript -e "devtools::install_github('peder2911/$pkg')"
done

find . -name "*.R" -exec perl -ne 'print "$1\n" if /(?<=library\()([^\)]+)/' {} + \
   |awk '!/parallel/&&!/splines/&&!/timelib/&&!/evallib/&&!/imlib/'\
   |awk '{$1=$1;print}'\
   |sort|uniq\
   |Rscript -e "install.packages(readLines(file('stdin')))"
   
