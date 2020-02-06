
echo "<head><style>$(cat Static/reportstyle.css)</style></head>" > Out/report.html
echo "</body><h1>Report tables</h1>" >> Out/report.html
for f in Out/*.tex
do
   echo "<h2>$f</h2>" >> Out/report.html

   cat $f |\
   ./util/tex2csv.awk |\
   r -d -e "writeLines(knitr::kable(X,format='html'))" |\
   sed 's/[\$\{\}]//g'>>\
   Out/report.html
done
echo "</body>" >> Out/report.html

