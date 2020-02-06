
clear
export DELIM="======================================"
export DELIM_SHORT="==========================="

echo $DELIM
echo -e "\e[33mRunning the workflow\e[0m"
echo $DELIM

runscript(){
   echo -e "\e[33mRunning $1:\e[0m"

   Rscript "scripts/$1" && echo -e "\e[32m$1 succeeded\e[0m"

   if [ $? != 0 ]
      then echo -e "\e[31m$1 failed\e[0m"
      exit 1 
   fi
   echo $DELIM_SHORT
}

runscript regular.R
runscript battledeaths.R
runscript ISROC.R
runscript vcitable.R

echo $DELIM
echo -e "\e[32mRun complete!\e[0m"
echo $DELIM
echo "Making report..."
bash util/makeTabReport.sh
echo "Done!"
echo $DELIM
