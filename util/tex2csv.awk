#!/usr/bin/awk -f
/&/ {gsub("&",","); gsub("\\",""); print}
