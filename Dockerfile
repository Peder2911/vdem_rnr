
FROM rocker/tidyverse:3.6.3

RUN apt update
RUN apt install python3
RUN apt install python3-pip -y

RUN mkdir /institutions
COPY . /institutions
WORKDIR /institutions

RUN ["bash","findDeps.sh"]
CMD ["bash","./makeEverything.sh"]
