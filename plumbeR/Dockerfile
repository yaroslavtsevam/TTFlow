FROM rocker/tidyverse:4.2.1

RUN apt-get update
RUN apt-get upgrade -y
RUN apt-get install libcurl4-openssl-dev -y
RUN apt-get install libxml2-dev -y
RUN apt-get install libssl-dev -y
RUN apt-get install libfontconfig1-dev -y
RUN apt-get install libudunits2-dev -y

RUN R -e "install.packages('plyr')"
RUN R -e "install.packages('taskscheduleR')"
RUN R -e "install.packages('tidyverse')"
RUN R -e "install.packages('tidyr')"
RUN R -e "install.packages('ggplot2')"
RUN R -e "install.packages('broom')"
RUN R -e "install.packages('lubridate')"
RUN R -e "install.packages('plumber')"
RUN R -e "install.packages('simplevis')"
RUN R -e "install.packages('ggsci')"
RUN R -e "install.packages('signal')"
RUN R -e "install.packages('prospectr')"
RUN R -e "install.packages('RCurl')"
RUN R -e "install.packages('bit64')"
RUN R -e "install.packages('Rcpp')"
RUN R -e "install.packages('DescTools')"
RUN R -e "install.packages('zoo')"
RUN R -e "install.packages('baytrends')"
RUN R -e "install.packages('future')"
RUN R -e "install.packages('dbplyr')"
RUN R -e "install.packages('RPostgres')"
RUN R -e "install.packages('RPostgreSQL')"
RUN R -e "install.packages('DBI')"

EXPOSE 8888