library(plumber)
library(dbplyr)
library(RPostgres)
library(DBI)
library(promises)
library(future)
library(RPostgreSQL)
plan(multisession)

source("/app/R/ttscrape.R")
source("/app/R/ttexport.R")
#library(plumber)
#source("/app/R/ttscrape.R")
# 'plumber.R' is the location of the file shown above
#pr_run(pr("/app/R/server.R"), port=8888) 
#myscript = "/app/R/schedule.R"
#taskscheduler_create(taskname = "hourly_task", starttime = "8:40",
#                     rscript = myscript,
#                     schedule = "MINUTE")
#repeat{
#  if(lubridate::minute(now(tzone = "")) %% 23 == 0) {
#    source(myscript)
#  }
#}

pr("/app/R/schedule_plumber.R") %>% pr_run(host = "0.0.0.0", port=8888)