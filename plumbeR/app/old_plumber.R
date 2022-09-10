library(lubridate)
#library(plumber)
#source("/app/R/ttscrape.R")
# 'plumber.R' is the location of the file shown above
#pr_run(pr("/app/R/server.R"), port=8888) 
myscript = "/app/R/schedule.R"
#taskscheduler_create(taskname = "hourly_task", starttime = "8:40",
#                     rscript = myscript,
#                     schedule = "MINUTE")
repeat{
  if(lubridate::minute(now(tzone = "")) %% 23 == 0) {
    source(myscript)
  }
}            