source("/app/R/ttscrape.R")
source("/app/R/ttexport.R")
TTdat = ttscrape(
  metadata_file = "/app/R/TT_metadata.csv",
  custom_url = "http://naturetalkers.altervista.org/C18AC097/ttcloud.txt",
  to_process = T)

write.csv(TTdat$mydata_exp, file="/app/data/skb_data.csv")