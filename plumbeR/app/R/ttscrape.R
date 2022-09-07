ttscrape <- function(IDs = NA,
                     metadata_file,
                     servers_list=c("http://naturetalkers.altervista.org/",
                                    "http://ittn.altervista.org/"),
                     custom_url = NA,
                     import_folder = NULL,
                     to_process = FALSE,
                     step_time = NULL
                    )
{
  library(signal)
  library(tidyr)
  library(ggplot2)
  library(tidyverse)
  library(zoo)
  library(prospectr)
  library(RCurl)
  library(bit64)
  library(lubridate)
  library(tidyverse)
  library(DescTools)
  source("/app/R/ttbandgapTcorrection.R")
  source("/app/R/removeduplicates.R")
  source("/app/R/ttbackup_read.R")
  source("/app/R/tttypes_to_list.R")
  #Specifying the urls for desired websites to be scraped
  if(is.na(IDs) & !missing(custom_url)){
  IDs = custom_url %>% str_remove("/ttcloud.txt") %>%
    str_trunc(8, "left", ellipsis = "")
  }
  if(!missing(custom_url)){
    urls  = custom_url
  } else {
    urls  =
      paste0(servers_list,
            rep(IDs,each = length(servers_list)),
            "/ttcloud.txt")
  }



  for( url in urls){
    if (RCurl::url.exists(url) == T){
      mydata_part <- data.table::fread(url,
                                   sep = ";",
                                   header = FALSE,
                                   fill = TRUE,
                                   integer64 = "numeric")
    } else {
      next()
    }

    if(exists("mydata_full")){

      varnum_full <- dim(mydata_full)[2]
      varnum_part <- dim(mydata_part)[2]
      if(varnum_full > varnum_part){
        mydata_part = cbind(mydata_part,
                            matrix(NA,
                                   nrow = dim(mydata_part)[1],
                                   ncol = varnum_full - varnum_part) %>%
                            as.data.frame()
                            )
        names(mydata_part) = names(mydata_full)
        mydata_full = rbind(mydata_full,mydata_part)
      } else {
        mydata_full = cbind(mydata_full,
                            matrix(NA,
                                   nrow = dim(mydata_full)[1],
                                   ncol = varnum_part - varnum_full) %>%
                              as.data.frame()
        )
        names(mydata_full) = names(mydata_part)
        mydata_full = rbind(mydata_full,mydata_part)
      }


    } else {
      mydata_full = mydata_part
    }
  }


  mydata = mydata_full[mydata_full$V1 != 0,]

  if(file.exists(metadata_file)){
    meta = data.table::fread(metadata_file,
                             sep = ",",
                             header = TRUE,
                             fill = TRUE,
                             integer64 = "numeric")
    print("Metadata file found and read")
    device_list = unique(meta$id)

  } else {
    print("Metadata file does not exist")
    device_list = NULL
  }

  TTdat = tttypes_to_list(mydata, device_list)
  print(dim(TTdat$mydata_4D))
  #verboseFlag = "con"
  if(!is.null(import_folder)){
    TTdat = tt_backup_import(import_folder, TTdat, device_list)
    print(dim(TTdat))
  }
  print(dim(TTdat$mydata_4D))




  return_list = list(IDs = IDs,
                     mydata = mydata,
                     metadata = as.data.frame(meta),
                     mydata_4D = TTdat$mydata_4D,
                     mydata_45 = TTdat$mydata_45,
                     mydata_49 = TTdat$mydata_49,
                     mydata_61 = TTdat$mydata_61,
                     mydata_60 = TTdat$mydata_60,
                     mydata_4B = TTdat$mydata_4B,
                     mydata_4C = TTdat$mydata_4C)
  data_type_present = check_datatype_present(TTdat$mydata_4D,
                                             TTdat$mydata_45,
                                             TTdat$mydata_49,
                                             TTdat$mydata_61,
                                             TTdat$mydata_60,
                                             TTdat$mydata_4B,
                                             TTdat$mydata_4C)
  return_list = return_list[c(T,T,T,data_type_present)]

  if(to_process == FALSE){

    return(return_list)

  } else {
    source("/app/R/ttfull.R")
    if(!is.null(step_time)){
      return(ttfull_calc(return_list, step_time = step_time))
    } else {
      return(ttfull_calc(return_list))
    }



    }


}









