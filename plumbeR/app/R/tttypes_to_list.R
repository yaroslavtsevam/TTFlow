check_datatype_present = function(mydata_4D,mydata_45,mydata_49,
                                 mydata_61,mydata_60, mydata_4B,mydata_4C){
  data_type_present = c(F,F,F,F,F,F,F)
  names(data_type_present) =  c("4D","45","49","61","60","4B","4C")
  if(nrow(mydata_4D) > 0){data_type_present["4D"] = T }
  if(nrow(mydata_45) > 0){data_type_present["45"] = T }
  if(nrow(mydata_49) > 0){data_type_present["49"] = T }
  if(nrow(mydata_61) > 0){data_type_present["61"] = T }
  if(nrow(mydata_60) > 0){data_type_present["60"] = T }
  if(nrow(mydata_4B) > 0){data_type_present["4B"] = T }
  if(nrow(mydata_4C) > 0){data_type_present["4C"] = T }
  return(data_type_present)
}


tttypes_to_list = function(mydata, device_list){
  mydata = as.data.frame(mydata)
  mydata_sep =
    separate(mydata,
             V1,
             into = c("SDate", "STime", "TT_ID"),
             sep = "[ ,]")
  data_type_present = c(F,F,F,F,F,F,F)
  names(data_type_present) = c("4D","45","49","61","60","4B","4C")

  mydata_sep = mydata_sep %>% dplyr::filter(TT_ID %in% device_list)
  #split the dataset
  mydata_4D <- mydata_sep[mydata_sep$V3 == "4D", ]
  mydata_4D <- Filter(function(x)!all(is.na(x)), mydata_4D)
  #convert possible integer64 to integer
  mydata_4D <- mydata_4D %>% mutate_if(bit64::is.integer64, as.integer)


  mydata_45 <- mydata_sep[mydata_sep$V3 == "45", ]
  mydata_45 <- Filter(function(x)!all(is.na(x)), mydata_45)
  #convert possible integer64 to integer
  mydata_45 <- mydata_45 %>% mutate_if(bit64::is.integer64, as.integer)



  mydata_49 <- mydata_sep[mydata_sep$V3 == "49", ]
  mydata_49 <- Filter(function(x)!all(is.na(x)), mydata_49)


  mydata_61 <- mydata_sep[mydata_sep$V3 == "61", ]
  mydata_61 <- Filter(function(x)!all(is.na(x)), mydata_61)



  mydata_60 <- mydata_sep[mydata_sep$V3 == "60", ]
  mydata_60 <- Filter(function(x)!all(is.na(x)), mydata_60)

  #the string 4B and 4C contain only TTcloud data
  mydata_4B <- mydata_sep[mydata_sep$V3 == "4B", ]
  mydata_4B <- Filter(function(x)!all(is.na(x)), mydata_4B)



  mydata_4C <- mydata_sep[mydata_sep$V3 == "4C", ]
  mydata_4C <- Filter(function(x)!all(is.na(x)), mydata_4C)


  data_type_present = check_datatype_present(mydata_4D,mydata_45,mydata_49,
                                   mydata_61,mydata_60, mydata_4B,mydata_4C)


  print("Data types present:")
  print(data_type_present)


  #TTv3
  header_4D <-
    c(
      "SDate",
      "STime",
      "TT_ID",
      "Rec_Nr",
      "Dev_Ty",
      "Timestamp",
      "Tref_0",
      "Theat_0",
      "growt_sens",
      "adc_bandgap",
      "Bits",
      "RH",
      "Tair",
      "gz_mean",
      "gz_sd",
      "gy_mean",
      "gy_sd",
      "gx_mean",
      "gx_sd",
      "Tref_1",
      "Theat_1",
      "MoistSensHz",
      "adc_Vbat"
    )
  #TTv2
  header_45 <-
    c(
      "SDate",
      "STime",
      "TT_ID",
      "Rec_Nr",
      "Dev_Ty",
      "Timestamp",
      "Tref_0",
      "Theat_0",
      "growt_sens",
      "adc_Vbat",
      "adc_bandgap",
      "RH",
      "Tair",
      "gx_mean",
      "gx_sd",
      "gy_mean",
      "gy_sd",
      "gz_mean",
      "gz_sd",
      "Tref_1",
      "Theat_1",
      "MoistSensHz"
    )

  #Spectra
  header_49 <-
    c(
      "SDate",
      "STime",
      "TT_ID",
      "Rec_Nr",
      "Dev_Ty",
      "Timestamp",
      "AS7263_610",
      "AS7263_680",
      "AS7263_730",
      "AS7263_760",
      "AS7263_810",
      "AS7263_860",
      "AS7262_450",
      "AS7262_500",
      "AS7262_550",
      "AS7262_570",
      "AS7262_600",
      "AS7262_650",
      "integration_T",
      "gain"
    )
  # TTG
  header_61 <-
    c(
      "SDate",
      "STime",
      "TT_ID",
      "Dev_Ty",
      "Timestamp",
      "adc_bandgap",
      "RH",
      "Tair",
      "g_min_x",   #minimum raw for acceleration on X axis (-16384 to +16383)
      "g_avg_x",   #average raw  for acceleration on X axis. (-16384 to +16383)
      "g_max_x",   #maximum raw  for acceleration on X axis. (-16384 to +16383)
      "g_sqr_x",   #acceleration squared standard deviation on X axis(65535 max)
      "g_min_y",   #minimum raw  for acceleration on Y axis.(-16384 to +16383)
      "g_avg_y",   #average raw  for acceleration on Y axis. (-16384 to +16383)
      "g_max_y",   #maximum raw  for acceleration on Y axis. (-16384 to +16383)
      "g_sqr_y",   #acceleration squared standard deviation on Y axis(65535 max)
      "g_min_z",   #minimum raw  for acceleration on Z axis. (-16384 to +16383)
      "g_avg_z",   #average raw  for acceleration on Z axis. (-16384 to +16383)
      "g_max_z",   #maximum raw  for acceleration on Z axis. (-16384 to +16383)
      "g_sqr_z",   #acceleration squared standard deviation on Z axis(65535 max)
      "adc_Vbat"   #battery voltage : adc raw value for battery voltage
      #measurement (this record appear on record 61 only)
    )

  header_60 <-
    c(
      "SDate",
      "STime",
      "TT_ID",
      "Dev_Ty",
      "Timestamp",
      "adc_bandgap",
      "RH",
      "Tair",
      "g_min_x",   #minimum raw for acceleration on X axis (-16384 to +16383)
      "g_avg_x",   #average raw  for acceleration on X axis. (-16384 to +16383)
      "g_max_x",   #maximum raw  for acceleration on X axis. (-16384 to +16383)
      "g_sqr_x",   #acceleration squared standard deviation on X axis(65535 max)
      "g_min_y",   #minimum raw  for acceleration on Y axis.(-16384 to +16383)
      "g_avg_y",   #average raw  for acceleration on Y axis. (-16384 to +16383)
      "g_max_y",   #maximum raw  for acceleration on Y axis. (-16384 to +16383)
      "g_sqr_y",   #acceleration squared standard deviation on Y axis(65535 max)
      "g_min_z",   #minimum raw  for acceleration on Z axis. (-16384 to +16383)
      "g_avg_z",   #average raw  for acceleration on Z axis. (-16384 to +16383)
      "g_max_z",   #maximum raw  for acceleration on Z axis. (-16384 to +16383)
      "g_sqr_z"   #acceleration squared standard deviation on Z axis(65535 max)
    )

  # Cloud
  header_4B <-
    c(

      "SDate",
      "STime",
      "TT_ID",
      "Rec_Nr",
      "Dev_Ty",
      "Timestamp",
      "Acc_Recs",
      "Recs_to_be_sent",
      "MCC_tel_op",
      "MNC_tel_op",
      "GSM_reg",
      "GSM_field",
      "Battery",
      "Firmware_ver"
    )

  header_4C <-
    c(
      "SDate",
      "STime",
      "TT_ID",
      "Rec_Nr",
      "Dev_Ty",
      "Timestamp",
      "TBL_locked",
      "n_first_sens",
      "RSSI_TT1",
      "RSSI_TT2",
      "RSSI_TT3",
      "RSSI_TT4",
      "RSSI_TT5",
      "RSSI_TT6",
      "RSSI_TT7",
      "RSSI_TT8",
      "RSSI_TT9",
      "RSSI_TT10",
      "RSSI_TT11",
      "RSSI_TT12",
      "RSSI_TT13",
      "RSSI_TT14",
      "RSSI_TT15",
      "RSSI_TT16",
      "RSSI_TT17",
      "RSSI_TT18",
      "RSSI_TT19",
      "RSSI_TT20",
      "RSSI_TT21",
      "RSSI_TT22",
      "RSSI_TT23",
      "RSSI_TT24"
    )


  if(data_type_present["4D"]){
    colnames(mydata_4D) <- header_4D
    mydata_4D$Timestamp <-
      as.POSIXct(as.integer(mydata_4D$Timestamp),
                 origin = "1970-01-01", tz="GMT")
  }
  if(data_type_present["45"]){
    colnames(mydata_45) <- header_45
    mydata_45$Timestamp <-
      as.POSIXct(as.integer(mydata_45$Timestamp),
                 origin = "1970-01-01", tz ="GMT")

  }
  if(data_type_present["49"]){

    colnames(mydata_49) <- header_49
    mydata_49$Timestamp <-
      as.POSIXct(as.integer(mydata_49$Timestamp),
                 origin = "1970-01-01", tz = "GMT")
  }
  if(data_type_present["60"]){
    colnames(mydata_60) <- header_60
    mydata_60$Timestamp <-
      as.POSIXct(as.integer(mydata_60$Timestamp),
                 origin = "1970-01-01", tz = "GMT")
  }
  if(data_type_present["61"]){
    colnames(mydata_61) <- header_61
    mydata_61$Timestamp <-
      as.POSIXct(mydata_61$Timestamp, origin = "1970-01-01", tz = "GMT")
  }
  if(data_type_present["4B"]){
    colnames(mydata_4B) <- header_4B
    mydata_4B$Timestamp <-
      as.POSIXct(as.integer(mydata_4B$Timestamp),
                 origin = "1970-01-01", tz ="GMT")
  }
  if(data_type_present["4C"]){
    header_4C <- header_4C[1:(dim(mydata_4C)[2])]
    colnames(mydata_4C) <- header_4C
    mydata_4C$Timestamp <-
      as.POSIXct(as.integer(mydata_4C$Timestamp)
                 , origin = "1970-01-01", tz ="GMT")
  }

  mydata_4D = remove_common_duplicates(mydata_4D)
  mydata_45 = remove_common_duplicates(mydata_45)
  mydata_49 = remove_common_duplicates(mydata_49)
  mydata_61 = remove_common_duplicates(mydata_61)
  mydata_60 = remove_common_duplicates(mydata_60)
  mydata_4B = remove_common_duplicates(mydata_4B)
  mydata_4C = remove_common_duplicates(mydata_4C)




  return(
    list(
      mydata_4D = mydata_4D,
      mydata_45 = mydata_45,
      mydata_49 = mydata_49,
      mydata_61 = mydata_61,
      mydata_60 = mydata_60,
      mydata_4B = mydata_4B,
      mydata_4C = mydata_4C

    )
  )

}

