
fun_log = function(verboseFlag = "mem", message){

  if (verboseFlag == "con"){
    cat(message)
  }
  if (verboseFlag == "mem"){
    assign(message_log,
           c("message_log", paste(message,collapse=" ")),
           env = globalenv())
  }
  if (verboseFlag == "file"){
    file_name = paste("logs//",
                      Sys.Date(),
                      "-calc-loging.txt",
                      collapse = "" )

    write( paste(message,collapse = ""),
           file = file_name,
           append = TRUE)
  }
}

ischarged = function(data){
  if(length(data$volt)>2){
    charged = c(F,(data$volt[2:length(data$volt)] -
                     data$volt[1:(length(data$volt)-1)]) > 0.05)
  } else {
    charged = F
  }
  charged
}

mark_continious_serv_time = function(data){
  #is.null(data) %>% print
  wst = which(!data$wrong_server)
  if (length(wst)>2){
    wst_cont = (wst[2:length(wst)]-wst[1:(length(wst)-1)]<2)[-1] &
      (wst[2:(length(wst)-1)]-wst[3:(length(wst))]>-2)
    data$wrong_server = T
    data$wrong_server[wst[c(F,wst_cont,F)]] = F
  }
  data$wrong_server = T
  return(data)
}

#Check measure pediod (step time) per one tt per charge=========================
check_measure_period = function(temp_data, verboseFlag="con"){
  real_time = which(!temp_data$time%>%is.na)
  if(real_time%>% length >1){
    mt = as.integer(temp_data$time[real_time])
    rti = as.integer(real_time)
    model = lm(mt~rti)
    step_time = plyr::round_any(summary(model)[[4]][2,1], 100)
    fun_log(verboseFlag = verboseFlag, c("Estimated step time is ",step_time))
    if(step_time < 4000) {step_time=3600}
    if(step_time > 3999) {step_time=5400}
    fun_log(verboseFlag = verboseFlag, c(" will be used step time ", step_time, "\n"))

  } else {
    step_time = 3600
  }
  return(step_time)
}
#Time correction - good old=====================================================
extrapolate_tt_date = function(tt_one, verboseFlag){
  fun_log(verboseFlag = verboseFlag, c("Starting data extrapolation \n"))
  if(tt_one %>% nrow <2 ){
    #print(tt_one)
  }
  if(tt_one$volt %>% length > 2){
    fun_log(verboseFlag = verboseFlag, c("Starting time correction for ", tt_one$id %>% unique,"\n"))
    bat_grow = c(F,
                 tt_one$volt[2:(nrow(tt_one))] - tt_one$volt[1:(nrow(tt_one)-1)] > 0.3)
    fun_log(verboseFlag = verboseFlag, c("Found ",which(bat_grow) %>% length,"recharges","\n"))

    tt_one$charge = cumsum(bat_grow)+1
    fun_log(verboseFlag = verboseFlag, c("So, charges states are", tt_one$charge %>% unique(),"\n"))

    tt_one$time = NA
    tt_one$corr_time = 1

    for (ci in tt_one$charge %>% unique)  {

      #temp_data = tt_one[tt_one$charge == ci,]
      temp_data = tt_one %>% dplyr::filter(charge == ci)
      #print(names(temp_data))
      temp_data$time[!temp_data$wrong_time] =
        temp_data$datetime[!temp_data$wrong_time]

      fun_log(verboseFlag = verboseFlag, c("Correct dates ",which(!temp_data$wrong_time) %>% length,
                                           "correct times ",which(!is.na(temp_data$time)) %>% length,"\n"))
      temp_data$corr_time[!temp_data$wrong_time] = 2

      check_serv_time_too_high_according_it_record = function(x, dt){

        # Check if record number of record with current serv_time is
        #  bigger of any record number of measurements with correct unix time
        #  which is less than current serv_time

        # Index of elemnt in vector of correct unix time whcih are
        # less than current serv_time
        corr_unix_time_index = which(dt$serv_datetime[x] >
                                       dt$datetime[!dt$wrong_time])
        #Correct unix time which are less than current serv_time
        if(length(corr_unix_time_index) == length(datetime)) {
          corr_unix_time = dt$datetime
        } else {
          corr_unix_time = dt$datetime[!dt$wrong_time[corr_unix_time_index]]
        }

        # Indexes of corr_unix_time elemnts in whole datetime variable
        datetime_index = which(dt$datetime %in% corr_unix_time)
        # If any record numbers of measurement with correct unix time, which
        # is less than the current serv_time, is bigger of record number of
        # current serv_time than this serv_time is INCORRECT
        return (any(dt$rec_num[datetime_index] > dt$rec_num[x]))
      }

      if (any(!temp_data$wrong_server)){
        cor_serv_records = which(!temp_data$wrong_server)
        wrong_serv_records = cor_serv_records[cor_serv_records %>%
                                                map_lgl(check_serv_time_too_high_according_it_record, dt = tt_one)]
        temp_data$wrong_server[wrong_serv_records] = T
      }


      if(length(temp_data$serv_datetime[temp_data$wrong_server == F])>0){
        # if there is any correct date in charge period don't use serv_date
        # but for the case when datetime is wrong use correct server time
        if (!any(!temp_data$time%>%is.na)) {
          only_server_time_ok = temp_data$wrong_time ==
            T & temp_data$wrong_server == F
          temp_data$time[only_server_time_ok] =
            temp_data$serv_datetime[only_server_time_ok]
          temp_data$corr_time[only_server_time_ok] = 3
        }

      }
      temp_data$corr_time[temp_data$datetime <
                            157680000 & temp_data$time %>% is.na] = 4
      temp_data$lost_connect = F
      temp_data$lost_connect[temp_data$datetime <
                               157680000 & temp_data$time %>% is.na] = T



      real_time = which(!is.na(temp_data$time))
      step_time = check_measure_period(temp_data, verboseFlag)
      tt_one$step_time = step_time
      fun_log(verboseFlag = verboseFlag, c("Charge:",ci," step time:",step_time,
                                           ", correct dates found",real_time %>% length,"\n"))
      #If there are any correct time inside this data
      if (real_time %>% length > 1){

        #If last elemnt do not have correct time - set it as correct
        if (real_time[length(real_time)] != length(temp_data$time)) {
          real_time = c(real_time,length(temp_data$time))
        }

        #Extrapolating back from the first elemet with correct time
        for( i in (real_time[1]-1):1) {
          temp_data$time[i] = temp_data$time[i+1]-step_time
        }

        fun_log(verboseFlag = verboseFlag, c("Found measurements with correct timestemp:",
                                             length(real_time)-1, "\n"))
        for( i in 1:(length(real_time)-1))  {
          if(real_time[i+1] == real_time[i]+1){
            next(i)
          }
          for( j in real_time[i]:(real_time[i+1]-2)){
            temp_data$time[j+1] = temp_data$time[j]+step_time
          }
        }
        # Since freaking tibble became very strict
        tt_one = tt_one %>% as.data.frame()
        temp_data = temp_data %>% as.data.frame()

        tt_one[tt_one$charge == ci,] = temp_data
        fun_log(verboseFlag = verboseFlag, c(tt_one$id %>% unique(), " charge ", ci,"was filled","\n"))
        fun_log(verboseFlag = verboseFlag, c(min(tt_one$time), " - ", max(tt_one$time), "\n"))

      }

    }


    tt_one$time = tt_one$time %>% as.POSIXct(origin="1970-01-01 00:00:00",
                                             tz="Europe/Moscow")
  }
  else {
    tt_one$charge=1
    tt_one$time = NA

  }

  return(tt_one)
}


correct_extrap_date = function(tt_one, verboseFlag){
  fun_log(verboseFlag = verboseFlag, c("TT ", tt_one$id %>% unique, " fixing wrong corrections","\n"))
  #Check if date goes back in time with with growing row nunber
  ends = nrow(tt_one)

  if(ends <1) {return(tt_one)}
  bad_extrap_index =c(F,(tt_one$time[2:ends] - tt_one$time[1:(ends-1)])<0)
  # If tt data is very small in size previous step can generate
  # bad_extrap_index longer than the tt data itself, so NA produced
  # lets remove them
  bad_extrap_index = na.exclude(bad_extrap_index)
  real_bad_extrap_index = which(bad_extrap_index)
  for (bi in which(bad_extrap_index)) {
    before_problem = bi - 1
    time_before_drop = tt_one$time[before_problem]
    real_problem = which(c(rep(F, bi),tt_one$time[bi:nrow(tt_one)] < time_before_drop))#wrong
    real_bad_extrap_index = c(real_bad_extrap_index,real_problem)
  }
  bad_extrap_index = rep(F,length(tt_one$time))
  bad_extrap_index[real_bad_extrap_index] = T
  bad_extrap_index = bad_extrap_index[1:nrow(tt_one)]
  #Set this times and datetimes to 0
  fun_log(verboseFlag = verboseFlag, c("Found ",which(bad_extrap_index)%>% length," bad extrapolation, fixing","\n" ))
  #print(bad_extrap_index)
  tt_one$time[bad_extrap_index]         = NA
  tt_one$wrong_time[bad_extrap_index]   = TRUE
  tt_one$wrong_server[bad_extrap_index] = TRUE
  #tt_one$datetime[bad_extrap_index] = as.POSIXct(0, origin="1970-01-01 00:00:00", tz="Europe/Moscow")
  #tt_one$serv_datetime[bad_extrap_index] = as.POSIXct(0, origin="1970-01-01 00:00:00", tz="Europe/Moscow")
  fun_log(verboseFlag = verboseFlag, c("Removed incorrect time indexes","\n"))
  #Restart extrapolation
  for (ci in tt_one$charge %>% unique){
    temp_data = tt_one %>% dplyr::filter(charge == ci)
    if(!any(is.na(temp_data$time))) {
      fun_log(verboseFlag = verboseFlag, c("Nothing to correct in ",ci," charge, skipping","\n"))
      next(ci)
    }
    real_time = which(!is.na(temp_data$time))
    step_time = temp_data$step_time[1]
    fun_log(verboseFlag = verboseFlag, c("Charge:",ci," step time:",step_time,
                                         ", correct dates found",real_time %>% length,"\n"))
    #If there are any correct time inside this data
    if (real_time %>% length > 1){
      #If last elemnt do not have correct time - set it as correct
      if (real_time[length(real_time)] != length(temp_data$time)) {
        real_time = c(real_time,length(temp_data$time))
      }
      if (real_time[1]>2){
        #Extrapolating back from the first elemet with correct time
        fun_log(verboseFlag = verboseFlag, c("Filling backward\n"))
        for( i in (real_time[1]-1):1) {
          temp_data$time[i] = temp_data$time[i+1]-step_time
        }
      }
      fun_log(verboseFlag = verboseFlag, c("Found measurements with correct timestamp:",
                                           length(real_time)-1, "\n"))
      for( i in 1:(length(real_time)-1)) {
        if(real_time[i+1] == real_time[i]+1){
          next(i)
        }
        for( j in real_time[i]:(real_time[i+1]-2)) {
          temp_data$time[j+1] = temp_data$time[j]+step_time
        }
      }

      tt_one[tt_one$charge == ci,] = temp_data
      fun_log(verboseFlag = verboseFlag, c(tt_one$id %>% unique(), " charge ", ci,"was filled","\n"))
      fun_log(verboseFlag = verboseFlag, c("Time desciption:",
                                           max(tt_one$time,na.rm = T),
                                           " - ",
                                           min(tt_one$time, na.rm = T),
                                           "\n"))
    }
  }
  tt_one$time = tt_one$time %>% as.POSIXct(origin="1970-01-01 00:00:00",tz="Europe/Moscow")
  return(tt_one)
}

correct_time_ts_shift_matching = function(data, verboseFlag){
  #data = tt_data_ec
  fun_log(verboseFlag = verboseFlag, c("Starting correct_time_ts_shift_matching \n"))
  abs.max.ccf <- function(a,b) {
    d <- ccf(a, b, plot=FALSE, lag.max=length(a)-5)
    cor <- d$acf[,,1]
    abscor <- abs(d$acf[,,1])
    lag <- d$lag[,,1]
    abs.cor.max <- abscor[which.max(abscor)]
    abs.cor.max.lag <- lag[which.max(abscor)]
    return(c( abs.cor.max, abs.cor.max.lag))
  }

  lag_table = data.frame()
  for(icharge in 1:6){
    for(iid in (data$id %>% unique)) {
      ttsi = data %>% dplyr::filter(id == iid, charge == icharge)
      if(nrow(ttsi)<64){ next()}
      for(jid in (data$id %>% unique)) {
        if(jid == iid){next()}
        ttsj = data %>% dplyr::filter(id == jid, charge == icharge)
        if(nrow(ttsj)<64){ next() }
        if(all(is.na(ttsj$time))) { next()}
        cor_time = which(!is.na(ttsj$time)) %>% length
        tsi = ts(ttsi$tair, start=1, end = nrow(ttsi), frequency = 1)
        tsj = ts(ttsj$tair, start=1, end = nrow(ttsj), frequency = 1)
        lag = abs.max.ccf(tsi,tsj)
        lag_row = data.frame(iid, jid, icharge, cor=lag[1], lag = lag[2], cor_time)
        lag_table = rbind(lag_table, lag_row)
      }
    }
  }
  if(nrow(lag_table)<1) {
    fun_log(verboseFlag = verboseFlag, c("Very strange data, returning without lag correction \n"))
    return(data)
  }
  result_table = lag_table %>% group_by(iid, icharge) %>%  summarise(
    MaxCor = max(cor), jid = jid[which.max(cor)], lag = lag[which.max(cor)],
    cor_time = cor_time[which.max(cor)]
  ) %>% as.data.frame()


  datac = data
  foreach(i = 1:nrow(result_table)) %do% {
    iid = result_table$iid[i]
    jid = result_table$jid[i]
    lag = result_table$lag[i]
    icharge = result_table$icharge[i]
    if(all(is.na(
      datac%>%filter(id == iid, charge == icharge) %>% select(time)))){

      subj = datac%>% dplyr::filter(id == jid & charge == icharge)
      subi = datac%>% dplyr::filter(id == iid & charge == icharge)
      time_index = match(subi$rn,subj$rn+lag)
      subi$time = subj$time[time_index]
      datac[datac$id == iid & datac$charge == icharge,] = subi
    }

  }
  fun_log(verboseFlag = verboseFlag, c("Starting  correct_extrap_date inside correct_time_ts_shift_matching \n"))

  #print(datac %>% group_by(id) %>% summarise(n = n()))

  dataec = datac  %>% group_by(id) %>%
    do(correct_extrap_date(., verboseFlag)) %>% as.data.frame

  fun_log(verboseFlag = verboseFlag, c("Stopped correct_time_ts_shift_matching \n"))
  return(dataec)

  # ggplot()+
  #   geom_point(data = filter(data, id=="218A0060",  charge == 1), aes(x= rn, y=tair), color = 1)+
  #   geom_point(data = filter(data, id=="218A0178",  charge == 1), aes(x= rn-23, y=tair), color = 2)
  # ggplot(data = dataec)+
  #   geom_point(aes(x=rn, y=tair, color=charge,shape=is.na(time)),size=.1)+
  #   facet_wrap(~id)
}

extrapolate_dates = function(data, verboseFlag="con", step_time=NULL){
  #print(data)
  data$datetime = data$Timestamp
  data$time = data$datetime
  data$id = data$TT_ID
  data$rn = data$Rec_Nr
  data$volt  = data$Bat_mV/1000
  if(is.null(step_time)){
    timestep = check_measure_period(data)
  }else{
    timestep = step_time
  }

  verboseFlag = "con"
  #print(names(data))
  data = data %>% group_by(id) %>% arrange(rn) %>%
    mutate(
      serv_datetime = dmy_hms(paste0(data$SDate," ",
                      as.character(data$STime))),

      cor_dt = (datetime > min(serv_datetime)) &
             (datetime < max(serv_datetime))
  )
  data=data %>% mutate(wrong_time=(datetime<157680000 | datetime>serv_datetime))
  tt_imported = as.data.frame(data)
  #print(head(tt_imported))
  # Marking server time wrong if there are more than one measurement per hour
  tt_imported$years = year(tt_imported$serv_datetime)
  tt_imported$doys  = yday(tt_imported$serv_datetime)
  tt_imported$hours = hour(tt_imported$serv_datetime)
  #tt_imported = lazy_dt(tt_imported)
  tt_imported  = tt_imported  %>% group_by(id,years,doys, hours) %>%
    mutate(wrong_server = length(doys)>1) %>%  as.data.frame
  #Marking recharge
  #tt_imported = lazy_dt(tt_imported)
  tt_imported = tt_imported %>% group_by(id) %>% arrange(rn) %>%
    mutate(recharge_flag = ischarged(.data))
  #Calculating charge cycles
  tt_imported = tt_imported %>% group_by(id) %>% arrange(rn) %>%
    mutate(charge = cumsum(recharge_flag))
  #Simple detect of clearly wrong datetimes
  tt_imported = tt_imported %>% group_by(id) %>% arrange(rn) %>%
    mutate(cor_dt = (datetime > min(serv_datetime)) &
             (datetime < max(serv_datetime))
    )
  #Detecting correct server time - first assumption -
  #it should one measurement per hour
  tt_imported = tt_imported %>% group_by(id, years, doys,hours) %>%
    mutate(serv_cor = length(serv_datetime) < 2)
  #timestep = 3600
  #tt_imported = tt_imported %>% group_by(id, charge) %>%
  #  do(extrapolate_dates(., timestep))%>% as.data.frame


  tt_imported = tt_imported %>% group_by(id) %>%
    do(mark_continious_serv_time(.)) %>% as.data.frame
  #tt_imported = lazy_dt(tt_imported)
  tt_imported = tt_imported %>% group_by(id) %>%
    do(mark_continious_serv_time(.)) %>% as.data.frame


  # SITE_list = suppressWarnings(suppressMessages(
  #   read_delim(descr_file,col_names = T, delim=",")))
  # if (!is.null(sitename)){
  #   SITE_list = SITE_list%>%filter(Site == sitename)
  #   if(SITE_list %>% length <1) {
  #     fun_log(verboseFlag = verboseFlag, c(
  #       "Looks like you have error in site name.\n"
  #     ))
  #   }
  # }
  #


  #tt_imported = tt_imported  %>% filter(id %in% SITE_list$id)
  #print(names(tt_imported))
  if(tt_imported %>% nrow <1) {
    fun_log(verboseFlag = verboseFlag, c(
      "Looks like you have error in site name.\n"
    ))
  }
  fun_log(verboseFlag = verboseFlag, c("Starting extrapolation of dates \n"))
  tt_data_e = tt_imported  %>% group_by(id) %>%
    do(extrapolate_tt_date(., verboseFlag)) %>% as.data.frame


  fun_log(verboseFlag = verboseFlag, c("Starting correction of extrapolated dates \n"))
  tt_data_ec = tt_data_e  %>% group_by(id) %>%
    do(correct_extrap_date(., verboseFlag)) %>% as.data.frame


  fun_log(verboseFlag = verboseFlag, c("Starting correct_time_ts_shift_matching of extrapolated dates \n"))
  tt_data_ec = correct_time_ts_shift_matching(tt_data_ec, verboseFlag)



  data = tt_data_ec %>% mutate(Timestamp = time) %>%
                  select(-volt,-rn,-time,-datetime,
                         -serv_datetime,-cor_dt,-wrong_server,
                         -id, -wrong_time, -wrong_server,
                         -recharge_flag,-serv_cor)
  return(data)
}



