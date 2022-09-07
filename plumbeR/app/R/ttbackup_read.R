tt_backup_import = function(import_folder_name, TTdat, device_list){
  if(!is.null(import_folder_name)){
    fun_log(verboseFlag = verboseFlag, c("Yep, there are some \n"))
    data_bc = data.frame()
    import_folder_name = paste("",import_folder_name,sep="")
    backup_file_list = list.files(import_folder_name,pattern="TT*")

    if(length(backup_file_list) < 1){
      fun_log(verboseFlag = verboseFlag, c("There is no files in backup folder,
                                           or they are not starting from TT
                                           letters","\n"))
    }
    for(file in backup_file_list ) {

      temp = suppressWarnings(suppressMessages(
        data.table::fread(paste0(import_folder_name,"/",file),
                          sep = ";",
                          header = FALSE,
                          fill = TRUE,
                          integer64 = "numeric")
      ))
      fun_log(verboseFlag = verboseFlag, c("Dimmension of the file", file,":",dim(temp)[1]," - rows, ",
                                           dim(temp)[2]," - columns.","\n"))
      data_bc = rbind(data_bc, temp)
    }


    fun_log(verboseFlag = verboseFlag, c("Binded array size", dim(data_bc)))
    #imported data have no server time,so we should add some
    data_bc = cbind(paste("10.05.27 12:55:19",data_bc$V1,sep=","), data_bc[,-1])

  }

  TTdati = tttypes_to_list(data_bc, device_list)


  for(data_type in names(TTdat)){

    tt_data = TTdat[[data_type]]
    tt_bc = TTdati[[data_type]]
    timestep = 3600

    #Adding flagging variable showing that this data was obtained from impport
    tt_bc$imported = T
    tt_data$imported = F
    fun_log(verboseFlag = verboseFlag,
            c("Basic calculated array size",
            dim(tt_bc),"\n"))
    #Inserting exported(backuped) from TT data into server data,
    #Very slow and stupid way, but there were no time for elegance
    #
    tt_imported = data.frame()
    # logging cases into file or memory
    log_imp = data.frame(id = "first", starts = NA, ends = NA,
                         var = 0, tt = 0,bc = 0,stringsAsFactors = FALSE)

    if(dim(tt_bc)[1] < 10){ next() }

    for (ids in unique(tt_data$TT_ID)){
      fun_log(verboseFlag = verboseFlag, c("Importing for TT:",ids,"\n"))

      bc = tt_bc %>% dplyr::filter(TT_ID == ids) %>% as.data.frame
      tt = tt_data %>% dplyr::filter(TT_ID == ids) %>% as.data.frame
      fun_log(verboseFlag = verboseFlag, c("Data from the TT ", ids," imported has ",
                                           dim(bc)[1]," - rows, ",dim(bc)[2]," - columns.","\n" ))
      fun_log(verboseFlag = verboseFlag, c("Data from the TT ", ids," on server has ",
                                           dim(tt)[1]," - rows, ",dim(tt)[2]," - columns.","\n" ))
      fun_log(verboseFlag = verboseFlag, c("Starting import of directly extracted from TT data","\n"))
      u = tt

      if(length(tt$volt)>0){


        badnames = c("SDate","STime","TT_ID","Rec_Nr","Dev_Ty","Timestamp","imported")
        names_for_compare = names(tt)[!(names(tt) %in% badnames)]
        frank=data.frame()
        logs=data.frame()

        if(dim(bc)[1]>10){ #if imported data from tt is to small in most cases it's crappy and better just to skip it
          matches = match(do.call("paste",tt[, names_for_compare]),
                          do.call("paste", bc[, names_for_compare]))
          msize = length(matches)
          edges  = c((is.na(matches[2:msize]) - is.na(matches[1:(msize-1)])),0)

          if(any(edges == -1) & any(edges == 1) ) {
            starts = which(edges == -1)
            ends = which(edges == 1)
            frank = rbind(tt[1:starts,], bc, tt[ends:msize,])
            #logs = data.frame(id = as.character(ids), starts =  starts, ends = ends,
            #                  var = 1, tt = dim(tt)[1],bc = dim(bc)[1],stringsAsFactors = FALSE)

            fun_log(verboseFlag = verboseFlag, c("Case 1 intersect, start is ", starts," end is ",ends,"bc size ",dim(bc)[1]," tt size ",dim(tt)[1],"\n"))
          } else {
            if(any(edges == -1)){
              starts = which(edges==-1)
              frank = rbind(tt[1:starts,], bc)
              #logs = data.frame(id = as.character(ids), starts = starts, ends = NA,
              #                  var = 2, tt = dim(tt)[1],bc = dim(bc)[1],stringsAsFactors = FALSE)
              fun_log(verboseFlag = verboseFlag, c("Case 2 add to end, start is ", starts,"bc size ",dim(bc)[1]," tt size ",dim(tt)[1],"\n"))
            }
            if(any(edges == 1)){
              ends = which(edges==1)
              frank = rbind( bc, tt[ends:msize,])
              #logs = data.frame(id = as.character(ids), starts = NA, ends = ends,
              #                 var = 3, tt = dim(tt)[1],bc = dim(bc)[1],stringsAsFactors = FALSE)
              fun_log(verboseFlag = verboseFlag, c("Case 3 add to begin, end is ",ends,"bc size ",dim(bc)[1]," tt size ",dim(tt)[1],"\n"))

            }
          }

        } else {
          frank = tt
          #logs = data.frame(id = as.character(ids), starts = NA, ends = NA,
          #                  var = 0, tt = dim(tt)[1],bc = dim(bc)[1], stringsAsFactors = FALSE)
          fun_log(verboseFlag = verboseFlag, c("Case 0 no backup","\n"))
        }
        u=frank

        #fun_log(verboseFlag = verboseFlag, c("Found ", matches %>% length,
        #    " matches between importing and server data for TT ", ids,"\n")


      } else {
        u = bc
        #logs = c(id = ids, starts = NA, ends = NA, var = 4)
        fun_log(verboseFlag = verboseFlag, c("Case 4, only backup result is ", dim(u)[1],"\n"))
      }

      tt_imported = rbind(tt_imported,u)

      #log_imp = rbind(log_imp,logs)

    } # end of cycling through ids instruction
    TTdat[[data_type]] = tt_imported

  }
  return(TTdat)
}
