remove_common_duplicates = function(data){
  if(!is.null(data)){
    if(dim(data)[1]>1){
      duplicate_rows_num = data[,c(-1,-2,-4)] %>%
        duplicated %>% which
      if(length(duplicate_rows_num)>0){
        unique_data = data[-duplicate_rows_num,]
      } else {
        unique_data = data
      }
      return(unique_data)
    } else {return(data) }


  } else{
    return(data)
  }

}
