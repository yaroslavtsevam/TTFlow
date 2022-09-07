### Exporting to csv============================================================
library(stringr)
ttexport_csv = function(TTdat, one_year=NULL){
  sitename = TTdat$metadata$Site %>% unique()
  sitename = sitename[1]
  sitename = sitename %>% str_replace_all(" ","_")
  if(!is.null(one_year)){
   mydata_exp = TTdat$mydata_exp
   mydata_exp = mydata_exp %>% dplyr::filter(years == one_year)
  } else {
    mydata_exp = TTdat$mydata_exp
  }

  max_date = max(mydata_exp$Timestamp, na.rm = T) %>% as.character() %>%
            str_trunc(7,"right", ellipsis = "")
  min_date = min(mydata_exp$Timestamp, na.rm = T) %>% as.character() %>%
    str_trunc(7,"right", ellipsis = "")

  cloud_name = mydata_exp$CloudName %>% unique()
  filename = paste0(sitename,"_",min_date,"_",max_date,"_",cloud_name,".csv")


  write.csv(mydata_exp, file = filename)
}
