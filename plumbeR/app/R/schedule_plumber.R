CreateSchemaifnotExistQuery = function(schema_name){
      return(paste0('CREATE SCHEMA IF NOT EXISTS "',schema_name,'"'))
}
UploadCalculatedCloudDataToDB = function(cloud){
    cloud = stringr::str_to_upper(cloud)
    url = paste0("http://naturetalkers.altervista.org/",cloud,"/ttcloud.txt")

    TTdat = ttscrape(
      metadata_file = "/app/R/TT_metadata.csv",
      custom_url = url,
      to_process = T)
    library(RPostgres)
    library(RPostgreSQL)
    library(DBI)
    
    DBI::dbDriver("PostgreSQL")
    drv <- dbDriver("PostgreSQL")
    # create a connection to the postgres database
    con <- RPostgreSQL::dbConnect(drv, dbname = "hello_flask_prod",
                                host = "postgre-db", port = 5432,
                                user = "hello_flask", password = "hello_flask")
    print("DB connected!")
    dbGetQuery(con, CreateSchemaifnotExistQuery(cloud))
    for(table_name in names(TTdat)){
      print(table_name)
      print(TTdat[[table_name]]) 
      if(is.null(TTdat[[table_name]])){next()}      
      RPostgreSQL::dbWriteTable(con, c(schema = cloud, table = table_name), value=TTdat[[table_name]], overwrite=TRUE)  
    }
}


#* Promise Example
#* @get /cloud/<cloud>
#* @serializer json
function(res,cloud) {
  
  future_promise({
    print("Calculation started...")
    #UploadCalculatedCloudDataToDB(cloud)
    print("Calculation is over!")
  }) %>%
    finally(~ {
      ## Email to alert user data is ready
      #print(data$mydata_p)
      UploadCalculatedCloudDataToDB(cloud)
      print("Finally")
    })
  
    ## While promise is evaluating, return a 200
    msg <- "The request has been queued. You will receive an email when the process is complete."
    res$status <- 200
    return(list(success = msg))

}


#* @get /hello
#* @serializer html
function() {
print("hello")
}


#* @get /createDBtable
function() {
    print("Created Table")
    library(RPostgreSQL)
    library(dbplyr)

    # load the PostgreSQL driver
    drv <- dbDriver("PostgreSQL")

    # create a connection to the postgres database
    con <- RPostgreSQL::dbConnect(drv, dbname = "hello_flask_prod",
                                host = "postgre-db", port = 5432,
                                user = "hello_flask", password = "hello_flask")
    data = iris
    dbGetQuery(con, CreateSchemaifnotExistQuery("test_cloud"))
    dbWriteTable(con, c(schema = "test_cloud", table = "iris_test"), value=data, overwrite=TRUE)  

}

#* @get /testDBtable
#* @serializer print
function() {
 
    library(RPostgreSQL)
    library(dbplyr)

    # load the PostgreSQL driver
    drv <- dbDriver("PostgreSQL")

    # create a connection to the postgres database
    con <- RPostgreSQL::dbConnect(drv, dbname = "hello_flask_prod",
                                host = "postgre-db", port = 5432,
                                user = "hello_flask", password = "hello_flask")

    test_data = dbReadTable(con, c(schema = "test_cloud", table = "iris_test"))                               
    return(test_data)
}

