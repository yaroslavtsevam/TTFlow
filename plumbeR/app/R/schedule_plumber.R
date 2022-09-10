


#* @post /cloud/<cloud>
#* @serializer print
function(cloud) {
  
url = paste0("http://naturetalkers.altervista.org/",cloud,"/ttcloud.txt")
TTdat = ttscrape(
  metadata_file = "/app/R/TT_metadata.csv",
  custom_url = url,
  to_process = T)
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
    dbWriteTable(con, Id(schema = "test_cloud", table = "iris_test"), value=data, overwrite=TRUE)  

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

    test_data = dbReadTable(con, 'test_iris_table')                               
    return(test_data)
}

