#* @get /getdatafor/<server>/<cloud>
function(server="s",cloud="s"){
    metadatalink = "R/TT_metadata.csv"
    #if(url=="") {print("No CLOUD url given")}
    #TTdat = ttscrape(
    #    metadata_file = metadatalink,
    #    custom_url = url,
    #    to_process = toprocess)
    print(metadatalink)
    print(paste("server=",server,"cloud=",cloud))
}
