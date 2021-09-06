get_priori <- function( wt_query, table_priori, dbconfig = NULL ) {
    library(dplyr)
    query <- paste0("SELECT * FROM ", table_priori)
    r <- db_query(query = query, db_config = dbconfig)
    priori <- r$data
    priori <- priori[priori$ld_id == wt_query$ld_id, ]
    priori <- priori[grep(paste0("^",substr(wt_query$fault,1,3)),priori$fault),]
    priori$date_ini <- as.POSIXct(priori$date_ini, tz = "UTC", origin = "1970-01-01")
    priori <- arrange(priori, date_ini)
}