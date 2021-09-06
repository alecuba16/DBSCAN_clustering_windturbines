normalize <- function(x, get_min_max = FALSE, x_name = NULL, dbconfig = NULL, park_info = NULL) {
    if(!get_min_max || is.null(x_name) || is.null(dbconfig) || is.null(park_info)) {
        mini <- min(x, na.rm = TRUE)
        maxi <- max(x, na.rm = TRUE)
    } else {
        query <- paste0("SELECT * FROM ", park_info$range_table_name, " WHERE var ='",x_name,"' GROUP BY var")
        rs <- db_query(query = query, db_config = dbconfig)
        rs <- rs$data
        if(nrow(rs) > 0) {
            mini <- rs$min_manual
            if(is.na(mini))
                mini <- rs$min_calc
            if(is.na(mini))
                mini <- min(x, na.rm = TRUE)
            maxi <- rs$max_manual
            if(is.na(maxi))
                maxi <- rs$max_calc
            if(is.na(maxi))
                maxi <- max(x, na.rm = TRUE)
        } else {
            query <- paste0("SELECT * FROM ", park_info$stat_table_name, " WHERE var ='",x_name,"'")
            rs <- db_query(query = query, db_config = dbconfig)
            rs <- rs$data
            if(nrow(rs) > 0) {
                mini <- rs$min.median
                maxi <- rs$max.median
            } else {
                mini <- min(x, na.rm = TRUE)
                maxi <- max(x, na.rm = TRUE)
            }
        }
    }
    x <- (x - mini)/(maxi - mini); 
    x
    
}
