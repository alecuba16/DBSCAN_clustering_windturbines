# Calcule the daily frequency of occurrence of all faults for all wind 
# turbines in the park specified in wt_query$park. 
# The result is a matrix where rows are the faults and columns are the frequency
# for each wind turbine.
ranking_by <- function(table_park_dic = "1_cast_park_table_dic", park, date1 = '1990-01-01', date2 = Sys.time(), target = "alarm", db_config) {
    iam='ranking_by'
    #Dependencia basica
    if(!exists("dependencyLoader")){
      if(!file.exists('functions_common/dependencyLoader.R')) return(list(error=TRUE,data=NULL,msg=paste0("\n",iam,":Missing dependency function: functions_common/dependencyLoader.R")));
      source('functions_common/dependencyLoader.R')
    }
      
    dep<-dependencyLoader(c('plyr','dplyr','reshape','functions_common/db_query.R'))
    
    if(dep$error)  return(list(error=TRUE,data=NULL,msg=paste0(iam,":on call dependencyLoader\n",dep$msg)))
    
    query <- paste("SELECT * FROM",table_park_dic)
    rs <- db_query(query=query,db_config=db_config)
    if(rs$error)
        return(list(error = TRUE, data = NULL, msg = paste("Error reading ", table_park_dic) ))
    db_tables <- rs$data # read_database(query)
    
    tbl <- db_tables$data_table_name[db_tables$wp_code == park]
    if(target == "alarm")
        tblevents <- db_tables$events_table_name[db_tables$wp_code == park]
    if(target == "ot")
        tblevents <- db_tables$ot_table_name[db_tables$wp_code == park]
    query <- paste0("SELECT * FROM ", tblevents, " WHERE date_time BETWEEN '", date1, "' AND '", date2, "'")
    rs <- db_query(query=query,db_config=db_config)
    if(rs$error)
        return(list(error = TRUE, data = NULL, msg = paste("Error reading ", tblevents) ))
    dfa <- rs$data # read_database(query)
    if(is.null(dfa)||is.data.frame(dfa)&&nrow(dfa)==0)
        return(list(error = TRUE, data = NULL, msg = paste0("Empty events ",tblevents," table") ))
    # Get column index of date/date_time/datetimestart
    idate <- grep("^date_time$|^date_time_start$", names(dfa))
    if(length(idate) < 1)
        return(list(error = TRUE, data = NULL, msg = paste("Timestamp field was not found in ", tblevents) ))
        # stop("Timestamp field was not found")
    # Format dfa (alarms) names
    dfa$date_time <- as.POSIXct(dfa[,idate[1]], format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

    if(target == "alarm")
        dfa <- dplyr::select(dfa, ld_id, id_walm, date_time)
    if(target == "ot")
        dfa <- dplyr::select(dfa, ld_id, id_walm = id_ot, date_time)
    
    dfa$ld_id <- tolower(dfa$ld_id)
    dfa$ld_id <- as.factor(dfa$ld_id)

    dfa <- arrange(dfa, ld_id, date_time)
    dfa2 <- dfa
    dfa2$date <- as.Date(dfa2$date_time)
    x <- group_by(dfa2, ld_id, date, id_walm)
    x <- dplyr::summarise(x, n = n(), alarm = ifelse(n() <= 0, 0, 1))
    unique(x$alarm)
    mx <- reshape::cast(x, id_walm ~ ld_id, sum, value = "alarm")
    return(list(error = FALSE, data = mx, msg = "Ranking done. OK" ))
}