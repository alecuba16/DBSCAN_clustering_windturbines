#' db_get_event_description
#' Return the description of an alarm given its short name. It consults the 
#' dictionary table "SC_DIC_VARSGEN_WTG" in the Data Base cast DB.
#' @param id: character with the short name (codi_vargen column in the dic-table)
#'
#' @return a dataframe with the description of: alarm, system and sub-sytem of 
#' the alarm
#' @export
#'
#' @examples
#' get_var_description("GriPhV_net")
db_get_event_description <- function(array_id_walm, table_dic,all_info=FALSE, target = "alarm", db_config) {
    
    if(target == "alarm") id_name <- "id_walm"
    if(target == "ot")  id_name <- "id_ot"
    if(all_info) {
        query <- paste0(
            "SELECT * 
               FROM ", table_dic," 
              WHERE ",id_name," IN (", array_id_walm,") 
              GROUP BY ",id_name,";" )
    } else {
        query <- paste0(
            "SELECT ",id_name,", MAX(descripcio_walm) AS descripcio_walm 
               FROM ", table_dic," 
              WHERE ",id_name," IN (", array_id_walm,") 
              GROUP BY ",id_name,";" )
    }
    rs <- db_query(query=query,db_config=db_config)
    
    return(rs$data)
}
