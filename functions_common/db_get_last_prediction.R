db_get_last_prediction <- function(wt_query, table_cast_results = "CAST_RESULTS", db_config){
    # OJO usar comillas simples para encerrar CHAR variables de la BDD.

    iam=match.call()[[1]]
    type_condition= paste0(" AND `type` = '",tolower(wt_query$type),"'");
    
    if("type" %in% names(wt_query) && wt_query$type=="normality") type_condition=''; #Normality results table doesn't have type column
    
    query <- paste0("SELECT * FROM ",table_cast_results," WHERE ld_id = ",
                    wt_query$ld_id,type_condition," AND fault = '",
                    tolower(wt_query$fault),"' ORDER BY date_time DESC LIMIT 1")
    
    rs <- db_query(query=query,db_config=db_config)
    if(!rs$error){
        if(nrow(rs$data)>0 && ("date_time" %in% names(rs$data)))
            return(list(error=FALSE,data=as.POSIXlt(rs$data$date_time, tz = "UTC"),msg="OK"))# It is supposed the server is in UTC timezone
        else
            return(list(error=FALSE,data=NULL,msg="OK"))
    }else{
        return(list(error=TRUE, data=NULL, msg=paste0("\n\n",iam,": On call queryTimeDB:",rs$msg,"\n\n")))
    }
}