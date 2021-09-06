getModelsToBeCreated <- function(table_cast_config = "1_cast_config",force=FALSE,type="phealth",db_config) {
  iam="getModelsToBeCreated"
  #Dependencias
  if(!exists("db_query")){
    if(!file.exists('functions_common/db_query.R')) return(list(error=TRUE,data=NULL,msg="Missing dependency function: functions_common/db_query.R"));
    source('functions_common/db_query.R')
  }
  if(!exists("type") || is.null(type) || !is.character(type)) return(list(error=TRUE,data=NULL,msg=paste0(iam," type is not defined, check main_models.R")));
  
  if(!force){
    condition <- paste("AND ISNULL(creation_date_ini) AND ISNULL(creation_date_end)")
  }else{
    condition <- ""
    
  }
  # query <- paste0("SELECT id,ld_id,ld_code,ld_id_fusion,wp_id,wp_code,seconds_to_aggregate,array_id_walm,
  #                         array_ot,freq_dat_med_min,fault,type,filter,power_condition,
  #                         include_variables,exclude_variables, exclude_pattern,target,
  #                         UNIX_TIMESTAMP(creation_wtdata_date_ini) as creation_wtdata_date_ini,
  #                         UNIX_TIMESTAMP(creation_wtdata_date_end) as creation_wtdata_date_end,
  #                         creation_trn_percent,creation_model_path,creation_log_path
  #                    FROM ",table_cast_config,
  #                 " WHERE creation_enable=1 AND `type`='",type,"' ",condition," ORDER BY id ASC");
  query <- paste0("SELECT * FROM ",table_cast_config," WHERE creation_enable=1 AND `type`='",type,"' ",condition," ORDER BY id ASC");
  
  rs<-db_query(query=query,db_config=db_config)
  if(rs$error) return(list(error=TRUE,data=NULL,msg=paste0(iam,': on call db_query\n',rs$msg)))
  # rs$data$creation_wtdata_date_ini <- as.numeric( as.POSIXct(rs$data$creation_wtdata_date_ini, tz = "UTC", origin = "1970-01-01") )
  # rs$data$creation_wtdata_date_end <- as.numeric( as.POSIXct(rs$data$creation_wtdata_date_end, tz = "UTC", origin = "1970-01-01") )
  rs$data$creation_wtdata_date_ini <- as.POSIXct(rs$data$creation_wtdata_date_ini, tz = "UTC", origin = "1970-01-01") 
  rs$data$creation_wtdata_date_end <- as.POSIXct(rs$data$creation_wtdata_date_end, tz = "UTC", origin = "1970-01-01") 
  return(list(error=FALSE,data=rs$data,msg="Ok"))
}