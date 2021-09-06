formatter_get_tableinfo<-function(table_cast_park_dic='1_cast_park_table_dic',wp_id=NULL,db_config=NULL){
    iam=match.call()[[1]]
    #Dependencias
    if(!exists("dependencyLoader")){
        if(!file.exists('functions_common/dependencyLoader.R')) return(list(error=TRUE,data=NULL,msg=paste0("\n",iam,":Missing dependency function: functions_common/dependencyLoader.R")));
        source('functions_common/dependencyLoader.R')
    }
    dep<-dependencyLoader(c('functions_common/db_query.R','functions_common/db_table_exists.R'))
    if(dep$error) return(list(error=TRUE, warning=FALSE, data=NULL,msg=paste0("\n",iam,":on call dependencyLoader\n",dep$msg)))
    
    use_db_for_park_dic<-'yourHistoricalBD'
    
    #Get data, ots,alarms, table name
    if(!db_table_exists(table_cast_park_dic, dbtype = "historical",db_config=db_config)) return(list(error=TRUE,data=NULL,msg=paste0("Data table ",table_cast_park_dic," doesn't exists.")))
    
    use_db_for_park_dic <- 'yourHistoricalBD'
    query <- paste0('SELECT data_table_name,events_table_name,events_table_dic_name,ot_table_name,ot_table_dic_name,priori_table_name,range_table_name,power_name,wind_name,freq_dat_med_min,na_codes FROM ',use_db_for_park_dic,'.',table_cast_park_dic,' WHERE wp_id=',wp_id)
    rs<-db_query(query=query,db_config=db_config)
    if(rs$error) return(list(error=TRUE,data=NULL,msg=rs$msg))
    
    #Data_table_name: 1-Check if exists on results,2-check not empty,null,na ,3-check if table exists , 4-assign
    if(!"data_table_name" %in% names(rs$data)) return(list(error=TRUE,data=NULL,msg=paste0("Column data_table_name doesn't exists on ",table_cast_park_dic)))
    if(is.null(rs$data$data_table_name)||is.na(rs$data$data_table_name)||(!is.null(rs$data$data_table_name)&&!is.na(rs$data$data_table_name)&&nchar(rs$data$data_table_name)<=1)) return(list(error=TRUE,data=NULL,msg=paste0("data_table_name for wp_id:",wp_id," is null or empty")))
    if(!db_table_exists(rs$data$data_table_name, dbtype = "historical",db_config=db_config)) return(list(error=TRUE,data=NULL,msg=paste0("Data table ",rs$data$data_table_name," doesn't exists.")))
    data_table_name<-rs$data$data_table_name
    
    #freq_dat_med_min: 1-Check if exists on results,2-check not empty,null,na , 3-assign
    if(!"freq_dat_med_min" %in% names(rs$data)) return(list(error=TRUE,data=NULL,msg=paste0("Column freq_dat_med_min doesn't exists on ",table_cast_park_dic)))
    if(is.null(rs$data$freq_dat_med_min)||is.na(rs$data$freq_dat_med_min)||(!is.null(rs$data$freq_dat_med_min)&&!is.na(rs$data$freq_dat_med_min)&&rs$data$freq_dat_med_min<=0)) return(list(error=TRUE,data=NULL,msg=paste0("freq_dat_med_min for wp_id:",wp_id," is null,empty or zero")))
    freq_dat_med_min<-rs$data$freq_dat_med_min
    
    #events-alarms_table_name: 1-Check if exists on results,2-check not empty,null,na ,3-check if table exists , 4-assign
    if(!"events_table_name" %in% names(rs$data)) return(list(error=TRUE,data=NULL,msg=paste0("Column events_table_name doesn't exists on ",table_cast_park_dic)))
    if(is.null(rs$data$events_table_name)||is.na(rs$data$events_table_name)||(!is.null(rs$data$events_table_name)&&!is.na(rs$data$events_table_name)&&nchar(is.null(rs$data$events_table_name))<=1)) alarms_table_name<-rs$data$events_table_name
    if(!is.na(rs$data$events_table_name)&&!db_table_exists(rs$data$events_table_name, dbtype = "historical",db_config=db_config)) return(list(error=TRUE,data=NULL,msg=paste0("Events-Alarms table ",events_table_name," doesn't exists.")))
    alarms_table_name<-rs$data$events_table_name
    
    #events-alarms_table_dic_name: 1-Check if exists on results,2-check not empty,null,na ,3-check if table exists , 4-assign
    if(!"events_table_dic_name" %in% names(rs$data)) return(list(error=TRUE,data=NULL,msg=paste0("Column events_table_dic_name doesn't exists on ",table_cast_park_dic)))
    if(is.null(rs$data$events_table_dic_name)||is.na(rs$data$events_table_dic_name)||(!is.null(rs$data$events_table_dic_name)&&!is.na(rs$data$events_table_dic_name)&&nchar(is.null(rs$data$events_table_dic_name))<=1)) alarms_table_dic_name<-rs$data$events_table_dic_name
    if(!is.na(rs$data$events_table_dic_name)&&!db_table_exists(rs$data$events_table_dic_name, dbtype = "historical",db_config=db_config)) return(list(error=TRUE,data=NULL,msg=paste0("Events-Alarms table ",events_table_dic_name," doesn't exists.")))
    alarms_table_dic_name<-rs$data$events_table_dic_name
    
    #ot_table_name: 1-Check if exists on results,2-check not empty,null,na ,3-check if table exists , 4-assign
    if(!"ot_table_name" %in% names(rs$data)) return(list(error=TRUE,data=NULL,msg=paste0("Column ot_table_name doesn't exists on ",table_cast_park_dic)))
    if(is.null(rs$data$ot_table_name)||is.na(rs$data$ot_table_name)||(!is.null(rs$data$ot_table_name)&&!is.na(rs$data$ot_table_name)&&nchar(is.null(rs$data$ot_table_name))<=1)) ot_table_name<-rs$data$ot_table_name
    if(!is.na(rs$data$ot_table_name)&&!db_table_exists(rs$data$ot_table_name, dbtype = "historical",db_config=db_config)) return(list(error=TRUE,data=NULL,msg=paste0("Ots table ",ot_table_name," doesn't exists.")))
    ot_table_name<-rs$data$ot_table_name
    
    #ot_table_dic: 1-Check if exists on results,2-check not empty,null,na ,3-check if table exists , 4-assign
    if(!"ot_table_dic_name" %in% names(rs$data)) return(list(error=TRUE,data=NULL,msg=paste0("Column ot_table_dic_name doesn't exists on ",table_cast_park_dic)))
    if(is.null(rs$data$ot_table_dic_name)||is.na(rs$data$ot_table_dic_name)||(!is.null(rs$data$ot_table_dic_name)&&!is.na(rs$data$ot_table_dic_name)&&nchar(is.null(rs$data$ot_table_dic_name))<=1)) ot_table_dic_name<-rs$data$ot_table_dic_name
    if(!is.na(rs$data$ot_table_dic_name)&&!db_table_exists(rs$data$ot_table_dic_name, dbtype = "historical",db_config=db_config)) return(list(error=TRUE,data=NULL,msg=paste0("Ots table ",ot_table_dic_name," doesn't exists.")))
    ot_table_dic_name<-rs$data$ot_table_dic_name
    
    #priori_table_name: 1-Check if exists on results,2-check not empty,null,na ,3-check if table exists , 4-assign
    if(!"priori_table_name" %in% names(rs$data)) return(list(error=TRUE,data=NULL,msg=paste0("Column priori_table_name doesn't exists on ",table_cast_park_dic)))
    if(is.null(rs$data$priori_table_name)||is.na(rs$data$priori_table_name)||(!is.null(rs$data$priori_table_name)&&!is.na(rs$data$priori_table_name)&&nchar(is.null(rs$data$priori_table_name))<=1)) priori_table_name<-rs$data$priori_table_name
    if(!is.na(rs$data$priori_table_name)&&!db_table_exists(rs$data$priori_table_name, dbtype = "historical",db_config=db_config)) return(list(error=TRUE,data=NULL,msg=paste0("Ots table ",priori_table_name," doesn't exists.")))
    priori_table_name<-rs$data$priori_table_name
    
    #range_table_name: 1-Check if exists on results,2-check not empty,null,na ,3-check if table exists , 4-assign
    if(!"range_table_name" %in% names(rs$data)) return(list(error=TRUE,data=NULL,msg=paste0("Column range_table_name doesn't exists on ",table_cast_park_dic)))
    if(is.null(rs$data$range_table_name)||is.na(rs$data$range_table_name)||(!is.null(rs$data$range_table_name)&&!is.na(rs$data$range_table_name)&&nchar(is.null(rs$data$range_table_name))<=1)) range_table_name<-rs$data$range_table_name
    if(!is.na(rs$data$range_table_name)&&!db_table_exists(rs$data$range_table_name, dbtype = "historical",db_config=db_config)) return(list(error=TRUE,data=NULL,msg=paste0("Ots table ",range_table_name," doesn't exists.")))
    range_table_name<-rs$data$range_table_name
    
    if(!"power_name" %in% names(rs$data)) return(list(error=TRUE,data=NULL,msg=paste0("Column power_name doesn't exists on ",table_cast_park_dic)))
    if(is.null(rs$data$power_name)||is.na(rs$data$power_name)||(!is.null(rs$data$power_name)&&!is.na(rs$data$power_name)&&nchar(is.null(rs$data$power_name))<=1)){
        power_variable_name<-NA
    }else{
        power_variable_name<-rs$data$power_name
        #Check if data table table has power_name
        query <- paste0("SHOW COLUMNS FROM ", data_table_name," where Field='",power_variable_name,"'")
        rs2 <- db_query(query = query, db_config = db_config)
        if(rs2$error) return(list(error=FALSE,data=wtdata,msg=rs2$msg))
        if(nrow(rs2$data)<=0) return(list(error=TRUE,data=NULL,msg=paste0("Column ",power_variable_name," doesn't exists on ",data_table_name)))
    }
    
    if(!"wind_name" %in% names(rs$data)) return(list(error=TRUE,data=NULL,msg=paste0("Column wind_name doesn't exists on ",table_cast_park_dic)))
    if(is.null(rs$data$wind_name)||is.na(rs$data$wind_name)||(!is.null(rs$data$wind_name)&&!is.na(rs$data$wind_name)&&nchar(is.null(rs$data$wind_name))<=1)){
        wind_variable_name<-NA
    }else{
        wind_variable_name<-rs$data$wind_name
        #Check if data table table has wind_variable_name
        query <- paste0("SHOW COLUMNS FROM ", data_table_name," where Field='",wind_variable_name,"'")
        rs2 <- db_query(query = query, db_config = db_config)
        if(rs2$error) return(list(error=FALSE,data=wtdata,msg=rs2$msg))
        if(nrow(rs2$data)<=0) return(list(error=TRUE,data=NULL,msg=paste0("Column ",wind_variable_name," doesn't exists on ",data_table_name)))
    }
    
    if(!"na_codes" %in% names(rs$data)) return(list(error=TRUE,data=NULL,msg=paste0("Column na_codes doesn't exists on ",table_cast_park_dic)))
    if(is.null(rs$data$na_codes)||is.na(rs$data$na_codes)||(!is.null(rs$data$na_codes)&&!is.na(rs$data$na_codes)&&nchar(is.null(rs$data$na_codes))<=1)){
        na_codes<-NA
    }else{
        na_codes<-rs$data$na_codes
    }
    
    return(list(error=FALSE,data=list(data_table_name=data_table_name,alarms_table_name=alarms_table_name,alarms_table_dic_name=alarms_table_dic_name,ot_table_name=ot_table_name,ot_table_dic_name=ot_table_dic_name,priori_table_name=priori_table_name,range_table_name=range_table_name,freq_dat_med_min=freq_dat_med_min,power_variable_name=power_variable_name,wind_variable_name=wind_variable_name,na_codes=na_codes),msg="ok"))
}