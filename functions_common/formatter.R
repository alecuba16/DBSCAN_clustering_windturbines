formatter<-function(wt_query=NULL,
                    table_cast_park_dic='1_cast_park_table_dic',
                    wp_id=wt_query$wp_id,
                    ld_id=wt_query$ld_id,
                    fault=wt_query$fault,
                    array_id_walm=wt_query$array_id_walm,
                    array_ot=wt_query$array_ot,
                    power_condition=wt_query$power_condition,
                    include_variables=wt_query$include_variables,
                    exclude_variables=wt_query$exclude_variables,
                    artificial_variables=wt_query$artificial_variables,
                    unix_timestamp_ini=wt_query$creation_wtdata_date_ini,
                    unix_timestamp_end=wt_query$creation_wtdata_date_end,
                    seconds_to_aggregate=wt_query$seconds_to_aggregate,
                    target_name=NULL,
                    date_time_name='date_time',
                    db_config=NULL){
    iam=match.call()[[1]]
    #Dependencias
    if(!exists("dependencyLoader")){
        if(!file.exists('functions_common/dependencyLoader.R')) return(list(error=TRUE,warning=F,data=NULL,msg=paste0("\n",iam,":Missing dependency function: functions_common/dependencyLoader.R")));
        source('functions_common/dependencyLoader.R')
    }
    dep<-dependencyLoader(paste0("functions_common/",c('db_query.R','formatter_include_exclude.R','formatter_get_events.R','formatter_get_tableinfo.R')))
    if(dep$error) return(list(error=TRUE, warning=FALSE, data=NULL,msg=paste0("\n",iam,":on call dependencyLoader\n",dep$msg)))
    
    #Init hardcoded defines
    alarms_table_name<-NA #By default NA -> alarms table not available.
    ots_table_name<-NA #By default NA -> alarms table not available.
    seconds_offset<-0
    freq_dat_med_min<-10
    use_db_for_park_dic<-'yourHistoricalBD'
    db_date_time<-'date_time'
    db_date_time_end<-'date_time_end'
    power_variable_name<-'Pot_avg'
    wind_variable_name<-'VelViento_avg'
    production_variable_name<-'production'
    
    rs<-formatter_get_tableinfo(table_cast_park_dic=table_cast_park_dic,
                                wp_id=wp_id,
                                db_config=db_config)
    if(rs$error) return(list(error=TRUE,warning=F,data=NULL,msg=rs$msg))
    data_table_name<-rs$data$data_table_name
    alarms_table_name<-rs$data$alarms_table_name
    ot_table_name<-rs$data$ot_table_name
    power_variable_name<-rs$data$power_variable_name
    wind_variable_name<-rs$data$wind_variable_name
    na_codes<-rs$data$na_codes
    
    #Get ALL columns names for date_table
    query <- paste0("SHOW COLUMNS FROM ", data_table_name)
    columns <- db_query(query = query, db_config = db_config)
    columns <- columns$data$Field
    
    #Exclude or include variables
    rs<-formatter_include_exclude(variables=columns,
                                  date_time_name=date_time_name,
                                  include_variables=include_variables,
                                  exclude_variables=exclude_variables,
                                  target_name=NULL)
    if(rs$error) return(list(error=TRUE,warning=F,data=NULL,msg=rs$msg))
    columns<-rs$data
    
    #Power condition
    if(is.null(power_condition)||is.na(power_condition)||!is.character(power_condition)) power_condition=''
    if(power_condition != '') condition <- paste0('AND ',power_variable_name,power_condition) else condition = ''
    
    #Fix timestaps chars
    if(is.character(unix_timestamp_ini)) unix_timestamp_ini<-as.POSIXct(unix_timestamp_ini,tz = 'UTC')
    if(is.character(unix_timestamp_end)) unix_timestamp_end<-as.POSIXct(unix_timestamp_end,tz = 'UTC')
    
    #Download data
    if(power_condition != '') condition <- paste0('AND ',power_variable_name,power_condition) else condition = ''
    query <- paste0('SELECT ',paste0(columns,collapse = ','),' FROM ',use_db_for_park_dic,'.',data_table_name,' WHERE ld_id=',ld_id,' AND ',db_date_time,' between FROM_UNIXTIME(',as.numeric(unix_timestamp_ini),') AND FROM_UNIXTIME(',as.numeric(unix_timestamp_end),') ',condition)
    rs<-db_query(query=query,db_config=db_config)
    if(rs$error) return(list(error=TRUE,warning=F,data=NULL,msg=rs$msg))
    wtdata<-rs$data
    
    if(nrow(wtdata)==0) return(list(error=F,warning=T,data=NULL,msg="No data for the interval"))
    
    # nrecords variable (for use in load_wtdata)
    rs<-formatter_include_exclude(variables=c('nrecords'),date_time_name=date_time_name,include_variables=include_variables,exclude_variables=exclude_variables)
    if(rs$error) return(list(error=TRUE,warning=F,data=NULL,msg=rs$msg))
    columns_tmp<-rs$data
    if(!is.null(columns_tmp)&&!is.na(columns_tmp)&&length(columns_tmp)>0&&('nrecords' %in% columns_tmp)&&!is.null(wtdata)&&nrow(wtdata)>0) wtdata$nrecords <- 1
    
    #Create production variable
    if(!is.na(power_variable_name) && (power_variable_name %in% names(wtdata))){
        #Check if production is included
        rs<-formatter_include_exclude(variables=c(columns,production_variable_name),date_time_name=date_time_name,include_variables=include_variables,exclude_variables=exclude_variables)
        if(rs$error) return(list(error=TRUE,warning=F,data=NULL,msg=rs$msg))
        columns_tmp<-rs$data
        if(production_variable_name %in% columns_tmp){
            power_division<-60/freq_dat_med_min
            wtdata[,production_variable_name]<- wtdata[,power_variable_name]/power_division
        }
    }
    
    #Special NA codes
    #TODO Optimize
    if(!is.na(na_codes)&&!is.null(na_codes)){
        selected_columns<-(!colnames(wtdata) %in% c("date_time"))
        if(length(selected_columns)>0&&any(selected_columns)){
            m <- as.matrix(wtdata[,selected_columns])
            m[m %in% na_codes] <- NA
            wtdata[,selected_columns] <- as.data.frame(m)
        }
    }
    
    #Date_time character -> asPosix and rename to the internal code date_time name.
    wtdata[,db_date_time]<-as.POSIXct(wtdata[,db_date_time],tz = 'UTC',origin='1970-01-01')
    colnames(wtdata)[colnames(wtdata)==db_date_time]<-date_time_name
    #Arrange date_time
    wtdata<-wtdata[with(wtdata, order(eval(as.name(date_time_name)))), ]
    
    
    #Generate full time vector (date_time without holes) and merge on wtdata
    full_date_time<-data.frame(date_time=seq.POSIXt(from=as.POSIXct(unix_timestamp_ini,origin='1970-01-01',tz='UTC'),to=as.POSIXct(max(wtdata[,date_time_name]),origin='1970-01-01',tz='UTC'),by=paste0(freq_dat_med_min,' min')))
    
    names(full_date_time)[1]<-date_time_name #Change date_time name if necessary
    wtdata<-merge(full_date_time, wtdata, by = date_time_name,all=TRUE)
    
    #Post Processing, remove duplicated date_time, replace infinite values with NA, replace NULL with NA....
    #Remove duplicate and na date time!
    wtdata<-wtdata[!is.na(wtdata[,date_time_name])&!is.null(wtdata[,date_time_name]),]
    wtdata<-wtdata[!duplicated(wtdata[,date_time_name]),]
    #Replace Infinite with NA
    wtdata<-do.call(data.frame,lapply(wtdata, function(x) replace(x, is.infinite(x),NA)))
    
    #Alarms selected
    rs<-formatter_get_events(wt_query=wt_query,
                             wtdata=wtdata,
                             table=alarms_table_name,
                             id_field='id_walm',
                             output_field1='alarm',
                             output_field2='alarm_block_code',
                             ld_id=ld_id,
                             array_id=array_id_walm,
                             freq_dat_med_min=freq_dat_med_min,
                             unix_timestamp_ini=unix_timestamp_ini,
                             unix_timestamp_end=unix_timestamp_end,
                             seconds_to_aggregate=seconds_to_aggregate,
                             include_variables=include_variables,
                             exclude_variables=exclude_variables,
                             check_event_enable=T,
                             date_time_name=date_time_name,
                             db_date_time=db_date_time,
                             db_date_time_end=db_date_time_end,
                             target_name=target_name,
                             db_config=db_config)
    if(rs$error) return(list(error=TRUE,warning=F,data=NULL,msg=rs$msg))
    wtdata<-rs$data$wtdata
    
    #Alarms all
    rs<-formatter_get_events(wt_query=wt_query,
                             wtdata=wtdata,
                             table=alarms_table_name,
                             date_time_name=date_time_name,
                             db_date_time=db_date_time,
                             db_date_time_end=db_date_time_end,
                             id_field='id_walm',
                             output_field1='alarm_all',
                             output_field2='alarm_all_block_code',
                             array_id=NULL,
                             check_event_enable=T,
                             seconds_to_aggregate=seconds_to_aggregate,
                             freq_dat_med_min=freq_dat_med_min,
                             ld_id=ld_id,
                             unix_timestamp_ini=unix_timestamp_ini,
                             unix_timestamp_end=unix_timestamp_end,
                             include_variables=include_variables,
                             exclude_variables=exclude_variables,
                             target_name=target_name,
                             db_config=db_config)
    if(rs$error) return(list(error=TRUE,warning=F,data=NULL,msg=rs$msg))
    wtdata<-rs$data$wtdata
    
    #Ots selected
    rs<-formatter_get_events(wt_query=wt_query,
                             wtdata=wtdata,
                             table=ot_table_name,
                             date_time_name=date_time_name,
                             db_date_time=db_date_time,
                             db_date_time_end=db_date_time_end,
                             id_field='id_ot',
                             output_field1='ot',
                             output_field2='ot_block_code',
                             array_id=array_ot,
                             check_event_enable=T,
                             seconds_to_aggregate=seconds_to_aggregate,
                             freq_dat_med_min=freq_dat_med_min,
                             ld_id=ld_id,
                             unix_timestamp_ini=unix_timestamp_ini,
                             unix_timestamp_end=unix_timestamp_end,
                             include_variables=include_variables,
                             exclude_variables=exclude_variables,
                             target_name=target_name,
                             db_config=db_config)
    if(rs$error) return(list(error=TRUE,warning=F,data=NULL,msg=rs$msg))
    wtdata<-rs$data$wtdata
    
    #Ots all
    rs<-formatter_get_events(wt_query=wt_query,
                             wtdata=wtdata,
                             table=ot_table_name,
                             date_time_name=date_time_name,
                             db_date_time=db_date_time,
                             db_date_time_end=db_date_time_end,
                             id_field='id_ot',
                             output_field1='ot_all',
                             output_field2='ot_all_block_code',
                             array_id=NULL,
                             check_event_enable=T,
                             seconds_to_aggregate=seconds_to_aggregate,
                             freq_dat_med_min=freq_dat_med_min,
                             ld_id=ld_id,
                             unix_timestamp_ini=unix_timestamp_ini,
                             unix_timestamp_end=unix_timestamp_end,
                             include_variables=include_variables,
                             exclude_variables=exclude_variables,
                             target_name=target_name,
                             db_config=db_config)
    if(rs$error) return(list(error=TRUE,warning=F,data=NULL,msg=rs$msg))
    wtdata<-rs$data$wtdata
    
    
    return(list(error=FALSE,warning=F,data=wtdata,msg="OK"))
}
