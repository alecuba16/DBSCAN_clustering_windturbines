artificial_generate_variables<-function(wt_query=NULL,
                                        wtdata=NULL,
                                        wtdata0=NULL,
                                        wp_id=wt_query$wp_id,
                                        ld_id=wt_query$ld_id,
                                        artificial_variables=wt_query$artificial_variables,
                                        unix_timestamp_ini=wt_query$creation_wtdata_date_ini,
                                        unix_timestamp_end=wt_query$creation_wtdata_date_end,
                                        freq_dat_med_min=wt_query$freq_dat_med_min,
                                        seconds_to_aggregate=wt_query$seconds_to_aggregate,
                                        date_time_name='date_time',
                                        table_cast_park_dic='1_cast_park_table_dic',
                                        table_filter_config='1_filter_config',
                                        table_artificial_config='1_artificial_config',
                                        target_name='alarm',
                                        db_config=NULL){
    iam=match.call()[[1]]
    # Sources
    if(!exists("dependencyLoader")){
        if(!file.exists('functions_common/dependencyLoader.R')) return(list(error=TRUE,data=NULL,msg=paste0("\n",iam,":Missing dependency function: functions_common/dependencyLoader.R")));
        source('functions_common/dependencyLoader.R')
    }
    sources<-paste0("functions_common/",c("artificial_analog_variables.R","artificial_event_variables.R"))
    dep<-dependencyLoader(sources)
    
    if(dep$error) stop(iam,":on call dependencyLoader\n",dep$msg)
    rm(dep)
    
    if(is.null(wtdata)||is.na(wtdata)||(!is.null(wtdata)&&!is.data.frame(wtdata))||(!is.null(wtdata)&&(nrow(wtdata)<=0||ncol(wtdata)<=0))) return(list(error=TRUE,data=NULL,msg=paste0(iam,": wtdata is null or empty")))
    
    #Analog variables
    rs<-artificial_analog_variables(wt_query=wt_query,
                                    artificial_variables=artificial_variables,
                                    wtdata=wtdata,
                                    wtdata0=wtdata0,
                                    date_time_name=date_time_name,
                                    table_cast_park_dic=table_cast_park_dic,
                                    table_filter_config=table_filter_config,
                                    target_name=target_name,
                                    db_config=db_config)
    if(rs$error) return(list(error=TRUE,data=NULL,msg=rs$msg))
    wtdata<-rs$data$wtdata
    wtdata0<-rs$data$wtdata0
    
    #Events , number of alarms per week , etc.
    rs<-artificial_event_variables(wt_query=wt_query,
                                   wp_id=wp_id,
                                   ld_id=ld_id,
                                   wtdata=wtdata,
                                   wtdata0=wtdata0,
                                   artificial_variables=artificial_variables,
                                   unix_timestamp_ini=unix_timestamp_ini,
                                   unix_timestamp_end=unix_timestamp_end,
                                   freq_dat_med_min=freq_dat_med_min,
                                   seconds_to_aggregate=seconds_to_aggregate,
                                   date_time_name=date_time_name,
                                   table_artificial_config=table_artificial_config,
                                   db_config=db_config)
    if(rs$error) return(list(error=TRUE,data=NULL,msg=rs$msg))
    wtdata<-rs$data$wtdata
    wtdata0<-rs$data$wtdata0
    
    return(list(error=FALSE,data=list(wtdata=wtdata,wtdata0=wtdata0),msg="ok"))
}
