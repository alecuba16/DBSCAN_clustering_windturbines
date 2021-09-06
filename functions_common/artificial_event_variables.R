norm_helper<-function(wt_query,ld_id=wt_query$ld_id,seconds_to_aggregate=wt_query$seconds_to_aggregate,wtdata,parameters,table_cast_park_dic,date_time_name='date_time',output_field,db_config,table_normality_results='3_norm_results'){
    iam=match.call()[[1]]
    if(!grepl("days=", parameters)) return(list(error=TRUE,data=NULL,msg=paste0("\n",iam,":Missing days parameter")))
    if(!grepl("target_name=", parameters)) return(list(error=TRUE,data=NULL,msg=paste0("\n",iam,":Missing target_name parameter")))
    days<-as.numeric(sub(".*days=(\\d+(.\\d+)?).*", "\\1", parameters))
    target_name<-sub(pattern = ".*target_name=([^,$]+)", replacement = "\\1", x = parameters,perl = T)
    
    unix_timestamp_ini<-min(wtdata[,date_time_name],na.rm = T)
    unix_timestamp_ini<-as.numeric(unix_timestamp_ini-as.difftime(days, units="days"))
    unix_timestamp_end<-as.numeric(max(wtdata[,date_time_name],na.rm = T))
    
    #Check if table exists
    if(!db_table_exists(table_test=table_normality_results, dbtype = "historical",db_config=db_config)) return(list(error=TRUE,data=NULL,msg=paste0("\n",iam,":table ",table_normality_results," doesn't exists\n"))) 
    
    query<-paste0('SELECT date_time,predict_value,real_value from ',table_normality_results,' where ld_id=',ld_id,' and target_name=\'',target_name,'\' and date_time between FROM_UNIXTIME(',unix_timestamp_ini,') AND FROM_UNIXTIME(',unix_timestamp_end,')')
    rs<-db_query(query=query,db_config = db_config)
    if(rs$error) return(list(error=TRUE,data=NULL,msg=paste0("\n",iam,":on call db_query\n\t",rs$msg)))
    if(!is.null(rs$data)&&nrow(rs$data)>0){
        norm_data<-rs$data
        norm_data[,date_time_name]<-as.POSIXct(norm_data[,date_time_name],tz = 'UTC',origin='1970-01-01')
        norm_data$error<-norm_data$real_value-norm_data$predict_value
        freq_dat_med_min<-as.numeric(median(diff(norm_data$date_time),na.rm = T))
        window<-days*(24*60/freq_dat_med_min)
    }else{
        norm_data<-NULL
    }
    
    return(list(error=FALSE,data=list(norm_data=norm_data,window=window),msg='ok'))
}

rmse_mov_avg_helper<-function(wt_query,ld_id=wt_query$ld_id,seconds_to_aggregate=wt_query$seconds_to_aggregate,wtdata,parameters,table_cast_park_dic,date_time_name='date_time',output_field,db_config){
    iam=match.call()[[1]]
    rs<-norm_helper(wt_query,ld_id,seconds_to_aggregate,wtdata,parameters,table_cast_park_dic,date_time_name,output_field,db_config)
    if(rs$error) return(list(error=TRUE,data=NULL,msg=paste0("\n",iam,":on call norm_helper\n\t",rs$msg)))
    norm_data<-rs$data$norm_data
    if(!is.null(norm_data)&&nrow(norm_data)>0){
        window<-rs$data$window
        
        norm_data$rmse<- sapply(window+1:length(norm_data$error),function(i) sqrt(mean(norm_data$error[i-window]^2, na.rm = T)))
        norm_data$rmse<-TTR::SMA(norm_data$rmse,window,na.rm=T)
        #Aggregate
        norm_data[,date_time_name]<-as.POSIXct((as.numeric(norm_data[,date_time_name])%/%seconds_to_aggregate)*seconds_to_aggregate,origin='1970-01-01',tz = "UTC")
        norm_data<-data.frame(norm_data[,c('rmse',date_time_name)] %>% group_by_(as.symbol(date_time_name)) %>% summarise_all(funs(mean(., na.rm = T))))
        wtdata[(wtdata[,date_time_name] %in% norm_data[,date_time_name]),output_field]<-norm_data[norm_data[,date_time_name] %in% wtdata[,date_time_name],'rmse']
    }else{
        warning(paste0("\n",iam,":No normality data for artificial variable ",output_field,"\n"))
        wtdata[,output_field]<-NA
    }
    return(wtdata)
}

error_mov_avg_helper<-function(wt_query,ld_id=wt_query$ld_id,seconds_to_aggregate=wt_query$seconds_to_aggregate,wtdata,parameters,table_cast_park_dic,date_time_name='date_time',output_field,db_config){
    iam=match.call()[[1]]
    rs<-norm_helper(wt_query,ld_id,seconds_to_aggregate,wtdata,parameters,table_cast_park_dic,date_time_name,output_field,db_config)
    if(rs$error) return(list(error=TRUE,data=NULL,msg=paste0("\n",iam,":on call norm_helper\n\t",rs$msg)))
    norm_data<-rs$data$norm_data
    if(!is.null(norm_data)&&nrow(norm_data)>0){
        window<-rs$data$window
        norm_data$error<-TTR::SMA(norm_data$error,window,na.rm=T)
        #Aggregate
        norm_data[,date_time_name]<-as.POSIXct((as.numeric(norm_data[,date_time_name])%/%seconds_to_aggregate)*seconds_to_aggregate,origin='1970-01-01',tz = "UTC")
        norm_data<-data.frame(norm_data[,c('error',date_time_name)] %>% group_by_(as.symbol(date_time_name)) %>% summarise_all(funs(mean(., na.rm = T))))
        wtdata[(wtdata[,date_time_name] %in% norm_data[,date_time_name]),output_field]<-norm_data[norm_data[,date_time_name] %in% wtdata[,date_time_name],'error']
    }else{
        warning(paste0("\n",iam,":No normality data for artificial variable ",output_field,"\n"))
        wtdata[,output_field]<-NA
    }
    return(wtdata)
}

events_helper<-function(wt_query,ld_id,seconds_to_aggregate,wtdata,parameters,table_cast_park_dic,wp_id,date_time_name='date_time',db_config){
    rs<-formatter_get_tableinfo(table_cast_park_dic=table_cast_park_dic,wp_id=wp_id,db_config=db_config)
    if(rs$error) return(list(error=TRUE,data=NULL,msg=paste0("\n",iam,":on call formatter_get_tableinfo\n\t",rs$msg)))
    freq_dat_med_min<-rs$data$freq_dat_med_min
    
    if(!grepl("days=", parameters)) return(list(error=TRUE,data=NULL,msg=paste0("\n",iam,":Missing days parameter")))
    if(grepl("array_id_walm=", parameters)&&grepl("array_id_ot=", parameters)) return(list(error=TRUE,data=NULL,msg=paste0("\n",iam,":Cannot define array_id_walm and array_id_ot parameter at the same time")))
    if(!grepl("array_id_walm=", parameters)&&!grepl("array_id_ot=", parameters)) return(list(error=TRUE,data=NULL,msg=paste0("\n",iam,":Missing array_id_walm or array_id_ot parameter")))
    days<-as.numeric(sub(".*days=(\\d+(.\\d+)?).*", "\\1", parameters))
    if(grepl("array_id_walm=", parameters)){
        type='array_id_walm'
        id_field='id_walm'
        table<-rs$data$alarms_table_name
    }else if(grepl("array_id_ot=", parameters)){
        type='array_id_ot'
        id_field='id_ot'
        table<-rs$data$ot_table_name
    }
    array_id<-sub(pattern = paste0(".*",type,"=\\(([^()]+)\\).*"), replacement = "\\1", x = parameters,perl = T)
    unix_timestamp_ini<-min(wtdata[,date_time_name],na.rm = T)
    unix_timestamp_ini<-as.numeric(unix_timestamp_ini-as.difftime(days, units="days"))
    unix_timestamp_end<-as.numeric(max(wtdata[,date_time_name],na.rm = T))
    
    rs<-formatter_get_events(wt_query=wt_query,
                             wtdata=wtdata,
                             table=table,
                             id_field=id_field,
                             output_field1='event',
                             output_field2='event_block_code',
                             ld_id = ld_id,
                             array_id=array_id,
                             freq_dat_med_min=freq_dat_med_min,
                             unix_timestamp_ini=unix_timestamp_ini,
                             unix_timestamp_end=unix_timestamp_end,
                             seconds_to_aggregate=seconds_to_aggregate,
                             include_variables='event',
                             exclude_variables=NA,
                             check_event_enable=T,
                             date_time_name='date_time',
                             db_date_time='date_time',
                             db_date_time_end='date_time_end',
                             target_name=NULL,
                             db_config=db_config)
    if(rs$error) return(list(error=TRUE,data=NULL,msg=paste0("\n",iam,":on call formatter_get_events\n\t",rs$msg)))
    events<-rs$data$wtdata[,c(date_time_name,'event')]
    events_in_interval<-rs$data$events_in_interval
    return(list(error=FALSE,data=list(days=days,type=type,events=events,events_in_interval=events_in_interval),msg='ok'))
}

count_events_helper<-function(wt_query,ld_id=wt_query$ld_id,wp_id=wt_query$wp_id,seconds_to_aggregate=wt_query$seconds_to_aggregate,wtdata,parameters,table_cast_park_dic,date_time_name='date_time',output_field,db_config){
    iam=match.call()[[1]]
    
    rs<-events_helper(wt_query=wt_query,ld_id=ld_id,wp_id=wp_id,seconds_to_aggregate=seconds_to_aggregate,wtdata=wtdata,parameters=parameters,table_cast_park_dic=table_cast_park_dic,date_time_name=date_time_name,db_config=db_config)
    if(rs$error) return(list(error=TRUE,data=NULL,msg=paste0("\n",iam,":on call events_helper\n\t",rs$msg)))
    events<-rs$data$events
    days<-rs$data$days
    
    if(!any(as.numeric(events$event)>0)){
        wtdata[,output_field]<-0
    }else{
        acumm<-sapply(events[,date_time_name],function(dt){
            to_sum<-seq.POSIXt(dt-as.difftime(days, units="days"),by='day',dt)
            if(is.na(to_sum)||is.null(to_sum)||length(to_sum)==0) 
                return(0)
            else
                return(sum(events[(events[,date_time_name] %in% to_sum),'event'],na.rm = T))
        })
        if(class(acumm)=='list') acumm<-unlist(acumm)
        wtdata[,output_field]<-acumm
    }
    return(wtdata)
}

time_btw_events_helper<-function(wt_query,ld_id=wt_query$ld_id,wp_id=wt_query$wp_id,seconds_to_aggregate=wt_query$seconds_to_aggregate,wtdata,parameters,table_cast_park_dic,date_time_name='date_time',output_field,db_config){
    iam=match.call()[[1]]
    rs<-events_helper(wt_query=wt_query,ld_id=ld_id,wp_id=wp_id,seconds_to_aggregate=seconds_to_aggregate,wtdata=wtdata,parameters=parameters,table_cast_park_dic=table_cast_park_dic,date_time_name=date_time_name,db_config=db_config)
    
    if(rs$error) return(list(error=TRUE,data=NULL,msg=paste0("\n",iam,":on call events_helper\n\t",rs$msg)))
    events<-rs$data$events
    events_in_interval<-rs$data$events_in_interval
    days<-rs$data$days
    levels_char<-c('<1day','>1day','>1week','>2weeks','>1month')
    factors<-list(key=c(-1,1,7,2*7,30),value=as.factor(levels_char))
    levels(factors$value)<-levels_char
    default<-length(factors$key)
    if(sum(as.numeric(events$event))==0){
        wtdata[,output_field]<-factors$value[default]
    }else{
        #Round to days
        events_in_interval[,date_time_name]<-as.POSIXct(floor(as.numeric(events_in_interval[,date_time_name])/(1*24*60*60))*(1*24*60*60),tz = 'UTC',origin='1970-01-01')
        avg_registers<-sapply(1:nrow(events),function(i){
            dt<-events[i,date_time_name]
            to_sum<-seq.POSIXt(dt-as.difftime(days, units="days"),by='day',dt)
            #Find the events_to_sum
            events_to_sum<-events_in_interval[(events_in_interval[,date_time_name] %in% to_sum),]
            events_to_sum<-events_to_sum[order(events_to_sum[,date_time_name]),]
            if(any(is.na(to_sum))||is.null(to_sum)||length(to_sum)==0||nrow(events_to_sum)<2) return(default)
            
            time_diff<-sapply(2:nrow(events_to_sum),function(j)(difftime(events_to_sum[j,date_time_name], events_to_sum[j-1,date_time_name], units = "days",tz = 'UTC')))
            time_diff<-median(time_diff,na.rm = T)
            #avg_diff<-(sum(diff)/(nrow(events_to_sum)-1))
            #To factor
            j<-length(factors$key)
            while(j>0){
                if(time_diff>factors$key[j]){
                    time_diff<-j
                    j<-0
                }
                j<-j-1
            }
            return(time_diff)
        })
        if(class(avg_registers)=='list') avg_registers<-unlist(avg_registers)
        #key->value
        wtdata[,output_field]<-factors$value[avg_registers]
    }
    return(wtdata)
}

artificial_event_variables<-function(wt_query=wt_query,
                                     wp_id=wt_query$wp_id,
                                     ld_id=wt_query$ld_id,
                                     wtdata=wtdata,
                                     wtdata0=wtdata0,
                                     artificial_variables=wt_query$artificial_variables,
                                     unix_timestamp_ini=wt_query$creation_wtdata_date_ini,
                                     unix_timestamp_end=wt_query$creation_wtdata_date_end,
                                     freq_dat_med_min=wt_query$freq_dat_med_min,
                                     seconds_to_aggregate=wt_query$seconds_to_aggregate,
                                     date_time_name='date_time',
                                     table_cast_park_dic='1_cast_park_table_dic',
                                     table_artificial_config='1_artificial_config',
                                     db_config=NULL){
    iam=match.call()[[1]]
    # Sources
    if(!exists("dependencyLoader")){
        if(!file.exists('functions_common/dependencyLoader.R')) return(list(error=TRUE,data=NULL,msg=paste0("\n",iam,":Missing dependency function: functions_common/dependencyLoader.R")));
        source('functions_common/dependencyLoader.R')
    }
    libraries<-c('TTR')
    sources<-paste0('functions_common/',c("db_query.R",'formatter_get_tableinfo.R',"formatter_get_events.R","db_table_exists.R"))
    dep<-dependencyLoader(c(sources,libraries))
    match<-gregexpr(pattern = '(?:^|,)_[^,]+',text = artificial_variables,perl = T)
    if(is.na(match)||length(match)==0||(!is.list(match)&&match==-1)||(is.list(match)&&unlist(match)==-1)) return(list(error=FALSE,data=list(wtdata=wtdata,wtdata0=wtdata0),msg=paste0("\nOk,no artificial_event_variables")))
    
    #There are artificial variables check if table exists..
    if(!db_table_exists(table_artificial_config, dbtype = "historical",db_config=db_config)) return(list(error=FALSE,data=list(wtdata=wtdata,wtdata0=wtdata0),msg=paste0("\nWARNING ,artificial variables defined and table config '",table_artificial_config,"' doesn't exists")))
    
    #Transfor to array
    match<-lapply(match, function(x) rbind(x, attr(x, "match.length")))[[1]]
    #get str
    match<-apply(match, 2,function(x) str_sub(artificial_variables,x[1],(x[1]+x[2])))
    #remove preceding commas:
    match<-gsub(pattern = ',',x = match,replacement = '')
    
    wtdata<-wtdata[order(wtdata[,date_time_name]),]
    
    for(m in 1:length(match)){
        current_match<-match[m]
        
        #Get generic operation from db and required paramenters
        query<-paste0('Select * from ',table_artificial_config,' where wp_id=',wp_id,' and artificial=\'',current_match,'\'')
        rs<-db_query(query=query,db_config=db_config)
        if(rs$error) return(list(error=TRUE,data=NULL,msg=paste0("\n",iam,":on call load_wtdata\n\t",rs$msg)))
        parameters<-NA
        generic_type<-NA
        if(!is.null(rs$data)){
            if(("parameters" %in% names(rs$data))&&!is.null(rs$data$parameters)&&(length(rs$data$parameters)>0)) parameters<-rs$data$parameters
            if(("generic_type" %in% names(rs$data))&&!is.null(rs$data$generic_type)&&(length(rs$data$generic_type)>0)) generic_type<-rs$data$generic_type
        }
        
        if(is.na(parameters)||is.na(generic_type)) return(list(error=TRUE,data=NULL,msg=paste0("\n",iam,":Unknown generic_type or no parameters defined")))
        ## For WTDATA 0
        wtdata<-switch(generic_type,
                       '_count_events'=count_events_helper(wt_query=wt_query,
                                                           wp_id=wp_id,
                                                           ld_id=ld_id,
                                                           seconds_to_aggregate=seconds_to_aggregate,
                                                           wtdata=wtdata,
                                                           parameters=parameters,
                                                           table_cast_park_dic=table_cast_park_dic,
                                                           date_time_name=date_time_name,
                                                           output_field=current_match,
                                                           db_config=db_config),
                       '_time_btw_events'=time_btw_events_helper(wt_query=wt_query,
                                                                 wp_id=wp_id,
                                                                 ld_id=ld_id,
                                                                 seconds_to_aggregate=seconds_to_aggregate,
                                                                 wtdata=wtdata,
                                                                 parameters=parameters,
                                                                 table_cast_park_dic=table_cast_park_dic,
                                                                 date_time_name=date_time_name,
                                                                 output_field=current_match,
                                                                 db_config=db_config),
                       '_error_mov_avg'=error_mov_avg_helper(wt_query=wt_query,
                                                             ld_id=ld_id,
                                                             seconds_to_aggregate=seconds_to_aggregate,
                                                             wtdata=wtdata,
                                                             parameters=parameters,
                                                             table_cast_park_dic=table_cast_park_dic,
                                                             date_time_name=date_time_name,
                                                             output_field=current_match,
                                                             db_config=db_config),
                       '_rmse_mov_avg'=rmse_mov_avg_helper(wt_query=wt_query,
                                                           ld_id=ld_id,
                                                           seconds_to_aggregate=seconds_to_aggregate,
                                                           wtdata=wtdata,
                                                           parameters=parameters,
                                                           table_cast_park_dic=table_cast_park_dic,
                                                           date_time_name=date_time_name,
                                                           output_field=current_match,
                                                           db_config=db_config)
        )
        ## For WTDATA 0
        wtdata0<-switch(generic_type,
                       '_count_events'=count_events_helper(wt_query=wt_query,
                                                           wp_id=wp_id,
                                                           ld_id=ld_id,
                                                           seconds_to_aggregate=seconds_to_aggregate,
                                                           wtdata=wtdata0,
                                                           parameters=parameters,
                                                           table_cast_park_dic=table_cast_park_dic,
                                                           date_time_name=date_time_name,
                                                           output_field=current_match,
                                                           db_config=db_config),
                       '_time_btw_events'=time_btw_events_helper(wt_query=wt_query,
                                                                 wp_id=wp_id,
                                                                 ld_id=ld_id,
                                                                 seconds_to_aggregate=seconds_to_aggregate,
                                                                 wtdata=wtdata0,
                                                                 parameters=parameters,
                                                                 table_cast_park_dic=table_cast_park_dic,
                                                                 date_time_name=date_time_name,
                                                                 output_field=current_match,
                                                                 db_config=db_config),
                       '_error_mov_avg'=error_mov_avg_helper(wt_query=wt_query,
                                                             ld_id=ld_id,
                                                             seconds_to_aggregate=seconds_to_aggregate,
                                                             wtdata=wtdata0,
                                                             parameters=parameters,
                                                             table_cast_park_dic=table_cast_park_dic,
                                                             date_time_name=date_time_name,
                                                             output_field=current_match,
                                                             db_config=db_config),
                       '_rmse_mov_avg'=rmse_mov_avg_helper(wt_query=wt_query,
                                                           ld_id=ld_id,
                                                           seconds_to_aggregate=seconds_to_aggregate,
                                                           wtdata=wtdata0,
                                                           parameters=parameters,
                                                           table_cast_park_dic=table_cast_park_dic,
                                                           date_time_name=date_time_name,
                                                           output_field=current_match,
                                                           db_config=db_config)
        )
        
        #Strip "_" from current_match to get a better variable name.
        if(grep(pattern = '^_.*',x = current_match)){
            names(wtdata)[names(wtdata)==current_match]<-substring(current_match, 2)
            names(wtdata0)[names(wtdata0)==current_match]<-substring(current_match, 2)
        }
    }
    return(list(error=FALSE,data=list(wtdata=wtdata,wtdata0=wtdata0),msg="ok"))
}
