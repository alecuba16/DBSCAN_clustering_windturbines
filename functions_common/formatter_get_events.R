formatter_get_events<-function(wt_query=NULL,
                               wtdata=NULL,
                               table=NULL,
                               id_field,
                               output_field1='event',
                               output_field2='event_block_code',
                               ld_id=wt_query$ld_id,
                               array_id=NULL,
                               freq_dat_med_min=wt_query$freq_dat_med_min,
                               unix_timestamp_ini=wt_query$creation_wtdata_date_ini,
                               unix_timestamp_end=wt_query$creation_wtdata_date_end,
                               seconds_to_aggregate=wt_query$seconds_to_aggregate,
                               include_variables=wt_query$include_variables,
                               exclude_variables=wt_query$exclude_variables,
                               check_event_enable=T,
                               date_time_name='date_time',
                               db_date_time='date_time',
                               db_date_time_end='date_time_end',
                               target_name=NULL,
                               db_config)
{
    iam=match.call()[[1]]
    #Dependencias
    if(!exists("dependencyLoader")){
        if(!file.exists('functions_common/dependencyLoader.R')) return(list(error=TRUE,data=NULL,msg=paste0("\n",iam,":Missing dependency function: functions_common/dependencyLoader.R")));
        source('functions_common/dependencyLoader.R')
    }
    dep<-dependencyLoader(c('dplyr','functions_common/db_query.R','functions_common/formatter_include_exclude.R'))
    if(dep$error) return(list(error=TRUE, warning=FALSE, data=NULL,msg=paste0("\n",iam,":on call dependencyLoader\n",dep$msg)))
    
    if(is.na(table)) return(list(error=FALSE,data=list(wtdata=wtdata,events_in_interval=NA),msg="\n Ok, skipping events because there is no table"))
    
    if(is.null(array_id)||is.na(array_id)||(!is.null(array_id)&&!is.na(array_id)&&(nchar(array_id)>1))){}
    
    rs<-formatter_include_exclude(variables=paste0(output_field1,',',output_field2),date_time_name=date_time_name,include_variables=include_variables,exclude_variables=exclude_variables,target_name=target_name)
    if(rs$error) return(list(error=TRUE,data=NULL,msg=rs$msg))
    columns<-rs$data
    if(length(columns)<=1&&!any(c(output_field1,output_field2) %in% columns)) return(list(error=FALSE,data=list(wtdata=wtdata,events_in_interval=NA),msg="\n ok: no columns selected"))
    
    #start <- Sys.time()
    
    # Defines
    #Alarm enable column name
    event_enable<-'alarm_enable'
    #event_timeout<-'7 days'
    event_timeout<-7*24*60*60 #In seconds
    date_time_end_name<-paste0(date_time_name,'_end')
    status_field_name<-'status'
    
    #Check if event table has date_time_end
    query <- paste0("SHOW COLUMNS FROM ", table," where Field='",db_date_time_end,"'")
    rs <- db_query(query = query, db_config = db_config)
    if(rs$error) return(list(error=TRUE,data=NULL,msg=rs$msg))
    columns<-rs$data
    
    #Prepare events condition
    if(!is.null(array_id)&&!is.na(array_id)&&is.character(array_id)&&nchar(array_id)>1){
        if(!grepl(pattern = 'IN',x = array_id)){
            array_id<-paste0(id_field,' IN(',array_id,') AND')
        }else{
            array_id<-paste0(array_id,' AND')
        }
    }else{
        array_id<-''
    }
    
    #By default no events
    wtdata[output_field1]<-0
    wtdata[output_field2]<-''
    events_in_interval<-NA
    
    #Events calculation
    if(nrow(columns)>0){
        ### events with date ini and end.
        query <- paste0("SELECT ",id_field,", GREATEST(",db_date_time,",FROM_UNIXTIME(",as.numeric(unix_timestamp_ini),")) as ",date_time_name,",LEAST(",db_date_time_end,",FROM_UNIXTIME(",as.numeric(unix_timestamp_end),")) as ",date_time_end_name," from  ", table," where ",array_id," ld_id=",ld_id," AND NOT(",db_date_time,">FROM_UNIXTIME(",as.numeric(unix_timestamp_end),") OR ",db_date_time_end,"<FROM_UNIXTIME(",as.numeric(unix_timestamp_ini),"))")
        rs <- db_query(query = query, db_config = db_config)
        if(rs$error) return(list(error=TRUE,data=NULL,msg=rs$msg))
        events_in_interval<-rs$data
        #Date to Posixct
        events_in_interval[,date_time_name]<-as.POSIXct(events_in_interval[,date_time_name],tz='UTC',origin='1970-01-01')
        events_in_interval[,date_time_end_name]<-as.POSIXct(events_in_interval[,date_time_end_name],tz='UTC',origin='1970-01-01')
    }else{
        ### events type ON/OFF
        #Check Required status column
        query <- paste0("SHOW COLUMNS FROM ", table," where Field='",status_field_name,"'")
        rs2 <- db_query(query = query, db_config = db_config)
        if(rs2$error) return(list(error=TRUE,data=NULL,msg=rs2$msg))
        if(nrow(rs2$data)<=0) return(list(error=TRUE,data=NULL,msg=paste0("Missing column '",status_field_name,"' from ",table," which is required to create ON/OFF")))
        
        #Query ON events
        query<- paste0('SELECT UNIX_TIMESTAMP(',db_date_time,') as ',date_time_name,',',id_field,' from ',table,' where ',db_date_time,' BETWEEN FROM_UNIXTIME(',as.numeric(unix_timestamp_ini-seconds_to_aggregate),') AND FROM_UNIXTIME(',as.numeric(unix_timestamp_end+seconds_to_aggregate),') AND IF(LENGTH(`',status_field_name,'`)=1,`',status_field_name,'`=1,`',status_field_name,'`=\'ON\') AND ',array_id,' ld_id = ',ld_id,' order by ',db_date_time,' ASC,',id_field,' ASC');
        
        if(check_event_enable){
            #Check if event table has date_time_end
            query2 <- paste0("SHOW COLUMNS FROM ", table," where Field='",event_enable,"'")
            rs <- db_query(query = query2, db_config = db_config)
            if(rs$error) return(list(error=TRUE,data=NULL,msg=rs$msg))
            columns<-rs$data
            if(nrow(columns)>0){
                query<- paste0('SELECT UNIX_TIMESTAMP(',db_date_time,') as ',date_time_name,',',id_field,' from ',table,' where ',db_date_time,' BETWEEN FROM_UNIXTIME(',as.numeric(unix_timestamp_ini-seconds_to_aggregate),') AND FROM_UNIXTIME(',as.numeric(unix_timestamp_end+seconds_to_aggregate),') AND IF(LENGTH(`',status_field_name,'`)=1,`',status_field_name,'`=1,`',status_field_name,'`=\'ON\') AND ',array_id,' ld_id = ',ld_id,' AND ',event_enable,'!=0 order by ',db_date_time,' ASC,',id_field,' ASC');
                #Download alarm enable=0 for alarms without off
                query_alt_off<- paste0('SELECT UNIX_TIMESTAMP(',db_date_time,') as ',date_time_name,' from ',table,' where ',db_date_time,' BETWEEN FROM_UNIXTIME(',as.numeric(unix_timestamp_ini-seconds_to_aggregate),') AND FROM_UNIXTIME(',as.numeric(unix_timestamp_end+seconds_to_aggregate),') AND ',array_id,' ld_id = ',ld_id,' AND ',event_enable,'=0 order by ',db_date_time,' ASC,',id_field,' ASC');
                rs <- db_query(query = query_alt_off, db_config = db_config)
                if(rs$error) return(list(error=TRUE,data=NULL,msg=rs$msg))
                alt_off<-rs$data
            }
        }
        #st<-Sys.time()
        # Download on events.
        rs <- db_query(query = query, db_config = db_config)
        if(rs$error) return(list(error=TRUE,data=NULL,msg=rs$msg))
        events_on<-rs$data
        if(nrow(events_on)>0){
            #end1<-difftime(Sys.time(), st, tz='UTC',units = c("secs"))
            events_on[,date_time_name]<-as.POSIXct(events_on[,date_time_name],origin = '1970-01-01',tz='UTC')
            # Download off events.
            query<- paste0('SELECT UNIX_TIMESTAMP(date_time) as date_time,',id_field,' from ',table,' where date_time BETWEEN FROM_UNIXTIME(',as.numeric(unix_timestamp_ini-seconds_to_aggregate),') AND FROM_UNIXTIME(',as.numeric(unix_timestamp_end+seconds_to_aggregate),') AND IF(LENGTH(`',status_field_name,'`)=1,`',status_field_name,'`=0,`',status_field_name,'`=\'OFF\') AND ',array_id,' ld_id = ',ld_id,' order by date_time ASC,',id_field,' ASC');
            rs <- db_query(query = query, db_config = db_config)
            if(rs$error) return(list(error=TRUE,data=NULL,msg=rs$msg))
            events_off<-rs$data
            events_off[,date_time_name]<-as.POSIXct(events_off[,date_time_name],origin = '1970-01-01',tz='UTC')
            
            #st<-Sys.time()
            #Merge on/off pairs.
            #TODO OPTIMIZE!
            events_in_interval<-by(events_on, 1:nrow(events_on), function(e_on){
                id_field_value<-e_on[,id_field]
                off_timeout<-as.numeric(e_on[,date_time_name])+event_timeout
                same_event_on<-as.numeric(events_on[id_field_value==events_on[,id_field],date_time_name])
                
                next_on_event_timeout<-same_event_on[(same_event_on>e_on[,date_time_name])&(same_event_on<=off_timeout)][1]
                
                same_event_off<-as.numeric(events_off[id_field_value==events_off[,id_field],date_time_name])
                off_event<-(same_event_off>=as.numeric(e_on[,date_time_name]))
                if(length(off_event)>0 && any(off_event)){#Check if there is off
                    e_off<-same_event_off[which(off_event==T)[1]] #By default off 
                    if((!is.na(next_on_event_timeout))&&(next_on_event_timeout<=e_off)&&off_timeout<e_off){#There is on between on/off and outside of timeout
                        e_off<-off_timeout
                    }
                }else if(!is.na(next_on_event_timeout)){#Missing off check if there is another ON event in timeout range
                    e_off<-next_on_event_timeout
                }else{
                    e_off<-off_timeout
                }
                df<-c(id_field_value,e_on[,date_time_name],e_off)
                return(df)
            })

            #To dataframe
            events_in_interval<-do.call(rbind, events_in_interval)
            events_in_interval<-as.data.frame(events_in_interval,stringsAsFactors = False)
            colnames(events_in_interval)<-c(id_field,date_time_name,date_time_end_name)
            events_in_interval[,date_time_name]<-as.POSIXct(events_in_interval[,date_time_name],tz = 'UTC',origin='1970-01-01')
            events_in_interval[,date_time_end_name]<-as.POSIXct(events_in_interval[,date_time_end_name],tz = 'UTC',origin='1970-01-01')
            #end2<-difftime(Sys.time(), st, tz='UTC',units = c("secs"))
            #cat(paste0("\n----- Create ON-OFF Pairs on:",end1," Download off and merge:",end2))
        }
    }
    
    if(exists("events_in_interval",inherits = F)&&!all(is.null(events_in_interval))&&!all(is.na(events_in_interval))&&is.data.frame(events_in_interval)&&(nrow(events_in_interval)>0)){
        #Round to machine frequency freq_dat_med_min
        #st<-Sys.time()
        events_in_interval[,date_time_name]<-as.POSIXct(floor(as.numeric(events_in_interval[,date_time_name])/seconds_to_aggregate)*seconds_to_aggregate,origin='1970-01-01',tz='UTC') #Round floor
        events_in_interval[,date_time_end_name]<-as.POSIXct(floor(as.numeric(events_in_interval[,date_time_end_name])/seconds_to_aggregate)*seconds_to_aggregate,origin='1970-01-01',tz='UTC') #Round floor
        #V2 individual wtdata and merge -500ms avg SQL formatter
        active<-lapply(1:nrow(events_in_interval),function(e){
          selected_rows<-((wtdata[,date_time_name] >= events_in_interval[e,date_time_name]) & (wtdata[,date_time_name] <= events_in_interval[e,date_time_end_name]))
          if(any(selected_rows)){
            num_elements<-length(events_in_interval[e,id_field])
            todo<-sum(selected_rows)-num_elements
            dates<-wtdata[selected_rows,date_time_name]
            return(matrix(c(dates,rep(events_in_interval[e,id_field],num_elements+todo)),ncol=2,byrow = F))
          }
        })
        active<-do.call(rbind, active)

        if(any(!is.null(active))&&(nrow(active)>0)){
            active<-data.frame(date_time=active[,1],output_field2=active[,2])
            names(active)[names(active)=='date_time']<-date_time_name
            #names(active)[names(active)=='output_field2']<-output_field2
            active[,date_time_name]<-as.POSIXct(active[,date_time_name],tz = 'UTC',origin='1970-01-01')
            #end3<-difftime(Sys.time(), st, tz='UTC',units = c("secs"))
            #st<-Sys.time()
            
            #active<-active %>% group_by(date_time_name) %>% summarise(date_time= max(eval(as.name(date_time_name))),output_field2 = paste0(unique(eval(as.name(output_field2))),collapse=','))
            active<-active %>% group_by_(as.symbol(date_time_name)) %>% summarise_all(funs(output_field2 = paste0(unique(output_field2),collapse=',')))
            active<-as.data.frame(do.call(cbind,active),stringsAsFactors = F)
            names(active)[names(active)=='output_field2']<-output_field2
            names(active)[names(active)=='date_time']<-date_time_name
            active[,date_time_name]<-as.POSIXct(as.numeric(active[,date_time_name]),tz='UTC',origin='1970-01-01')
            wtdata[wtdata[,date_time_name] %in% active[,date_time_name],output_field1]<-1
            wtdata[wtdata[,date_time_name] %in% active[,date_time_name],output_field2]<-active[,output_field2]
        }
        #End V2
        #end4<-difftime(Sys.time(), st, tz='UTC',units = c("secs"))
        #cat(paste0(" create alarm registers:",end3,"  merge alarm registers wtdata:",end4))
    }

    #Exclude or include variables
    rs<-formatter_include_exclude(variables=names(wtdata),include_variables=include_variables,exclude_variables=exclude_variables,target_name = target_name)
    if(rs$error) return(list(error=TRUE,data=NULL,msg=rs$msg))
    columns<-rs$data
    columns<-names(wtdata)[(names(wtdata) %in% columns)]
    wtdata<-wtdata[,(names(wtdata) %in% columns)]
    #For debug runtime.
    #cat(paste0('Selected ',output_field1,' + ',output_field2,' takes ',Sys.time() - start),' \n')
    return(list(error=FALSE,data=list(wtdata=wtdata,events_in_interval=events_in_interval),msg="ok"))
}
