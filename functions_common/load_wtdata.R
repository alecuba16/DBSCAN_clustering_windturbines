comb<-function(old,new){
  # Intersect
  if(!is.null(old)){
    column_names<-colnames(new$data$wtdata)
    if(!is.null(old$data$wtdata_final)) column_names<-intersect(colnames(old$data$wtdata_final),colnames(new$data$wtdata))
    wtdata_final<-rbind(old$data$wtdata_final[,column_names],new$data$wtdata[,column_names])
    column_names0<-colnames(new$data$wtdata0)
    wtdata0_final<-rbind(old$data$wtdata0_final[,column_names0],new$data$wtdata0[,column_names0])
    outliers_final<-rbind(old$data$outliers_final,new$data$outliers)
  }else{
    wtdata_final<-new$data$wtdata
    wtdata0_final<-new$data$wtdata0
    outliers_final<-new$data$outliers
  }
  return(list(error=new$error,warning=new$warning,data=list(wtdata_final=wtdata_final,wtdata0_final=wtdata0_final,outliers_final=outliers_final),msg=new$msg))
}

load_wtdata_helper<-function(p,
                             wt_query,
                             array_id_walm,
                             array_ot,
                             power_condition,
                             table_cast_park_dic,
                             wp_id,
                             wp_code,
                             ld_id,
                             fault,
                             include_variables,
                             exclude_variables,
                             artificial_variables,
                             unix_timestamp_ini,
                             unix_timestamp_end,
                             seconds_to_aggregate,
                             freq_dat_med_min,
                             target_name,
                             date_time_name,
                             filter_exclude,
                             debug_mode,
                             filter,
                             filter_verbose,
                             filter_range_data,
                             patch,
                             table_filter_config,
                             num,
                             db_config){
  debug_mode<-T
  current_wt_query<-NULL
  if(exists('wt_query',inherits = F)&&!is.null(wt_query)&&!is.na(wt_query)) current_wt_query<-wt_query[p,]
  
  alarm_condition=''
  if(!is.null(array_id_walm)&&!is.na(array_id_walm)&&!is.null(array_id_walm[p])&&!is.na(array_id_walm[p])&&is.character(array_id_walm[p])&&nchar(array_id_walm[p])>0) alarm_condition=paste0("id_walm IN (",array_id_walm[p],")")
  ot_condition=''
  if(!is.null(array_ot)&&!is.na(array_ot)&&!is.null(array_ot[p])&&!is.na(array_ot[p])&&is.character(array_ot[p])&&nchar(array_ot[p])>0) ot_condition=paste0("id_ot IN (",array_ot[p],")")
  
  if(is.null(power_condition)||is.na(power_condition)||is.null(power_condition[p])||is.na(power_condition[p])||!is.character(power_condition[p])) power_condition[p]=''
  if(exists('debug_mode')&&debug_mode) cat(paste0('\nLoad_wtdata:formatter call'))
  rs<-formatter(wt_query=current_wt_query,
                table_cast_park_dic=table_cast_park_dic,
                wp_id=wp_id[p],
                ld_id=ld_id[p],
                fault=fault[p],
                array_id_walm=alarm_condition,
                array_ot=ot_condition,
                power_condition=power_condition[p],
                include_variables=include_variables[p],
                exclude_variables=exclude_variables[p],
                artificial_variables=artificial_variables[p],
                unix_timestamp_ini=unix_timestamp_ini[p],
                unix_timestamp_end=unix_timestamp_end[p],
                seconds_to_aggregate=seconds_to_aggregate[p],
                target_name=target_name[p],
                date_time_name=date_time_name,
                db_config=db_config)
  if(rs$error){
    output_msg <- paste0("\n",iam,":on call formatter\n\t",rs$msg)
    close_protocol(output_msg, iam, debug_mode)
    return(list(error=TRUE,warning=F,data=NULL,msg=output_msg))
  }
  if(rs$warning){
    output_msg <- paste0("\n",iam,":on call formatter\n\t",rs$msg)
    close_protocol(output_msg, iam, debug_mode)
    return(list(error=F,warning=T,data=NULL,msg=output_msg))
  }
  wtdata0<-rs$data
  rm(rs)
  gc(verbose = F)
  
  if(exists('debug_mode')&&debug_mode) cat(paste0('\nLoad_wtdata:end formatter call'))
  
  #Check if target_name is on exclude list if not , then add it.
  if(!is.null(target_name)&&!is.na(target_name)&&!is.null(target_name[p])&&!is.na(target_name[p])&&!(target_name[p] %in% unlist(strsplit(filter_exclude,",")))) filter_exclude<-paste0(target_name,',',filter_exclude)
  
  if(exists('debug_mode')&&debug_mode) cat(paste0('\nLoad_wtdata:Filter call'))
  #####################################
  # Filter outliers
  rs<-filter_custom(config_table=table_filter_config,
                    filter=filter[p],
                    data=wtdata0,
                    wp_code=tolower(wp_code[p]),
                    exclude=filter_exclude,
                    date_time_name=date_time_name,
                    target_name=target_name[p],
                    table_cast_park_dic=table_cast_park_dic,
                    verbose=filter_verbose,
                    update_ranges=update_filter_ranges,
                    filter_range_data=filter_range_data,
                    db_config=db_config)
  if(rs$error){
    output_msg <- paste0("\n",iam,":on call filter_custom\n\t",rs$msg)
    close_protocol(output_msg, iam, debug_mode)
    return(list(error=TRUE,warning=F,data=NULL,msg=output_msg))
  }
  wtdata <- rs$data$clean
  if(('outliers' %in% names(rs$data)) && !is.null(rs$data$outliers) && !is.na(rs$data$outliers)&&nrow(rs$data$outliers)>0) {
    outliers<- rs$data$outliers
  }else{
    outliers<-NULL
  }
  rm(rs)
  gc(verbose = F)
  # Finish outliers
  if(exists('debug_mode')&&debug_mode) cat(paste0('\nLoad_wtdata:end filter call'))
  #####################################
  # Artificial variables:Analog variables
  if(exists('debug_mode')&&debug_mode) cat(paste0('\nLoad_wtdata:artificial analog variables call'))
  rs<-artificial_analog_variables(wt_query=current_wt_query,
                                  artificial_variables=artificial_variables[p],
                                  wtdata=wtdata,
                                  wtdata0=wtdata0,
                                  date_time_name=date_time_name,
                                  table_cast_park_dic=table_cast_park_dic,
                                  table_filter_config=table_filter_config,
                                  target_name=target_name[p],
                                  db_config=db_config)
  if(rs$error) return(list(error=TRUE,warning=F,data=NULL,msg=rs$msg))
  wtdata<-rs$data$wtdata
  wtdata0<-rs$data$wtdata0
  rm(rs)
  gc(verbose = F)
  # End artificial
  if(exists('debug_mode')&&debug_mode) cat(paste0('\nLoad_wtdata:end artificial analog variables'))
  if(!is.null(patch)&&!is.na(patch)&&patch){
    if(exists('debug_mode')&&debug_mode) cat(paste0('\nLoad_wtdata:artificial patch variables'))
    #####################################
    # PARCHE 2: Test using TempCojLOA_avg - TempCojLA_avg como entrada
    # TODO meter en artificial analog variables.
    if("TempCojLOA_avg" %in% names(wtdata) && "TempCojLA_avg" %in% names(wtdata)) {
      wtdata$diff_tempcoj <- wtdata$TempCojLOA_avg - wtdata$TempCojLA_avg
      wtdata$rate_tempcoj <- wtdata$TempCojLOA_avg / wtdata$TempCojLA_avg
      wtdata$rate_tempcoj[is.infinite(wtdata$rate_tempcoj) | is.nan(wtdata$rate_tempcoj)] <- NA
    }
    if("TempAceiteMultip_avg" %in% names(wtdata) && "TempMultip_avg" %in% names(wtdata)) {
      wtdata$diff_tempmult <- wtdata$TempMultip_avg - wtdata$TempAceiteMultip_avg    
      wtdata$rate_tempmult <- wtdata$TempMultip_avg / wtdata$TempAceiteMultip_avg
      wtdata$rate_tempmult[is.infinite(wtdata$rate_tempmult) | is.nan(wtdata$rate_tempmult)] <- NA
      wtdata$rate_tempmult_oil <- wtdata$TempAceiteMultip_avg / wtdata$TempMultip_avg
      wtdata$rate_tempmult_oil[is.infinite(wtdata$rate_tempmult_oil) | is.nan(wtdata$rate_tempmult_oil)] <- NA
    }
    if("TempAceiteMultip_avg" %in% names(wtdata) && "TempRodamMultip_avg" %in% names(wtdata)) {
      wtdata$diff_tempmult <- wtdata$TempRodamMultip_avg - wtdata$TempAceiteMultip_avg    
      wtdata$rate_tempmult <- wtdata$TempRodamMultip_avg / wtdata$TempAceiteMultip_avg
      wtdata$rate_tempmult[is.infinite(wtdata$rate_tempmult) | is.nan(wtdata$rate_tempmult)] <- NA
      wtdata$rate_tempmult_oil <- wtdata$TempAceiteMultip_avg / wtdata$TempRodamMultip_avg
      wtdata$rate_tempmult_oil[is.infinite(wtdata$rate_tempmult_oil) | is.nan(wtdata$rate_tempmult_oil)] <- NA
    }
    # PARCHE 3: Test using KPIs related with energy input and energy output
    # TODO meter en artificial analog variables.
    if("TempAmb_avg" %in% names(wtdata) && "VelViento_avg" %in% names(wtdata) && "Pot_avg" %in% names(wtdata) && "TempAceiteMultip_avg" %in% names(wtdata) && "TempRodamMultip_avg" %in% names(wtdata)) {
      wtdata$rate_energy_mult <- with(wtdata, Pot_avg/(TempRodamMultip_avg + TempAceiteMultip_avg - TempAmb_avg))
      wtdata$rate_energy_mult[is.infinite(wtdata$rate_energy_mult) | is.nan(wtdata$rate_energy_mult)] <- NA
    }
    if("TempAmb_avg" %in% names(wtdata) && "VelViento_avg" %in% names(wtdata) && "Pot_avg" %in% names(wtdata) && "TempAceiteMultip_avg" %in% names(wtdata) && "TempMultip_avg" %in% names(wtdata)) {
      wtdata$rate_energy_mult <- with(wtdata, Pot_avg/(TempMultip_avg + TempAceiteMultip_avg - TempAmb_avg))
      wtdata$rate_energy_mult[is.infinite(wtdata$rate_energy_mult) | is.nan(wtdata$rate_energy_mult)] <- NA
    }
    if("TempAmb_avg" %in% names(wtdata) && "VelViento_avg" %in% names(wtdata) && "Pot_avg" %in% names(wtdata) && "TempCojLOA_avg" %in% names(wtdata) && "TempCojLA_avg" %in% names(wtdata)) {
      wtdata$rate_energy_gen <- with(wtdata, Pot_avg/(TempCojLA_avg + TempCojLOA_avg - TempAmb_avg))
      wtdata$rate_energy_gen[is.infinite(wtdata$rate_energy_gen) | is.nan(wtdata$rate_energy_gen)] <- NA
    }
    if(exists('debug_mode')&&debug_mode) cat(paste0('\nLoad_wtdata:end artificial patch variables'))
  }        
  #####################################
  # Aggregate zone 
  if(seconds_to_aggregate[p]>(freq_dat_med_min[p]*60)){#Only aggregate if needed
    if(exists('debug_mode')&&debug_mode) cat(paste0('\nLoad_wtdata:aggregate seconds_to_aggregate zone'))
    wtdata[,date_time_name]<-as.POSIXct((as.numeric(wtdata[,date_time_name])%/%seconds_to_aggregate[p])*seconds_to_aggregate[p],origin='1970-01-01',tz = "UTC")
    wtdata0[,date_time_name]<-as.POSIXct((as.numeric(wtdata0[,date_time_name])%/%seconds_to_aggregate[p])*seconds_to_aggregate[p],origin='1970-01-01',tz = "UTC")
    
    exclude_columns<-c('id','ld_id',"alarm","alarm_block_code","alarm_all","alarm_all_block_code","ot","ot_block_code","ot_all","ot_all_block_code","n1","production")
    if(!is.null(target_name)&&!is.na(target_name)&&length(unique(wtdata[,target_name]))<=2) exclude_columns<-c(target_name,exclude_columns); #Target is binary exclude for max
    
    columns_to_aggregate<-data.frame(wtdata[, !(names(wtdata) %in% exclude_columns)])
    if(ncol(columns_to_aggregate)<2) return(list(error=TRUE,warning=F,data=NULL,msg=paste0("\n",iam,": no data to process for ld_id:",ld_id," at the range (",as.POSIXct(unix_timestamp_ini,origin="1970-01-01",tz="UTC"),",",as.POSIXct(unix_timestamp_end,origin="1970-01-01",tz="UTC"),")")))
    wtdata_out_day<-data.frame(columns_to_aggregate %>% group_by_(as.symbol(date_time_name)) %>% summarise_all(funs(mean(., na.rm = T))))
    wtdata0_out_day<-data.frame(wtdata0[, !(names(wtdata0) %in% exclude_columns)] %>% group_by_(as.symbol(date_time_name)) %>% summarise_all(funs(mean(., na.rm = T))))
    
    sum_columns<-c("production","nrecords");# Production is a sum of active power / 6 not average
    if(any(sum_columns %in% names(wtdata)))
    {
      sum_data<-wtdata[,  (names(wtdata) %in% c(date_time_name,sum_columns))] %>% group_by_(as.symbol(date_time_name)) %>% summarise_all(funs(sum(., na.rm = T)))
      to_replace<-(names(wtdata_out_day) %in% sum_columns)
      if(any(to_replace)) wtdata_out_day<-wtdata_out_day[,!(to_replace)]
      wtdata_out_day<-cbind(wtdata_out_day,sum_data[,!(names(sum_data) %in% date_time_name)])
      sum_data0<-wtdata0[,  (names(wtdata0) %in% c(date_time_name,sum_columns))] %>% group_by_(as.symbol(date_time_name)) %>% summarise_all(funs(sum(., na.rm = T)))
      to_replace0<-(names(wtdata0_out_day) %in% sum_columns)
      if(any(to_replace0)) wtdata0_out_day<-wtdata0_out_day[,!(to_replace0)]
      wtdata0_out_day<-cbind(wtdata0_out_day,sum_data0[,!(names(sum_data0) %in% date_time_name)])
    }
    
    alarm_ot_columns<-c("alarm","alarm_all","ot","ot_all");
    if(!is.na(target_name)&&!is.null(target_name)&&length(unique(wtdata[,target_name]))<=2&&!(target_name %in% alarm_ot_columns)) alarm_ot_columns<-c(target_name,alarm_ot_columns); #Target is binary
    if(any(alarm_ot_columns %in% names(wtdata)))
    {
      alarm_ot_columns<-c(date_time_name,alarm_ot_columns);
      alarm_ot_columns<-alarm_ot_columns[alarm_ot_columns %in% names(wtdata)];
      grouped_alarm_ot<-wtdata[,  (names(wtdata) %in% alarm_ot_columns)] %>% group_by_(as.symbol(date_time_name)) %>% summarise_all(funs(max(., na.rm = T)))
      wtdata_out_day<-cbind(wtdata_out_day,grouped_alarm_ot[,!(names(grouped_alarm_ot) %in% date_time_name)])
      grouped0_alarm_ot<-wtdata0[,  (names(wtdata0) %in% alarm_ot_columns)] %>% group_by_(as.symbol(date_time_name)) %>% summarise_all(funs(max(., na.rm = T)))
      wtdata0_out_day<-cbind(wtdata0_out_day,grouped0_alarm_ot[,!(names(grouped0_alarm_ot) %in% date_time_name)])
    }
    
    alarm_ot_desc_columns<-c("alarm_block_code","alarm_all_block_code","ot_block_code","ot_all_block_code");
    if(any(alarm_ot_desc_columns %in% names(wtdata))){
      alarm_ot_desc_columns<-c(date_time_name,alarm_ot_desc_columns);
      alarm_ot_desc_columns<-alarm_ot_desc_columns[alarm_ot_desc_columns %in% names(wtdata)];
      grouped_alarm_ot_desc<-wtdata[, (names(wtdata) %in% alarm_ot_desc_columns)] %>% group_by_(as.symbol(date_time_name)) %>% summarise_all(funs(paste(unique(unlist(strsplit(.,','))),collapse = ',')))
      wtdata<-cbind(wtdata_out_day,grouped_alarm_ot_desc[,!(names(grouped_alarm_ot_desc) %in% date_time_name)])
      grouped0_alarm_ot_desc<-wtdata0[, (names(wtdata0) %in% alarm_ot_desc_columns)] %>% group_by_(as.symbol(date_time_name)) %>% summarise_all(funs(paste(unique(unlist(strsplit(.,','))),collapse = ',')))
      wtdata0<-cbind(wtdata0_out_day,grouped0_alarm_ot_desc[,!(names(grouped0_alarm_ot_desc) %in% date_time_name)])
    }else{
      wtdata<-wtdata_out_day
      wtdata0<-wtdata0_out_day
    }
    if(exists('debug_mode')&&debug_mode) cat(paste0('\nLoad_wtdata:end aggregate seconds_to_aggregate zone'))
    rm(list = c('wtdata_out_day','wtdata0_out_day'))
    gc(verbose = F)
  }
  #### End aggregate
  
  if(exists('debug_mode')&&debug_mode) cat(paste0('\nLoad_wtdata:artificial event call'))
  #####################################
  # Artificial variables:Events , number of alarms per week , etc.
  rs<-artificial_event_variables(wt_query=current_wt_query,
                                 wp_id=wp_id[p],
                                 ld_id=ld_id[p],
                                 wtdata=wtdata,
                                 wtdata0=wtdata0,
                                 artificial_variables=artificial_variables[p],
                                 unix_timestamp_ini=unix_timestamp_ini[p],
                                 unix_timestamp_end=unix_timestamp_end[p],
                                 freq_dat_med_min=freq_dat_med_min[p],
                                 seconds_to_aggregate=seconds_to_aggregate[p],
                                 date_time_name=date_time_name,
                                 table_cast_park_dic=table_cast_park_dic,
                                 table_artificial_config=table_artificial_config,
                                 db_config=db_config)
  if(rs$error) return(list(error=TRUE,warning=F,data=NULL,msg=rs$msg))
  wtdata<-rs$data$wtdata
  wtdata0<-rs$data$wtdata0
  rm(rs)
  gc(verbose = F)
  if(exists('debug_mode')&&debug_mode) cat(paste0('\nLoad_wtdata:end artificial event call'))
  # Finish Artificial variables
  
  #####################################
  # Build weekly_production 
  if("production" %in% names(wtdata)
     #Include condition
     && (is.null(include_variables) || is.na(include_variables) || is.null(include_variables[p]) || is.na(include_variables[p]) || nchar(include_variables[p])<=1 || ('weekly_production' %in% unlist(strsplit(include_variables[p],',')))) 
     #Exclude condition
     && (!is.null(exclude_variables) && !is.na(exclude_variables) && !is.null(exclude_variables[p]) && !is.na(exclude_variables[p])  && !'weekly_production' %in% unlist(strsplit(exclude_variables[p],',')))) {
    wtdata$weekly_production <- zoo::rollapplyr(wtdata$production, 7*24*60*60/seconds_to_aggregate[p], FUN = sum, fill = NA, na.rm = TRUE)
    wtdata0$weekly_production <- zoo::rollapplyr(wtdata0$production, 7*24*60*60/seconds_to_aggregate[p], FUN = sum, fill = NA, na.rm = TRUE)
  }
  #### End build weekly_production
  
  #### Target variable always at the end of the dataset
  if(!is.null(target_name)&&!is.na(target_name)&&!is.null(target_name[p])&&!is.na(target_name[p])){
    itarget <- which( names(wtdata) == target_name[p] )
    wtdata <- dplyr::select(wtdata, -itarget, itarget)
    itarget0 <- which( names(wtdata0) == target_name[p] )
    wtdata0 <- dplyr::select(wtdata0, -itarget0, itarget0)
  }
  
  #Fix NaN infinite
  is.na(wtdata) <- sapply(wtdata, is.infinite)
  is.na(wtdata) <- sapply(wtdata, is.nan)
  is.na(wtdata0) <- sapply(wtdata0, is.infinite)
  is.na(wtdata0) <- sapply(wtdata0, is.nan)
  
  if(length(num)>1){# we have several machines, then check if the LD_ID column is present
    if(!('ld_id' %in% names(wtdata))) wtdata$ld_id<-ld_id[p]
    if(!('ld_id' %in% names(wtdata0))) wtdata0$ld_id<-ld_id[p]
    if(any(!is.null(outliers))&&any(!is.na(outliers))&&!('ld_id' %in% names(outliers))) outliers$ld_id<-ld_id[p]
  }
  
  #Add good bad
  if(!is.null(wt_query)&&!is.na(wt_query)&&(('health_status' %in% names(wt_query)&&any(!is.null(wt_query$health_status))&&any(!is.na(wt_query$health_status))))){
    wtdata$health_status<-wt_query$health_status[p]
    wtdata0$health_status<-wt_query$health_status[p]
    if(any(!is.null(outliers))&&any(!is.na(outliers))&&!('ld_id' %in% names(outliers))) outliers$health_status<-wt_query$health_status[p]
  }
  return(list(error=F,warning=F,data=list(wtdata=wtdata,wtdata0=wtdata0,outliers=outliers)))
}

#version v4.0
load_wtdata <- function(wt_query=NULL,
                        ld_id=wt_query$ld_id,
                        wp_id=wt_query$wp_id,
                        wp_code=wt_query$wp_code,
                        fault=wt_query$fault,
                        array_id_walm=wt_query$array_id_walm,
                        array_ot=wt_query$array_ot,
                        power_condition=wt_query$power_condition,
                        include_variables=wt_query$include_variables,
                        exclude_variables=wt_query$exclude_variables,
                        artificial_variables=wt_query$artificial_variables,
                        unix_timestamp_ini=wt_query$creation_wtdata_date_ini,
                        unix_timestamp_end=wt_query$creation_wtdata_date_end,
                        freq_dat_med_min=wt_query$freq_dat_med_min,
                        seconds_to_aggregate=wt_query$seconds_to_aggregate,
                        seconds_offset=0,
                        date_time_name='date_time',
                        target_name=wt_query$target_name,
                        table_cast_park_dic='1_cast_park_table_dic',
                        table_filter_config='1_filter_config',
                        table_artificial_config='1_artificial_config',
                        filter=wt_query$filter,
                        filter_exclude='date_time',
                        filter_range_data=NULL,
                        update_filter_ranges=FALSE,
                        filter_verbose=T,
                        show_progress=T,
                        patch=T,
                        logfile=NULL,
                        parallel=T,
                        db_config) {
  iam=match.call()[[1]]
  #Dependencias
  if(!exists("dependencyLoader")){
    if(!file.exists('functions_common/dependencyLoader.R')) return(list(error=TRUE,warning=F,data=NULL,msg=paste0("\n",iam,":Missing dependency function: functions_common/dependencyLoader.R")));
    source('functions_common/dependencyLoader.R')
  }
  sourcesandlib<-c('parallel','doSNOW','dplyr','zoo',paste0('functions_common/',c('filter_custom.R','formatter.R','artificial_analog_variables.R',"artificial_event_variables.R",'close_protocol.R')))
  dep<-dependencyLoader(sourcesandlib)
  if(dep$error) return(list(error=TRUE, warning=FALSE, data=NULL,msg=paste0("\n",iam,":on call dependencyLoader\n",dep$msg)))
  
  if(!exists('debug_mode',inherits = F))
    debug_mode<-F
  else
    debug_mode<-debug_mode
  
  num<-1:length(ld_id)
  wtdata_final<-NULL
  wtdata0_final<-NULL
  outliers_final<-NULL
  if(show_progress){
    pb<-txtProgressBar(min=0,max=max(num),initial=0,title='Load_wtdata download',style=3)
  }
  
  if(parallel){
    mb_per_thread<-3000
    if(Sys.info()[['sysname']]=="Linux"){
      free<-as.numeric(system("awk '/MemAvailable/ {print $2}' /proc/meminfo",intern=TRUE))
    }else{#windows
      free<-as.numeric(gsub(x=paste0(system("wmic OS get FreePhysicalMemory /Value",intern = T),collapse = ''),pattern = '.*=([0-9]+)',replacement = '\\1'))
    }
    if(is.na(free)||is.nan(free)||is.infinite(free)) free<-1048576
    num_threads<-floor((free/1024)/mb_per_thread)
    num_threads<-min(num_threads,length(num))
    num_threads<-min(num_threads,parallel::detectCores())
    num_threads<-min(length(num),floor(num_threads))
    num_threads<-max(num_threads,1) #If numthreads is 0 force to be 1.
    if(is.null(logfile)||is.na(logfile)){
      cl <- parallel::makePSOCKcluster(num_threads)
    }else{
      cl <- parallel::makePSOCKcluster(num_threads,outfile=logfile)
    }
    doSNOW::registerDoSNOW(cl)
    progress <- function(n) setTxtProgressBar(pb, n)
    opts <- list(progress = progress)
    rs <- foreach(p = num,.verbose = F,.packages = c('zoo'),.export = c('load_wtdata_helper','filter_custom','formatter','artificial_analog_variables','artificial_event_variables','close_protocol'), .combine = 'comb',.options.snow = opts) %dopar% {
      load_wtdata_helper(p,wt_query,array_id_walm,array_ot,power_condition,table_cast_park_dic,wp_id,wp_code,ld_id,fault,include_variables,exclude_variables,artificial_variables,unix_timestamp_ini,unix_timestamp_end,seconds_to_aggregate,freq_dat_med_min,target_name,date_time_name,filter_exclude,debug_mode,filter,filter_verbose,filter_range_data,patch,table_filter_config,num,db_config)
    }
    stopCluster(cl)
    if(rs$error)  return(list(error=T,warning=F,data=NULL,msg="OK"))
    wtdata_final<-rs$data$wtdata_final
    wtdata0_final<-rs$data$wtdata0_final
    outliers_final<-rs$data$outliers_final
    rm(rs)
    gc(verbose = F)
  }else{
    for(p in num){
      rs<-load_wtdata_helper(p,wt_query,array_id_walm,array_ot,power_condition,table_cast_park_dic,wp_id,wp_code,ld_id,fault,include_variables,exclude_variables,artificial_variables,unix_timestamp_ini,unix_timestamp_end,seconds_to_aggregate,freq_dat_med_min,target_name,date_time_name,filter_exclude,debug_mode,filter,filter_verbose,filter_range_data,patch,table_filter_config,num,db_config)
      if(rs$error) return(list(error=T,warning=F,data=NULL,msg="OK"))
      wtdata<-rs$data$wtdata
      wtdata0<-rs$data$wtdata0
      outliers<-rs$data$outliers
      column_names<-colnames(wtdata)
      if(!is.null(wtdata_final)) column_names<-intersect(colnames(wtdata_final),colnames(wtdata))
      wtdata_final<-rbind(wtdata_final[,column_names],wtdata[,column_names])
      column_names0<-colnames(wtdata0)
      wtdata0_final<-rbind(wtdata0_final[,column_names0],wtdata0[,column_names0])
      outliers_final<-rbind(outliers_final,outliers)
      if(show_progress){
        setTxtProgressBar(pb,p)
      }
    }
  }
  if(show_progress) close(pb)
  
  return(list(error=FALSE,warning=F,data=list(wtdata0=wtdata0_final,wtdata=wtdata_final,outliers=outliers_final),msg="OK"))
}
