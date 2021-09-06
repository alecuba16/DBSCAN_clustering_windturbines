check_acciona_rules_cm3 <- function(predicted_period = NULL,anticipation = 14, margin = 14,date_time_name='date_time',pre_alarm_name='pre_alarm',target_name='alarm') {
    iam=match.call()[[1]]
    #Dependencia basica
    if(!exists("dependencyLoader")){
        if(!file.exists('functions_common/dependencyLoader.R')) return(list(error=TRUE,data=NULL,msg=paste0("\n",iam,":Missing dependency function: functions_common/dependencyLoader.R")));
        source('functions_common/dependencyLoader.R')
    }
    libraries<-c('lubridate')
    dep<-dependencyLoader(c(libraries))
    if(dep$error)  return(list(error=TRUE,data=NULL,msg=paste0("\n",iam,":on call dependencyLoader\n",dep$msg)))
    
    # if(is.null(predicted_period)||is.na(predicted_period)) return(list(error=FALSE,data=predicted_period,msg="Ok but predicted_period is null"))
    if(is.null(predicted_period)) return(list(error=FALSE,data=predicted_period,msg="Ok but predicted_period is null"))
    
    if(!pre_alarm_name%in% names(predicted_period) ||!target_name%in% names(predicted_period) ) return(list(error=TRUE,data=NULL,msg=paste0("Missing ",pre_alarm_name," or ",target_name," in predicted_period")))
    
    # if(is.null(predicted_period[,pre_alarm_name])||is.na(predicted_period[,pre_alarm_name])||is.null(predicted_period[,target_name])||is.na(predicted_period[,target_name])) return(list(error=FALSE,data=predicted_period,msg="Ok but predicted_period is null"))
    if(is.null(predicted_period[,pre_alarm_name])||is.null(predicted_period[,target_name])) return(list(error=FALSE,data=predicted_period,msg="Ok but predicted_period is null"))
    #Defines
    var_name<-'conf_matrix3'
    #Create confusion matrix variable.
    predicted_period[var_name]<-factor(numeric(length(predicted_period[,pre_alarm_name])), levels = c('tp','fp','fn','tn'))
    
    #Fix date to posix (requirement for seq.POSIXt)
    date_time<-as.POSIXct(predicted_period[,date_time_name],tz='UTC',origin='1970-01-01')
    
    #Create ids of the real alarms
    alarm_ids<-(predicted_period[,target_name]=='t')
    
    #Create a temporary binary real_pre_alarm by default false
    real_pre_alarm<-rep('f',length(predicted_period[,pre_alarm_name]))
    
    if(any(alarm_ids)){
        #Get the real_prealarm vector of each alarm and the dates between alarm-horitzon -> alarm
        real_pre_alarm_dates<-as.POSIXct(unlist(lapply(date_time[alarm_ids],function(dt) seq.POSIXt(dt-as.difftime(anticipation+margin, units="days"),dt,by='day'))),origin='1970-01-01',tz = "UTC")
        #Asign true only to pre_real_alarm_dates.
        real_pre_alarm[date_time %in% real_pre_alarm_dates]<-'t'
    }
    #Generate consfusion matrix ids
    tp_ids<-which(predicted_period[,pre_alarm_name]=='t'&real_pre_alarm=='t')
    tn_ids<-which(predicted_period[,pre_alarm_name]=='f'&real_pre_alarm=='f')
    fn_ids<-which(predicted_period[,pre_alarm_name]=='f'&real_pre_alarm=='t')
    fp_ids<-which(predicted_period[,pre_alarm_name]=='t'&real_pre_alarm=='f')
    #Asign values
    predicted_period[tp_ids,var_name]<-'tp'
    predicted_period[tn_ids,var_name]<-'tn'
    predicted_period[fn_ids,var_name]<-'fn'
    predicted_period[fp_ids,var_name]<-'fp'
    return(list(error=FALSE,data=predicted_period,msg="Ok"))
}