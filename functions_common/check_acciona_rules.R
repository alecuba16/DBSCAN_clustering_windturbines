#' check_acciona_rules:
#' This function check the acciona rules:
#' Acciona rules require a minimum of 14 days of anticipation of the alarm and 
#' give us 14 days after prediction of margin (time allowed). In others words, 
#' if the 01-January a 14_days pre-alarm is activated, the real alarm has to 
#' occurr between the 15-January and 29-January. If the fault falls in this 
#' interval it will be marked a True positive, otherwise, it will be marked a 
#' False Positive.
#' 
#' This function return a factor indicating if the sample is a True positive,
#' False Positive, False Negative or True Negative accordint to acciona's rules.
#'
#' @param date_time 
#' @param pre_alarm 
#' @param alarm 
#' @param prediction 
#' @param anticipation 
#' @param margin
#' @param critic_fault: if TRUE, just the first alarm is checked others are 
#' converted to NAtp or NAfn according to TP or FN. 
#'
#' @return
#' @export
#'
#' @examples
#' date_time <- as.POSIXct("2014-12-31"), origin = "1970-01-01",tz='UTC')
#' pre_alarm <- sample(c(0,1), 365, replace = TRUE)
#' alarm <- numeric(length(date_time))
#' id_alarm <- sample(1:length(date_time), 3, replace = FALSE)
#' alarm[id_alarm] <- 1
#' anticipation <- 14
#' margin <- 14
#' critic_fault = TRUE
#' check_acciona_rules(date_time, pre_alarm, alarm, anticipation, margin)
check_acciona_rules <- function(prediction = NULL, 
                                date_time = prediction$date_time, 
                                pre_alarm = prediction$pre_alarm, 
                                alarm = prediction$alarm, 
                                anticipation = 14, margin = 14, critic_fault = TRUE) {
    iam=match.call()[[1]]
    #Dependencia basica
    if(!exists("dependencyLoader")){
        if(!file.exists('functions_common/dependencyLoader.R')) return(list(error=TRUE,data=NULL,msg=paste0("\n",iam,":Missing dependency function: functions_common/dependencyLoader.R")));
        source('functions_common/dependencyLoader.R')
    }
    libraries<-c('lubridate')
    sources<-paste0("functions_cast/",c('check_acciona_rules_cm3.R'))
    dep<-dependencyLoader(c(libraries,sources))
    if(dep$error)  return(list(error=TRUE,data=NULL,msg=paste0("\n",iam,":on call dependencyLoader\n",dep$msg)))
    
    # Checking types
    pre_alarm <- as.factor(pre_alarm)
    levels(pre_alarm) <- c('f','t')
    alarm <- as.factor(alarm)
    levels(alarm) <- c('f','t')
    date_time <- as.POSIXct(date_time, origin = "1970-01-01",tz = 'UTC')
    
    # result dataframe
    predict_tested <- data.frame(id = 1:length(date_time), 
                                 date_time, 
                                 pre_alarm, 
                                 alarm, 
                                 conf_matrix = factor(numeric(length(date_time)), levels = c('tp','fp','fn','tn')),
                                 conf_matrix2 = factor(numeric(length(date_time)), levels = c('tp','fp','fn','tn','NAtp','NAfn')),
                                 asociated_alarm_date = character(length(date_time)),
                                 asociated_period = numeric(length(date_time)),
                                 stringsAsFactors = FALSE)
    
    
    id_pre <- which(pre_alarm == 't')
    id_alarm <- which(alarm == 't')
    id_start_alarm <- which(date_time >= min(date_time) + period(anticipation+margin-1, units = "day"))[1] # The idex [1] is used to get just the first value.
    id_alarm <- id_alarm[id_alarm >= id_start_alarm] # All id_alarms less than id_start_alarm are ommitted (they are out of the prediction scope)
    if(critic_fault && length(id_alarm) > 0) {
        id_alarm_full <- id_alarm
        id_alarm <- id_alarm[1]
        warning("\nJust the first alarm (ot) is considered because it is critic failure.\n")
    }
    id_pre_tested <- numeric(0)
    
    # Go through alarms and check if the pre_alarm was activated in the right period
    # So, in this part we check for falses negatives and true positives
    for(i in id_alarm ) { # 1:length(date_time)
        date_alarm <- date_time[i]
        date_low_threshold <- date_alarm - as.difftime(min(anticipation + margin, difftime(date_alarm,min(date_time), units = c("days"),tz = 'UTC')), units="days")
        date_high_threshold <- date_alarm - as.difftime(min(anticipation, difftime(date_alarm,min(date_time), units = c("days"),tz = 'UTC')), units="days")
        predicted_period_short <- dplyr::filter(predict_tested, date_time >= date_low_threshold & date_time <= date_high_threshold)
        if(critic_fault && length(id_alarm_full) > 1) {
            if(any(diff(id_alarm_full) > 1)){
                id_alarm_consecutive_last <- id_alarm_full[ which(diff(id_alarm_full) > 1)[1] ]
            }
            else {
                id_alarm_consecutive_last <- id_alarm_full[length(id_alarm_full)]
            }
            predicted_period <- dplyr::filter(predict_tested, date_time >= date_low_threshold & date_time <= date_time[id_alarm_consecutive_last])
        } else {
            predicted_period <- dplyr::filter(predict_tested, date_time >= date_low_threshold & date_time <= date_alarm)
        }
        id_pre_tested <- c(id_pre_tested, predicted_period$id)
        if( any(predicted_period_short$pre_alarm == 't', na.rm = TRUE) ) {
            # Confusion Matrix 1 (multiply 'tp')
            predict_tested$conf_matrix[predicted_period$id] <- 'tp'
            # Confusion Matrix 2 (not multiply 'tp', 1 alarm -> 1 'tp'/'fn')
            predict_tested$conf_matrix2[predicted_period$id[1]] <- 'tp'
            predict_tested$conf_matrix2[predicted_period$id[2:nrow(predicted_period)]] <- 'NAtp'
        } else {
            # Confusion Matrix 1 (multiply 'fn')
            predict_tested$conf_matrix[predicted_period$id] <- 'fn'
            # Confusion Matrix 2 (not multiply 'tp', 1 alarm -> 1 'tp'/'fn')
            predict_tested$conf_matrix2[predicted_period$id[1]] <- 'fn'
            predict_tested$conf_matrix2[predicted_period$id[2:nrow(predicted_period)]] <- 'NAfn'
        }
        # Saved date of the related alarm
        predict_tested$asociated_alarm_date[predicted_period$id] <- as.POSIXct(date_alarm, origin = "1970-01-01",tz = 'UTC')
        # Saved related alarm
        predict_tested$asociated_period[predicted_period_short$id] <- 1
    }
    
    ### Now check for true negative and false positives ###
    # Select all observations not yet tested (no near to an alarm)
    if(length(id_pre_tested)==0){
        predicted_period <- predict_tested
    }else{
        predicted_period <- predict_tested[-id_pre_tested,]
    }
    # Predictions out of scope (no event information)
    predicted_period$pre_alarm[predicted_period$date_time > max(date_time) - as.difftime(anticipation, units="days")] <- NA
    # Check CM1 for false positives
    predicted_period$conf_matrix[predicted_period$pre_alarm == 't'] <- 'fp'
    # Check CM1 for true negatives
    predicted_period$conf_matrix[predicted_period$pre_alarm == 'f'] <- 'tn'
    # Check CM2 for false positives
    predicted_period$conf_matrix2[predicted_period$pre_alarm == 't'] <- 'fp'
    # Check CM2 for true positives
    predicted_period$conf_matrix2[predicted_period$pre_alarm == 'f'] <- 'tn'
    # Update predict_testd 
    if(length(id_pre_tested)==0){
        predict_tested <- predicted_period 
    }else{ 
        predict_tested[-id_pre_tested,] <- predicted_period
    }
    
    predict_tested$asociated_alarm_date <- as.POSIXct(as.numeric(predict_tested$asociated_alarm_date), origin = "1970-01-01",tz='UTC')
    
    rs<-check_acciona_rules_cm3(predicted_period=predict_tested,anticipation=anticipation, margin=margin,date_time_name='date_time',pre_alarm_name='pre_alarm',target_name='alarm')
    if(rs$error) return(list(error=T,data=NULL,msg=paste0(iam,' on call check_acciona_rules_cm3:',rs$msg)))
    if(!rs$error){
        predict_tested<-rs$data
    }
    # Predictions out of scope (no event information, hence no evaluation)
    predict_tested[predict_tested$date_time > max(date_time) - anticipation,c("pre_alarm", "conf_matrix", "conf_matrix2", "conf_matrix3")] <- NA
    # END
    return(list(error=F,data=predict_tested,msg='ok'))
}
