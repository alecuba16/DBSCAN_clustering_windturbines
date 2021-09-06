result_h_to_dataframe <- function(mylist, horizon,prob_alarm_threshold=NULL) {
    if(!any('prob' %in% names(mylist))&&is.null(prob_alarm_threshold)){ #Normality result
        data.frame( date_time = mylist$date_time,
                    target_name = mylist$target_name,
                    predict_value = mylist$predicted,
                    real_value = mylist$real,
                    norm_error = mylist$real-mylist$predicted,
                    stringsAsFactors = FALSE )
    }else{ #Classificator result    
        if( nrow(mylist$data) != nrow(mylist$prob) | 
            length(mylist$date_time) != length(mylist$predicted) | 
            length(mylist$date_time) != nrow(mylist$prob) ) {
            cat("\nMyWarning: Different length in objects into result_h2\n")
            
            return ( data.frame( model = NULL, 
                                 horizon = NULL,
                                 date_time = NULL,
                                 pre_alarm = NULL,
                                 predicted = NULL,
                                 prob = NULL,
                                 threshold = NULL,
                                 stringsAsFactors = FALSE ) )
        }
        
        data.frame( model = mylist$model, 
                    horizon = horizon,
                    date_time = mylist$date_time,
                    pre_alarm = mylist$data$pre_alarm,
                    predicted = mylist$predicted,
                    prob = mylist$prob,
                    threshold = ifelse(is.null(mylist$threshold),prob_alarm_threshold,mylist$threshold), #if no threshold then 0.6 60%
                    stringsAsFactors = FALSE )
    }
}
