update_table_report <- function(table_cast_report, wt_query, report_path, db_config, results = NULL, error = FALSE, 
                                id_model = ifelse(!is.null(wt_query),wt_query$id,results$id_model) ) {
    source('functions_common/db_query.R')
    if(error) { # Something happen so just update with error.
        
        query <- paste0("UPDATE ",table_cast_report,
                        " SET report_status = 'Error.'",
                        " WHERE id_model = ",id_model,";")
        
        
    } else if(is.null(results)){ # No error and results is not ready, so it should be the start of the report.
        
        query <- paste0("INSERT INTO ",table_cast_report," (id_model, ld_id, fault, 
                    date_time_execution, report_status, report_path) VALUES(",
                        wt_query$id,",",wt_query$ld_id,",'",wt_query$fault,"','",
                        Sys.time(),"','Processing...','",report_path,"') 
                    ON DUPLICATE KEY UPDATE date_time_execution = '",Sys.time(),
                        "', report_status = 'Processing...', report_path = '",report_path,"';")
        
        
    } else { # No error and results is ready, so it should be the end of the report.
        check_NA <- function(x) ifelse(is.na(x),'NULL',x)
        query <- paste0("UPDATE ",table_cast_report,
                        " SET best_inputs = '",results$best_inputs,
                        "', best_kappa = ", check_NA(results$best_kappa), 
                        ", best_horizon = ", check_NA(results$best_horizon), 
                        ", gt_kappa = ", check_NA(results$gt_kappa), 
                        ", gt_true_positive = ", check_NA(results$gt_true_positive),
                        ", gt_false_negative = ", check_NA(results$gt_false_negative),
                        ", gt_false_positive = ", check_NA(results$gt_false_positive),
                        ", gt_true_negative = ", check_NA(results$gt_true_negative),
                        ", report_status = 'Done.'",
                        " WHERE id_model = ",results$id_model,";")
        
    }
    
    db_query(query=query, db_config=db_config)
}