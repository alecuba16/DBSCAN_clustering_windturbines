get_most_unhealthy <- function(m, rank_wts, db_config) {
    library(dplyr)
    if(!exists("db_check_dic"))
        source('functions_common/db_check_dic.R')

    target <- ifelse("target" %in% names(m), m$target[1], "alarm")
    if(target == "alarm")
        id_walm_final <- as.numeric(strsplit(m$array_id_walm, split = ",")[[1]])
    if(target == "ot")
        id_walm_final <- as.numeric(strsplit(m$array_ot, split = ",")[[1]])
    
    subrank <- filter(rank_wts, id_walm %in% id_walm_final)
    if(nrow(subrank) < 1) {
      msg <- paste("in: get_most_unhealthy, id_walm_final:", id_walm_final, "were not found.")
      return( list(error = TRUE, data = NULL, msg = msg) )
    }
    total_alarms_by_wt <- colSums(subrank[,-1])
    ld_id_most_unhealthy <- names(total_alarms_by_wt)[which.max(total_alarms_by_wt)]
    m2 <- m
    ld_id_value <- as.numeric(ld_id_most_unhealthy)
    m2$ld_id <- ld_id_value
    r <- db_check_dic("SC_LOGICALDEVICE", desired_variable = "ld_code", key_name="ld_id", 
                   key_value = ld_id_value, db_config=db_config)
    m2$ld_code <- r$data$ld_code
    return( list(error = r$error, data = m2, msg = r$msg) )
}

# Old version when used ld_code instead of ld_id 
# get_most_unhealthy <- function(m, rank_wts) {
#     require(dplyr)
#     if(!exists("db_check_dic"))
#         source('functions_common/db_check_dic.R')
#     id_walm_final <- as.numeric(strsplit(m$array_id_walm, split = ",")[[1]])
#     subrank <- filter(rank_wts, id_walm %in% id_walm_final)
#     total_alarms_by_wt <- colSums(subrank[,-1])
#     ld_code_most_unhealthy <- names(total_alarms_by_wt)[which.max(total_alarms_by_wt)]
#     m2 <- m
#     m2$ld_code <- ld_code_most_unhealthy
#     ld_code_value <- paste0("'", ld_code_most_unhealthy,"'")
#     r <- db_check_dic("SC_LOGICALDEVICE", desired_variable = "ld_id", key_name="ld_code", 
#                    key_value = ld_code_value)
#     m2$ld_id <- r$data$ld_id
#     return( list(error = r$error, data = m2, msg = r$msg) )
# }