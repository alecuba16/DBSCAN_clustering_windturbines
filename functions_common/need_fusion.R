need_fusion <- function(m, rank_wts, daily_alarms_threshold = 2) {
  iam='need_fusion'
  target <- ifelse("target" %in% names(m), m$target[1], "alarm")
  #Dependencia basica
  if(!exists("dependencyLoader")){
    if(!file.exists('functions_common/dependencyLoader.R')) return(list(error=TRUE,data=NULL,msg=paste0("\n",iam,":Missing dependency function: functions_common/dependencyLoader.R")));
    source('functions_common/dependencyLoader.R')
  }
  dep<-dependencyLoader(c('plyr', 'dplyr'))
  if(dep$error)  return(list(error=TRUE,data=NULL,msg=paste0("\n",iam,":on call dependencyLoader\n",dep$msg)))
  
  if(target == "alarm")
      id_walm_final <- as.numeric(strsplit(m$array_id_walm, split = ",")[[1]])
  if(target == "ot")
      id_walm_final <- as.numeric(strsplit(m$array_ot, split = ",")[[1]])
  subrank <- dplyr::filter(rank_wts, id_walm %in% id_walm_final)
  total_daily_alarms <- try(sum(subrank[,as.character(m$ld_id)]), silent = TRUE)
  if(inherits(total_daily_alarms, "try-error")) {
      cat("Warning: ld_id", m$ld_id, "no found in rank_wts.\n")
      cat(total_daily_alarms)
      return(list(error=FALSE,data=FALSE,msg="ok"))
  }
      
  if( total_daily_alarms < daily_alarms_threshold )
    return(list(error=FALSE,data=TRUE,msg="ok"))  
  else
    return(list(error=FALSE,data=FALSE,msg="ok"))  
}