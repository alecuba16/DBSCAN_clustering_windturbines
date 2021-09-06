filter_clean_data <- function(wtdata,exclude='date_time',balanced_threshold=0.5, verbose = FALSE, delete_counters = FALSE) {
  iam<-match.call()[[1]]
  #Dependencia basica
  if(!exists("dependencyLoader")){
    if(!file.exists('functions_common/dependencyLoader.R')) return(list(error=TRUE,warning=F,data=NULL,msg=paste0("\n",iam,":Missing dependency function: functions_common/dependencyLoader.R")));
    source('functions_common/dependencyLoader.R')
  }
  sources<-c("functions_common/filter_is_counter.R")
  dep<-dependencyLoader(sources)
  if(dep$error)  return(list(error=TRUE,warning=F,data=NULL,msg=paste0("\n",iam,":on call dependencyLoader\n",dep$msg)))
  
  # Get out of constant variables
  n <- dim(wtdata)[1]
  m <- dim(wtdata)[2]
  
  constant_id <- numeric(length = m)
  cons_count <- 0
  acc_id <- numeric(length = m)
  acc_count <- 0
  null_id <- numeric(length = m)
  null_count <- 0
  
  for( i in 1:m ) {
    if(!(names(wtdata)[i] %in% strsplit(exclude,",")[[1]])){
      x <- wtdata[,i]
      if( is.numeric(x) ) {
        stdx <- sd(x, na.rm = TRUE)
        # No Constants: Check if the variable is a constant (std=0) An alternative can be use unique() method
        if( is.na(stdx) | stdx == 0 ) { 
          cons_count <- cons_count+1
          constant_id[cons_count] <- i # Keep the id to delete later, if delete now the indexation will get lost
          if( verbose ) cat("\nFilter: The variable", names(wtdata)[i]," is a constant, it will be deleted. std: ",stdx,"\n")
        } else {
          # No Counters: IF similar to a line (a counter), then, apply diff
          # if ( abs(cor(x,1:n, use = "pairwise.complete.obs")) > 0.9 ) {   
          if ( filter_is_counter(x,balanced_threshold=balanced_threshold,verbose=verbose) ) { 
            acc_count <- acc_count+1
            acc_id[acc_count] <-  i
            dx <- diff(wtdata[,i])
            wtdata[, i] <- c(dx[1], dx) # Repeat the first delta value in order to keep the same length
            if( verbose ) cat("\nFilter: The variable", names(wtdata)[i]," is a counter, it has been derived.\n")
          }
          # No 999, Na or Null: Check if 999, Na or Null is a commun value
          if( sum(x==999, na.rm = TRUE) > n*0.10 
              || sum(is.na(x), na.rm = TRUE) > n*0.10 
              || sum(is.null(x), na.rm = TRUE) > n*0.10 ) {
            null_count <- null_count+1
            null_id[null_count] <- i
            
            if( verbose ) cat("\nFilter: The variable", names(wtdata)[i]," has many 999 values, they could be null values.\n")
          }
        }
      }
    }
  }
  
  constant_id <- constant_id[constant_id > 0]
  acc_id <- acc_id[acc_id > 0]
  null_id <- null_id[null_id > 0]
  
  if(!delete_counters) {
    cat("\nFilter: Deleting constant variables... ")
    wtdata[, constant_id] <- NA #NO DELETE JUANCHOO Delete constant variables
    cat("Done.")
  } else {
    cat("\nFilter: Deleting constant and counters variables... ")
    wtdata[, c(constant_id, acc_id)] <- NA # NO DELETE JUANCHOO  Delete constant variables
    cat("Done.")
  }
  cat("\nFilter: Number of constant variables deleted: ",cons_count, " (", 100*cons_count/(m-2), "%)", sep = "")
  if(!delete_counters)
    cat("\nFilter: Number of counter variables derived: ",acc_count, " (", 100*acc_count/(m-2), "%)", sep = "")
  else
    cat("\nFilter: Number of counter variables deleted: ",acc_count, " (", 100*acc_count/(m-2), "%)", sep = "")
  cat("\nFilter: Number of many null values variables detected: ",null_count, " (", 100*null_count/(m-2), "%)", sep = "")
  
  result <- list(constant_id, acc_id, null_id, wtdata)
  names(result)[1] <- "constants"
  names(result)[2] <- "counters" # "accomulatives"
  names(result)[3] <- "many_999_values"
  names(result)[4] <- "data"
  return(list(error=F,warning=F,data=result,msg="ok"))
}
