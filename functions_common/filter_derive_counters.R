filter_derive_counters <- function(wtdata) {
    source('functions_common/filter_is_counter.R')
    derive_counter <- function(x) { 
        if(filter_is_counter(x)) {
            dx <- diff(x)
            x <- c(dx[1], dx)
        }  
        x 
    }
    lwt <- as.list(wtdata)
    lwt <- lapply(lwt, FUN = derive_counter)
    as.data.frame(lwt)
}