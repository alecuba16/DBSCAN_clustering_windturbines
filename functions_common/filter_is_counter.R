filter_is_counter <- function(x, balanced_threshold = 0.5,verbose=F) {
    if(is.null(x)) return(FALSE)
    if(!is.numeric(x)) return(FALSE) # No is numeric
    if(length(na.omit(x)) < 2) return(FALSE) # Is all NA
    #remove NA
    x<-x[!is.na(x)]
    sdv<-sd(x, na.rm = TRUE)
    if(is.null(sdv[1])||is.na(sdv[1])||sdv[1] == 0) return(FALSE) # Is a constant
    
    n <- length(x)
    if( abs(cor(x,1:n, use = "pairwise.complete.obs")) > 0.9 ) return(TRUE)
    
    dx <- diff(x)
    
    positives <- sum( dx > 0, na.rm = TRUE ) 
    negatives <- sum( dx < 0, na.rm = TRUE )
    
    # Para pillar el bug
    if(is.null(positives) | is.null(negatives)) {
        if(verbose) cat("\n\npositives or negatives is NULL", x, "\n\n")
        return(FALSE)
    }
    
    balance <- min(positives, negatives)/max(positives, negatives) # Near 1 if there is good balance
    
    # Para pillar el bug
    if(is.na(balance < balanced_threshold)) {
        if(verbose) cat("\n\npositives or negatives NULL", x, "\n\n")
        return(FALSE)
    }
    
    if(balance < balanced_threshold) return(TRUE)
    
    return(FALSE)
    
}
