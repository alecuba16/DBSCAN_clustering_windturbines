filter_quantile<-function(currentVar=NULL,params=NULL,name=NULL) {
    iam='filter_quantile'
    if(is.null(params)||!(class(params) %in% c("data.frame","list"))||(class(params)=="data.frame"&&nrow(params)==0)||class(params)=="list"&&length(params)<0)
        return(list(error=TRUE,warning=F,data=NULL,msg=paste0(iam,":params is null or empty")))
    if(!('iqr_multiplier' %in% names(params)))
        return(list(error=TRUE,warning=F,data=NULL,msg=paste0(iam,": missing iqr_multiplier parameter is null/empty")))
    qnt <- quantile(currentVar,probs=c(.25,.75),na.rm=TRUE)
    H <- params$iqr_multiplier * IQR(currentVar, na.rm = TRUE)
    return(list(error=FALSE,warning=F,data=list(min=(qnt[1] - H),max=(qnt[2] + H)),mgs='ok'))
}
