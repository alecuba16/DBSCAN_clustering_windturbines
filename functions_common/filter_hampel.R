filter_hampel<-function(currentVar=NULL,params=NULL,name=NULL) {
    iam='filter_hampel'
    if(is.null(params)||!(class(params) %in% c("data.frame","list"))||(class(params)=="data.frame"&&nrow(params)==0)||class(params)=="list"&&length(params)<0)
        return(list(error=TRUE,warning=F,data=NULL,msg=paste0(iam,":params is null or empty")))
    if(!('threshold' %in% names(params)))
        return(list(error=TRUE,warning=F,data=NULL,msg=paste0(iam,": missing threshold parameter is null/empty")))
    L <- 1.4826 # 1/Q(75) asumiendo normalidad.
    x0 <- median(currentVar,na.rm=TRUE)
    S0 <- L * median(abs(currentVar - x0),na.rm=TRUE)
    return(list(error=FALSE,warning=F,data=list(min=(x0 - (params$threshold * S0)),max=(x0 + (params$threshold * S0))),msg='ok'))
}
