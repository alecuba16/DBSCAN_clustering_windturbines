filter_esd<-function(currentVar=NULL,params=NULL,name=NULL) {
    iam='filter_esd'
    if(is.null(params)||!(class(params) %in% c("data.frame","list"))||(class(params)=="data.frame"&&nrow(params)==0)||class(params)=="list"&&length(params)<0)
        return(list(error=TRUE,warning=F,data=NULL,msg=paste0(iam,":params is null or empty")))
    if(!('sd' %in% names(params)))
        return(list(error=TRUE,warning=F,data=NULL,msg=paste0(iam,": missing sd parameter is null/empty")))
    n_std <- abs(sd(currentVar,na.rm=TRUE) * params$sd)
    mean <- mean(currentVar,na.rm=TRUE)
    return(list(error=FALSE,warning=F,data=list(min=(mean-n_std),max=(mean+n_std)),msg="ok"))
}
