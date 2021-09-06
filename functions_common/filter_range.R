filter_range<-function(currentVar=NULL,params=NULL,name=NULL) {
    iam='filter_range'
    
    if(is.null(name)||class(name)!="character"||nchar(name)==0)
        return(list(error=TRUE,warning=F,data=NULL,msg=paste0(iam,": name parameter is null/empty")))
    if(is.null(params)||!(class(params) %in% c("data.frame","list"))||(class(params)=="data.frame"&&nrow(params)==0)||class(params)=="list"&&length(params)<0)
        return(list(error=TRUE,warning=F,data=NULL,msg=paste0(iam,":params is null or empty")))
    
    limits<-params$limits
    
    if(!any(limits$var==name)) return(list(error=FALSE,warning=F,data=list(min=NA,max=NA),msg="ok"))
    
    min<-NA
    max<-NA
    
    if('min' %in% names(limits)){
      if(!is.null(limits$min[limits$var==name])&&!is.na(limits$min[limits$var==name])){
          min<-limits$min[limits$var==name]
      }else if(!is.null(limits$method[limits$var==name])
               && !is.na(limits$method[limits$var==name])
               && (nchar(limits$method[limits$var==name])>1)
               && (limits$method[limits$var==name]=='stat')
               && !is.null(limits$min_calc[limits$var==name])
               && !is.na(limits$min_calc[limits$var==name])){
          min<-limits$min[limits$var==name]
      }
    }
    
    if('max' %in% names(limits)){
      if(!is.null(limits$max[limits$var==name])&&!is.na(limits$max[limits$var==name])){
        max<-limits$max[limits$var==name]
      }else if(!is.null(limits$method[limits$var==name])
               && !is.na(limits$method[limits$var==name])
               && (nchar(limits$method[limits$var==name])>1)
               && (limits$method[limits$var==name]=='stat')
               && !is.null(limits$max_calc[limits$var==name])
               && !is.na(limits$max_calc[limits$var==name])){
        max<-limits$max[limits$var==name]
      }
    }
    
    return(list(error=FALSE,warning=F,data=list(min=min,max=max),msg="ok"))
}
