filter_loop_window<-function(wtdata=NULL,exclude="",date_time_name="date_time",limits=NULL,func=NULL,params=NULL,window=30,slide=30) {
    iam='filter_loop_window'
    outliers <- data.frame(
        date_time=character(),
        variable=character(),
        value = as.numeric(),
        stringsAsFactors=FALSE
    )
    colnames(outliers)[1]<-date_time_name
    if(is.null(wtdata)||nrow(wtdata)<=0||ncol(wtdata)<=0)
        return(list(error=TRUE,data=NULL,msg="wtdata is empty or null"))
    
    if(!date_time_name %in% colnames(wtdata))
        return(list(error=TRUE,data=NULL,msg=paste0(date_time_name," doesn't exists in wtdata")))
    date_time_col <-which(colnames(wtdata)==date_time_name)[1]
    if(is.na(date_time_col))
        return(list(error=TRUE,data=NULL,msg=paste0(date_time_name," doesn't exists in wtdata")))
    
    limits<-NULL
    for( col in 1:ncol(wtdata) ) {
        name <- colnames(wtdata)[col]
        if(!(name %in% exclude)){
            currentVar <- wtdata[,col]
            currentVar <- as.numeric(currentVar)
            if(!is.null(currentVar)&&is.numeric(currentVar)&&length(currentVar)>0){
                nrows <- length(currentVar)  
                for (i in (window + 1):(nrows - window)) {
                    currentRange <- currentVar[(i - window):(i + window)]
                    rs<-func(currentVar=currentRange,params=params,name=name)
                    if(!rs$error&&!is.null(rs$data)){
                        limits_calc<-rs$data
                        #Filter values
                        if(!is.null(limits_calc$min)){
                            out_rows<-(currentRange < limits_calc$min)
                            limits_temp<-data.frame(var=name,min=limits_calc$min)
                        }
                        if(!is.null(limits_calc$max)){
                            if(exists("limits_temp"))
                                limits_temp$max<-limits_calc$max
                            else
                                limits_temp<-data.frame(var=name,min=NULL,max=limits_calc$max)
                            
                            if(exists("out_rows"))
                                out_rows<-(out_rows |(currentRange > limits_calc$max))
                            else
                                out_rows<-(currentRange > limits_calc$max)
                            
                        }
                        
                        #Add to limit lists
                        if(exists("limits_temp")&&!is.null(limits_temp)&&!is.null(limits)){
                            limits<-rbind(limits,limits_temp)
                        }else if(exists("limits_temp")&&!is.null(limits_temp)){
                            limits<-limits_temp
                        }
                        
                        if(exists("out_rows")&&any(out_rows,na.rm = TRUE)){ 
                            outliersTmp<-wtdata[which(out_rows),c(date_time_col,col)]
                            outliersTmp<-data.frame(outliersTmp[,1],name,outliersTmp[,2]) 
                            colnames(outliersTmp)<- c(date_time_name,"variable","value")
                            outliers<-rbind(outliers,outliersTmp);
                            wtdata[which(out_rows),col]<- NA
                        }
                    }
                }
            }
        }
    }
    return(list(error=FALSE,data=list(clean=wtdata,outliers=outliers,limits=limits),msg="OK"))
}
