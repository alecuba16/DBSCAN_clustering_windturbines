filter_plot<-function(wtdata0=NULL,alarms=NULL,outliers=NULL,filter='',table_filter_config,wp_code,db_config_data,parent_directory,reduce_healthy_points=NULL,biggest_font=FALSE,plotly=FALSE){
    
    iam="filter_plot"
    #Dependencia basica
    if(!exists("dependencyLoader")){
        if( !file.exists(paste0(parent_directory,'/functions_common/dependencyLoader.R')) ) 
            return(list(error=TRUE,warning=F,data=NULL,
                        msg=paste0("\n",iam,": Missing dependency function: functions_common/dependencyLoader.R")));
        source( paste0(parent_directory,'/functions_common/dependencyLoader.R') )
    }
    
    libraries<-c('plyr','dplyr','plotly','htmltools')
    sources<-paste0(parent_directory,"/functions_common/",c('db_query.R','close_protocol.R'))
    dep<-dependencyLoader(c(libraries,sources))
    if(dep$error)  return(list(error=TRUE,warning=F,data=NULL,msg=paste0("\n",iam,":on call dependencyLoader\n",dep$msg)))
    
    if(!is.na(filter)&&!is.null(filter)&&grepl(filter, ",")) filter<-strsplit(filter,",")
    
    cat(paste0("\n The following filters has been applied:"),sep="\n")
    for(f in filter){
        rs <- db_query(query=paste0("SELECT description FROM ",table_filter_config," WHERE filter='",f,"' AND (wp_code ='",tolower(wp_code),"' OR wp_code is NULL) ORDER BY wp_code DESC LIMIT 1"),db_config=db_config_data)
        if(rs$error){
            output_msg <- paste0("\n Report rmd: on call db_query\n",rs$msg)
            close_protocol(output_msg, "Report rmd", debug_mode)
            return(list(error=TRUE,warning=F,data=NULL,msg=output_msg))
        }
        cat(paste0("\n * __",f,"__ :",rs$data$description[1],"  "))
    }
    
    if(!is.null(outliers)&&!is.na(outliers)&&length(outliers)>0&&nrow(outliers)>0){
        cat(paste0("\n A total of __",nrow(outliers),"__ outliers ( __",round((nrow(outliers)*100)/length(wtdata0$date_time),1),"%__ of Train data) where removed   "))
        if(plotly){
            l <- htmltools::tagList()
        }else{
            l<-list()
        }
        vars<-as.character(unique(outliers$variable))
        plc<-1
        alarms_on_datetime=alarms[alarms$alarm==1,"date_time"]
        for(varPos in 1:length(vars)){
            currentVar<-vars[varPos]
            figtitle = paste0("Variable ",currentVar,".")
            
            data<-wtdata0[,c("date_time",currentVar)]
            data$type<-'healthy'
            
            
            if(length(alarms_on_datetime)>0) data$type[(data$date_time %in% alarms_on_datetime)]<-"alarm"  #set alarms
            
            current_outliers<-data$date_time[data$date_time %in% outliers[outliers$variable==currentVar,c("date_time")]]
         
            #Mark as outlier ontraindata
            data[data$type=='alarm'&(data$date_time %in% current_outliers),"type"]<-"outlier alarm"
            data[data$type=='healthy'&(data$date_time %in% current_outliers),"type"]<-"outlier"
        
            if(!is.null(reduce_healthy_points)){
                num<-floor(sum(data$type=='healthy')*reduce_healthy_points/100)
                pos_to_remove<-sample(which(data$type=='healthy'), num)
                data<-data[-pos_to_remove,]
            }
            
            p<-ggplot(data,aes_string(x='date_time',y=vars[varPos],color='type'))+
                xlab('date time')+
                geom_point()+
                scale_colour_manual(values=c('healthy'="#BEF781",'outlier'="orange",'outlier alarm'="#5F04B4",'alarm'='red'))+
                theme_bw()
            if(biggest_font){
                p<-p+theme(legend.position="bottom",legend.text=element_text(size=16),axis.title.x=element_text(size=16),axis.title.y=element_text(size=16),text=element_text(size=16))
            }else{
                p<-p+theme(legend.position="bottom",legend.text=element_text(size=12),axis.title.x=element_text(size=12),axis.title.y=element_text(size=10),text=element_text(size=12))
                }
            
            if(plotly) p<-ggplotly(p)
            l[[plc]]<-p
            plc<-plc+1
        }
        return(list(error = FALSE,warning=F, data = l, msg = "Ok"))
    }else{
        return(list(error = FALSE,warning=F, data = NULL, msg = "Ok"))
    }
}