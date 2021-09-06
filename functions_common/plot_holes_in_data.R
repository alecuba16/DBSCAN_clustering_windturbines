plot_holes_in_data<-function(wp_code=NULL,wtdata=NULL,threshold=12,date_time_name='date_time',exclude_columns=c('ld_id',date_time_name),big_holes_only=F,save_plot=F){
  #wtdata$date_time<-as.POSIXct(wtdata$date_time,tz='UTC',origin='1970-01-01')
  iam=match.call()[[1]]
  #Dependencias
  if(!exists("dependencyLoader")){
    if(!file.exists('functions_common/dependencyLoader.R')) return(list(error=TRUE,data=NULL,msg=paste0("\n",iam,":Missing dependency function: functions_common/dependencyLoader.R")));
    source('functions_common/dependencyLoader.R')
  }
  dep<-dependencyLoader(c('reshape2','ggplot2','scales'))
  if(dep$error) return(list(error=TRUE, warning=FALSE, data=NULL,msg=paste0("\n",iam,":on call dependencyLoader\n",dep$msg)))
  
  if(is.null(wtdata)) return(list(error=TRUE, warning=FALSE, data=NULL,msg=paste0("\n",iam," wtdata cannot be null")))
  if(!(date_time_name %in% colnames(wtdata))) return(list(error=TRUE, warning=FALSE, data=NULL,msg=paste0("\n",iam," missing ",date_time_name," on wtdata")))
  if(!('ld_id' %in% colnames(wtdata))) return(list(error=TRUE, warning=FALSE, data=NULL,msg=paste0("\n",iam," missing ld_id on wtdata")))
  
  plots<-list()
  for(ld in unique(wtdata$ld_id)){
    tmpdata<-wtdata[wtdata$ld_id==ld,]
    tmpdata<-tmpdata[order(tmpdata$date_time),]
    holes<-matrix(ifelse(big_holes_only,NA,'data'),ncol=ncol(tmpdata),nrow=nrow(tmpdata),dimnames = list(tmpdata[,date_time_name],colnames(tmpdata)))
    for(c in 1:ncol(tmpdata)){
      if(!(colnames(tmpdata)[c] %in% exclude_columns)){
        min_c<-min(tmpdata[,c],na.rm = T)
        max_c<-max(tmpdata[,c],na.rm = T)
        na_values<-is.na(tmpdata[,c])
        na_values_pos<-which(na_values==T)
        d<-diff(na_values_pos)
        if(big_holes_only){
          big_holes_position<-na_values_pos[(d>threshold)]
          for(h in big_holes_position){
            #find first value
            hole_start<-which(is.na(tmpdata[order(h:1,decreasing=T),c])==F)[1]
            hole_end<-which(is.na(tmpdata[h:nrow(tmpdata),c])==F)[1]
            data_diff<-(tmpdata[hole_end,c]-tmpdata[hole_start,c])
            holes[h,c]<-((data_diff-min_c)/(max_c-min_c))*100
          }
        }else{
          holes[na_values_pos,c]<-'missing'
        }
      }
    }
    holes<-melt(holes)
    holes$Var1<-as.POSIXct(holes$Var1,tz = 'UTC',origin='1970-01-01')
    p<-ggplot(data = holes, aes(x = Var1, y = Var2, fill= value)) + geom_tile()+xlab('Row')+ylab('Variable')+ggtitle(ld)+ theme(axis.text.x = element_text(angle = 90, hjust = 1))+scale_x_datetime(labels = date_format("%d-%m-%Y"),date_breaks='1 month')
    p<-p+theme_bw()+ theme(axis.text.x = element_text(angle = 90, hjust = 1),legend.position="bottom")
    p<-p+ scale_fill_manual(breaks = c("missing", "data"),values=c("#A7C763", "#AA3939"))
    
    #For publications:
    #p<-ggplot(data = holes, aes(x = Var1, y = Var2, fill= value)) + geom_tile()+ theme(axis.text.x = element_text(angle = 90, hjust = 1))+scale_x_datetime(labels = date_format("%d-%m-%Y"),date_breaks='1 month')
    #p<-p+theme_bw()+ theme(axis.text.x = element_text(angle = 90, hjust = 1),axis.title.x=element_blank(),axis.title.y=element_blank(),legend.position="bottom")
    #p<-p+ scale_fill_manual(name = "",breaks = c("missing", "data"),values=c("#A7C763", "#AA3939"))
    #prefix<-if(!is.null(wp_code)) paste0(wp_code,'_') else '' 
    #ggsave(filename = paste0(prefix,'plot_',ifelse(big_holes_only,'big_holes','holes'),'_',ld,'.png'),plot = p,width = 18,height=10)
    
    if(!save_plot){
      plots<-c(plots,p)
    }else{
      prefix<-if(!is.null(wp_code)) paste0(wp_code,'_') else '' 
      ggsave(filename = paste0(prefix,'plot_',ifelse(big_holes_only,'big_holes','holes'),'_',ld,'.png'),plot = p,width = 22,height=10) 
    }
  }
}

