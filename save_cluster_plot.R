save_tags <- function (tags, file, selfcontained = F, libdir = "./lib") 
{
    if (is.null(libdir)) {
        libdir <- paste(tools::file_path_sans_ext(basename(file)), 
                        "_files", sep = "")
    }
    htmltools::save_html(tags, file = file, libdir = libdir)
    if (selfcontained) {
        if (!htmlwidgets:::pandoc_available()) {
            stop("Saving a widget with selfcontained = TRUE requires pandoc. For details see:\n", 
                 "https://github.com/rstudio/rmarkdown/blob/master/PANDOC.md")
        }
        htmlwidgets:::pandoc_self_contained_html(file, file)
        unlink(libdir, recursive = TRUE)
    }
    return(file)
}

save_cluster_plot<-function(wtdata,clusters,alarms,file_pattern,alarm_block_code,zipFile=FALSE){
    iam="save_cluster_plot"
    #Dependencia basica
    if(!exists("dependencyLoader")){
        if(!file.exists('functions_common/dependencyLoader.R')) return(list(error=TRUE,data=NULL,msg=paste0("\n",iam,":Missing dependency function: functions_common/dependencyLoader.R")));
        source('functions_common/dependencyLoader.R')
    }
    
    libraries<-c('plotly')
    sources<-paste0("functions_common/",
                    c('get_time_mark.R','load_wdata_hist.R','close_protocol.R','save_cluster_plot.R'))
    dep<-dependencyLoader(c(libraries,sources))
    if(dep$error)  return(list(error=TRUE,data=NULL,msg=paste0("\n",iam,":on call dependencyLoader\n",dep$msg)))
    
    
    alarmcors <-unlist(sapply(min(clusters):max(clusters),function(c) 
        mean(unlist(sapply(1:max(ncol(wtdata)),function(v) if(!is.na(cor(wtdata[clusters==c,v],alarms[clusters==c],use="pairwise.complete.obs"))) {cor(wtdata[clusters==c,v],alarms[clusters==c],use="pairwise.complete.obs")} else{NA})),na.rm = TRUE)))
    
    alarmsum <-unlist(sapply(min(clusters):max(clusters),function(c) sum(alarms[clusters==c])))
    crsum<-data.frame(cr=alarmcors,sum=alarmsum,idcluster=min(clusters):max(clusters))
    sortsum<-crsum[with(crsum,order(-sum)),]
    sortsum$type<-''
    sortcor<-crsum[with(crsum,order(cr)),]
    sortcor$type<-''
    #blades_pitchangle_max,blades_pitchangle_min,reactive_power_set_point,hourcounters_average_yaw_avg,grid_production_power_internalderatetime,power_factor_set_point_source
    
    xvar<-names(wtdata)[1]
    yvar<-names(wtdata)[2]
    zvar<-names(wtdata)[3]
    
    p<-plotly::plot_ly(type = 'scatter3d', mode = 'markers',width = 1800, height = 720, hoverinfo="text")
    
    for(j in 1:nrow(sortsum)){
        current<-sortsum[j,]
        if(length(clusters==current$idcluster)>0){
            alarmscluster<-paste(unique(strsplit(paste(alarm_block_code[clusters==current$idcluster],collapse=","),",")[[1]]),collapse=",")
            
            pos<-unlist(gregexpr(pattern =',',alarmscluster))
            postoreplace<-pos[seq(1, length(pos), 4)]
            for(r in length(postoreplace):1){
                if(r>1)
                    alarmscluster<-paste(substring(alarmscluster, 1, postoreplace[r]-1),'<br>',substring(alarmscluster,postoreplace[r]+1), sep = "")
            }
            
            if(is.null(alarmscluster)||is.na(alarmscluster)||nchar(alarmscluster)==0) alarmscluster<-' '
            current_cluster_data<-wtdata[clusters==current$idcluster,]
            if(nrow(current_cluster_data)>0){
                current_cluster_data[,xvar]
                #plotdata<-data.frame(x=alarm$date_time[clusters==i],y=wtdata[clusters==i,'blades_pitchangle_max'],z=wtdata[clusters==i,'blades_pitchangle_min'])
                plotdata<-data.frame(mx=current_cluster_data[,xvar],my=current_cluster_data[,yvar],mz=current_cluster_data[,zvar])
                if((!is.null(current$sum)&&!is.na(current$sum)&&nrow(plotdata)==current$sum && ((current$sum*100)/nrow(plotdata))>1))
                    isvisible<-'true'
                else
                    isvisible<-'legendonly'
                name<-paste0("c_",current$idcluster,"_size(",nrow(plotdata),") alarms(",current$sum,",",round((current$sum*100)/nrow(plotdata),digits=1),"%):",alarmscluster)
                if(!is.null(plotdata)&&nrow(plotdata)>0)
                    p<-plotly::add_trace(p,data=plotdata,x=~mx,y=~my,z=~mz,name=name,visible=isvisible,text=~paste(xvar,':',round(mx,3),'</br>',yvar,':',round(my,3),'</br>',zvar,':',round(mz,3)),hoverinfo="text",marker=list(size=5))
            }
        }
    }
    
    p<-plotly::layout(p,scene = list(xaxis = list(title = xvar),yaxis = list(title = yvar),zaxis = list(title = zvar)))
    
    alarmscluster<-do.call(rbind, lapply(min(clusters):max(clusters), function(c) data.frame(c,length(wtdata[clusters==c,1]),paste(unique(strsplit(paste(alarm_block_code[clusters==c],collapse=","),",")[[1]]),collapse=",")))) 
    names(alarmscluster)[1]<-'cid'
    names(alarmscluster)[2]<-'size'
    names(alarmscluster)[3]<-'alarms'
    
    p2 <- plot_ly(data=alarmscluster,
                  x = ~cid,
                  y = ~size,
                  name = "Cluster",
                  text = ~paste('Alarms: ', alarms),
                  type = "bar",
                  width = 1800, height = 350)
    l <- htmltools::tagList()
    l[[1]]<-p
    l[[2]]<-p2
    #p3 <- subplot(p, p2, nrows = 2,heights=c(0.80,0.20))
    #p3<-subplot(p,p2,nrows = 2,heights=c(0.80,0.20),specs=c(,))
    #htmlwidgets::saveWidget(p3, paste0(file_pattern,".html"))
    htmlFile<-paste0(file_pattern,".html")
    save_tags(l, htmlFile,selfcontained=TRUE)
    #htmltools::save_html(l, paste0(file_pattern,".html"))
    if(zipFile&&file.exists(htmlFile)){
        try(zip(paste0(htmlFile,".zip"), htmlFile, flags = "-9Xjq", extras = "",zip = Sys.getenv("R_ZIPCMD", "zip")))
        unlink(htmlFile)
    }
}