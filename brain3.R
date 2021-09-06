# Brain3: Clustering. Script principal
# Construye los modelos predictivos (clasificadores) de las turbinas y fallos
# especificados en la variable "wt_queries". Esta versión usa el pvalue como
# criterio de selección.
# Los resultados más relevantes son almacenados en la variable "...results.RData"
# También se almacena una imagen de todas las variables en cada iteración.
# La versión para el ECO100 lee los datos desde la base de datos histórica y no
# desde la base de DD local.

# computeGapStatistic <- function(data, KMax,enable_plots=FALSE,verbose=FALSE) {
#   # gap <- clusGap((data), FUN = kmeans, K.max = 8, B = 3) 
#   gap <- clusGap((data), FUN = kmeans, K.max = KMax, B = 3) 
#   if (enable_plots) {
#     plot(gap, main = "Gap statistic for the Nursing shift data")
#   }
#   clusterCount <- with(gap,maxSE(Tab[,"gap"],Tab[,"SE.sim"]))
#   if (verbose) {
#     print(paste("gap statsitics: ", gap[[1]]))
#     print(paste("K: ", clusterCount))
#   }
#   return(clusterCount)
# }

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


brain3<-function(currentTimestampUTC, m = NULL, m_un = NULL,parallel=FALSE,
                 table_filter_config="1_filter_config",
                 table_cast_park_dic="1_cast_park_table_dic",
                 date_time_name="date_time",
                 horizon_list = 5:14, # In days
                 db_config,
                 ld_id = m$ld_id,
                 ld_code = m$ld_code,
                 wp_code = m$wp_code,
                 wp_id = m$wp_id,
                 array_id_walm = m$array_id_walm,
                 array_ot = m$array_ot,
                 fault = m$fault,
                 target=m$target,
                 filter = m$filter,
                 seconds_to_aggregate = m$seconds_to_aggregate,
                 freq_dat_med_min=m$freq_dat_med_min,
                 include_variables=m$include_variables,
                 exclude_variables=m$exclude_variables,
                 power_condition = m$power_condition,
                 unix_timestamp_ini = m$creation_wtdata_date_ini,
                 unix_timestamp_end = m$creation_wtdata_date_end,
                 creation_trn_percent = m$creation_trn_percent,
                 creation_model_path = m$creation_model_path,
                 creation_log_path = m$creation_log_path) {
    iam="brain3"
    Sys.setenv(TZ='UTC')
    TZ='UTC'
    imagePath='images/'
    
    #Dependencia basica
    if(!exists("dependencyLoader")){
        if(!file.exists('functions_common/dependencyLoader.R')) return(list(error=TRUE,data=NULL,msg=paste0("\n",iam,":Missing dependency function: functions_common/dependencyLoader.R")));
        source('functions_common/dependencyLoader.R')
    }
    
    if(!exists("createFile.R")){
        if(!file.exists('functions_common/createFile.R')) return(list(error=TRUE,data=NULL,msg=paste0("\n",iam,":Missing dependency function: functions_common/createFile.R")));
        source('functions_common/createFile.R')
    }
    
    if(!exists("dirname2")){
        if(!file.exists('functions_common/dirname2.R')) return(list(error=TRUE,data=NULL,msg=paste0("\n",iam,":Missing dependency function: functions_common/dirname2.R")));
        source('functions_common/dirname2.R')
    }
    
    #Get pathname
    d<-dirname2(creation_log_path)
    if(d$error) return(list(error=TRUE,data=NULL,msg=paste0("\n",iam,":on call createFile\n",d$msg)))
    d<-d$data
    
    
    # Sources
    libraries<-c('caret','RMySQL','plyr','dplyr','dbscan')
    if(parallel==TRUE&&.Platform$OS.type != "windows") { # domc only linux-unix
        libraries<-c(libraries,'doMC')
    }
    sources<-paste0("functions_common/",c('get_time_mark.R','load_wtdata.R','close_protocol.R','filter_custom.R'))
     
    dep<-dependencyLoader(c(libraries,sources,'save_cluster_plot.R','n_cluster_horitzon.R'))
    if(dep$error)  return(list(error=TRUE,data=NULL,msg=paste0("\n",iam,":on call dependencyLoader\n",dep$msg)))
    
    # Parallel setup
    if(parallel==TRUE&&.Platform$OS.type != "windows") { # domc only linux-unix
        registerDoMC(cores = detectCores())
    }
    
    #Add filter type to fault
    if(!is.null(filter)&&!is.na(filter)&&filter!="") fault_filter<-paste0(fault,"_",gsub(",", "", filter))
    
    # Log taskes
    if(!debug_mode){
        log_path<- paste0(d,wp_code,"/",ld_id,"/logs/",fault_filter,"_model.log")
        r<-createFile(log_path,append=TRUE)
        if(r$error) return(list(error=TRUE,data=NULL,msg=paste0("\n",iam,":on call createFile\n",r$msg)))
        sink(log_path)
    }else{
        log_path <- NULL
    }
    
    if(parallel==TRUE&&.Platform$OS.type != "windows") { # prints here to be inserted on the log.
        cat("\n Parallel mode on\n")
    } 
    
    cat(paste("\n------------------- INI MODEL -----------------------\n",
              as.POSIXct(currentTimestampUTC,tz=TZ,origin="1970-01-01"),
              " ld_id(",ld_id,") wp_code(",wp_code,")\n",fault,
              " id_walm(",array_id_walm,")\n"))
    
    horizon_list <- 1 # In days
    n_horizons <- length(horizon_list)
    
    ## Record elements
    results <- vector("list", 1)
    time_mark <- get_time_mark(currentTimestampUTC)
    war_rec <- character(1*n_horizons)
    count_war <- 0
    file_pattern <- paste(ld_code,ld_id,fault_filter,sep = "_")
    imageZipFile<-paste0(imagePath,time_mark,"_",file_pattern,"_wsp.zip") #to store image files in zip
    names(results)[1] <- file_pattern
    cat("\n\nSTARTING MODELLING PROCESS FOR: ", file_pattern,"\n")
    
    # Record element
    result_h <- vector("list", n_horizons)
    # Number of models and list to record models and confusion matrix
    n_models <- 1
    fit_list <- vector("list", n_models)
    alarm_threshold <- vector("list", n_models)
    cat("\nReading data...")
    
    ini <- as.POSIXct(unix_timestamp_ini, tz = "UTC", origin = "1970-01-01")
    end <- as.POSIXct(unix_timestamp_end, tz = "UTC", origin = "1970-01-01")
    cat("\nStart time:\t", as.character(ini), "\nEnd time:\t", as.character(end), "\n")
    #Check if include variable is declared, then target variable must be in the include variables
    if(length(include_variables)>0 && nchar(include_variables)>2){
        if(!(target %in% unlist(strsplit(include_variables,',')))) 
            tmp_include_variables<-paste0(include_variables,',',target)
        else
            tmp_include_variables<-include_variables
    }else{
        tmp_include_variables<-''
    }
    #Add selected alarms for image
    if(length(tmp_include_variables)>0 && nchar(tmp_include_variables)>2 && !('alarm' %in% unlist(strsplit(include_variables,','))))
        tmp_include_variables<-paste0(tmp_include_variables,',','alarm')
    if(length(tmp_include_variables)>0 && nchar(tmp_include_variables)>2 && !('alarm_block_code' %in% unlist(strsplit(include_variables,','))))
        tmp_include_variables<-paste0(tmp_include_variables,',','alarm_block_code')
    
    #Add all alarms for image
    if(length(tmp_include_variables)>0 && nchar(tmp_include_variables)>2 && !('alarm_all' %in% unlist(strsplit(include_variables,','))))
        tmp_include_variables<-paste0(tmp_include_variables,',','alarm_all')
    if(length(tmp_include_variables)>0 && nchar(tmp_include_variables)>2 && !('alarm_all_block_code' %in% unlist(strsplit(include_variables,','))))
        tmp_include_variables<-paste0(tmp_include_variables,',','alarm_all_block_code')
    
    #Add selected ots for image
    if(length(tmp_include_variables)>0 && nchar(tmp_include_variables)>2 && !('ot' %in% unlist(strsplit(include_variables,','))))
        tmp_include_variables<-paste0(tmp_include_variables,',','ot')
    if(length(tmp_include_variables)>0 && nchar(tmp_include_variables)>2 && !('ot_block_code' %in% unlist(strsplit(include_variables,','))))
        tmp_include_variables<-paste0(tmp_include_variables,',','ot_block_code')
    
    #Add all ots for image
    if(length(tmp_include_variables)>0 && nchar(tmp_include_variables)>2 && !('ot_all' %in% unlist(strsplit(include_variables,','))))
        tmp_include_variables<-paste0(tmp_include_variables,',','ot_all')
    if(length(tmp_include_variables)>0 && nchar(tmp_include_variables)>2 && !('ot_all_block_code' %in% unlist(strsplit(include_variables,','))))
        tmp_include_variables<-paste0(tmp_include_variables,',','ot_all_block_code')
    
    ## Load data
    cat("\nLoading data... ")
    # Current wt
    #Define aggregation 86400 -> aggregate per day
    seconds_offset <- 0
    power_condition <- '' #Always predict without filter power
    rs  <-  load_wtdata(wt_query=NULL,
                        ld_id=ld_id,
                        wp_id=wp_id,
                        wp_code=wp_code,
                        fault=fault,
                        array_id_walm=array_id_walm,
                        array_ot=array_ot,
                        power_condition=power_condition,
                        include_variables=tmp_include_variables,
                        exclude_variables=exclude_variables,
                        unix_timestamp_ini=unix_timestamp_ini,
                        unix_timestamp_end=unix_timestamp_end,
                        freq_dat_med_min=freq_dat_med_min,
                        seconds_to_aggregate=seconds_to_aggregate,
                        seconds_offset=seconds_offset,
                        date_time_name=date_time_name,
                        target=target,
                        table_cast_park_dic=table_cast_park_dic,
                        table_filter_config=table_filter_config,
                        filter=filter,
                        filter_exclude=paste(date_time_name,"ld_id,alarm,alarm_block_code,alarm_all,alarm_all_block_code,ot,ot_block_code,ot_all,ot_all_block_code,n1,weekly_n1,weekly_power",sep=","),
                        update_filter_ranges=TRUE,
                        db_config=db_config)
    if(rs$error) {
        output_msg <- paste0("\n",iam,":on call load_wtdata\n\t",rs$msg)
        close_protocol(output_msg, iam, debug_mode)
        return(list(error=TRUE,data=NULL,msg=output_msg))
    }
    wtdata <- rs$data$wtdata
    outliers<- rs$data$outliers
    wtdata0 <- rs$data$wtdata0
    
    rm(rs)
    # Unhealthy wt
    if(!is.null(m_un)) {
        rs_un  <-  load_wtdata(
            wp_id=m_un$wp_id,
            wp_code=m_un$wp_code,
            array_id_walm=m_un$array_id_walm,
            array_ot=m_un$array_ot,
            power_condition=m_un$power_condition,
            include_variables=tmp_include_variables,
            exclude_variables=m_un$exclude_variables,
            m_un$creation_wtdata_date_ini,
            m_un$creation_wtdata_date_end,
            freq_dat_med_min=freq_dat_med_min,
            seconds_to_aggregate=seconds_to_aggregate,
            seconds_offset=seconds_offset,
            date_time_name=date_time_name,
            target=target,
            table_cast_park_dic=table_cast_park_dic,
            table_filter_config=table_filter_config,
            db_config)
        if(rs_un$error) {
            cat("\n",iam,":on call load_wtdata\n",rs_un$msg)
            cat("\nThe model will be built without fusion options.")
            undata0 <- NULL
        } else {
            undata0 <- rs_un$data$wtdata
        }
        rm(rs_un)
    } else undata0 <- NULL
    
    cat(" Done.\n")
    #------------------------------ Horizon LOOP ------------------------------
    # Loop to iterate over horizon_list
    for( k in 1:n_horizons ) {
        horizon <- horizon_list[k]
        names(result_h)[k] <- as.character(horizon)
        cat("\n\nHORIZON: ", horizon,"\n")
        numTimestamps<-sum(sapply(wtdata, inherits,"Date"))
        numTimestamps2<-sum(sapply(wtdata, inherits,"POSIXct"))
        if(numTimestamps>1||numTimestamps2>1){
            count_war <- count_war + 1
            if(numTimestamps>1)
                war_rec[count_war] <- paste("Multiple timestamp at ", file_pattern, "_", horizon, sep = "")
            else
                war_rec[count_war] <- paste("no timestamp at ", file_pattern, "_", horizon, sep = "")
            warning("WARNING: ", war_rec[count_war])
        }
        
        date_time0 <- wtdata[,date_time_name] # Date time backup
        assign(paste(date_time_name, 0, sep=''), date_time0) #Assign date_time
        
        date_time<-wtdata[,date_time_name]
        #Save alarm and block code because can contain NA
        if('alarm' %in% names(wtdata)) {
            alarm<-wtdata[,c(date_time_name,'alarm')] #backup alarm for futures analysis
            wtdata<-wtdata[ , !(names(wtdata) %in% 'alarm')] # Drop alarm column
        }
        if( 'alarm_block_code' %in% names(wtdata)){
            alarm_block_code<-wtdata[,c(date_time_name,'alarm_block_code')] #backup alarm_block_code for futures analysis and see what alarms are active.
            wtdata<-wtdata[ , !(names(wtdata) %in% 'alarm_block_code')] # Drop alarm_block_code column
        }
        
        #Save alarm and block code because can contain NA
        if('alarm_all' %in% names(wtdata)) {
            alarm_all<-wtdata[,c(date_time_name,'alarm_all')] #backup alarm for futures analysis
            wtdata<-wtdata[ , !(names(wtdata) %in% 'alarm_all')] # Drop alarm column
        }
        if( 'alarm_all_block_code' %in% names(wtdata)){
            alarm_all_block_code<-wtdata[,c(date_time_name,'alarm_all_block_code')] #backup alarm_block_code for futures analysis and see what alarms are active.
            wtdata<-wtdata[ , !(names(wtdata) %in% 'alarm_all_block_code')] # Drop alarm_block_code column
        }
        
        if('ot' %in% names(wtdata)) {
            ot<-wtdata[,c(date_time_name,'ot')] #backup alarm for futures analysis
            wtdata<-wtdata[ , !(names(wtdata) %in% 'ot')] # Drop alarm column
        }
        if( 'ot_block_code' %in% names(wtdata)){
            ot_block_code<-wtdata[,c(date_time_name,'ot_block_code')] #backup alarm_block_code for futures analysis and see what alarms are active.
            wtdata<-wtdata[ , !(names(wtdata) %in% 'ot_block_code')] # Drop alarm_block_code column
        }
        
        if('ot_all' %in% names(wtdata)) {
            ot_all<-wtdata[,c(date_time_name,'ot_all')] #backup alarm for futures analysis
            wtdata<-wtdata[ , !(names(wtdata) %in% 'ot_all')] # Drop alarm column
        }
        if( 'ot_all_block_code' %in% names(wtdata)){
            ot_all_block_code<-wtdata[,c(date_time_name,'ot_all_block_code')] #backup alarm_block_code for futures analysis and see what alarms are active.
            wtdata<-wtdata[ , !(names(wtdata) %in% 'ot_all_block_code')] # Drop alarm_block_code column
        }
        
        ### Cleandata
        wtdata[,date_time_name]<-NULL
        
        #remove columns with Cont
        wtdata<-wtdata[,!grepl("Cont", names(wtdata))]
        #remove columns with _sdv
        #wtdata<-wtdata[,!grepl("_sdv", names(wtdata))]
        #remove columns with _max
        #wtdata<-wtdata[,!grepl("_max", names(wtdata))]
        #remove columns with _min
        #wtdata<-wtdata[,!grepl("_min", names(wtdata))]
        
        sdv<-apply(wtdata,2,function(col) sd(x = col,na.rm = TRUE))
        nazerosdv<-which(is.na(sdv)|sdv==0)
        if(length(nazerosdv)>0)   wtdata<-wtdata[,-nazerosdv]
        na_count <-sapply(wtdata, function(y) sum(length(which(is.na(y)))))
        if(sum(na_count>(nrow(wtdata)*0.2))>0){
            wtdata<-wtdata[,-which(na_count>(nrow(wtdata)*0.2))]#Remove columns with more than 20% NA
        }
        keep<-complete.cases(wtdata)
        
        if((!is.null(keep))&&(length(keep)>0)){
            wtdata<-wtdata[keep,]
            #Sync dates,alarms,ots
            date_time<-date_time[keep]
            if('alarm' %in% names(wtdata0)) alarm<-alarm[keep,]
            if('alarm_block_code' %in% names(wtdata0)) alarm_block_code<-alarm_block_code[keep,]
            if('alarm_all' %in% names(wtdata0)) alarm_all<-alarm_all[keep,]
            if('alarm_all_block_code' %in% names(wtdata0)) alarm_all_block_code<-alarm_all_block_code[keep,]
            if('ot' %in% names(wtdata0)) ot<-ot[keep,]
            if('ot_block_code' %in% names(wtdata0)) ot_block_code<-ot_block_code[keep,]
            if('ot_all' %in% names(wtdata0)) ot_all<-ot_all[keep,]
            if('ot_all_block_code' %in% names(wtdata0)) ot_all_block_code<-ot_all_block_code[keep,]
            #End Sync
        }
        
        #Remove constant columns variables
        wtdata<-wtdata[,(apply(wtdata, 2, var, na.rm=TRUE) != 0)]
        #Final clean
        wtdata<-na.omit(wtdata)
        
        if(nrow(wtdata0)==0){
            cat("Empty wtdata!!!");
            return(list(error=TRUE,data=NULL,msg="Error empty wtdata"));
        }
        wtdata_scaled<-scale(wtdata)
        
        
        save(wtdata_scaled,wtdata,alarm,alarm_block_code,alarm_all,alarm_all_block_code,ot,ot_block_code,ot_all,ot_all_block_code,file_pattern,alarm_block_code,file=paste0("test.RData"))
        #load("test.RData")
        
        d<-dist(wtdata_scaled)
        hclust.out<-hclust(d)
        # Inspect the result
        summary(hclust.out)
        
        
        #k=5
        #k=floor(sqrt(nrow(wtdata0))/2)
        dist<-dbscan::kNNdist(wtdata_scaled,k=1)
        dist2 <- dist[order(dist)]
        ddist <- diff(dist2) / ( 1 / length(dist2))
        eps <- dist2[length(ddist)- length(ddist[ddist >= 1])]
        res<-frNN(wtdata_scaled, eps)
        n_neib_x_point<-sapply(1:length(res$id),function(p) length(res$id[[p]]))
        n_neibg_unique<-sort(unique(n_neib_x_point))
        n_neib_freq<-do.call(rbind, lapply(n_neibg_unique, function(n) data.frame(n_neib=n,freq=length(n_neib_x_point[n_neib_x_point==n])))) 
        rownames(n_neib_freq)<-n_neib_freq$n_neib
        
        fitcurve<-lm(n_neib_freq$freq~poly(n_neib_freq$n_neib,9,raw=TRUE))
        fitcurvevalues<-predict(fitcurve,data.frame(n_neib_freq$n_neib))
        fitcurvedist <- fitcurvevalues / max(fitcurvevalues)
        dfitcurvedist <- diff(fitcurvedist) / ( 1 / length(fitcurvedist))
        firstminimum<-which(dfitcurvedist>0)[[1]]
        
        k<-firstminimum
        #dat <- data.frame(x=n_neib_freq$n_neib, y=n_neib_freq$freq)
        
        #p<-plot_ly(data=dat,x = ~x,y = ~y,name = "Histogram of distance vs Neighbors",type = "bar")
        #p<-layout(p,xaxis=list(title = 'Distance (eps)'),yaxis = list(title = 'Neighbors(k)')) 
        #p
        #htmltools::save_html(p,"histogram.html")
        #freq3<-n_neib_freq$n_neib[order(n_neib_freq$n_neib)]
        #dist3 <- freq3 / max(freq3)
        #ddist3 <- diff(dist3) / ( 1 / length(dist3))
        #k <- ddist3[length(ddist3)- length(ddist3[ddist3 >1])]
        #k<-readline(prompt = "Enter best K (check plot):")
        
        #dbscan::kNNdistplot(wtdata_scaled, k = k )
        #abline(h = eps1, lty = 2,col='red')
        #abline(h = eps2, lty = 2,col='blue')
        #abline(h =eps, lty = 2,col='cyan')
        #readline("Press <return to continue") 
        #Cluster work
        set.seed(1)
        
        #repeat{
        
        dist<-dbscan::kNNdist(wtdata_scaled,k=k)
        # #---------------Forma por la curva-----------
        # # order result
        dist2 <- dist[order(dist[,k])]
        # # scale
        dist2 <- dist2 / max(dist2)
        # # derivative
        ddist <- diff(dist2) / ( 1 / length(dist2))
        # # get first point where derivative is higher than 1
        eps1 <- dist2[length(ddist)- length(ddist[ddist >= 1.2])]
        # #-------------- fin forma-----------------
        # 
        # #------------- Por el quantil 80 -----------
        eps2<-quantile(dist[,k],probs=c(.75),na.rm=TRUE)
        # #------------ fin quantil 80---------------
        # 
        epsbef<-eps
        eps<-max(eps1,eps2)
        # 
        cat(paste0("\n k:",k," eps before:",epsbef," eps now:",eps,", eps with slope is big-equal than 1 is:",eps1,", at quantile 75% eps would be:",eps2,"\n"))
        o <- dbscan::optics(wtdata_scaled,eps=eps,minPts=k)
        #db <- dbscan::dbscan(wtdata_scaled, eps = eps, minPts = k)
        #plot(db, wtdata_scaled[,2], main = "DBSCAN", frame = FALSE)
        #clusters<-db$cluster
        o1<-extractDBSCAN(o, eps_cl=k)
        clusters<-o1$cluster
        if(class(wtdata_scaled)=="matrix") wtdata_scaled<-as.data.frame(wtdata_scaled)
        #save_cluster_plot(wtdata=wtdata_scaled,clusters=clusters,alarms=alarm$alarm,file_pattern=paste0(file_pattern,"_",k,"k"),alarm_block_code=alarm_block_code$alarm_block_code,zipFile=TRUE)
        
        
        
        #Calculate centroid for each cluster
        clusters_id<-unique(clusters)
        #Todo optimize function
        centroid<-NULL
        cluster_data<-NULL
        for(cid in 1:length(clusters_id)){
            cat(cid)
            selected_rows<-wtdata_scaled[(clusters==clusters_id[cid]),]
            if(nrow(selected_rows)>1)
                currentCentroid<-apply(selected_rows,2,mean,na.rm=TRUE)
            else
                currentCentroid<-selected_rows
            size<-nrow(selected_rows)
            num_alarms<-sum(alarm$alarm[(clusters==clusters_id[cid])])
            cluster_data<-data.frame(rbind(cluster_data,cbind(cluster=clusters_id[cid],size=size,num_alarms=num_alarms,distance=sqrt(sum((abs(selected_rows-currentCentroid))^2))/size)))
        }
        cluster_data$distance<-scale(cluster_data$distance)
        wtdata_scaled$cluster=as.factor(clusters)
        wtdata_plot<-wtdata
        wtdata_plot$cluster=as.factor(clusters)
        
        wtdata_negative_density<-wtdata[(wtdata_scaled$cluster %in% cluster_data$cluster[cluster_data$distance<=0]),]
        date_time_negative_density<-date_time[(wtdata_scaled$cluster %in% cluster_data$cluster[cluster_data$distance<=0])]
        
        pre_alarm_name<-"pre_alarm"
        wtdata_negative_density[,pre_alarm_name]<-0
        anticipation<-21
        marging<-14
        if(length(which(alarm$alarm==1))>0){
            pre_alarm_dates<-as.POSIXct(unlist(lapply(date_time_negative_density[alarm$alarm==1],function(dt) seq.POSIXt(dt-as.difftime(anticipation+marging-1, units="days"),dt-as.difftime(anticipation, units="days"),by='day'))),origin='1970-01-01',tz = "UTC")
            wtdata_negative_density[which(date_time_negative_density %in% pre_alarm_dates),pre_alarm_name]<-1
        }
        
        #################
        #Data partition
        id_tr <- 1:round(nrow(wtdata)*(50/100))
        trdata <- wtdata_negative_density[id_tr,]
        trdata<-na.omit(trdata)
        tedata <- wtdata_negative_density[-id_tr,]
        tedata<-na.omit(tedata)
        rf_model<-train(pre_alarm~.,data=trdata,method="rf",
                        trControl=trainControl(method="cv",number=5),
                        prox=TRUE,allowParallel=TRUE)
        
        p<-ggplot(data=wtdata_scaled,aes(x = TempGond_avg,y = TempRodamMultip_avg ,color=cluster))+
            geom_point()
        
        p<-plot_ly(wtdata_plot, x = ~TempCojLOA_avg, y = ~TempCojLA_avg, z = ~VelRotor_avg, color = ~cluster) %>%
            add_markers()
        
        clusters_pos_dens<-clusters_id[(cluster_data$distance>0)&(cluster_data$size>5)]
        #p<-layout(p,xaxis=list(title = 'Distance (eps)'),yaxis = list(title = 'Neighbors(k)')) 
        #p
        
        
        
        cat('Number of clusters generated:',max(clusters))
        
        save(clusters,alarm,ot,file=paste0("afterclust.RData"))
        load("afterclust.RData")
        library("plotly")
        library("dplyr")
        source("n_cluster_horitzon.R")
        
        #get alarms
        alarms_per_day<-as.data.frame.table(tapply(alarm$alarm,format(alarm$date_time, '%Y-%m-%d'),FUN=sum))
        names(alarms_per_day)[1]<-"date_time"
        names(alarms_per_day)[2]<-"count"
        alarms_per_day$date_time<-as.POSIXct(alarms_per_day$date_time)
        alarms_per_day$count<-as.numeric(alarms_per_day$count)
        active_alarms_per_day<-alarms_per_day[alarms_per_day$count>0,]
        rs<-lapply(1:nrow(active_alarms_per_day),function(i) n_cluster_horitzon(alarm,clusters,active_alarms_per_day$date_time[i],7)$data)
        df<-ldply(rs,data.frame)
        ay = list(
            side = "left",title = "Cluster"
        )
        ay2 = list(
            title = "Alarm", side = "right",overlaying="y"
        )
        ay3 = list(
            title = "Ots", side = "right",overlaying="y"
        )
        p<-plot_ly()
        p<-plotly::layout(p,yaxis=ay,yaxis2 = ay2,yaxis3=ay3,barmode = 'stack')
        for(c in 0:max(clusters)){
            name=paste0("Alarm Cluster ",c)
            #if(c==0) name<-paste0("Alarm Cluster ",c," intermedio")
            #if(c==1) name<-paste0("Alarm Cluster ",c," produccion intensa")
            #if(c==2) name<-paste0("Alarm Cluster ",c," abajo no produccion")
            p<-add_trace(p,data=df[df$cluster==c,],x=~date_time,y=~Freq,type = 'bar',name=name,yaxis = "y1")
        }
        selected_alarms<-alarms_per_day[alarms_per_day$count>0,]
        p<-add_trace(p,x=selected_alarms$date_time,y=selected_alarms$count,type = 'bar',name="alarms",yaxis = "y2",visible = "legendonly")
        
        
        #get ots
        ot_per_day<-as.data.frame.table(tapply(ot$ot,format(ot$date_time, '%Y-%m-%d'),FUN=sum))
        names(ot_per_day)[1]<-"date_time"
        names(ot_per_day)[2]<-"count"
        ot_per_day$date_time<-as.POSIXct(ot_per_day$date_time)
        ot_per_day$count<-as.numeric(ot_per_day$count)
        active_ots_per_day<-ot_per_day[ot_per_day$count>0,]
        if(nrow(active_ots_per_day)>0){
            rs<-lapply(1:nrow(active_ots_per_day),function(i) n_cluster_horitzon(ot,clusters,active_ots_per_day$date_time[i],7)$data)
            df<-ldply(rs,data.frame)
            for(c in 0:max(clusters)){
                p<-add_trace(p,data=df[df$cluster==c,],x=~generated_date,y=~Freq,type = 'bar',name=paste0("Ots Cluster ",c),yaxis = "y1")
            }
            selected_ots<-ot_per_day[ot_per_day$count>0,]
            p<-add_trace(p,x=selected_ots$date_time,y=selected_ots$count,type = 'bar',name="ots",yaxis = "y3", visible = "legendonly")
        }
        #p
        
        l <- htmltools::tagList()
        l[[1]]<-p
        htmlFile<-paste0(file_pattern,".html")
        save_tags(l, htmlFile,selfcontained=TRUE)
        #htmltools::save_html(l, paste0(file_pattern,".html"))
        
        #rep<-readline(prompt = "Repeat with other k (y/n)?")
        
        #if (substr(rep, 1, 1) == "y")
        #  k<-readline(prompt = "Enter best K (check plot):")
        #else
        #  break
        #}
        #save(clusM,wtdata_scaled,file=paste0(file_pattern,"_",k,"k.RData"))
    }
    
    output_msg <- paste0("\n",iam,":OK. END.\n")
    close_protocol(output_msg, iam, debug_mode)
    
    return(list(error=FALSE,data=list(modelPath=modelPath,logPath=log_path),msg=output_msg));
}