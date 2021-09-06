filter_custom<-function(config_table="1_filter_config",
                        filter=NULL,
                        data=NULL,
                        wp_code=NULL,
                        exclude=NULL,
                        date_time_name="date_time",
                        target_name=NULL,
                        table_cast_park_dic='1_cast_park_table_dic',
                        verbose=FALSE,
                        update_ranges=FALSE,
                        filter_range_data=NULL,
                        db_config=NULL) {
  iam=match.call()[[1]]
  #Dependencia basica
  if(!exists("dependencyLoader")){
    if(!file.exists('functions_common/dependencyLoader.R')) return(list(error=TRUE,warning=F,data=NULL,msg=paste0("\n",iam,":Missing dependency function: functions_common/dependencyLoader.R")));
    source('functions_common/dependencyLoader.R')
  }
  libraries<-c('caret')
  sources<-paste0("functions_common/",
                  c('db_query.R','close_protocol.R','filter_clean_data.R','filter_loop.R','filter_loop_window.R','filter_range.R','filter_quantile.R','filter_hampel.R','filter_esd.R','filter_check_data_range.R','filter_update_data_range.R'))
  dep<-dependencyLoader(c(libraries,sources))
  if(dep$error)  return(list(error=TRUE,warning=F,data=NULL,msg=paste0("\n",iam,":on call dependencyLoader\n",dep$msg)))
  
  #No filter applied
  if(is.null(filter)||is.na(filter)||(is.character(filter)&&filter==''))
    return(list(error=FALSE,warning=F,data=list(clean=data,outliers=NULL),msg="OK"))
  
  filter<-unlist(strsplit(filter,","))
  
  filteredData<-data
  
  if(!is.null(target_name)&&!is.na(target_name)&&(!target_name %in% names(data))) return(list(error=TRUE,warning=F,data=NULL,msg=paste0("Error: specified target variable '",target_name,"' but is missing from wtdata!!")))
  #Remove variables that haven't to be in Workspace before the loop...
  if(exists("limits_out",inherits = F)) rm(limits_out)
  if(exists("outliers",inherits = F)) rm(outliers)
  
  warning<-F
  final_msg<-'ok'
  
  #Check if exclude is comma separated or vector
  if(!is.null(exclude)&&!is.na(exclude)&&length(exclude)==1&&grepl(pattern = ',',x = exclude)) exclude<-unlist(strsplit(exclude,','))
  
  #Check if target name and if it is in exclude
  if(!is.null(exclude)&&!is.na(exclude)&&!is.null(target_name)&&!is.na(target_name)&&!(target_name %in% exclude)) return(list(error=T,warning=F,data=NULL,msg=paste0('Defined target_name:',target_name,' but is missing from exclude list')))
  
  #Check if date_time is in exclude
  if(!is.null(exclude)&&!is.na(exclude)&&!is.null(date_time_name)&&!is.na(date_time_name)&&!(date_time_name %in% exclude)) return(list(error=T,warning=F,data=NULL,msg=paste0('Defined date_time_name:',date_time_name,' but is missing from exclude list')))
  
  #Check if all variables are numeric, if not exclude and warning
  selected_columns<-colnames(filteredData) %in% exclude
  notnumeric<-sapply(1:ncol(filteredData[,selected_columns]),function(col) !is.numeric(filteredData[1,col]))
  if(any(notnumeric)){
    warning<-T
    final_msg<-paste0('The following columns are not numeric and have been excluded:',paste0(selected_columns[notnumeric],collapse = ','))
    exclude<-c(exclude,selected_columns[notnumeric])
  }
  
  for(currentFilter in filter){
    if(length(currentFilter)==0||currentFilter == ""){
      rs<-list(error=FALSE,data=list(clean=filteredData,outliers=NULL),msg="OK")
    }else{
      if(!is.null(wp_code)&&!is.na(wp_code))
        query<-paste0("SELECT generic_type,parameters FROM ",config_table," WHERE filter='",currentFilter,"' AND (wp_code='",tolower(wp_code),"' OR wp_code IS NULL) ORDER BY wp_code DESC LIMIT 1;")
      else
        query<-paste0("SELECT generic_type,parameters FROM ",config_table," WHERE filter='",currentFilter,"' ORDER BY wp_code DESC LIMIT 1;")
      
      rs<-db_query(query,db_config=db_config)
      if(rs$error) return(list(error=TRUE,warning=F,data=NULL,msg=paste0("\n",iam,":on call db_query\n",dep$msg)))
      if(!is.null(rs$data)){
        if(("parameters" %in% names(rs$data))&&!is.null(rs$data$parameters)&&length(rs$data$parameters)>0){
          if(grepl(",",rs$data$parameters))
            parameters<-strsplit(rs$data,",")
          else
            parameters<-rs$data$parameters
        }else{
          parameters<-""
        }
        if(("generic_type" %in% names(rs$data))&&!is.null(rs$data$generic_type)&&length(rs$data$generic_type)>0){
          generic_type<-rs$data$generic_type
        }else{
          generic_type<-"unknown"
          return(list(error=TRUE,warning=F,data=NULL,msg=paste0("\n",iam,":Unknown filter:",currentFilter)))
        }
      }
    }
    
    if(length(currentFilter)>0&&generic_type == "fq") {
      if(verbose) cat("\nFilter: Filtering Outliers with quantile algorithm, ")
      output_msg <- paste0("\n",iam,":No iqr_multiplier parameter,  it must be: iqr_multiplier=value , example iqr_multipier=1.5\n\t",rs$msg)
      
      if(length(parameters)<=0||!grepl("iqr_multiplier=", parameters)) {
        close_protocol(output_msg, iam, debug_mode)
        return(list(error=TRUE,warning=F,data=NULL,msg=output_msg))
      }
      iqr_multiplier<-as.numeric(sub(".*iqr_multiplier=(\\d+(.\\d+)?).*", "\\1", parameters))
      if(!exists("iqr_multiplier",inherits=F)||is.null(iqr_multiplier)||!is.numeric(iqr_multiplier)||is.na(iqr_multiplier)||is.nan(iqr_multiplier)){
        close_protocol(output_msg, iam, debug_mode)
        return(list(error=TRUE,warning=F,data=NULL,msg=output_msg))
      }
      if(verbose) cat(paste0(" parameters: interquartile range:",iqr_multiplier," ..."))
      func<-filter_quantile
      params<-data.frame(iqr_multiplier=iqr_multiplier)
      rs<-filter_loop(wtdata = filteredData,exclude = exclude,date_time_name = date_time_name,func=func,params=params)
      if(rs$error){
        output_msg <- paste0("\n",iam,":on call filter_quantile\n\t",rs$msg)
        close_protocol(output_msg, iam, debug_mode)
        return(list(error=TRUE,warning=F,data=NULL,msg=output_msg))
      }
    }
    
    if(length(currentFilter)>0&&generic_type == "fh") {
      if(verbose) cat("\nFilter: Filtering Outliers with hampel algorithm, ")
      output_msg <- paste0("\n",iam,":No threshold parameter,  it must be: threshold=value , example threshold=3\n\t",rs$msg)
      
      if(length(parameters)<=0||!grepl("threshold=", parameters)) {
        close_protocol(output_msg, iam, debug_mode)
        return(list(error=TRUE,warning=F,data=NULL,msg=output_msg))
      }
      threshold<-as.numeric(sub(".*threshold=(\\d+(.\\d+)?).*", "\\1", parameters))
      if(!exists("threshold",inherits=F)||is.null(threshold)||!is.numeric(threshold)||is.na(threshold)||is.nan(threshold)){
        close_protocol(output_msg, iam, debug_mode)
        return(list(error=TRUE,warning=F,data=NULL,msg=output_msg))
      }
      if(verbose) cat(paste0(" parameters: threshold:",threshold," ..."))
      func<-filter_hampel
      params<-data.frame(threshold=threshold)
      rs<-filter_loop(wtdata = filteredData,exclude = exclude,date_time_name = date_time_name,func=func,params=params)
      if(rs$error){
        output_msg <- paste0("\n",iam,":on call filter_hampel\n\t",rs$msg)
        close_protocol(output_msg, iam, debug_mode)
        return(list(error=TRUE,warning=F,data=NULL,msg=output_msg))
      }
    }
    
    if(length(currentFilter)>0&&generic_type == "fqw") {
      if(verbose) cat("\nFilter: Filtering Outliers with quantile moving window algorithm, ")
      output_msg <- paste0("\n",iam,":No iqr_multiplier/window/slide parameter,  it must be: iqr_multiplier=value,window=value,slide=value , example iqr_multipier=1.5,window=30,slide=30\n\t",rs$msg)
      
      if(length(parameters)<=0||!grepl("iqr_multiplier=", parameters)||!grepl("window=", parameters)||!grepl("slide=", parameters)) {
        close_protocol(output_msg, iam, debug_mode)
        return(list(error=TRUE,warning=F,data=NULL,msg=output_msg))
      }
      iqr_multiplier<-as.numeric(sub(".*iqr_multiplier=(\\d+(.\\d+)?).*", "\\1", parameters))
      if(!exists("iqr_multiplier",inherits=F)||is.null(iqr_multiplier)||!is.numeric(iqr_multiplier)||is.na(iqr_multiplier)||is.nan(iqr_multiplier)){
        close_protocol(output_msg, iam, debug_mode)
        return(list(error=TRUE,warning=F,data=NULL,msg=output_msg))
      }
      window<-as.numeric(sub(".*window=(\\d+(.\\d+)?).*", "\\1", parameters))
      if(!exists("window",inherits=F)||is.null(window)||!is.numeric(window)||is.na(window)||is.nan(window)){
        close_protocol(output_msg, iam, debug_mode)
        return(list(error=TRUE,warning=F,data=NULL,msg=output_msg))
      }
      slide<-as.numeric(sub(".*slide=(\\d+(.\\d+)?).*", "\\1", parameters))
      if(!exists("slide",inherits=F)||is.null(slide)||!is.numeric(slide)||is.na(slide)||is.nan(slide)){
        close_protocol(output_msg, iam, debug_mode)
        return(list(error=TRUE,warning=F,data=NULL,msg=output_msg))
      }
      if(verbose) cat(paste0(" parameters: iqr_multiplier:",iqr_multiplier,", window:",window,", slide each:",slide," ..."))
      func<-filter_quantile
      params<-data.frame(threshold=threshold)
      rs<-filter_loop_window(wtdata = filteredData,exclude = exclude,date_time_name = date_time_name,func=func,params=params,window=30,slide=30)
      if(rs$error){
        output_msg <- paste0("\n",iam,":on call filter_quantile_movingWindow\n\t",rs$msg)
        close_protocol(output_msg, iam, debug_mode)
        return(list(error=TRUE,warning=F,data=NULL,msg=output_msg))
      }
    }
    
    if(length(currentFilter)>0&&generic_type == "fhw") {
      if(verbose) cat("\nFilter: Filtering Outliers with hampel moving window algorithm, ")
      output_msg <- paste0("\n",iam,":No threshold/window/slide parameter,  it must be: threshold=value,window=value,slide=value , example threshold=1.5,window=30,slide=30\n\t",rs$msg)
      
      if(length(parameters)<=0||!grepl("threshold=", parameters)||!grepl("window=", parameters)||!grepl("slide=", parameters)) {
        close_protocol(output_msg, iam, debug_mode)
        return(list(error=TRUE,warning=F,data=NULL,msg=output_msg))
      }
      threshold<-as.numeric(sub(".*threshold=(\\d+(.\\d+)?).*", "\\1", parameters))
      if(!exists("threshold",inherits=F)||is.null(threshold)||!is.numeric(threshold)||is.na(threshold)||is.nan(threshold)){
        close_protocol(output_msg, iam, debug_mode)
        return(list(error=TRUE,warning=F,data=NULL,msg=output_msg))
      }
      window<-as.numeric(sub(".*window=(\\d+(.\\d+)?).*", "\\1", parameters))
      if(!exists("window",inherits=F)||is.null(window)||!is.numeric(window)||is.na(window)||is.nan(window)){
        close_protocol(output_msg, iam, debug_mode)
        return(list(error=TRUE,warning=F,data=NULL,msg=output_msg))
      }
      slide<-as.numeric(sub(".*slide=(\\d+(.\\d+)?).*", "\\1", parameters))
      if(!exists("slide",inherits=F)||is.null(slide)||!is.numeric(slide)||is.na(slide)||is.nan(slide)){
        close_protocol(output_msg, iam, debug_mode)
        return(list(error=TRUE,warning=F,data=NULL,msg=output_msg))
      }
      if(verbose) cat(paste0(" parameters: threshold:",threshold,", window:",window,", slide each:",slide," ..."))
      func<-filter_hampel
      params<-data.frame(threshold=threshold)
      rs<-filter_loop_window(wtdata = filteredData,exclude = exclude,date_time_name = date_time_name,func=func,params=params,window=30,slide=30)
      if(rs$error){
        output_msg <- paste0("\n",iam,":on call filter_hampel_movingWindow\n\t",rs$msg)
        close_protocol(output_msg, iam, debug_mode)
        return(list(error=TRUE,warning=F,data=NULL,msg=output_msg))
      }
    }
    
    if(length(currentFilter)>0&&generic_type == "fsd") {
      if(verbose) cat("\nFilter: Filtering Outliers with Extreme Studentized Deviate (ESD) algorithm... ")
      output_msg <- paste0("\n",iam,":No sd parameter,  it must be: sd=value , example sd=3\n\t",rs$msg)
      if(length(parameters)<=0||!grepl("sd=", parameters)) {
        close_protocol(output_msg, iam, debug_mode)
        return(list(error=TRUE,warning=F,data=NULL,msg=output_msg))
      }
      sd<-as.numeric(sub(".*sd=(\\d+(.\\d+)?).*", "\\1", parameters))
      if(!exists("sd",inherits=F)||is.null(sd)||!is.numeric(sd)||is.na(sd)||is.nan(sd)){
        close_protocol(output_msg, iam, debug_mode)
        return(list(error=TRUE,warning=F,data=NULL,msg=output_msg))
      }
      func<-filter_esd
      params<-data.frame(sd=sd)
      rs<-filter_loop(wtdata = filteredData,exclude = exclude,date_time_name = date_time_name,func=func,params=params)
      if(rs$error){
        output_msg <- paste0("\n",iam,":on call filter_esd\n\t",rs$msg)
        close_protocol(output_msg, iam, debug_mode)
        return(list(error=TRUE,warning=F,data=NULL,msg=output_msg))
      }
    }
    
    if(length(currentFilter)>0&&generic_type == "fsdw") {
      if(verbose) cat("\nFilter: Filtering Outliers with Extreme Studentized Deviate (ESD) moving window algorithm, ")
      output_msg <- paste0("\n",iam,":No sd/window/slide parameter,  it must be: sd=value,window=value,slide=value , example sd=3,window=30,slide=30\n\t",rs$msg)
      if(length(parameters)<=0||!grepl("sd=", parameters)) {
        close_protocol(output_msg, iam, debug_mode)
        return(list(error=TRUE,warning=F,data=NULL,msg=output_msg))
      }
      sd<-as.numeric(sub(".*sd=(\\d+(.\\d+)?).*", "\\1", parameters))
      if(!exists("sd",inherits=F)||is.null(sd)||!is.numeric(sd)||is.na(sd)||is.nan(sd)){
        close_protocol(output_msg, iam, debug_mode)
        return(list(error=TRUE,warning=F,data=NULL,msg=output_msg))
      }
      if(length(parameters)<=0||!grepl("window=", parameters)||!grepl("slide=", parameters)) {
        close_protocol(output_msg, iam, debug_mode)
        return(list(error=TRUE,warning=F,data=NULL,msg=output_msg))
      }
      window<-as.numeric(sub(".*window=(\\d+(.\\d+)?).*", "\\1", parameters))
      if(!exists("window",inherits=F)||is.null(window)||!is.numeric(window)||is.na(window)||is.nan(window)){
        close_protocol(output_msg, iam, debug_mode)
        return(list(error=TRUE,warning=F,data=NULL,msg=output_msg))
      }
      slide<-as.numeric(sub(".*slide=(\\d+(.\\d+)?).*", "\\1", parameters))
      if(!exists("slide",inherits=F)||is.null(slide)||!is.numeric(slide)||is.na(slide)||is.nan(slide)){
        close_protocol(output_msg, iam, debug_mode)
        return(list(error=TRUE,warning=F,data=NULL,msg=output_msg))
      }
      if(verbose) cat(paste0(" parameters:window:",window,", slide each:",slide," ..."))
      func<-filter_esd
      params<-data.frame(sd=sd)
      rs<-filter_loop_window(wtdata = filteredData,exclude = exclude,date_time_name = date_time_name,func=func,params=params,window=30,slide=30)
      if(rs$error){
        output_msg <- paste0("\n",iam,":on call filter_esd_movingWindow\n\t",rs$msg)
        close_protocol(output_msg, iam, debug_mode)
        return(list(error=TRUE,warning=F,data=NULL,msg=output_msg))
      }
    }
    
    if(length(currentFilter)>0&&generic_type == "frange") {
      if(verbose) cat("\nFilter: Filtering Outliers with manual range method... ")
      if(is.null(filter_range_data)||(!is.null(filter_range_data)&&is.na(filter_range_data))){#Download filters
        if(is.null(wp_code)||is.na(wp_code)) return(list(error=TRUE,warning=F,data=NULL,msg=paste0("\n",iam,":Specified filter Frange but the wp_code is not defined")))
        rs<-filter_check_data_range(wp_code=tolower(wp_code),table_cast_park_dic=table_cast_park_dic,db_config=db_config)
        if(!(rs$error)&&!is.null(rs$data)){
          params<-list(limits=rs$data)
        }else{
          params<-list(limits=NULL)
        }
      }else{#Use the submitted filters
        params<-list(limits=filter_range_data)
      }
      func<-filter_range
      rs<-filter_loop(wtdata = filteredData,exclude = exclude,date_time_name = date_time_name,func=func,params=params)
      if(rs$error){
        output_msg <- paste0("\n",iam,":on call filter_range\n\t",rs$msg)
        close_protocol(output_msg, iam, debug_mode)
        return(list(error=TRUE,warning=F,data=NULL,msg=output_msg))
      }
    }
    
    if(length(currentFilter)>0&&generic_type == "fclean") {
      if(verbose) cat("\nFilter: Filtering Outliers with filter_clean_data algorithm... ")
      rs<-NULL
      output_msg <- paste0("\n",iam,":No balanced_threshold prob parameter,  it must be: balanced_threshold=value , example balanced_threshold=0.1\n\t",rs$msg)
      if(length(parameters)<=0||!grepl("balanced_threshold=", parameters)) {
        close_protocol(output_msg, iam, debug_mode)
        return(list(error=TRUE,warning=F,data=NULL,msg=output_msg))
      }
      balanced_threshold<-as.numeric(sub(".*balanced_threshold=(\\d+(.\\d+)?).*", "\\1", parameters))
      if(!exists("balanced_threshold",inherits=F)||is.null(balanced_threshold)||!is.numeric(balanced_threshold)||is.na(balanced_threshold)||is.nan(balanced_threshold)){
        close_protocol(output_msg, iam, debug_mode)
        return(list(error=TRUE,warning=F,data=NULL,msg=output_msg))
      }
      rs<-filter_clean_data(wtdata=filteredData,exclude=exclude,balanced_threshold=balanced_threshold)
      if(rs$error){
        output_msg <- paste0("\n",iam,":on call filter_range\n\t",rs$msg)
        close_protocol(output_msg, iam, debug_mode)
        return(list(error=TRUE,warning=F,data=NULL,msg=output_msg))
      }
      rs$data$clean <- as.data.frame(rs$data[[4]],verbose=verbose) # TODO: include near zero detection
      rs$data$outliers <- NULL
    }
    
    if(length(currentFilter)>0&&generic_type == "fnzv") {
      if(verbose) cat("\nFilter: Filtering Outliers with nearZeroVar algorithm... ")
      rs<-NULL
      rs$data$clean<- filteredData # By default all
      subwtdata <- as.data.frame(filteredData[, !(names(filteredData) %in% unlist(strsplit(exclude,","))), drop=F]) #Exclude columns , date_time,etc since the is no alarm
      uniqueCut <- 5 #Unique cut defaults to 5
      if(!is.null(target_name)&&!is.na(target_name)) uniqueCut<- min( mean(filteredData[,target_name],na.rm = T)*100,5 ) #Use target variable for uniqueCut.
      id_nzv <- nearZeroVar(subwtdata, freqCut = 97/3, uniqueCut = uniqueCut ) + 1 # el uno corrige el indice
      if(!is.null(id_nzv) && !is.na(id_nzv) && length(id_nzv) > 0 ) {
        if(verbose) cat("\nFilter: These variables have near zero variance, they are omited:", paste(names(filteredData)[id_nzv], collapse = ", "))
        filteredData[,names(filteredData) %in% names(subwtdata)[id_nzv]]<-NA
        rs$data$clean <- as.data.frame(filteredData) # Delete near zero variables omiting datetime and target_name.
      }
      rs$data$outliers <- NULL #Todo put NZV columns
    }
    
    final_msg<-paste0(final_msg,', On filter:',currentFilter,' ',rs$msg)
    
    if(generic_type!="unknown"){
      filteredData<-rs$data$clean
      if(exists("outliers",inherits=F)&&!is.null(rs$data$outliers)&&nrow(rs$data$outliers)>0)
        outliers<-rbind(outliers,rs$data$outliers)
      else
        outliers<-rs$data$outliers
      
      #Update limits
      if(!is.null(rs$data$limits)){
        if(exists("limits_out",inherits=F)){
          #Merge by var name
          limits_out<-merge(limits_out,rs$data$limits,by='var',all=TRUE)
          #get the most restrictive one, this means the one with the smaller range.
          tmp_minmax<-lapply(1:nrow(limits_out),function(i){
            if(any(is.na(c(limits_out$min.x[i],limits_out$max.x[i],limits_out$min.y[i],limits_out$max.y[i])))){ #If some of them are NA, return the min and the max of the max.
              data.frame(min=min(limits_out$min.x[i],limits_out$min.y[i],na.rm=TRUE),max=max(limits_out$max.x[i],limits_out$max.y[i],na.rm=TRUE))
            }else{
              #Return the smaller range which is the most restrictive
              if(abs(limits_out$max.x[i]-limits_out$min.x[i])<=abs(limits_out$max.y[i]-limits_out$min.y[i]))
                data.frame(min=limits_out$min.x[i],max=limits_out$max.x[i])
              else
                data.frame(min=limits_out$min.y[i],max=limits_out$max.y[i])
            }
          }) 
          tmp_minmax<-do.call(rbind, tmp_minmax) #to df
          limits_out<-cbind(limits_out,tmp_minmax)
          limits_out<-limits_out[, !(colnames(limits_out) %in% c("min.x","max.x","min.y","max.y"))]
        }else{
          limits_out<-rs$data$limits
        }
      }
    }else{
      return(list(error=TRUE,warning=F,data=NULL,msg=paste0("\n",iam,"Filter :",currentFilter," doesn't exists in ",config_table)))
    }
  }
  if(length(currentFilter)>0&&currentFilter!=""&&exists("outliers",inherits=F)&&!is.null(outliers)&&NROW(outliers)>0){
    if(update_ranges&&exists("limits_out",inherits=F)&&!is.null(limits_out)&&nrow(limits_out)>0){
      if(is.null(wp_code)||is.na(wp_code)) return(list(error=TRUE,warning=F,data=NULL,msg=paste0("\n",iam,":Specified update_ranges but the wp_code is not defined")))
      rs<-filter_update_data_range(limits_out,wp_code=tolower(wp_code),table_cast_park_dic=table_cast_park_dic,db_config=db_config)
      if(rs$error)  return(list(error=TRUE,warning=F,data=NULL,msg=paste0("\n",iam,":on call filter_update_data_range\n",rs$msg)))
      if(rs$warning) warning<-T
      final_msg<-paste0(final_msg,', On range update ',rs$msg)
    }
    n_registers<-sum((outliers %>% group_by_(as.symbol(date_time_name)) %>% summarise(count = length(variable)))$count)
    n_vars<-length(unique(outliers$variable))
    if(verbose) cat(paste0("Filter: Done, filtered ",n_registers," register entries with at least 1 outlier, ",n_vars," variables have at least one outlier. Total outliers in dataset:",NROW(outliers)))
  }
  
  return(list(error=FALSE,warning=warning,data=list(clean=filteredData,outliers=outliers),msg=final_msg))
}
