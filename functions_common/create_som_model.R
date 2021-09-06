helper_te<-function(rows,som_codes,som_pts,hex){
  te<-apply(X = som_codes,MARGIN = 1,FUN = function(sc){
    te<-apply(X = rows,1,function(r){
      distances<-sapply(X = sc,function(sc){
        return(sqrt(sum(r-sc)^2))
      })
      #Get first distance
      #first_pos<-which(distances==max(distances,na.rm = T)) #Paper test
      first_pos<-which(distances==min(distances,na.rm = T))
      first<-som_pts[first_pos,]
      #second_pos<-which(distances==max(distances[-first_pos],na.rm = T)) #Paper test
      second_pos<-which(distances==min(distances[-first_pos],na.rm = T))
      second<-som_pts[second_pos,]
      diff_x<-first[1]-second[1]
      diff_y<-first[2]-second[2]
      if(hex){
        ret<-ifelse(((abs(diff_x)==1)&&(diff_y!=0))||((abs(diff_x)!=1)&&(diff_x>0.5||diff_y>0.5)),1,0)
      }else{
        ret<-ifelse(((diff_x>1)||(diff_y>1)),1,0)
      }
      return(ret)
    })
    return(te)
  })
  return(te)
}

calculate_metrics<-function(som_codes=NULL,som_pts=NULL,som_distances=NULL,wtdata=NULL,parallel=T,hex=F,log_file=NULL,verbose=F,normalize=T,stat=NULL){
  if(!exists("dependencyLoader")){
    if(!file.exists('functions_common/dependencyLoader.R')) return(list(error=TRUE,data=NULL,msg=paste0("\n",iam,":Missing dependency function: functions_common/dependencyLoader.R")));
    source('functions_common/dependencyLoader.R')
  }
  # Sources
  libraries<-c('doSNOW','bigmemory','itertools','parallel')
  dep<-dependencyLoader(libraries)
  if(dep$error) return(list(error=TRUE, warning=FALSE, data=NULL,msg=paste0("\n",iam,":on call dependencyLoader\n",dep$msg)))
  
  if((is.null(wtdata)||all(is.na(wtdata)))) return(list(error=T,data=NULL,msg="missing data"))
  if((is.null(som_codes)||all(is.na(som_codes)))) return(list(error=T,data=NULL,msg="missing som_codes"))
  if((is.null(som_pts)||all(is.na(som_pts)))) return(list(error=T,data=NULL,msg="missing som_pts"))
  if((is.null(som_distances)||all(is.na(som_distances)))) return(list(error=T,data=NULL,msg="missing som_distances"))
  
  
  #Normalize
  if(normalize){
    wtdata<-sapply(1:ncol(wtdata), function(c) {(wtdata[,c] - stat$mean[c])/stat$sdv[c]}) 
  }
  
  dim=max(som_pts,na.rm = T)#I assume is squared som
  
  #### Measure som quality
  # QE :The quantization error is computed by determining the average distance of
  # the sample vectors to the cluster centroids by which they are represented.
  # In case of the SOM, the cluster centroids are the prototype vectors.
  qe<-mean(som_distances,na.rm=T)
  
  #Topographic error, the proportion of all data vectors for which first and second BMUs are not adjacent units. Measures topology preservation.
  #http://www.cis.hut.fi/somtoolbox/package/docs2/som_quality.html
  mb_per_thread<-500
  if(Sys.info()[['sysname']]=="Linux"){
    free<-as.numeric(system("awk '/MemAvailable/ {print $2}' /proc/meminfo",intern=TRUE))/1024
  }else{#windows
    free<-as.numeric(gsub(x=paste0(system("wmic OS get FreePhysicalMemory /Value",intern = T),collapse = ''),pattern = '.*=([0-9]+)',replacement = '\\1'))/1024
  }
  if(is.na(free)||is.nan(free)||is.infinite(free)) free<-1024
  num_threads<-floor(free/mb_per_thread)
  
  if((parallel&&(num_threads>1))){
    num_threads<-min(num_threads,parallel::detectCores())
    num_threads<-min(nrow(wtdata),floor(num_threads)) #Model in memory uses about 3% memory @600 @2years
    time_ini<-Sys.time()
    if(verbose)cat(paste0('SOM ',paste0(dim,collapse = ','),' quality(qe,te) INI parallel(',num_threads,' threads):',time_ini,'\n'))
    #Divide work
    chunk_length=floor(nrow(wtdata)/num_threads)
    if(!is.null(log_file))
      cl <- parallel::makePSOCKcluster(num_threads,outfile=log_file)
    else
      cl <- parallel::makePSOCKcluster(num_threads)
    doSNOW::registerDoSNOW(cl)
    max_rows<-nrow(wtdata)
    x<-bigmemory::as.big.matrix(as.matrix(wtdata))
    rm(wtdata)
    gc(verbose = F)
    mdesc <- describe(x)
    te <- foreach(i = seq(from = 1,to = max_rows,by = chunk_length),.export = 'helper_te',.verbose = F, .combine = 'c') %dopar% {
      wtdata<-bigmemory::attach.big.matrix(mdesc)
      helper_te(wtdata[i:min(max_rows,i+chunk_length-1),],som_codes,som_pts,hex)
    }
    stopCluster(cl)
    rm(list=c('cl','num_threads','log_file','free','mb_per_thread'))
    time_ini
    if(verbose){
      cat(paste0('SOM ',paste0(dim,collapse = ','),' quality(qe,te) END parallel:',Sys.time(),', took '))
      Sys.time() - time_ini
      cat('\n')
    }
  }else{
    time_ini<-Sys.time()
    if(verbose)cat(paste0('SOM ',paste0(dim,collapse = ','),' quality(qe,te) INI sequential:',time_ini,'\n'))
    te<-apply(wtdata,1,FUN = helper_te,som_codes,som_pts)
    if(verbose){
      cat(paste0('SOM ',paste0(dim,collapse = ','),' quality(qe,te) END sequential:',Sys.time(),', took '))
      Sys.time() - time_ini
      cat('\n')
    }
  }
  te<-sum(te)/max_rows
  return(list(error=F,data=list(qe=qe,te=te),msg='ok'))
}

compute_bmu_helper<-function(rows,som_codes){
  if(!is.matrix(rows)&&!is.data.frame(rows)) rows<- t(as.data.frame(rows))
  wtdata_bmu<-apply(X = rows,MARGIN = 1,FUN = function(r){
    d<-NA
    tmp_bmu<-NA
    for(sc in 1:nrow(som_codes)){
      tmp<-sqrt(sum((r-som_codes[sc,])^2,na.rm = T))
      if(is.na(d)||tmp<d){
        d<-tmp
        tmp_bmu<-sc
      }
    }
    return(tmp_bmu)
  })
  return(wtdata_bmu)
}

get_optimal_som<-function(wtdata=NULL,dim=c(20,100,10),sizeRuleOfTheThumb=T,rlen=10,alpha=c(0.05,0.01),parallel_mode=T,log_file=NULL,verbose=F){
  iam=match.call()[[1]]
  if(!exists("dependencyLoader")){
    if(!file.exists('functions_common/dependencyLoader.R')) return(list(error=TRUE,data=NULL,msg=paste0("\n",iam,":Missing dependency function: functions_common/dependencyLoader.R")));
    source('functions_common/dependencyLoader.R')
  }
  # Sources
  sources<-c('reshape2')
  dep<-dependencyLoader(sources)
  if(dep$error) return(list(error=TRUE, warning=FALSE, data=NULL,msg=paste0("\n",iam,":on call dependencyLoader\n",dep$msg)))
  
  if(sizeRuleOfTheThumb) return(list(error=F,data=list(optimal_dim=round(sqrt(5*sqrt(nrow(wtdata))),0),metrics_smoothed=NULL,metrics=NULL),msg='ok'))
  
  metrics<-data.frame(dim=numeric(),te=numeric(),qe=numeric())
  for(d in dim){
    rs<-som_model(dim=d,wtdata=wtdata,alpha=alpha,rlen=rlen,hex=hex,verbose=verbose)
    type<-rs$data$type
    model<-rs$data$model
    wtdata_bmu<-rs$data$wtdata_bmu
    som_distances<-rs$data$som_distances
    som_codes=rs$data$som_codes
    som_pts=rs$data$som_pts
    rs<-get_metrics(som_codes = som_codes,som_pts=som_pts,som_distances = som_distances,data = wtdata,verbose=T)
    if(rs$error) stop(rs$msg)
    te<-rs$data$te
    qe<-rs$data$qe
    metrics<-rbind(metrics,data.frame(dim=d,te=te,qe=qe))
  }
  max_dim<-max(metrics$dim)
  min_dim<-min(metrics$dim)
  if(nrow(metrics)>2){
    metrics$qe_norm<-(metrics$qe-min(metrics$qe))/(max(metrics$qe)-min(metrics$qe))
    metrics$te_norm<-(metrics$te-min(metrics$te))/(max(metrics$te)-min(metrics$te))
    #Get best som size intersection between two scores
    qs<-data.frame(dim=seq(min(q$dim),max(q$dim),0.01))
    qs$qe_norm_s<-predict(loess(q$qe_norm~q$dim),newdata = qs$dim)
    qs$te_norm_s<-predict(loess(q$te_norm~q$dim),newdata = qs$dim)
    #qsm<-melt(qs,id='dim')
    #ggplot(qsm,aes(x=dim,y=value,group=variable,color=variable))+geom_line()
    intersection<-qs$dim[as.logical(abs(diff(qs$qe_norm_s < qs$te_norm_s)))]
    dim<-ceiling(intersection)
  }else{
    dim<-mean(c(max_dim,min_dim))
  }
  return(list(error=F,data=list(optimal_dim=dim,metrics_smoothed=qs,metrics=metrics),msg='ok'))
}

som_model<-function(dim=10,wtdata=NULL,alpha=c(0.05,0.01),rlen=10,hex=F,verbose=F){
  iam=match.call()[[1]]
  #if(hex) return(list(error=T,data=NULL,msg="TODO IMPLEMENT somcodes hexmap"))
  if(!exists("dependencyLoader")){
    if(!file.exists('functions_common/dependencyLoader.R')) return(list(error=TRUE,data=NULL,msg=paste0("\n",iam,":Missing dependency function: functions_common/dependencyLoader.R")));
    source('functions_common/dependencyLoader.R')
  }
  # Sources
  libraries<-c('kohonen')
  dep<-dependencyLoader(libraries)
  if(dep$error) return(list(error=TRUE, warning=FALSE, data=NULL,msg=paste0("\n",iam,":on call dependencyLoader\n",dep$msg)))
  
  if(dim*dim>nrow(wtdata)) return(list(error=TRUE,msg=paste0('\ncannot create map bigger than:',floor(sqrt(nrow(wtdata))))))
  if(verbose) cat(paste0('\nSOM ',dim,' start:',Sys.time()))
  set.seed(1)
  dim<-min(dim*dim,nrow(wtdata))
  dim<-floor(sqrt(dim))
  som_grid <- kohonen::somgrid(xdim = dim, ydim=dim, topo=ifelse(hex,"hexagonal","rectangular"))
  model <- kohonen::som(as.matrix(wtdata), grid=som_grid, rlen=rlen,  alpha=alpha, keep.data = T)
  rm(som_grid)
  if(verbose) cat(paste0('\nSOM ',dim,' end:',Sys.time(),'\n'))
  #Fixes for other functions
  som_pts<-model$grid$pts
  if(hex){
    r<-1
    c<-1
    c_dec<-1.75
    step_x<-0.5
    step_y<-1.5
    for(i in seq(1,dim^2,dim)){
      if(r%%1==0){#1.0 not fraction
        som_pts[i:(i+dim-1),]<-c(seq(r,r+dim-1,1),rep(c,dim))
        c<-c+step_y
        r<-1.5
      }else{
        som_pts[i:(i+dim-1),]<-c(seq(r,r+dim-1,1),rep(c_dec,dim))
        c_dec<-c_dec+step_y
        r<-1
      }
    }
  }
  som_pts<-as.data.frame(som_pts)
  return(list(error=F,data=list(type='som',dim=dim,hex=hex,model=model,wtdata_bmu=model$unit.classif,som_distances=model$distances,som_codes=model$codes[[1]],som_pts=som_pts,center_neuron_position=NULL),msg='ok'))
}

hdgsom_model<-function(wtdata=NULL,spreadFactor=0.8,hex=F,verbose=F,parallel_mode=F,log_file=NULL){
  iam=match.call()[[1]]
  if(!exists("dependencyLoader")){
    if(!file.exists('functions_common/dependencyLoader.R')) return(list(error=TRUE,data=NULL,msg=paste0("\n",iam,":Missing dependency function: functions_common/dependencyLoader.R")));
    source('functions_common/dependencyLoader.R')
  }
  # Sources
  libraries<-c('doSNOW','bigmemory','itertools','parallel','devtools')
  dep<-dependencyLoader(libraries)
  if(dep$error) return(list(error=TRUE, warning=FALSE, data=NULL,msg=paste0("\n",iam,":on call dependencyLoader\n",dep$msg)))
  #alecuba16 repo for growing som
  if(!is.element('hdgsom', installed.packages()[,1])){
    devtools::install_github('alecuba16/HDGSOM')
    library('hdgsom')
  }
  ################  create model  ###################
  set.seed(1)
  model <- hdgsom::train.hdgsom(wtdata, spreadFactor=spreadFactor,nhood=ifelse(hex,'hex','rect'))
  if(verbose) cat(paste0('HDGSOM model ended:',Sys.time()," computing BMU"))
  som_distances<-model$nodes$distance
  som_codes<-model$nodes$codes
  som_pts<-model$nodes$position
  ################  Calcular BMU  ##################
  wtdata_bmu<-NA
  mb_per_thread<-500
  if(Sys.info()[['sysname']]=="Linux"){
    free<-as.numeric(system("awk '/MemAvailable/ {print $2}' /proc/meminfo",intern=TRUE))/1024
  }else{#windows
    free<-as.numeric(gsub(x=paste0(system("wmic OS get FreePhysicalMemory /Value",intern = T),collapse = ''),pattern = '.*=([0-9]+)',replacement = '\\1'))/1024
  }
  if(is.na(free)||is.nan(free)||is.infinite(free)) free<-1024
  num_threads<-floor(free/mb_per_thread)
  if((parallel_mode&&(num_threads>1))){
    num_threads<-min(num_threads,parallel::detectCores())
    num_threads<-min(nrow(wtdata),floor(num_threads)) #Model in memory uses about 3% memory @600 @2years
    if(verbose) cat(paste0(' in parallel using ',num_threads,' threads'))
    time_ini<-Sys.time()
    #Divide work
    chunk_length=floor(nrow(wtdata)/num_threads)
    cl<- if(!is.null(log_file)) parallel::makePSOCKcluster(num_threads,outfile=log_file) else parallel::makePSOCKcluster(num_threads)
    
    doSNOW::registerDoSNOW(cl)
    max_rows<-nrow(wtdata)
    x<-bigmemory::as.big.matrix(as.matrix(wtdata))
    rm(wtdata)
    gc(verbose = F)
    mdesc <- describe(x)
    wtdata_bmu <- foreach(i = seq(from = 1,to = max_rows,by = chunk_length),.export = c('compute_bmu_helper'),.verbose = F, .combine = 'c') %dopar% {
      wtdata<-bigmemory::attach.big.matrix(mdesc)
      compute_bmu_helper(wtdata[i:min(max_rows,i+chunk_length-1),],som_codes)
    }
    stopCluster(cl)
    rm(list=c('cl','num_threads','log_file','free','mb_per_thread'))
  }else{
    if(verbose)cat(paste0(' in sequential'))
    wtdata_bmu<-apply(wtdata,1,FUN = compute_bmu_helper,som_codes)
  }
  
  #add offset x,y map
  center_neuron_position<-which((som_pts$x==0)&(som_pts$y==0))
  som_pts$x<-som_pts$x+abs(min(som_pts$x,na.rm=T))+1
  som_pts$y<-som_pts$y+abs(min(som_pts$y,na.rm=T))+1
  if(verbose) cat(paste0('\nHDGSOM compute BMU end, final model end:',Sys.time()))
  return(list(error=F,data=list(type='HDGSOM',dim=max(som_pts,na.rm = T),hex=hex,model=model,wtdata_bmu=wtdata_bmu,som_distances=som_distances,som_codes=som_codes,som_pts=som_pts,center_neuron_position=center_neuron_position),msg='ok'))
}

gsom_model<-function(wtdata=NULL,spreadFactor=0.8,hex=F,verbose=F,parallel_mode=F,log_file=NULL){
  iam=match.call()[[1]]
  if(!exists("dependencyLoader")){
    if(!file.exists('functions_common/dependencyLoader.R')) return(list(error=TRUE,data=NULL,msg=paste0("\n",iam,":Missing dependency function: functions_common/dependencyLoader.R")));
    source('functions_common/dependencyLoader.R')
  }
  # Sources
  libraries<-c('doSNOW','bigmemory','itertools','parallel','devtools')
  dep<-dependencyLoader(libraries)
  if(dep$error) return(list(error=TRUE, warning=FALSE, data=NULL,msg=paste0("\n",iam,":on call dependencyLoader\n",dep$msg)))
  #alecuba16 repo for growing som
  if(!is.element('GrowingSOM', installed.packages()[,1])){
    devtools::install_github('alecuba16/GrowingSOM')
    library('GrowingSOM')
  }
  ################  create model  ###################
  set.seed(1)
  model <- GrowingSOM::train.gsom(wtdata, spreadFactor=spreadFactor,nhood=ifelse(hex,'hex','rect'))
  if(verbose) cat(paste0('GSOM model ended:',Sys.time()," computing BMU"))
  som_distances<-model$nodes$distance
  som_codes<-model$nodes$codes
  som_pts<-model$nodes$position
  ################  Calcular BMU  ##################
  wtdata_bmu<-NA
  mb_per_thread<-500
  if(Sys.info()[['sysname']]=="Linux"){
    free<-as.numeric(system("awk '/MemAvailable/ {print $2}' /proc/meminfo",intern=TRUE))/1024
  }else{#windows
    free<-as.numeric(gsub(x=paste0(system("wmic OS get FreePhysicalMemory /Value",intern = T),collapse = ''),pattern = '.*=([0-9]+)',replacement = '\\1'))/1024
  }
  if(is.na(free)||is.nan(free)||is.infinite(free)) free<-1024
  num_threads<-floor(free/mb_per_thread)
  if((parallel_mode&&(num_threads>1))){
    num_threads<-min(num_threads,parallel::detectCores())
    num_threads<-min(nrow(wtdata),floor(num_threads)) #Model in memory uses about 3% memory @600 @2years
    if(verbose) cat(paste0(' in parallel using ',num_threads,' threads'))
    time_ini<-Sys.time()
    #Divide work
    chunk_length=floor(nrow(wtdata)/num_threads)
    cl<- if(!is.null(log_file)) parallel::makePSOCKcluster(num_threads,outfile=log_file) else parallel::makePSOCKcluster(num_threads)
    
    doSNOW::registerDoSNOW(cl)
    max_rows<-nrow(wtdata)
    x<-bigmemory::as.big.matrix(as.matrix(wtdata))
    rm(wtdata)
    gc(verbose = F)
    mdesc <- describe(x)
    wtdata_bmu <- foreach(i = seq(from = 1,to = max_rows,by = chunk_length),.export = c('compute_bmu_helper'),.verbose = F, .combine = 'c') %dopar% {
      wtdata<-bigmemory::attach.big.matrix(mdesc)
      compute_bmu_helper(wtdata[i:min(max_rows,i+chunk_length-1),],som_codes)
    }
    stopCluster(cl)
    rm(list=c('cl','num_threads','log_file','free','mb_per_thread'))
  }else{
    if(verbose)cat(paste0(' in sequential'))
    wtdata_bmu<-apply(wtdata,1,FUN = compute_bmu_helper,som_codes)
  }
  
  #add offset x,y map
  center_neuron_position<-which((som_pts$x==0)&(som_pts$y==0))
  som_pts$x<-som_pts$x+abs(min(som_pts$x,na.rm=T))+1
  som_pts$y<-som_pts$y+abs(min(som_pts$y,na.rm=T))+1
  if(verbose) cat(paste0('\nGSOM compute BMU end, final model end:',Sys.time()))
  return(list(error=F,data=list(type='GSOM',dim=max(som_pts,na.rm = T),hex=hex,model=model,wtdata_bmu=wtdata_bmu,som_distances=som_distances,som_codes=som_codes,som_pts=som_pts,center_neuron_position=center_neuron_position),msg='ok'))
}

create_som_model<-function(wtdata=NULL,model_params=NULL,parallel_mode=F,log_file=NULL,verbose=F,normalize=T){
  iam=match.call()[[1]]
  if(!exists("dependencyLoader")){
    if(!file.exists('functions_common/dependencyLoader.R')) return(list(error=TRUE,data=NULL,msg=paste0("\n",iam,":Missing dependency function: functions_common/dependencyLoader.R")));
    source('functions_common/dependencyLoader.R')
  }
  # Sources
  #dep<-dependencyLoader(libraries)
  #if(dep$error) return(list(error=TRUE, warning=FALSE, data=NULL,msg=paste0("\n",iam,":on call dependencyLoader\n",dep$msg)))
  
  if(is.null(wtdata)) return(list(error=TRUE, warning=FALSE, data=NULL,msg="Wtdata cannot be null"))
  if(!is.null(log_file)&&file.exists(log_file)) file.remove(log_file)
  if(is.null(model_params)||!(exists('model_params',inherits = F)))return(list(error=T,data=NULL,msg="Define model_params: list(type='gsom',spreadFactor=0.8) or list(type='hdgsom',spreadFactor=0.8) or list(type='som',dim=10,rlen=10,alpha<-c(0.05,0.01),sizeRuleOfTheThumb=T)"))
  
  if(normalize){
    #Scale
    sdv<-apply(wtdata,2,sd,na.rm=T)
    zero_sdv_columns<-(sdv==0)
    wtdata<-wtdata[,!zero_sdv_columns]
    sdv<-sdv[!zero_sdv_columns]
    mean<-sapply(1:ncol(wtdata), function(c) mean(wtdata[,c],na.rm=T))
    selected_columns<-colnames(wtdata)
    wtdata<-sapply(1:ncol(wtdata), function(c) {(wtdata[,c] - mean[c])/sdv[c]}) 
    stat<-data.frame(name=selected_columns,mean=mean,sdv=sdv)
  }else{
    #Selected columns
    selected_columns<-colnames(wtdata)
  }
  
  #complete cases only
  selected_rows<-complete.cases(wtdata)
  wtdata<-wtdata[selected_rows,]
  #To matrix
  wtdata<-as.matrix(wtdata)
  invisible(gc(verbose = F))
  
  ################  create model  ###################
  if(verbose) cat(paste0('\n',toupper(model_params$type),' model type ',ifelse(model_params$hex,'hex','rect'),' start at:',Sys.time()))
  if(toupper(model_params$type)=='GSOM'){
    rs<-gsom_model(wtdata=wtdata,spreadFactor=model_params$spreadFactor,hex=model_params$hex,verbose=verbose,parallel_mode=parallel_mode,log_file=log_file)
  } else if(toupper(model_params$type)=='HDGSOM'){
    rs<-hdgsom_model(wtdata=wtdata,spreadFactor=model_params$spreadFactor,hex=model_params$hex,verbose=verbose,parallel_mode=parallel_mode,log_file=log_file)
  } else if(toupper(model_params$type)=='SOM'){
    if(length(model_params$dim)>1||model_params$sizeRuleOfTheThumb){#Search for optimal
      rs<-get_optimal_som(wtdata=wtdata,dim=model_params$dim,sizeRuleOfTheThumb=model_params$sizeRuleOfTheThumb,rlen=model_params$rlen,alpha=model_params$alpha,parallel_mode=parallel_mode,log_file=log_file,verbose=verbose)
      if(rs$error) return(list(error=T,data=NULL,msg=rs$msg))
      model_params$dim<-rs$data$optimal_dim
    }
    rs<-som_model(dim=model_params$dim,wtdata=wtdata,alpha=model_params$alpha,rlen=model_params$rlen,hex=model_params$hex,verbose=verbose)
  }
  if(rs$error) return(list(error=T,data=NULL,msg=rs$msg))
  if(verbose) cat(paste0(toupper(model_params$type),' model ended:',Sys.time()))
  #Save model vars
  outstat<-stat
  gc(verbose = F)
  return(list(error=FALSE,data=list(model_params=model_params,dim=rs$data$dim,hex=rs$data$hex,model=rs$data$model,wtdata_bmu=rs$data$wtdata_bmu,som_distances=rs$data$som_distances,som_codes=rs$data$som_codes,som_pts=rs$data$som_pts,center_neuron_position=rs$data$center_neuron_position,get_metrics=calculate_metrics,selected_rows=selected_rows,selected_columns=selected_columns,stat=stat),msg='ok'))
}
