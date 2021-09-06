fs_equal_freq <- function(x,nbins){
  nx <- length(x)
  nrepl <- floor(nx/nbins)
  nplus <- sample(1:nbins,nx - nrepl*nbins)
  nrep <- rep(nrepl,nbins)
  nrep[nplus] <- nrepl+1
  x[order(x)] <- rep(seq.int(nbins),nrep)
  x
}

fs_discretize_freq<-function(data=NULL,method='freedman_diaconis',nbins=NULL){
  #freedman_diaconis https://en.wikipedia.org/wiki/Freedman–Diaconis_rule
  for(col in 1:ncol(data)){
    if(is.null(nbins)){
      nbins<-switch(method,
                    'freedman_diaconis'=nclass.FD(x = data[,col]),
                    'Sturges'=nclass.Sturges(x = data[,col]),
                    'scott'=nclass.scott(x = data[,col]),
                    'nrow/3'=nrow(data)/3)
    }
    nbins<-ceiling(nbins)
    y <- fs_equal_freq(data[,col],nbins)
    ubins<-unique(y)
    bins_median<-sapply(1:max(ubins),function(bin) bin_median<-median(data[y==bin,col]))
    data[,col]<-sapply(1:nrow(data),function(row) bins_median[y[row]])
  }
  return(data)
}

caretrfe_helper<-function(wtdata=wtdata,target=target,params=NULL,parallel=F,logfile=NULL){
  method<-'cv'
  k<-ncol(wtdata)
  q<-NA
  parallel<-F# todo fix out of memory
  if(!is.null(params)&&!is.na(params)&&('method' %in% names(params))&&!is.null(params$method)&&!is.na(params$method)) method<-params$method
  
  if(!is.null(params)&&!is.na(params)&&('q' %in% names(params))&&!is.null(params$q)&&!is.na(params$q)){ # Cut by quantile
    k<-ncol(wtdata)
    q<-params$q
  }else if(!is.null(params)&&!is.na(params)&&('k' %in% names(params))&&!is.null(params$k)&&!is.na(params$k)){
    k<-params$k
  }
  
  if(parallel){
    #Dependencias
    if(!exists("dependencyLoader")){
      if(!file.exists('functions_common/dependencyLoader.R')) return(list(error=TRUE,data=NULL,msg=paste0("\n",iam,":Missing dependency function: functions_common/dependencyLoader.R")));
      source('functions_common/dependencyLoader.R')
    }
    
    dep<-dependencyLoader(c('parallel','doSNOW','bigmemory'))
    if(dep$error) return(list(error=TRUE, warning=FALSE, data=NULL,msg=paste0("\n",iam,":on call dependencyLoader\n",dep$msg)))
    
    #mb_per_thread<-1500 #Worst case 1500MB
    #mb_per_thread<-((nrow(wtdata)*2500)/(78000)) #78000 rows x 120 column df uses about 2500MB
    mb_per_thread<-((nrow(wtdata)*7500)/(294000)) #294000 rows x 120 column df uses about 5700MB
    
    if(Sys.info()[['sysname']]=="Linux"){
      free<-as.numeric(system("awk '/MemFree/ {print $2}' /proc/meminfo",intern=TRUE))
    }else{#windows
      free<-as.numeric(gsub(x=paste0(system("wmic OS get FreePhysicalMemory /Value",intern = T),collapse = ''),pattern = '.*=([0-9]+)',replacement = '\\1'))
    }
    if(is.na(free)||is.nan(free)||is.infinite(free)) free<-1048576
    num_threads<-floor((free/1024)/mb_per_thread)
    num_threads<-min(num_threads,ncol(wtdata))
    num_threads<-min(num_threads,parallel::detectCores())
    num_threads<-min(ncol(wtdata),floor(num_threads))
    num_threads<-max(num_threads,1) #If numthreads is 0 force to be 1.
    if(is.null(logfile)||is.na(logfile)){
      cl <- parallel::makePSOCKcluster(num_threads)
    }else{
      cl <- parallel::makePSOCKcluster(num_threads,outfile=logfile)
    }
    doSNOW::registerDoSNOW(cl)
  }
  
  #control <- caret::rfeControl(functions=caret::rfFuncs, method=method, repeats=3)
  control <- caret::rfeControl(functions=caret::rfFuncs, method=method, number=5,repeats=1,allowParallel = parallel)
  # run the RFE algorithm
  results <- caret::rfe(x=wtdata,y=target, sizes=k, rfeControl=control, na.rm=TRUE)
  
  if(parallel) stopCluster(cl)
  # summarize the results
  #print(results)
  # list the chosen features
  #predictors(results)
  #save(results,file='featureSelectionCaret.RData')
  results<-results$variables[,c('var','Overall')] %>% group_by(var) %>% summarise_all(max)
  results<-results[with(results, order(-Overall)), ]
  df<-data.frame(name=results$var,score=results$Overall,selected=rep(F,nrow(results)))
  
  if(!is.na(q)){
    v<-quantile( df$score,probs=(1-q),na.rm=TRUE)
    df$selected[df$score>=v]<-T
  }else{
    df$selected[1:max(k,nrow(df))]<-T
  }
  return(df)
}

praznik_helper<-function(wtdata=wtdata,target=target,params=NULL,func=praznik::CMIM,logfile=NULL,random_iterations=1000){
  k<-ncol(wtdata)
  q<-NA
  
  if(!is.null(params)&&!is.na(params)&&('q' %in% names(params))&&!is.null(params$q)&&!is.na(params$q)){ # Cut by quantile
    k<-ncol(wtdata)
    q<-params$q
  }else if(!is.null(params)&&!is.na(params)&&('k' %in% names(params))&&!is.null(params$k)&&!is.na(params$k)){
    k<-params$k
  }
  
  wtdata[]<-lapply(1:ncol(wtdata),function(col){
    if(!is.factor(wtdata[,col]))
      return(as.factor(wtdata[,col]))
    else
      return(wtdata[,col])
  }
  )
  
  vars<-func(wtdata,target,k=ncol(wtdata)) #Compute all cases
  vars<-as.data.frame(vars)
  
  #Removes duplicaded columns
  while(any(duplicated(vars$selection))){
    dup<-which(duplicated(vars$selection))[1]
    poss<-which(vars$selection==vars$selection[dup])
    best<-poss[order(-vars$scores[poss])][1]
    vars<-vars[-poss[poss!=best],]
  }
  vars$score<-praznik::impScores(wtdata[,vars$selection,drop=F],target)
  
  #TEST
  i<-1
  stop<-F
  selected<-vars$selection
  if(length(selected)>1){
    while(!stop&&i<length(selected)){
      X=wtdata[,selected]
      #Y=wtdata[,selected[i],drop=F]
      Y=target
      #Z=target
      Z=wtdata[,selected[i]]
      base_score<-cmiScores(X=X,Y=Y,Z=Z)
      base_score<-base_score[length(base_score)]
      random_scores<-NULL
      for(j in 1:random_iterations){
        curr_score<-cmiScores(X = X,Y=Y,Z=Z[sample(length(Z))])
        random_scores<-c(random_scores,curr_score[length(curr_score)])
      }
      if(base_score<=median(random_scores,na.rm = T)){
        stop<-T
      }else{
        i<-i+1
      }
    }
    selected<-selected[1:max(i,1)]#Select atleast 1
  }
  
  #df<-data.frame(name=colnames(wtdata)[vars$selection],score=vars$score,selected=rep(F,nrow(vars)))
  #Temporary fix
  df<-data.frame(name=colnames(wtdata)[vars$selection],score=(ncol(wtdata)-vars$selection+1),selected=rep(F,nrow(vars)))
  df<-df[order(-df$score),] #Most to less important
  
  # if(!is.na(q)){
  #   v<-quantile( df$score,probs=(1-q),na.rm=TRUE)
  #   df$selected[df$score>=v]<-T
  # }else{
  #   df$selected[1:min(k,dim(df)[1])]<-T
  # }
  
  df$selected[vars$selection %in% selected]<-T
  
  return(df)
}

pvalue_helper<-function(wtdata=wtdata,target=target,params=NULL,parallel=F,logfile=NULL){
  method<-'theoretical'
  nsim<-1000
  pvalue<-0.05
  
  if(!is.null(params)&&!is.na(params)&&('method' %in% names(params))&&!is.null(params$method)&&!is.na(params$method)) method<-params$method
  if(!is.null(params)&&!is.na(params)&&('nsim' %in% names(params))&&!is.null(params$nsim)&&!is.na(params$nsim)) nsim<-params$nsim
  if(!is.null(params)&&!is.na(params)&&('pvalue' %in% names(params))&&!is.null(params$pvalue)&&!is.na(params$pvalue)) pvalue<-params$pvalue
  
  colnames<-colnames(wtdata)
  p <- calcule_pvalues(wtdata=wtdata, target=target,method = method, est = "mean", disp = FALSE, file_jpeg = '',nsim = nsim,pvalue=pvalue,parallel=parallel,logfile=logfile)
  if(p$error) return(list(error=TRUE,data=NULL,msg=paste0(iam,": on call calcule_pvalues",p$msg)))
  pvalues<-p$data
  df<-data.frame(name=colnames,score=pvalues,selected=pvalues<pvalue)
  df<-df[order(df$score),] #Most to less important
  return(df)
}

boruta_helper<-function(wtdata=wtdata,target=target,params=NULL,parallel=F,logfile=NULL){
  maxRuns<-40
  field<-'medianImp'
  k<-ncol(wtdata)
  q<-NA
  
  if(!is.null(params)&&!is.na(params)&&('maxRuns' %in% names(params))&&!is.null(params$maxRuns)&&!is.na(params$maxRuns)) maxRuns<-params$maxRuns
  if(!is.null(params)&&!is.na(params)&&('field' %in% names(params))&&!is.null(params$field)&&!is.na(params$field)&&(params$field %in% c("meanImp","medianImp","minImp","maxImp"))) field<-params$field
  
  if(!is.null(params)&&!is.na(params)&&('q' %in% names(params))&&!is.null(params$q)&&!is.na(params$q)){ # Cut by quantile
    k<-ncol(wtdata)
    q<-params$q
  }else if(!is.null(params)&&!is.na(params)&&('k' %in% names(params))&&!is.null(params$k)&&!is.na(params$k)){
    k<-params$k
  }
  
  varfs <- Boruta::Boruta(wtdata,target, doTrace = 2,maxRuns=maxRuns)
  varfinalfs <- Boruta::TentativeRoughFix(varfs)
  Boruta::getSelectedAttributes(varfinalfs, withTentative = F)
  varfinalfs_dataframe <- Boruta::attStats(varfinalfs)
  varfinalfs_dataframe<-varfinalfs_dataframe[order(-varfinalfs_dataframe[,field]),]
  df<-data.frame(name=rownames(varfinalfs_dataframe),score=varfinalfs_dataframe[,field],selected=(varfinalfs_dataframe$decision=="Confirmed"))
  
  if(!is.na(q)){
    v<-quantile( df$score[df$selected],probs=(1-q),na.rm=TRUE)
    selected<-(df$score>=v)&df$selected
  }else{
    selected_boruta<-which(df$selected)
    selected<-selected_boruta[min(length(selected_boruta),k)]
  }
  df$selected<-F
  df$selected[selected]<-T
  return(df)
}

encode_characters<-function(X=X){
  is_numeric<-sapply(1:ncol(X),function(c) is.numeric(X[,c]))
  #if(any(!is_numeric)) return(list(error=T,data=NULL,msg=paste0('\n',iam,' this columns are not numeric and must be excluded:',paste0(colnames(X)[!is_numeric],collapse = ','))))
  if(any(!is_numeric)){
    for(col in which(!is_numeric)){
      if(!is.factor(X[,col])) X[,col]<-as.factor(X[,col])
      #X[,col]<-sapply(1:nrow(X),function(r) which(levels(X[,col])==as.character(X[r,col])))
      X[,col]<-as.numeric(X[,col])
    }
  }
  return(list(error=F,data=list(X=X),msg='ok'))
}


#params
#for pvalue:            list(method='simulation/theorical',nsim=1000,pvalue=0.05)
#for praznik(cmim,etc): list(k=3,q=0.75) #k fixes the number of selected features to max of 3, q specifies the quartile range from 0-75% in this case of the scores(0%->best score, 100%->worst score)
#for boruta:            list(k=3,q=0.75,maxRuns=20,field='meanImp') #k and q the same as praznik, maxRuns specifies the max number of iterations, field is the importance field to be used , possible: meanImp,medianImp,minImp,maxImp
feature_selection <- function(wtdata=NULL,exclude_columns='date_time',target_name='alarm',algorithm='caret',date_time_name='date_time',cor_threshold=0.95,na_columns_per_exclude=10,complete_cases=T,params=NULL,discretize=F,parallel=F,standarize=T,normalize=F,logfile=NULL){
  iam=match.call()[[1]]
  before_vars<-ls()
  #Dependencias
  if(!exists("dependencyLoader")){
    if(!file.exists('functions_common/dependencyLoader.R')) return(list(error=TRUE,data=NULL,msg=paste0("\n",iam,":Missing dependency function: functions_common/dependencyLoader.R")));
    source('functions_common/dependencyLoader.R')
  }
  
  warning<-F
  msg<-'ok'
  
  sourcesandlib<-c('praznik','Boruta','caret','dplyr','functions_common/calcule_pvalues.R','functions_common/myInference.R')
  
  dep<-dependencyLoader(sourcesandlib)
  if(dep$error) return(list(error=TRUE, warning=FALSE, data=NULL,msg=paste0("\n",iam,":on call dependencyLoader\n",dep$msg)))
  
  if(any(is.null(target_name))||any(is.na(target_name))) return(list(error=TRUE, warning=FALSE, data=NULL,msg=paste0("\n",iam,": target_name cannot be null or na\n",dep$msg)))
  if(!(target_name %in% names(wtdata))) return(list(error=TRUE, warning=FALSE, data=NULL,msg=paste0("\n",iam,": target_name (",target_name,") doesn't exists on wtdata\n",dep$msg)))
  
  #Separe target from data
  target<-wtdata[,target_name]
  X<-wtdata[,names(wtdata)!=target_name]
  
  #Check if target is unique
  if(length(na.omit(unique(target)))<2) return(list(error=T,warning=F,data=NULL,msg=paste0("\n",iam," target variable is constant")))
  
  #Prepare target as factor '1'(true) '0'(false)
  if(is.logical(target)) target<-as.factor(target)
  if(is.numeric(target)) target<-as.factor(target)
  if(is.factor(target)){
    if(all(c('TRUE','FALSE') %in% levels(target))||all(c('t','f') %in% levels(target))){#convert to 1/0
      levels(target)[levels(target) %in% c('TRUE','t')]<-'1'
      levels(target)[levels(target) %in% c('FALSE','f')]<-'0'
    }
  }else{
    return(list(error=T,warning=F,data=NULL,msg=paste0('\n',iam,' target variable is not a numeric,logical,factor or doesn\'t contains the accepted values: "t/f", "TRUE/FALSE", "0/1"')))
  }
  level0<-which(levels(target)==0)
  level1<-which(levels(target)==1)
  target<-factor(target,levels(target)[c(level1,level0)])
  
  #Exclude columns
  if(all(!is.null(exclude_columns)) && all(!is.na(exclude_columns))){
    if(length(exclude_columns)==1) exclude_columns<-unlist(strsplit(exclude_columns,','))
    X<-X[,!(colnames(X) %in% exclude_columns)]
  }
  
  #Remove columns if all are NA or number of na's>na_columns_per_exclude
  columns_na<-((colSums(is.na(X))>=nrow(X))|(colSums(is.na(X))>(nrow(X)*na_columns_per_exclude/100)))
  if(any(columns_na)){
    columns_na_names<-colnames(X)[columns_na]
    X<-X[,!columns_na]
  }
  rm(columns_na)
  #Remove rows if there are some NA
  #rowsTosave<-which(complete.cases(X))
  #X<-X[rowsTosave,]
  #target<-target[rowsTosave]
  
  
  #Final check if all are numeric
  rs<-encode_characters(X=X)
  if(rs$error) return(list(error=rs$error,warning=F,data=NULL,msg=rs$msg))
  X<-rs$data$X
  
  #Remove variable columns with SD=0
  sdv<-apply(X,2,function(x) sd(x = x,na.rm = T))
  if(any(sdv==0)) zero_sdv_names<-names(X)[sdv==0]
  X<-X[,sdv!=0]
  rm(sdv)
  
  if(!(algorithm %in% c('pvalue','boruta','caretrfe'))&&discretize) X<-fs_discretize_freq(data=X,method='freedman_diaconis')   #Without descretize it will run out of memory....
  
  if(standarize){
      #standarize
      colnames<-colnames(X)#Backup colnames
      X_mu<-apply(X,2,function(x) mean( x= x,na.rm = T))
      X_sigma<-apply(X,2,function(x) sd( x= x,na.rm = T))
      X<-sapply(1:ncol(X),function(c) (X[,c]-X_mu[c])/X_sigma[c])
      colnames(X)<-colnames
      X<-as.data.frame(X)
      rm(list = c('X_mu','X_sigma','colnames'))
  }else if(normalize){
    colnames<-colnames(X)#Backup colnames
    X<-sapply(1:ncol(X),function(c) ((X[,c]-min(X[,c]))/(max(X[,c])-min(X[,c]))))
    colnames(X)<-colnames
    X<-as.data.frame(X)
  }
  #Complete casses
  if(algorithm!='pvalue'||(!is.null(complete_cases)&&any(!is.na(complete_cases))&&is.logical(complete_cases)&&complete_cases)){
    if((is.null(complete_cases)||is.na(complete_cases)||!complete_cases)&&algorithm!='pvalue'){
      warning<-T
      msg<-paste0("\n",iam," You are using the ",algorithm," algorithm which requires complete_cases and you called FS with complete_cases=FALSE, Enabling complete_cases=T")
      warning(msg)
    }
    rowsTosave<-which(complete.cases(X))
    X<-X[rowsTosave,]
    target<-target[rowsTosave]
    
    #Check again if target variable is unique and remove columns with sdv=0
    if(length(na.omit(unique(target)))<2) return(list(error=T,warning=F,data=NULL,msg=paste0("\n",iam," target variable is constant after complete cases")))
    sdv<-apply(X,2,function(x) sd(x = x,na.rm = T))
    if(any(sdv==0)){
      if(!exists('zero_sdv_names',inherits = F)){
        zero_sdv_names<-names(X)[sdv==0]
      }else{
        zero_sdv_names<-c(zero_sdv_names,names(X)[sdv==0])
      }
    }
    X<-X[,sdv!=0]
    rm(list=c('rowsTosave','sdv'))
  }
  
  if(length(na.omit(unique(target)))<2) return(list(error=T,warning=F,data=NULL,msg=paste0("\n",iam," target variable is constant")))
  
  gc()
  set.seed(1)
  before_vars2<-ls()
  scores_variables<-switch(algorithm,
                           'caretrfe'=caretrfe_helper(wtdata=X,target=target,params=params,parallel=parallel),
                           'cmim'=praznik_helper(wtdata=X,target=target,fun=praznik::CMIM,params=params,logfile=logfile),
                           'disr'=praznik_helper(wtdata=X,target=target,fun=praznik::DISR,params=params,logfile=logfile),
                           'jmi'=praznik_helper(wtdata=X,target=target,fun=praznik::JMI,params=params,logfile=logfile),
                           'jmim'=praznik_helper(wtdata=X,target=target,fun=praznik::JMIM,params=params,logfile=logfile),
                           'mim'=praznik_helper(wtdata=X,target=target,fun=praznik::MIM,params=params,logfile=logfile),
                           'mrmr'=praznik_helper(wtdata=X,target=target,fun=praznik::MRMR,params=params,logfile=logfile),
                           'njmim'=praznik_helper(wtdata=X,target=target,fun=praznik::NJMIM,params=params,logfile=logfile),
                           'pvalue'=pvalue_helper(wtdata=X,target=target,params=params,parallel=parallel,logfile=logfile),
                           'boruta'=boruta_helper(wtdata=X,target=target,params=params,parallel=parallel,logfile=logfile))
  
  if(nrow(scores_variables)==0) return(list(error=T,data=NULL,msg=paste0("\n",iam," error at selected feature the algorithm returned nothing")))
  rm(list=ls()[!(ls() %in% c(before_vars2,'scores_variables'))])
  gc(verbose = F)
  
  #Set default reason to NA and to score if not selected
  scores_variables$reason<-NA
  scores_variables$reason[scores_variables$selected==F]<-'score'
  selected_vars<-as.character(scores_variables$name[scores_variables$selected])
  if(sum(scores_variables$selected,na.rm = T)>1){
    #Check correlations
    wtdata_tmp<-wtdata[,selected_vars]
    rs<-encode_characters(X=wtdata_tmp)
    if(rs$error) return(list(error=rs$error,data=NULL,msg=rs$msg))
    wtdata_tmp<-rs$data$X
    
    #Remove highly correlated columns cor_threshold
    id_selected<-1
    correlated_with<-data.frame(name=rep(NA,length(selected_vars)),with=rep(NA,length(selected_vars)))
    for(i in 2:length(selected_vars)){
      current_var<-wtdata_tmp[,selected_vars[i],drop=F]
      selected<-wtdata_tmp[,selected_vars[id_selected],drop=F]
      correlation_matrix<-abs(cor(selected,current_var, use = "pairwise.complete.obs"))
      if(all(correlation_matrix <= cor_threshold, na.rm = T)) {
        id_selected<-c(id_selected,i)
      }else{
        correlated_with[i,]<-data.frame(name=selected_vars[i],with=rownames(correlation_matrix)[order(correlation_matrix,na.last = T,decreasing = T)[1]],stringsAsFactors = F)
      }
    }
    selected_vars<-selected_vars[id_selected]
    if(any(!is.na(correlated_with))){
      # There are correlated variables, set high correlated to no selected. and remove from selected variables
      to_change<-scores_variables$name %in% correlated_with$name
      scores_variables$selected[to_change]<-F
      scores_variables$reason[to_change]<-paste0('high_correlated:',correlated_with$with[!is.na(correlated_with$with)])
    }
    
  }
  #Generate non selected dataframe from variables that where excluded or nor considered
  not_considered<-!(names(wtdata) %in% scores_variables$name)
  
  if(any(not_considered)){
    not_considered_df<-data.frame(name=names(wtdata)[not_considered],score=NA,selected=F,reason='not_considered',stringsAsFactors = F)
    if(exists('exclude_columns',inherits = F)&&!is.null(exclude_columns)&&!is.na(exclude_columns)) not_considered_df$reason[not_considered_df$name %in% exclude_columns]<-'excluded'
    if(exists('columns_na_names',inherits = F)&&!is.null(columns_na_names)&&!is.na(columns_na_names)) not_considered_df$reason[not_considered_df$name %in% columns_na_names]<-'many_na'
    if(exists('zero_sdv_names',inherits = F)&&!is.null(zero_sdv_names)&&!is.na(zero_sdv_names)) not_considered_df$reason[not_considered_df$name %in% zero_sdv_names]<-'sdv==0'
  }
  
  if(any(not_considered)) scores_variables<-rbind(scores_variables,not_considered_df)
  #Date_time and target are excluded
  scores_variables$reason[scores_variables$name==date_time_name]<-'excluded'
  scores_variables$reason[scores_variables$name==target_name]<-'excluded'
  #Generate the result dataframe
  wtdata<-wtdata[,names(wtdata) %in% c(date_time_name,selected_vars,target_name)]
  rm(list=ls()[!(ls() %in% c(before_vars,'wtdata','selected_vars','scores_variables','msg','warning'))])
  gc(verbose = F)
  return(list(error=F,warning=warning,data=list(wtdata=wtdata,selected=selected_vars,scores_variables=scores_variables),msg=msg))
}

