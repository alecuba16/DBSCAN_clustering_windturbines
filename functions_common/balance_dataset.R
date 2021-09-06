fs_equal_freq <- function(x,nbins){
  nbins<-ceiling(nbins)
  nx <- length(x)
  nrepl <- floor(nx/nbins)
  nplus <- sample(1:nbins,nx - nrepl*nbins)
  nrep <- rep(nrepl,nbins)
  nrep[nplus] <- nrepl+1
  x[order(x)] <- rep(seq.int(nbins),nrep)
  x
}

fs_discretize_freq<-function(data=NULL,method='freedman_diaconis'){
  #freedman_diaconis https://en.wikipedia.org/wiki/Freedman–Diaconis_rule
  for(col in 1:ncol(data)){
    bins<-switch(method,
                 'freedman_diaconis'=(diff(range(data[,col]))/(2*IQR(data[,col])/length(data[,col])^(1/3))),
                 'nrow/3'=nrow(data)/3)
    y <- fs_equal_freq(data[,col],bins)
    ubins<-unique(y)
    bins_median<-sapply(1:max(ubins),function(bin) bin_median<-median(data[y==bin,col]))
    data[,col]<-sapply(1:nrow(data),function(row) bins_median[y[row]])
  }
  return(data)
}

select_good_with_bad_variables<-function(wtdata=NULL,find_by='knn',balance=30,selected_variables=NULL,target_name='ot',find_by='knn',discretize=F,standarize=T,n_epochs=500,n_batchsize=100,learningrate=0.8){
  if(!exists("dependencyLoader")){
    if(!file.exists('functions_common/dependencyLoader.R')) return(list(error=TRUE,data=NULL,msg=paste0("\n",iam,":Missing dependency function: functions_common/dependencyLoader.R")));
    source('functions_common/dependencyLoader.R')
  }
  
  sourcesandlib<-c('dbscan','deepnet','kohonen')
  dep<-dependencyLoader(sourcesandlib)
  if(dep$error) return(list(error=TRUE, warning=FALSE, data=NULL,msg=paste0("\n",iam,":on call dependencyLoader\n",dep$msg)))
  set.seed(1)
  selected_good<-NULL
  #selected_variables<-grep(pattern = 'TempAmb_.*|VelViento_.*',x = colnames(wtdata),value = T)
  #Select vars
  scaled_wtdata<-wtdata[,c(selected_variables,target_name)]
  #Complete cases
  complete_cases<-which(complete.cases(scaled_wtdata))
  scaled_wtdata<-scaled_wtdata[complete_cases,]
  #Discretize
  if(discretize) scaled_wtdata[,selected_variables]<-fs_discretize_freq(scaled_wtdata[,selected_variables])
  #Standarize
  if(standarize) scaled_wtdata[,selected_variables]<-scale(scaled_wtdata[,selected_variables],center = T,scale = T)
  # selected_variables<-c('VelViento_avg','VelViento_max','VelViento_min','VelViento_sdv','TempAmb_avg','TempAmb_min','TempAmb_max','TempAmb_sdv')
  good_cases<-which(scaled_wtdata[,target_name]==0)
  bad_cases<-which(scaled_wtdata[,target_name]==1)
  n_good_cases_to_select <- round(((length(bad_cases)*(100-balance))/balance)-length(bad_cases),digits = 0)
  
  if(find_by=='knn'){
    d<-dist(scaled_wtdata[,selected_variables])
    d<-as.matrix(d)
    d<-d[bad_cases,good_cases]
    d<-lapply(1:nrow(d),function(rid){
      l<-lapply(1:length(d[rid,]),function(cid) c(rownames(d)[rid],colnames(d)[cid],d[rid,cid]))
      l<-do.call("rbind",l)
      return(l)
    })
    d<-do.call("rbind",d)
    d<-as.data.frame(d)
    colnames(d)<-c('badid','goodid','distance')
    d$badid<-as.numeric(d$badid)
    d$goodid<-as.numeric(d$goodid)
    d<-d[order(d$distance),]
    good_like_bad<-d$goodid[1:n_good_cases_to_select]
    good_like_bad<-complete_cases[good_like_bad]
  }
  
  if(find_by=='rbm'){
    if(verbose){cat('training RBM with original bad points...')}
    model_rbm <- rbm.train(as.matrix(scaled_wtdata[bad_cases,selected_variables]), hidden = ncol(scaled_wtdata),batchsize = n_batchsize,numepochs = n_epochs,learningrate = learningrate)
    x_new <- rbm.up(model_rbm, as.matrix(scaled_wtdata[good_cases,selected_variables]))
    x_new <- rbm.down(model_rbm, x_new)
    #Compute rmse
    d<-sapply(1:length(good_cases),function(i) sqrt(sum((scaled_wtdata[good_cases[i],selected_variables]-x_new)^2)))
    d<-data.frame(goodid=1:length(d),rmse=d)
    d<-d[order(d$rmse),]
    good_like_bad<-d$goodid[1:n_good_cases_to_select]
    good_like_bad<-complete_cases[good_like_bad]
  }
  
  if(find_by=='som'){
    if(verbose){cat('training RBM with original bad points...')}
    model_params<-list(type='som',dim=10,rlen=10,alpha=c(0.05,0.01),sizeRuleOfTheThumb=T,hex=F) #Use som model
    rs<-create_som_model(wtdata=scaled_wtdata[,selected_variables],model_params=model_params,parallel_mode=F,log_file=NULL,verbose=T,normalize=F)
    if(rs$error) return(list(error=T,data=NULL,msg=rs$msg))
    som_result<-rs$data
    model_params<-rs$data$model_params
    som_model<-som_result$model
    som_pts<-rs$data$som_pts
    som_codes<-rs$data$som_codes
    som_distances<-rs$data$som_distances
    som_center_neuron_position<-rs$data$center_neuron_position
    dim<-rs$data$dim
    som_selected_rows<-som_result$selected_rows
    som_selected_columns<-som_result$selected_columns
    wtdata_bmu<-rs$data$wtdata_bmu
    hex<-rs$data$hex
    stat<-rs$data$stat
    bad_neurons<-unique(wtdata_bmu[bad_cases])
    selected_good<-which(wtdata_bmu[good_cases] %in% bad_neurons)
    #Iteration 0 resampling
    selected_good<-sample(x = selected_good,size = n_good_cases_to_select,replace = T)
    good_like_bad<-complete_cases[selected_good]
  }
  
  #Plots
  #df<-wtdata[,c('TempAmb_avg','VelViento_avg','VelViento_sdv')]
  #df$type<-'good'
  #df$type[bad_cases]<-'bad'
  #df$type[good_like_bad]<-'good_sel'
  #plot_ly(df,x=~VelViento_avg,y=~VelViento_sdv,z=~TempAmb_avg,color=~type)
  
  return(list(error=F,data=list(selected_good=good_like_bad),msg='ok'))
}

balance_by_oversampling<-function(wtdata=NULL,selected_rows=selected_rows,target_name='pre_alarm',balance=NULL){
  num_good<-sum(wtdata[,target_name]==0)
  num_bad<-sum(wtdata[,target_name]==1)
  possible_bad<-which(wtdata[,target_name]==1)
  possible_bad<-possible_bad[possible_bad %in% selected_rows]
  num_wanted_bad<-floor((balance*num_good)/100)
  num_wanted_bad<-num_wanted_bad-num_bad
  if(num_wanted_bad>0) selected_rows<-c(selected_rows,sample(possible_bad, size = num_wanted_bad-num_bad,replace = T))
  return(list(error=F,data=list(selected_rows=selected_rows,wtdata=wtdata),msg='ok'))
}

balance_by_undersampling<-function(wtdata=NULL,selected_rows=selected_rows,target_name='pre_alarm',date_time_name='date_time',select_good_same_bad_interval=T,select_all_goods_after_bad=T,marging_after_pre_alarm=24*60*60,balance=NULL){
  #Select refigster after change ot
  already_selected_good<-NULL
  if(select_all_goods_after_bad){
    for(ld in unique(wtdata$ld_id[train_selected_rows])){
      current_pre_alarms<-which((wtdata$ld_id==ld)&(wtdata[,target_name]))
      if(length(current_pre_alarms)>0){
        last_day<-max(wtdata[current_pre_alarms,date_time_name],na.rm = T)
        last_day<-last_day+as.difftime(marging_after_pre_alarm, units="secs")
        already_selected_good<-which((wtdata$ld_id==ld)&(wtdata[,date_time_name]>last_day))
      }
    }
  }
  
  bad_selected_rows<-which(wtdata[,target_name]==1) #Preselect alarms
  bad_selected_rows<-bad_selected_rows[bad_selected_rows %in% selected_rows]
  bad_selected_datetime<-date_time[bad_selected_rows]
  max_bad<-sum(wtdata[selected_rows,target_name]==1)
  max_good<-sum(wtdata[selected_rows,target_name]==0)
  
  total<-(max_bad*100)/balance
  num_wanted_good<-round(total-max_bad,0)
  #Add same datetime as bad from good
  possible_good<-which(wtdata[,target_name]==0)
  possible_good<-possible_good[possible_good %in% selected_rows]
  #real_balance<-length(selected_rows)*100/(length(possible_good)+length(selected_rows)) #Balance with available good.
  if(!is.null(select_good_same_bad_interval)&&select_good_same_bad_interval){
    tmp_sel<-which(date_time[possible_good] %in% bad_selected_datetime)
    already_selected_good<-c(already_selected_good,possible_good[possible_good %in% tmp_sel]) #Selects from good the same date_time as  from bad.
    possible_good<-possible_good[!(possible_good %in% already_selected_good)]
  }
  good_selected_rows<-c(already_selected_good,sample(possible_good, size = min(length(possible_good),num_wanted_good-length(already_selected_good)), replace = F))
  selected_rows<-c(bad_selected_rows,good_selected_rows)
  return(list(error=F,data=list(selected_rows=selected_rows,wtdata=wtdata),msg='ok'))
}

fix_with_rbm<-function(wtdata=NULL,selected_rows=NULL,selected_rows_tofix=NULL,n_epochs=500,n_batchsize=100,learningrate=0.8,verbose=F){
  if(verbose){cat('\nSmoothing with RBM...')}
  #library(deepnet)
  tmp_selected_rows<-selected_rows[!(selected_rows %in% selected_rows_tofix)]
  
  #complete cases
  complete_cases<-which(complete.cases(wtdata))
  
  tmp_selected_rows<-tmp_selected_rows[tmp_selected_rows %in% complete_cases]
  tmpdata<-as.matrix(wtdata[tmp_selected_rows,])
  
  #Normalize train data
  min<-NULL
  max<-NULL
  for(col in 1:ncol(tmpdata)){
    tmin<-min(tmpdata[,col],na.rm = T)
    tmax<-max(tmpdata[,col],na.rm = T)
    min<-c(min,tmin)
    max<-c(max,tmax)
    tmpdata[,col]<-((tmpdata[,col]-tmin)/(tmax-tmin))
  }
  
  if ((dim(tmpdata)[1] %% n_batchsize) != 0) {
    n_trim <- floor(dim(tmpdata)[1] / n_batchsize) * n_batchsize
    row_samp <- sample(1:dim(tmpdata)[1], n_trim)
    tmpdata <- tmpdata[row_samp,]
  }
  if(verbose){cat('training RBM with original bad points...')}
  model_rbm <- rbm.train(tmpdata, hidden = ncol(tmpdata),batchsize = n_batchsize,numepochs = n_epochs,learningrate = 0.2)
  
  tmpdata<-wtdata[selected_rows_tofix,]
  #Normalize train data
  tmpdata<-sapply(1:ncol(tmpdata),function(col) (tmpdata[,col]-min[col])/(max[col]-min[col]))
  
  x_new <- rbm.up(model_rbm, tmpdata)
  x_new <- rbm.down(model_rbm, x_new)
  tmp<-sapply(1:ncol(x_new),function(col) (x_new[,col]*(max[col]-min[col]))+min[col])
  x_new<-matrix(tmp,nrow = dim(x_new)[1])
  if(verbose){cat('finish RBM\n')}
  return(x_new)
}  

my_smote<-function(wtdata=NULL,balance=50,k=5,selected_rows=NULL,target_name='pre_alarm',date_time_name='date_time',seconds_to_aggregate=86400,exclude_columns=NULL,verbose=F){
  if(verbose){cat('\nGenerating SMOTE points...\n')}
  smote_generated_rows<-NULL
  bad_rows<-which(wtdata[,target_name]==1)
  bad_selected_rows<-bad_rows[bad_rows %in% selected_rows]
  d<-data.frame(n=1:length(bad_selected_rows),o=bad_selected_rows)
  nnarray<-dbscan::kNN(wtdata[bad_selected_rows,!(colnames(wtdata) %in% exclude_columns)], k=k)
  alarm_cases<-sum(selected_rows %in% which(wtdata[,target_name]==1))
  ok_cases<-sum(selected_rows %in% which(wtdata[,target_name]==0))
  prob_balance<-balance/100
  num_wanted_bad<-floor((ok_cases*prob_balance/(1-prob_balance))-alarm_cases)
  older_alarm_turbine<-lapply(unique(wtdata$ld_id),function(ld_id){
    bads<-which((wtdata[,target_name]==1)&(wtdata$ld_id==ld_id))
    if(length(bads)>0){
      bads<-bads[bads %in% selected_rows]
      if(length(bads)>0) data.frame(ld_id=ld_id,date_time=min(wtdata[bads,date_time_name],na.rm = T))
    }
  })
  older_alarm_turbine<-do.call('rbind',older_alarm_turbine)
  for(i in bad_selected_rows){
    cat(paste0('\n',which(i==bad_selected_rows),' of ',length(bad_selected_rows)))
    N<-floor(num_wanted_bad/length(bad_selected_rows))
    ir<-d$n[d$o==i]
    while(N>0){
      tmp_df<-NULL
      current_row<-nnarray$id[ir,ceiling(runif(1,1,k))]
      neib<-wtdata[d$o[d$n==current_row],]
      if(date_time_name %in% colnames(wtdata)){#Datetime
        cld_id<-wtdata[i,'ld_id']
        new_dt<-older_alarm_turbine$date_time[older_alarm_turbine$ld_id==cld_id]-as.difftime(seconds_to_aggregate,units = 'secs')
        exists<-which((wtdata[,'ld_id']==cld_id)&(wtdata[,date_time_name]==new_dt))
        if(length(exists)==0){
          tmp_df<-data.frame(date_time=new_dt)
          colnames(tmp_df)[1]<-date_time_name
        }
        older_alarm_turbine$date_time[older_alarm_turbine$ld_id==cld_id]<-new_dt
      }
      columns<-colnames(wtdata)[colnames(wtdata)!=date_time_name]
      for(col in columns){
        if(!(col %in% exclude_columns)){
          diff<-wtdata[i,col]-neib[,col]
          tmp_val<-wtdata[i,col]+(diff*runif(1,0,1))
        }else if((col %in% exclude_columns)&&col!=date_time_name){
          tmp_val<-wtdata[i,col]
        }
        if(length(exists)>0){
          wtdata[exists[1],col]<-tmp_val
        }else{
          tmp_df<-cbind(tmp_df,tmp_val) 
          colnames(tmp_df)[ncol(tmp_df)]<-col
        }
      }
      if(length(exists)==0){
        tmp_df[,target_name]<-1
        wtdata<-rbind(wtdata,tmp_df)
        smote_generated_rows<-c(smote_generated_rows,nrow(wtdata))
        selected_rows<-c(selected_rows,nrow(wtdata))
        bad_rows<-c(bad_rows,nrow(wtdata))
      }else{
        if(!(exists[1] %in% selected_rows)) selected_rows<-c(selected_rows,exists[1])
        if(!(exists[1] %in% bad_rows)) bad_rows<-c(bad_rows,exists[1])
        wtdata[exists[1],target_name]<-1
        smote_generated_rows<-c(smote_generated_rows,exists[1])
      }
      N<-N-1
    }
  }
  return(list(error=F,data=list(wtdata=wtdata,selected_rows=selected_rows,smote_generated_rows=smote_generated_rows),msg='ok'))
}

balance_by_neighbor_clr_rule<-function(wtdata=NULL,selected_rows=NULL,k=3,target_name='pre_alarm',date_time_name='date_time',seconds_to_aggregate=86400,exclude_columns=NULL){
  #Neighborhood Cleaning Rule
  iam=match.call()[[1]]
  set.seed(1)
  X<-wtdata[selected_rows,(!(colnames(wtdata) %in% exclude_columns))]
  y<-wtdata[selected_rows,(colnames(wtdata)==target_name)]
  tmp_to_wtdata<-data.frame(rowtmp=1:nrow(X),rowwtdata=selected_rows)
  rs<-ubNCL(as.matrix(X), y, k = k, verbose = TRUE)
  rm_index = rs$id.rm
  rm_index_wtdata<-tmp_to_wtdata$rowwtdata[tmp_to_wtdata$rowtmp %in% rm_index]
  tomek_selected_rows<-selected_rows[!(selected_rows %in% rm_index_wtdata)]
  selected_rows<-tomek_selected_rows
  return(list(error=F,data=list(wtdata=wtdata,selected_rows=selected_rows),msg='ok'))
}

balance_by_tomek_smote_rbm<-function(wtdata=NULL,selected_rows=NULL,target_name='pre_alarm',date_time_name='date_time',seconds_to_aggregate=86400,exclude_columns=NULL,balance=50,tomek=T,smote=T,rbm=T,verbose=F){
  iam=match.call()[[1]]
  tmp<-wtdata[selected_rows,(!(colnames(wtdata) %in% exclude_columns)|(colnames(wtdata)==target_name))]
  tmp_to_wtdata<-data.frame(rowtmp=1:nrow(tmp),rowwtdata=selected_rows)
  #Turbine per turbine
  #Tomek
  set.seed(1)
  if(verbose) cat('\nTOMEK Cleaning borders between classes...')
  tomek = unbalanced::ubTomek(tmp[,colnames(tmp)!=target_name],tmp[,target_name],verbose = verbose)
  rm_index = tomek$id.rm
  rm_index_wtdata<-tmp_to_wtdata$rowwtdata[tmp_to_wtdata$rowtmp %in% rm_index]
  tomek_selected_rows<-selected_rows[!(selected_rows %in% rm_index_wtdata)]
  if(length(unique(wtdata[tomek_selected_rows,target_name]))<2) return(list(error=T,data=NULL,msg=paste0("\n",iam," target variable is constant after tomek_selected_rows")))
  if(smote){
    #SMOTE
    rs<-my_smote(wtdata=wtdata,balance=balance,k=5,selected_rows=tomek_selected_rows,target_name=target_name,date_time_name=date_time_name,seconds_to_aggregate=seconds_to_aggregate,exclude_columns=exclude_columns,verbose=verbose)
    wtdata<-rs$data$wtdata
    selected_rows<-rs$data$selected_rows
    smote_generated_rows<-rs$data$smote_generated_rows
    #Fix smote points with RBM
    if(rbm){
      bad_rows<-which(wtdata[,target_name]==1)
      selected_columns<-!(colnames(wtdata) %in% exclude_columns)
      wtdata[smote_generated_rows,selected_columns]<-fix_with_rbm(wtdata=wtdata[,selected_columns],selected_rows=bad_rows,selected_rows_tofix=smote_generated_rows,n_epochs=5000,n_batchsize=50,learningrate=0.2,verbose=verbose)
    }
  }else{
    selected_rows<-tomek_selected_rows
    wtdata<-wtdata
  }
  
  #smote_generated_rows
  originalAlarms<-which(wtdata$pre_alarm==1)
  originalAlarms<-originalAlarms[originalAlarms %in% selected_rows]
  originalAlarms<-originalAlarms[!(originalAlarms %in% smote_generated_rows)]
  originalNoalarms<-which(wtdata$pre_alarm==0)
  originalNoalarms<-originalNoalarms[!(originalNoalarms %in% originalAlarms)]
  originalNoalarms<-originalNoalarms[!(originalNoalarms %in% smote_generated_rows)]
  ggplot()+
    geom_point(data = wtdata[originalNoalarms,],aes(x=Pot_avg,y=TempAceiteMultip_avg),color='green',alpha=0.05)+
    geom_point(data = wtdata[originalAlarms,],aes(x=Pot_avg,y=TempAceiteMultip_avg),color='violet',alpha=0.2)+
    geom_point(data = wtdata[smote_generated_rows,],aes(x=Pot_avg,y=TempAceiteMultip_avg),color='red',alpha=0.07)+
    theme_bw()
  
  return(list(error=F,data=list(wtdata=wtdata,selected_rows=selected_rows),msg='ok'))
}

#Balance -> number of pre_alarm false in proportion of true in percentage.
balance_dataset<-function(lstm=F,wtdata=NULL,selected_rows=NULL,target_name=NULL,seconds_to_aggregate=60*60*24,balance=NULL,balance_by='copy',select_good_same_bad_interval=NULL,select_all_goods_after_bad=NULL,date_time_name='date_time',exclude_columns='date_time,ld_id,health_status,ot,ot_block_code,alarm,alarm_block_code,alarm_all,alarm_all_block_code,ot_all,ot_all_block_code,pre_alarm',verbose=F){
  iam=match.call()[[1]]
  if(!exists("dependencyLoader")){
    if(!file.exists('functions_common/dependencyLoader.R')) return(list(error=TRUE,data=NULL,msg=paste0("\n",iam,":Missing dependency function: functions_common/dependencyLoader.R")));
    source('functions_common/dependencyLoader.R')
  }
  
  sourcesandlib<-c('unbalanced','dbscan','deepnet')
  dep<-dependencyLoader(sourcesandlib)
  if(dep$error) return(list(error=TRUE, warning=FALSE, data=NULL,msg=paste0("\n",iam,":on call dependencyLoader\n",dep$msg)))
  set.seed(1)
  
  if(!is.null(exclude_columns)&&length(exclude_columns)==1) exclude_columns<-unlist(strsplit(x = exclude_columns,','))
  
  #Change target variable to numeric 0/1
  if(is.logical(wtdata[,target_name])) wtdata[,target_name]<-as.numeric(wtdata[,target_name])
  if(!(is.logical(wtdata[,target_name])||is.numeric(wtdata[,target_name]))||(is.numeric(wtdata[,target_name])&&((length(unique(wtdata[,target_name]))!=2)||!all(c(0,1) %in% unique(wtdata[,target_name]))))) return(list(error=T,data=NULL,msg=paste0("\n",iam," target variable(",target_name,") must be or logical or numeric with 0/1")))
  
  #Balance
  if(!is.null(balance_by)&&!is.na(balance_by)){
    smote<-ifelse(lstm,F,T)
    if((balance_by %in% c('oversampling','undersampling'))&&(is.null(balance)||is.na(balance)||balance>=100||balance<=0)) return(list(error=T,data=NULL,msg=paste0('\n',iam,': defined balance by:',balance_by,' which requires that balance variable is defined 0<balance<100')))
    rs<-switch(balance_by,
               'oversampling'=balance_by_oversampling(wtdata=wtdata,target_name=target_name,balance=balance,selected_rows=selected_rows),
               'undersampling'=balance_by_undersampling(wtdata=wtdata,target_name=target_name,date_time_name=date_time_name,selected_rows=selected_rows,select_good_same_bad_interval=select_good_same_bad_interval,select_all_goods_after_bad=select_all_goods_after_bad,balance=balance),
               'tomek_smote_rbm'=balance_by_tomek_smote_rbm(wtdata=wtdata,exclude_columns=exclude_columns,smote=smote,selected_rows=selected_rows,seconds_to_aggregate=seconds_to_aggregate,balance=balance,verbose=verbose),
               'neighbor_clr_rule'=balance_by_neighbor_clr_rule(wtdata=wtdata,selected_rows=selected_rows,target_name=target_name,date_time_name=date_time_name,seconds_to_aggregate=seconds_to_aggregate,exclude_columns=exclude_columns),
               return(list(error=T,data=NULL,msg="\nSpecified balance but not the method, possible values: oversampling,undersampling,tomek_smote_rbm")))
    if(rs$error) return(list(error=T,data=NULL,msg=rs$msg))
    selected_rows<-rs$data$selected_rows
    wtdata<-rs$data$wtdata
  }
  real_balance<-sum(wtdata[selected_rows,target_name]==1,na.rm = T)*100/length(selected_rows) #Balance with available good.
  #Return target name to boolean
  wtdata[,target_name]<-(wtdata[,target_name]==1)
  
  return(list(error=F,data=list(wtdata=wtdata,selected_rows=selected_rows,real_balance=real_balance),msg='ok'))
}