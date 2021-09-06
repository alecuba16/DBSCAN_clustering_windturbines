unitary_test<-function(iterations=10,nvars=3,npoints_orig=10,npoints_selected=5,npoints_no_selected=3){
  iam=match.call()[[1]]
  knn_pass<-T
  rbm_pass<-T
  som_pass<-T
  it<-1
  set.seed(1)
  while(knn_pass&&rbm_pass&&som_pass&&it<=iterations){
    cat(paste0('\n Iteration ',it,' of ',iterations,' ... '))
    #Knn
    m<-matrix(runif(npoints*(nvars+2)),nrow = npoints)
    m<-as.data.frame(m)
    m[,dim(m)[2]-1]<-1 #1 alarm 0 healthy
    m[,dim(m)[2]]<-0 #0 for original 1toselect 2noselect
    colnames(m)[dim(m)[2]-1]<-'target'
    colnames(m)[dim(m)[2]]<-'orig0_sel1_no2'
    #Generate selected
    generated<-0
    for(x in 1:dim(m)[1]){
      to_add<-min(npoints_selected-generated,ceiling(npoints_orig/npoints_selected))
      if(to_add>0){
        to_add<-runif(to_add,min=0.01,max=0.2)
        for(i in to_add){
          tmp<-NULL
          for(y in 1:(dim(m)[2]-2)){
            tmp<-c(tmp,m[x,y]+i)
          }
          m<-rbind(m,c(tmp,0,1))
          generated<-generated+1
        }
      }
    }
    
    #Generate no selected
    generated<-0
    for(x in 1:dim(m)[1]){
      to_add<-min(npoints_no_selected-generated,ceiling(npoints_orig/npoints_no_selected))
      if(to_add>0){
        to_add<-runif(to_add,min=0.5,max=0.8)
        for(i in to_add){
          tmp<-NULL
          for(y in 1:(dim(m)[2]-2)){
            tmp<-c(tmp,m[x,y]+i)
          }
          m<-rbind(m,c(tmp,0,2))
          generated<-generated+1
        }
      }
    }
    
    #Call knn
    rs<-select_good_with_bad_variables(wtdata=m,find_by='knn',balance=30,selected_variables=colnames(m)[1:(dim(m)[2]-2)],target_name='target',k=10,discretize=F,standarize=T,n_epochs=500,n_batchsize=100,learningrate=0.8)
    if(rs$error) return(list(error=T,data=NULL,msg=paste0(iam,' on call select_good_with_bad_variables:',rs$msg)))
    selected_good<-rs$data$selected_good
    selected_good<-selected_good[!is.na(selected_good)]
    if(!all(m$orig0_sel1_no2[selected_good]==1)){
      knn_pass<-F
      cat(' KNN failed')
    }else{
      #Call rbm
      rs<-select_good_with_bad_variables(wtdata=m,find_by='rbm',balance=30,selected_variables=colnames(m)[1:(dim(m)[2]-2)],target_name='target',k=10,discretize=F,standarize=T,n_epochs=500,n_batchsize=100,learningrate=0.8)
      if(rs$error) return(list(error=T,data=NULL,msg=paste0(iam,' on call select_good_with_bad_variables:',rs$msg)))
      selected_good<-rs$data$selected_good
      selected_good<-selected_good[!is.na(selected_good)]
      if(!all(m$orig0_sel1_no2[selected_good]==1)){
        rbm_pass<-F
        cat(' RBM failed')
      }else{
        #Call som
        rs<-select_good_with_bad_variables(wtdata=m,find_by='som',balance=30,selected_variables=colnames(m)[1:(dim(m)[2]-2)],target_name='target',k=10,discretize=F,standarize=T,n_epochs=500,n_batchsize=100,learningrate=0.8)
        if(rs$error) return(list(error=T,data=NULL,msg=paste0(iam,' on call select_good_with_bad_variables:',rs$msg)))
        selected_good<-rs$data$selected_good
        selected_good<-selected_good[!is.na(selected_good)]
        if(!all(m$orig0_sel1_no2[selected_good]==1)){
          som_pass<-F
          cat(' SOM failed')
        }
      }
    }
    it<-it+1
  }
  return(list(error=F,data=list(knn_pass=knn_pass,rbm_pass=rbm_pass,som_pass=som_pass),msg='ok'))
}

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

select_good_samples<-function(wtdata=NULL,
                              find_by='knn',
                              balance=30,
                              selected_variables=NULL,
                              target_name='ot',
                              k=10,
                              discretize=F,
                              standarize=T,
                              n_epochs=500,
                              n_batchsize=100,
                              learningrate=0.8,
                              verbose=F){
  iam=match.call()[[1]]
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
    colnames(d)<-c('badid','goodid','dist')
    d$badid<-as.numeric(d$badid)
    d$goodid<-as.numeric(d$goodid)
    d$dist<-as.numeric(levels(d$dist)[d$dist])
    tmpd<-NULL
    for(gid in unique(d$goodid)){
      tmpd<-rbind(tmpd,d[which((d$dist==min(d$dist[d$goodid==gid],na.rm = T)&(d$goodid==gid))),])
    }
    d<-tmpd
    #Normalize distance 0-1
    d$dist<-(d$dist-min(d$dist))/(max(d$dist)-min(d$dist))
    #Exclude dists > 25%
    d<-d[d$dist<=0.25,]
    d<-d[order(d$dist),]
    good_like_bad<-complete_cases[good_cases[d$goodid]]
  }
  
  if(find_by=='rbm'){
    if(verbose){cat('training RBM with original bad points...')}
    model_rbm <- rbm.train(as.matrix(scaled_wtdata[bad_cases,selected_variables]), hidden = length(selected_variables),batchsize = n_batchsize,numepochs = n_epochs,learningrate = learningrate)
    x_new <- rbm.up(model_rbm, as.matrix(scaled_wtdata[good_cases,selected_variables]))
    x_new <- rbm.down(model_rbm, x_new)
    #Compute distance
    x<-scaled_wtdata[good_cases,selected_variables]
    dist<-sapply(1:dim(x)[1],function(r) sqrt(sum((x[r,]-x_new[r,])^2,na.rm = T)))
    d<-data.frame(goodid=1:length(dist),dist=dist)
    #Normalize distance 0-1
    d$dist<-(d$dist-min(d$dist))/(max(d$dist)-min(d$dist))
    #Exclude dists > 25%
    d<-d[d$dist<=0.25,]
    d<-d[order(d$dist),]
    good_like_bad<-d$goodid[1:min(n_good_cases_to_select,dim(d)[1])]
    good_like_bad<-complete_cases[good_cases[good_like_bad]]
  }
  
  if(find_by=='som'){
    if(verbose){cat('training RBM with original bad points...')}
    model_params<-list(type='gsom',spreadFactor=0.8,hex=T) #Use som model
    rs<-create_som_model(wtdata=scaled_wtdata[,selected_variables],model_params=model_params,parallel_mode=F,log_file=NULL,verbose=verbose,normalize=T)
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
    good_like_bad<-complete_cases[good_cases[selected_good]]
  }
  
  good_like_bad<-unique(good_like_bad)
  
  #Plots
  #df<-wtdata[,c('TempAmb_avg','VelViento_avg','VelViento_sdv')]
  #df$type<-'good'
  #df$type[bad_cases]<-'bad'
  #df$type[good_like_bad]<-'good_sel'
  #plot_ly(df,x=~VelViento_avg,y=~VelViento_sdv,z=~TempAmb_avg,color=~type)
  
  return(list(error=F,data=list(selected_good=good_like_bad),msg='ok'))
}