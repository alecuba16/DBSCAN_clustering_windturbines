calcule_pvalues <- function(wtdata,target, method = "theoretical", est = "mean",disp = FALSE, file_jpeg="", nsim = 1000,pvalue=0.05,parallel=F,logfile=NULL) {
  iam=match.call()[[1]]
  #Dependencias
  if(!exists("dependencyLoader")){
    if(!file.exists('functions_common/dependencyLoader.R')) return(list(error=TRUE,data=NULL,msg=paste0("\n",iam,":Missing dependency function: functions_common/dependencyLoader.R")));
    source('functions_common/dependencyLoader.R')
  }
  dep<-'functions_common/myInference.R'
  if(parallel) dep<-c(dep,'parallel','doSNOW','bigmemory')
  dep<-dependencyLoader(dep)
  if(dep$error) return(list(error=TRUE, warning=FALSE, data=NULL,msg=paste0("\n",iam,":on call dependencyLoader\n",dep$msg)))
  ## 2. Loop to calculate p-values
  n <- ncol(wtdata)
  if(n<=1)return(list(error=TRUE,data=NULL,msg=paste0(iam,":WTdata must be atleast 2 columns")))
  pvalues <- rep(1,n)
  
  if(!parallel){ #original method sequential
    for ( i in 1:n) {
      y <- wtdata[,i] # Numeric variable
      if(length(unique(y[!is.na(y)]))<2) next() #Variable is constant
      if( is.numeric(y) ) {
        log <- capture.output({
          pvalue <- myInference(y = y, x = target, 
                                est = est, type = "ht", null = 0, alternative = "twosided", 
                                method = method, sum_stats = FALSE, eda_plot = FALSE, 
                                inf_plot = FALSE, inf_lines = FALSE, nsim = nsim,siglevel=pvalue )  
        })
        
        #cat("\nP-value for ", names(wtdata)[i], " ", pvalue, "\n")
        if( !is.nan(pvalue) ) {
          pvalues[i] <- pvalue
          # IF p-value less than 0.05 plot results
          if( disp == TRUE && pvalues[i] < pvalue ) {
            jpeg(filename = paste(file_jpeg, names(Y)[i],"_", substr(method,1,4),".jpeg",sep = ""))
            myInference(y = y, x = target, 
                        est = est, type = "ht", null = 0, alternative = "twosided", 
                        method = method)
            dev.off()
            # dev.print(jpeg, 'filename.jpeg') # Other alternative to print the current picture on the browser
          }
        }
      }
    }
  }else{ #new method embarrassing parallelism
    mb_per_thread<-((nrow(wtdata)*nsim*2400)/(20000*10000)) #20000 rows x 120 column x 10000nsim df uses about 2600MB
    #mb_per_thread<-3000 #Worst case 3000MB
    if(Sys.info()[['sysname']]=="Linux"){
      free<-as.numeric(system("awk '/MemAvailable/ {print $2}' /proc/meminfo",intern=TRUE))
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
    x<-bigmemory::as.big.matrix(as.matrix(wtdata))
    rm(wtdata)
    gc(verbose=F)
    mdesc <- describe(x)
    pvalues <- foreach(i = 1:ncol(x),.packages = 'bigmemory',.export = c('myInference'),.verbose = F, .combine = 'c') %dopar% {
      wtdata<-bigmemory::attach.big.matrix(mdesc)
      myInference(y = as.numeric(wtdata[,i]), x = target, est = est, type = "ht", null = 0, alternative = "twosided", method = method, sum_stats = FALSE, eda_plot = FALSE, inf_plot = FALSE, inf_lines = FALSE, nsim = nsim,siglevel=pvalue )  
    }
    stopCluster(cl)
    rm(x)
    gc(verbose = F)
  }
  return(list(error=FALSE,data=pvalues,msg="OK"))
}

