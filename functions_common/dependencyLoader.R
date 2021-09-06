dependencyLoader<-function(dependency_list){
  iam='dependencyLoader'
  error=FALSE
  total=length(dependency_list)
  msg="OK"
  i=1
  while(i<=total&&!error) {
    function_name=strsplit(basename(dependency_list[i]),".",fixed=TRUE)[[1]][1]
    if(!exists(function_name,mode='function')){
      if(!file.exists(dependency_list[i])){
        if(dependency_list[i] %in% rownames(installed.packages())){
          library(dependency_list[i],character.only = TRUE,verbose = F)
        }else{
          cat('Trying to install ',dependency_list[i],' as a package...')
           install.packages(dependency_list[i],dep=TRUE,repos = "http://cran.r-project.org")
          if(!require(dependency_list[i],character.only = TRUE,warn.conflicts = F)) {
            error=TRUE
            msg=paste0(iam,": Missing dependency function/library: ",dependency_list[i])
          }
        }
      }else{
        source(dependency_list[i])
      }
    }
    i=i+1
  }
  return(list(error=error,data=NULL,msg=msg));
}