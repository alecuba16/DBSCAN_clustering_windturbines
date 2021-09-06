get_file_path <- function(folder, wt_code, horizon, time_mark) {
  fileBaseName = paste0(time_mark,'_', wt_code,"_",horizon,"d_wsp.RData")
  fileName = paste0(folder,"/",fileBaseName)
  zipFileName = paste0(folder,"/",time_mark,'_', wt_code,"_wsp.zip")
  # file = paste0(folder,"\\",time_mark,"_",ld_code,"_",ld_id,"_",fault,"_",horizon,"d_wsp.RData")
  if(file.exists(zipFileName)) { 
    fileNames <- unzip(zipFileName, list = TRUE)$Name
    if(grep(fileBaseName, fileNames)){
      return(list(error=FALSE,data=list("fileBaseName"=fileBaseName,"fileName"=fileName,"zipFileName"=zipFileName,msg="OK")))
    }else{
      return(list(error=TRUE,data=NULL,msg=paste0("get_file_path: zipFile(",zipFileName,") exists but doesn't contain ",fileName)));
    }
  } else {
    # hstr <- substr(time_mark,12,13)
    # hnum <- as.numeric(hstr)
    # mstr <- substr(time_mark,15,16)
    # mnum <- as.numeric(mstr)
    # for( i in 1L:10L ){
    #     hnum <- hnum + 1L
    #     time_mark2 <- gsub(paste0("_",hstr,"_"), paste0("_",hnum,"_"), time_mark)
    #     file = paste0(folder,"\\",time_mark2, wt_code,"_",horizon,"d_wsp.RData")
    #     if(file.exists(file)) { 
    #         return(file)
    #     }
    # }
    warning(paste0(zipFileName, " File no found\n"))
    return(list(error=TRUE,data=NULL,msg=paste0("get_file_path: zipFile(",zipFileName,") doesn't exists")));
  }
}