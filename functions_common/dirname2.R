# dirname2<-function(path){
#   file_info_path=file.info(path)[,"isdir"]
#   if(!is.na(file_info_path) && file_info_path){
#     return(list(error=FALSE,data=path,msg="OK:is a directory"))
#   }else{
#     return(list(error=FALSE,data=dirname(path),msg="OK:is a file"))
#   }
# }

dirname2 <-function(path) {
  # switch(.Platform$OS.type, 
  #        unix={
  #         comparator='/'
  #        },
  #        windows={
  #          comparator='\\'
  #        },
  #        {
  #         comparator='\\'
  #        }
  # )
  comparator <- c('/','\\')  
  if(any(substr(path, nchar(path), nchar(path))==comparator))
    return(list(error=FALSE,data=path,msg="OK:is a folder"))
  else
    return(list(error=FALSE,data=paste0(dirname(path),"/"),msg="OK:is a file"))
} 