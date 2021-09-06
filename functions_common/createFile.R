createFile<-function(path,append=FALSE){
  if(!file.exists(path)) {#No existe archivo
    if(dirname(path)!="."){#El path tiene subdirs, no es el actual
      dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE, mode = "0777")
      if(!dir.exists(dirname(path))) return(list(error=TRUE,data=NULL,msg=paste0("cannot create: ",dirname(path))))
    }
    file.create(path)
    if(!file.exists(path)) return(list(error=TRUE,data=NULL,msg=paste0("cannot create: ",basename(path)," in ",dirname(path))))
  }else if(!append){#Existe el archivo y se quiere limpiar
    file.remove(path)
    file.create(path)
    if(!file.exists(path)) return(list(error=TRUE,data=NULL,msg=paste0("cannot create: ",path)))
  }
  return(list(error=FALSE,data=NULL,msg="OK"))
}