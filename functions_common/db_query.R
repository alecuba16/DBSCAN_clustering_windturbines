db_query <- function(query=NULL,db_config=NULL) {
  #Dependencias
  if(!exists("dbConnect")){
    if(!is.element("RMySQL", installed.packages()[,1])) return(list(error=TRUE,data=NULL,msg="Please install RMySQL package"));
    library(RMySQL)
  }
  
  if(is.null(query)) return(list(error=TRUE,data=NULL,msg="query is null"));
  
  if(is.null(db_config)||!is.data.frame(db_config)) return(list(error=TRUE,data=NULL,msg="Missing db_config"));
  
  if(!("user" %in% colnames(db_config))||is.null(db_config$user)||nchar(as.character(db_config$user))<=0) return(list(error=TRUE,data=NULL,msg="Missing db_config$user"));
  if(!("password" %in% colnames(db_config))||is.null(db_config$password)||nchar(as.character(db_config$password))<=0) return(list(error=TRUE,data=NULL,msg="Missing db_config$password"));
  if(!("dbname" %in% colnames(db_config))||is.null(db_config$dbname)||nchar(as.character(db_config$dbname))<=0) return(list(error=TRUE,data=NULL,msg="Missing db_config$dbname"));
  if(!("host" %in% colnames(db_config))||is.null(db_config$host)||nchar(as.character(db_config$host))<=0) return(list(error=TRUE,data=NULL,msg="Missing db_config$host"));
  if(!("port" %in% colnames(db_config))||is.null(db_config$port)||as.numeric(db_config$port)<=0 || as.numeric(db_config$port)>65536) return(list(error=TRUE,data=NULL,msg="Missing db_config$port"));
  
  user<-as.character(db_config$user)
  password<-as.character(db_config$password)
  dbname<-as.character(db_config$dbname)
  host<-as.character(db_config$host)
  port<-as.numeric(db_config$port)
    
  con <- dbConnect(MySQL(), user=user, password=password,dbname=dbname, host=host, port=port)
  if(is.null(con))  return(list(error=TRUE,data=NULL,msg="Error connecting yourHistoricalBD"));
  on.exit(dbDisconnect(con))    
  if(exists("debug_mode")&& debug_mode) cat(paste0("\nQuery:",query))
  rs <- dbSendQuery(con, query)
  if(is.null(rs))  return(list(error=TRUE,data=NULL,msg=paste0("Error at query: ",query)));
  
  querydata <- dbFetch(rs, n=-1)

  dbClearResult(rs)
  return(list(error=FALSE,data=querydata,msg="OK"));   
}
