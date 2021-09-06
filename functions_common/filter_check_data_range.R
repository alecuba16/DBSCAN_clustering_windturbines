filter_check_data_range<-function(wp_code,table_cast_park_dic,db_config){
  iam="filter_check_data_range"
  sources<-paste0("functions_common/",
                  c('db_query.R','close_protocol.R'))
  dep<-dependencyLoader(sources)
  if(dep$error)  return(list(error=TRUE,warning=F,data=NULL,msg=paste0("\n",iam,":on call dependencyLoader\n",dep$msg)))
  
  if(is.null(table_cast_park_dic)||nchar(table_cast_park_dic)<=1) return(list(error=TRUE,warning=F,data=NULL,msg=paste0("\n",iam,": No table_cast_park_dic\n")))
  
  query<-paste0("SELECT range_table_name FROM ",table_cast_park_dic," WHERE wp_code='",tolower(wp_code),"' LIMIT 1;")
  rs<-db_query(query,db_config=db_config)
  
  if(rs$error)
    return(list(error=TRUE,warning=F,data=NULL,msg=paste0("\n",iam,":on call db_query\n",dep$msg)))
  
  if(is.null(rs$data)|| nrow(rs$data)==0 ||is.na(rs$data$range_table_name) || nchar(rs$data)<=1) #No data range table name
    return(list(error=FALSE,warning=T,data=NULL,msg="ok"))
  
  range_table_name<-rs$data
  query<-paste0("SELECT var,min_manual as min,max_manual as max,min_calc as min_calc,max_calc as max_calc,method FROM ",range_table_name,";")
  rs<-db_query(query,db_config=db_config)
  if(rs$error) return(list(error=TRUE,warning=F,data=NULL,msg=paste0("\n",iam,":on call db_query\n",dep$msg)))
  
  if(is.null(rs$data)||nrow(rs$data)==0) #No data in range table
    return(list(error=F,warning=TRUE,data=NULL,msg="No data in range table"))
  
  limit_list<-rs$data
  #check unique
  unique_list<-unique(limit_list$var)
  for(v in 1:length(unique_list)){
    possible_duplicates<-limit_list[which(limit_list$var==unique_list[v]),]
    if(nrow(possible_duplicates)>1){#Duplicate check if manual is defined
      #Get the first with both manual not null
      notmanualna<-which(!is.na(possible_duplicates$min)&!is.na(possible_duplicates$max)==TRUE)
      if(length(notmanualna)==0){#There is no row with both manual not null, get the first with some of them
        notmanualna<-which(!is.na(possible_duplicates$min)|!is.na(possible_duplicates$max)==TRUE)
      }
      if(length(notmanualna)>0){#Delete duplicates from limit_list there are some manual
        rows_drop<-rownames(possible_duplicates)[which(rownames(possible_duplicates)!=rownames(possible_duplicates)[notmanualna[1]])]
      }else{#Remove all except the first since there is no manual not null
        rows_drop<-rownames(possible_duplicates)[which(rownames(possible_duplicates)!=rownames(possible_duplicates)[1])]
      }
      limit_list<-limit_list[!(rownames(limit_list) %in% rows_drop),]
    }
  }
  return(list(error=FALSE,warning=F,data=limit_list,msg='ok'))
}
