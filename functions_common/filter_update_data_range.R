filter_update_data_range<-function(limits=limits,wp_code=wp_code,table_cast_park_dic=table_cast_park_dic,db_config=db_config){
  iam=match.call()[[1]]
  sources<-paste0("functions_common/",
                  c('db_query.R','close_protocol.R','filter_check_data_range.R'))
  dep<-dependencyLoader(sources)
  if(dep$error)  return(list(error=TRUE,warning=F,data=NULL,msg=paste0("\n",iam,":on call dependencyLoader\n",dep$msg)))
  
  if(is.null(db_config)||!is.data.frame(db_config)) return(list(error=TRUE,warning=F,data=NULL,msg="Missing db_config"));
  
  if(is.null(table_cast_park_dic)||nchar(table_cast_park_dic)<=1) return(list(error=TRUE,warning=F,data=NULL,msg=paste0("\n",iam,": No table_cast_park_dic\n")))
  
  query<-paste0("SELECT range_table_name FROM ",table_cast_park_dic," WHERE wp_code='",tolower(wp_code),"' LIMIT 1;")
  rs<-db_query(query,db_config=db_config)
  
  if(rs$error)
    return(list(error=TRUE,warning=F,data=NULL,msg=paste0("\n",iam,":on call db_query\n",dep$msg)))
  
  if(is.null(rs$data)|| nrow(rs$data)==0 ||is.na(rs$data$range_table_name)||nchar(rs$data)<=1) #No data range table name
    return(list(error=FALSE,warning=T,data=NULL,msg="No data range table!!!"))
  
  range_table_name<-rs$data
  
  rs<-filter_check_data_range(wp_code=tolower(wp_code),table_cast_park_dic=table_cast_park_dic,db_config=db_config)
  if(rs$error)
    return(list(error=TRUE,warning=F,data=NULL,msg=paste0("\n",iam,":on call filter_check_data_range\n",dep$msg)))
  current_manual_limits<-rs$data
  
  #Set inf to NA
  limits$min[!is.numeric(limits$min)|is.nan(limits$min)|is.infinite(limits$min)]<-NA
  limits$max[!is.numeric(limits$max)|is.nan(limits$max)|is.infinite(limits$max)]<-NA
  
  #Check to insert or update.
  to_update_vars<-NA
  to_insert_vars<-NA
  current_manual_limits_vars<-unique(current_manual_limits$var)
  new_calc_limits_vars<-unique(limits$var)
  common<-(new_calc_limits_vars %in% current_manual_limits_vars)
  if(all(common)){
    to_update_vars<-new_calc_limits_vars
  }else if(!all(common)){
    to_insert_vars<-new_calc_limits_vars[!common]
    to_update_vars<-new_calc_limits_vars[common]
  }else{
    to_insert_vars<-new_calc_limits_vars
  }
  
  #Insert zone
  if(!all(is.na(to_insert_vars))){
    to_insert<-limits[to_insert_vars,,drop=F]
    for(i in 1:nrow(to_insert)){
      query<-paste0('INSERT INTO ',range_table_name,' (min_calc,max_calc,method) VALUES (',ifelse(is.na(to_update$min[i]),'NULL',to_update$min[i]),',',ifelse(is.na(to_update$max[i]),'NULL',to_update$max[i]),',\'filter\')')
      rs<-db_query(query,db_config=db_config)
      if(rs$error)
        return(list(error=TRUE,warning=F,data=NULL,msg=paste0("\n",iam,":on call db_query for insert ranges\n",dep$msg)))
    }
  }
  #Update Zone
  if(!all(is.na(to_update_vars))){
    to_update<-limits[to_update_vars,,drop=F]
    for(i in 1:nrow(to_update)){
      query<-paste0('UPDATE ',range_table_name,' SET min_calc= IF(method=\'stat\',min_calc,',ifelse(is.na(to_update$min[i]),'NULL',to_update$min[i]),'), max_calc=IF(method=\'stat\',max_calc,',ifelse(is.na(to_update$max[i]),'NULL',to_update$max[i]),'), method=IF(method=\'stat\',\'stat\',\'filter\') where var=\'',to_update$var[i],'\'')
      #query<-paste0('UPDATE ',range_table_name,' SET min_calc=',ifelse(is.na(to_update$min[i]),'NULL',to_update$min[i]),', max_calc=',ifelse(is.na(to_update$max[i]),'NULL',to_update$max[i]),' where var=\'',to_update$var[i],'\'')
      rs<-db_query(query,db_config=db_config)
      if(rs$error)
        return(list(error=TRUE,warning=F,data=NULL,msg=paste0("\n",iam,":on call db_query for update ranges\n",dep$msg)))
    }
  }
  return(list(error=FALSE,warning=F,data=NULL,msg='ok'))
}
