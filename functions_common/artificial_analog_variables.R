artificial_analog_variables<-function(wtdata=NULL,wtdata0=NULL,wt_query=NULL,artificial_variables=wt_query$artificial_variables,date_time_name='date_time',table_cast_park_dic='1_cast_park_table_dic',table_filter_config='1_filter_config',target_name='alarm',db_config=NULL){
  iam=match.call()[[1]]
  # Sources
  if(!exists("dependencyLoader")){
    if(!file.exists('functions_common/dependencyLoader.R')) return(list(error=TRUE,data=NULL,msg=paste0("\n",iam,":Missing dependency function: functions_common/dependencyLoader.R")));
    source('functions_common/dependencyLoader.R')
  }
  sources<-c("functions_common/formatter_include_exclude.R","functions_common/close_protocol.R")
  dep<-dependencyLoader(sources)
  
  
  #TODO to implement!!!
  # if("TempAmb_avg" %in% names(wtdata) && "VelViento_avg" %in% names(wtdata) && "Pot_avg" %in% names(wtdata) && "TempAceiteMultip_avg" %in% names(wtdata) && "TempRodamMultip_avg" %in% names(wtdata)) {
  #     wtdata$rate_energy_mult <- with(wtdata, Pot_avg/(TempRodamMultip_avg + TempAceiteMultip_avg - TempAmb_avg))
  #     wtdata$rate_energy_mult[is.infinite(wtdata$rate_energy_mult) | is.nan(wtdata$rate_energy_mult)] <- NA
  # }
  # if("TempAmb_avg" %in% names(wtdata) && "VelViento_avg" %in% names(wtdata) && "Pot_avg" %in% names(wtdata) && "TempAceiteMultip_avg" %in% names(wtdata) && "TempMultip_avg" %in% names(wtdata)) {
  #     wtdata$rate_energy_mult <- with(wtdata, Pot_avg/(TempMultip_avg + TempAceiteMultip_avg - TempAmb_avg))
  #     wtdata$rate_energy_mult[is.infinite(wtdata$rate_energy_mult) | is.nan(wtdata$rate_energy_mult)] <- NA
  # }
  # if("TempAmb_avg" %in% names(wtdata) && "VelViento_avg" %in% names(wtdata) && "Pot_avg" %in% names(wtdata) && "TempCojLOA_avg" %in% names(wtdata) && "TempCojLA_avg" %in% names(wtdata)) {
  #     wtdata$rate_energy_gen <- with(wtdata, Pot_avg/(TempCojLA_avg + TempCojLOA_avg - TempAmb_avg))
  #     wtdata$rate_energy_gen[is.infinite(wtdata$rate_energy_gen) | is.nan(wtdata$rate_energy_gen)] <- NA
  # }
  
  
  
  if(!is.null(artificial_variables) && !is.na(artificial_variables) && length(artificial_variables)>0 && nchar(artificial_variables)>2){
    artificial_variables<-unlist(strsplit(artificial_variables,','))
    only_good_defined<-grepl('.*=.*[/+\\-\\*].*',artificial_variables,perl=TRUE)
    if(any(only_good_defined)){
      artificial_variables_df<-data.frame(artificial_name=character(0),operation=character(0),variable1=character(0),variable2=character(0),stringsAsFactors = FALSE)
      artificial_variables<-artificial_variables[only_good_defined]
      for(a in artificial_variables){
        artificial_name<-gsub('(.*)=(.*)([/+\\-\\*])(.*)',"\\1",a,perl = TRUE)
        artificial_input_var1<-gsub('(.*)=(.*)([/+\\-\\*])(.*)',"\\2",a,perl=TRUE)
        artificial_input_var2<-gsub('(.*)=(.*)([/+\\-\\*])(.*)',"\\4",a,perl=TRUE)
        artificial_operation<-gsub('(.*)=(.*)([/+\\-\\*])(.*)',"\\3",a,perl=TRUE)
        artificial_variables_df<-rbind(artificial_variables_df,data.frame(artificial_name=artificial_name,operation=artificial_operation,variable1=artificial_input_var1,variable2=artificial_input_var2,stringsAsFactors = FALSE))    
      }
      artificial_needed_variables<-unique(unlist(strsplit(paste(artificial_variables_df$variable2,paste(artificial_variables_df$variable1,collapse = ','),collapse = ',',sep=','),split = ',')))
    }
  }
  
  if(!exists("artificial_variables_df",inherits = F)||is.null(artificial_variables_df)||is.na(artificial_variables_df)||nrow(artificial_variables_df)==0) return(list(error=FALSE,data=list(wtdata=wtdata,wtdata0=wtdata0),msg="ok: no artificial analog variables"))
  
  #Download needed variables
  if(exists("tmpdf",inherits = F)) rm(tmpdf)
  if(exists("tmpdf0",inherits = F)) rm(tmpdf0)
  to_download<-NULL
  for(need in artificial_needed_variables){
    if(need %in% names(wtdata)){
      if(exists("tmpdf",inherits = F)) 
        tmpdf<-cbind(tmpdf,wtdata[,need,drop=F])
      else
        tmpdf<-wtdata[,need,drop=F]
      
      if(exists("tmpdf0",inherits = F))
        tmpdf0<-cbind(tmpdf0,wtdata0[,need,drop=F])
      else
        tmpdf0<-wtdata0[,need,drop=F]
    }else{
      to_download<-c(to_download,need)
    }
  }
  #Add date time
  if(exists("tmpdf",inherits = F)) tmpdf[,date_time_name]<-wtdata[,date_time_name,drop=F]
  if(exists("tmpdf0",inherits = F)) tmpdf0[,date_time_name]<-wtdata0[,date_time_name,drop=F]
  
  if(exists("to_download",inherits = F)&&!is.null(to_download)&&!is.na(to_download)){
    rs<-formatter(wt_query=wt_query,
                  table_cast_park_dic=table_cast_park_dic,
                  date_time_name=date_time_name,
                  include_variables=to_download,
                  seconds_to_aggregate=seconds_to_aggregate,
                  target_name=NULL,
                  db_config=db_config)
    if(rs$error) {
      output_msg <- paste0("\n",iam,":on call load_wtdata\n\t",rs$msg)
      close_protocol(output_msg, iam, debug_mode)
      return(list(error=TRUE,data=NULL,msg=output_msg))
    }
    downloaded<-rs$data[rs$data[,date_time_name] %in% wtdata[,date_time_name],]
    downloaded0<-rs$data[rs$data[,date_time_name] %in% wtdata0[,date_time_name],]
    for(col in names(downloaded)[names(downloaded)!=date_time_name]){
      if(exists("tmpdf",inherits = F)){
        tmpdf[,col]<-NA
        tmpdf[(tmpdf[,date_time_name] %in% downloaded[,date_time_name]),col]<-downloaded[,col]
      }else{
        tmpdf<-downloaded[,c(col,date_time_name)]
      }
      if(exists("tmpdf0",inherits = F)){
        tmpdf0[,col]<-NA
        tmpdf0[(tmpdf0[,date_time_name] %in% downloaded0[,date_time_name]),col]<-downloaded0[,col]
      }else{
        tmpdf0<-downloaded0[,c(col,date_time_name)]
      }
    }
    rm(list = c('downloaded','downloaded0','to_download'))
  }
  
  
  if(!is.null(artificial_variables_df)||!is.na(artificial_variables_df)){
    if(!is.na(artificial_variables_df)){
      artificial_variables_names<-artificial_variables_df$artificial_name
      for(include in artificial_variables_names){
        current_art<-artificial_variables_df[artificial_variables_names==include,]
        if(exists("tmpdf",inherits = F) && sum(c(current_art$variable1,current_art$variable2) %in% names(tmpdf))==2){#Check if exists in downloaded data
          #Exclude if some of them is na
          nas<-is.na(tmpdf[,current_art$variable1])|is.na(tmpdf[,current_art$variable2])
          if(length(nas)>0 && any(nas)){
            wtdata[nas,current_art$artificial_name]<-NA
            tmp<-tmpdf[!nas,c(current_art$variable1,current_art$variable2)]
          }else{
            tmp<-tmpdf[,c(current_art$variable1,current_art$variable2)]
          }
          #Compute artificial value
          value<-switch(current_art$operation, 
                        '/'={tmp[,current_art$variable1]/tmp[,current_art$variable2]},
                        '*'={tmp[,current_art$variable1]*tmp[,current_art$variable2]},
                        '+'={tmp[,current_art$variable1]+tmp[,current_art$variable2]},
                        '-'={tmp[,current_art$variable1]-tmp[,current_art$variable2]})
          #Fix nan and infinite
          value[is.infinite(value) | is.nan(value)|!is.numeric(value)] <- NA
          wtdata[!nas,current_art$artificial_name]<-value
          rm(list = c('tmp','value','nas'))
        }else{
          cat(paste0("\nWarning unable to calculate aritifical variable ",include," one of the variables that it needs is null or missing from wtdata"))
        }
        if(exists("tmpdf0",inherits = F)&& sum(c(current_art$variable1,current_art$variable2) %in% names(tmpdf0))==2){#Check if exists in downloaded data
          #Exclude if some of them is na
          nas0<-is.na(tmpdf0[,current_art$variable1])|is.na(tmpdf0[,current_art$variable2])
          if(length(nas0)>0 && any(nas0)){
            wtdata0[nas0,current_art$artificial_name]<-NA
            tmp0<-tmpdf0[!nas0,c(current_art$variable1,current_art$variable2)]
          }else{
            tmp0<-tmpdf0[,c(current_art$variable1,current_art$variable2)]
          }
          #Compute artificial value
          value0<-switch(current_art$operation, 
                         '/'={tmp0[,current_art$variable1]/tmp0[,current_art$variable2]},
                         '*'={tmp0[,current_art$variable1]*tmp0[,current_art$variable2]},
                         '+'={tmp0[,current_art$variable1]+tmp0[,current_art$variable2]},
                         '-'={tmp0[,current_art$variable1]-tmp0[,current_art$variable2]})
          #Fix nan and infinite
          value0[is.infinite(value0) | is.nan(value0)|!is.numeric(value0)] <- NA
          wtdata0[!nas0,current_art$artificial_name]<-value0
          rm(list = c('tmp0','value0','nas0'))
        }else{
          cat(paste0("\nWarning unable to calculate aritifical variable ",include," one of the variables that it needs is null or missing from wtdata0"))
        }
      }
    }
  }
  
  return(list(error=FALSE,data=list(wtdata=wtdata,wtdata0=wtdata0),msg="ok"))
}