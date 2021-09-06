variable_helper<-function(variables,to_check,action='include'){
    ## Regex
    #Extract regex patterns first
    regex_patterns<-unlist(regmatches(to_check,gregexpr(pattern = "(\\^[^,]*[\\$]?)",text= to_check,perl = T)))
    if(length(regex_patterns)>0){
        #Remove regex patterns from to_check
        for(rp in regex_patterns){
            to_check<-gsub(pattern = rp,x = to_check,replacement = '',fixed = T) 
        }
        #Merge regex patterns
        regex_patterns<-paste0(regex_patterns,collapse = '|')
        variables_regex<-grep(pattern = regex_patterns,x = variables,value = TRUE)
    }else{
        variables_regex<-NULL
    }
    ## End regex
    
    ## Regular variables (fixed name)
    #remove empty commas due the regex patterns removal ,,
    array_to_check_variables<-unlist(strsplit(to_check,split=','))
    nchars<-unlist(lapply(array_to_check_variables,function(v) nchar(v)))
    array_to_check_variables<-array_to_check_variables[nchars>0]
    
    #Get final variables
    if(action=='include')
        variables<-variables[(variables %in% array_to_check_variables)|(variables %in% variables_regex)]
    else
        variables<-variables[!((variables %in% array_to_check_variables)|(variables %in% variables_regex))]
    return(variables)
}

formatter_include_exclude<-function(variables=NULL,date_time_name='date_time',include_variables='',exclude_variables='',target_name=NULL)
{
    iam=match.call()[[1]]
    
    # Sources
    if(!exists("dependencyLoader")){
        if(!file.exists('functions_common/dependencyLoader.R')) return(list(error=TRUE,data=NULL,msg=paste0("\n",iam,":Missing dependency function: functions_common/dependencyLoader.R")));
        source('functions_common/dependencyLoader.R')
    }
    dep<-dependencyLoader(c('stringr'))
    if(is.null(variables)||is.na(variables)) return(list(error=TRUE,data=NULL,msg="no variables available."))
    #variables<-'var8,var3,amigo_avg,temperatura_min_a,temperatura_min,var7'
    #include_variables<-'var1,^.*_avg$,^.*_min,var3'
    #check if it is a comma separated
    if(length(variables)==1) variables<-unlist(strsplit(variables,split=','))
    
    #Check include variables
    if(!is.null(include_variables)&&!is.na(include_variables)&&(nchar(include_variables)>1)){ #Defined include variables
        variables<-variable_helper(variables,include_variables,'include')
    }
    
    #Check exclude variables
    if(!is.null(exclude_variables)&&!is.na(exclude_variables)&&(nchar(exclude_variables)>1)){ #Defined exclude variables
        variables<-variable_helper(variables,exclude_variables,'exclude')
    }
    
    #Check if date_time is in selected columns, if not append it.
    if(!(date_time_name %in% variables)){variables<-c(date_time_name,variables)}
    #Check if target variable is not null it must be on include
    if(!is.null(target_name)&&!is.na(target_name)&&!(target_name %in% variables)) variables<-c(variables,target_name)
    
    return(list(error=FALSE,data=variables,msg="ok"))
}