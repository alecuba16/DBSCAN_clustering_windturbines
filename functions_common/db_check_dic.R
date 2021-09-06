db_check_dic <- function(table_dic, desired_variable, key_name, key_value,db_config) {
    source("functions_common/db_query.R")
    #If is a factor change to character
    if(is.factor(key_value)) key_value<-as.character(key_value)
    
    if(is.character(key_value)&&!grepl(pattern = '^\'.*\'$',x=key_value)) #Check if text and is enclosed in quotes start end.
        key_value <- paste0("'",key_value,"'")

    query <- paste0("SELECT `", desired_variable,
                     "` FROM `",table_dic,
                    "` WHERE `",key_name,"`=",key_value,
                    " LIMIT 1;")
    
    equi_value <- db_query(query=query,db_config=db_config)
    return(equi_value)

}