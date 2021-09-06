update_value <- function(row_id, var_name, value, table_name, db_config) {
    source('functions_common/db_query.R')
    if(!is.numeric(value))
        value <- paste0("'",value,"'")
    query <- paste("UPDATE", table_name, "SET", var_name, "=", value, "WHERE id =",row_id)
    db_query(query=query, db_config=db_config)
}