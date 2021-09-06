update_null_values <- function(table_name, table_dic, df,var_names = c("wp_id", "wp_code", "wp_desc"),db_config) {
    source('functions_common/db_check_dic.R')
    source('functions_common/update_value.R')
    updated_df <- df
    for( i in 1:nrow(df) ) {
        key_name <- var_names[!is.na(df[i,var_names])][1]
        key_value <- df[i,key_name]
        for( var_target in var_names ) {
            if( is.na(df[i,var_target]) ) {
                new_value <- db_check_dic(table_dic = table_dic, 
                                       desired_variable = var_target, 
                                       key_name = key_name, 
                                       key_value = key_value,
									   db_config=db_config)$data
                updated_df[i, var_target] <- new_value
                update_value(row_id = df$id[i], var_name = var_target, 
                             value = new_value, table_name = table_name, 
                             db_config=db_config)
            }
        }
    }
    return(updated_df)
}
