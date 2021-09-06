update_report_path <- function(df,table_config, windfarms_path,db_config) {
    updated_df <- df
    id_for_update <- which(is.na(df$report_path))
    
    if( length(id_for_update) > 0 ) {
        report_path <- paste0(windfarms_path,df$wp_code,"/reports/")
        for( i in id_for_update ) {
            updated_df[i, "report_path"] <- report_path[i]
            update_value(row_id = df$id[i], var_name = "report_path", 
                         value = report_path[i], table_name = table_config,db_config=db_config)
        }
    }
    return(updated_df)
}