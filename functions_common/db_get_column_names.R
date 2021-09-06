db_get_column_names <- function(wp_code, db_config) {
    source("functions_common/db_check_dic.R")
    source("functions_common/db_query.R")
    data_table_name <- db_check_dic('1_cast_park_table_dic','data_table_name','wp_code', wp_code, db_config = db_config)$data[1]
    query <- paste0("SHOW COLUMNS FROM ", data_table_name)
    column_names <- db_query(query = query, db_config = db_config)
    column_names <- column_names$data$Field
    # to_exclude <- grep(pattern = "^Tot|^Cont|^FrecRed|^CosPhi|^Estado|^Top", column_names, value = T)
    # wtdata <- wtdata0[,!(names(wtdata0) %in% to_exclude)]
    return(column_names)
}