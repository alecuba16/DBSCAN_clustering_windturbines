get_ot_data <- function(ld_id = 126, wp_code = "escamb", date_start = "2015-01-01", date_end = "2016-01-01", db_config, group_ot = TRUE, key_word = NULL) {
    library(dplyr)
    source('functions_common/db_query.R')
    debug_mode <- FALSE
    query <- paste0("SELECT * 
                    FROM `1_cast_park_table_dic` WHERE `wp_code`='",wp_code,
                    "' ORDER BY `id` DESC LIMIT 1")
    rs<-db_query(query=query,  db_config=db_config)
    
    if(is.null(key_word)) {
        condition <- ""
    } else {
        condition <- paste0("    
            AND (desc_substma LIKE '%",key_word,"%'
            OR ot_description LIKE '%",key_word,"%'
            OR material_code LIKE '%",key_word,"%'
            OR problem_found LIKE '%",key_word,"%'
            OR problem_source LIKE '%",key_word,"%'
            OR problem_solution LIKE '%",key_word,"%'
            OR work_description LIKE '%",key_word,"%')")
    }
        
    
    query <- paste0("SELECT * 
                    FROM ",rs$data$ot_table_name," 
                    WHERE date_time BETWEEN '",date_start,"' AND '",date_end,"'
                    AND ld_id = ",ld_id,condition,";")
    
    rs<-db_query(query=query, db_config=db_config)
    
    rs$data$date_time <- as.POSIXct(rs$data$date_time,tz="UTC",origin="1970-01-01")
    rs$data$date_time_end <- as.POSIXct(rs$data$date_time_end,tz="UTC",origin="1970-01-01")
    if( !group_ot )
        return(rs$data)
    
    ot_data <- rs$data
    # ot_data_sum <- group_by(ot_data, ot, date_time, date_time_end, ot_type, ot_description, desc_substma) %>%
    #     summarise(problem = max(problem_found), source = max(problem_source),
    #               solution = max(problem_solution))
    ot_data_sum <- group_by(ot_data, ot, date_time) %>%
        summarise(date_time_end = max(date_time_end),
                  ot_type = max(ot_type), ot_description = max(ot_description),
                  desc_substma = max(desc_substma),
                  problem = max(problem_found), source = max(problem_source),
                  solution = max(problem_solution))
    return(ot_data_sum)
}