detach("package:dplyr")
library(RMySQL)
library(dplyr)
source('functions_common/db_query.R')
events <- read.csv("ESCAMB_20160101_Alarmas_2.csv")
head(events)
events <- rename(events, model = turbina, id_walm = codigo, date_time = fechahora, descripcio_walm = descripcion, status = evento)
head(events)
events <- select(events, model, id_walm, descripcio_walm, date_time, status)
head(events)
unique(events$status)



db_config_data <- data.frame(user='user',
                       password='password',
                       dbname='yourHistoricalDB', 
                       host='127.0.0.1'
                       port=3306)
wp_id <- 19
query <- paste0("SELECT ld_id, ld_code, wp_id FROM smartcast_DB.SC_LOGICALDEVICE WHERE wp_id = ",wp_id)
model_dic <- db_query(query, db_config = db_config_data)$data
head(model_dic)

events <- merge(events, select(model_dic, ld_id, model = ld_code) , by = "model", all.x = T, all.y = F) 
events <- select(events, ld_id, model:status)
head(events)
events$date_time <- as.POSIXct(events$date_time, tz = "UTC", format = "%d/%m/%Y %H:%M")
head(events)


group_by(events, ld_id) %>% summarise(min(date_time, na.rm = T), max(date_time, na.rm = T), n())

events <- arrange(events, ld_id, date_time, desc(status))
head(events)

con <- dbConnect( MySQL(),
                  user=db_config_data$user,
                  password=db_config_data$password,
                  dbname=db_config_data$dbname, 
                  host=db_config_data$host,
                  port=db_config_data$port)
# Write the full dataframe all at once.    
ok_write_table <- try(dbWriteTable( con, value = events, name = "escamb_awp3k_events", append = TRUE, row.names = FALSE ))
# Check for errors
if( inherits(ok_write_table, "try-error") ) {
    dbDisconnect( con )
    output_msg <- paste0("\nError writing in table: escamb_awp3k_events")
    cast(output_msg)
}
dbDisconnect( con )
