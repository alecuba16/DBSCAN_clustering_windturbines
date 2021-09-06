# Date-time pattern to add to file names
get_time_mark <- function(unix_timestamp_ini) {
  return(gsub(":| ","-",substr(as.POSIXct(unix_timestamp_ini,origin="1970-01-01",tz="UTC"),1,16)))
}
