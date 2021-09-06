close_protocol <- function(output_msg = "DONE.", iam, dbug_mode, TZ = "UTC") {
    cat(output_msg)
    
    t_fin<-as.POSIXct(Sys.time(),tz=TZ,origin="1970-01-01");
    t_fin_madrid<-t_fin
    attr(t_fin_madrid, "tzone") <- "Europe/Madrid"
    cat("\n\n",paste0("<<------------------- END ", iam, " at ", t_fin," UTC (",t_fin_madrid," Madrid ) -----------------------<<\n"))

    if(!dbug_mode)
        sink()
}