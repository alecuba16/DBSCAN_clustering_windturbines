#!/usr/bin/env Rscript
# Todo , el log debe decir rows en vez de register y 23 variables of total of 66 vars...
# Poner en el rmarkdown los puntos outliers en rojo.
# Para ejecutar desde linea de comandos dejando log de warnings y errores en
# models_main.Rout:
# R CMD BATCH --no-save --no-restore models_main.R
# with args:
# R CMD BATCH --no-save --no-restore '--args parallel=TRUE' models_main.R
# Para ejecutar igual per imprimiendo warnings y errores a pantalla:
# Rscript models_main.R

#Patch for use my local db
if(Sys.info()["nodename"] == "alexsmartive"){
    db_config_data<- data.frame(user='user',
                                password='passwd',
                                dbname='example',
                                host='127.0.0.1',
                                port=3306
    )
}

#Parsear argumentos si es por script tipo etc
args=(commandArgs(TRUE))
if(length(args)>0){
    for(i in 1:length(args)){
        eval(parse(text=args[[i]]))
    }
}
# Reset sink() (sink define the file output for messages. It should start at 0)
while(sink.number() > 0) sink()

Sys.setenv(TZ='UTC')
TZ='UTC'
iam='models_main'
setwd(".") #asumimos que debajo esta functions/ etc.
#If not set debug mode manually....
if(!exists("debug_mode")||(exists("debug_mode")&&!is.logical(debug_mode))) debug_mode <- FALSE # If true, messages will be displayed directly to console and not in log file.
if(!exists("force_regenerate")||(exists("force_regenerate")&&!is.logical(force_regenerate))) force_regenerate <- FALSE # If true, will regenerate the model event if it exists.
if(!exists("parallel")||(exists("parallel")&&!is.logical(parallel))) parallel <- FALSE # true if want parallelism
if(!exists("table_cast_config")||(exists("table_cast_config")&&!is.character(table_cast_config))) table_cast_config <- '1_cast_config'  # cast config table
if(!exists("table_filter_config")||(exists("table_filter_config")&&!is.character(table_filter_config))) table_filter_config <- '1_filter_config'  # cast config table
if(!exists("table_cast_park_dic")||(exists("table_cast_park_dic")&&!is.character(table_cast_park_dic))) table_cast_park_dic <- '1_cast_park_table_dic' # cast config dic table
if(!exists("type")||(exists("type")&&!is.character(type))) type <- 'clustering'  # cast config table
if(!exists("date_time_name")||(exists("date_time_name")&&!is.character(date_time_name))) date_time_name <- 'date_time'

# DB data
if(!exists("db_config_data")||(exists("db_config_data")&&!is.data.frame(db_config_data)))
    db_config_data<- data.frame(user='user',
                                password='password',
                                dbname='yourHistoricalBD',
                                host='yourHost',#'127.0.0.1', #
                                port=3306)

if(!exists("db_config_realtime")||(exists("db_config_realtime")&&!is.data.frame(db_config_realtime)))
    db_config_realtime<- data.frame(user='user',
                                    password='password',
                                    dbname='smartcast_DB',
                                    host='yourHost',
                                    port=3306)

default_path <- "windfarms/" # Default directory to store engines made by brain0 or brain1. The "/" at the end is obligatory.



daily_alarms_threshold <- 0 # Define el umbral para decidir si necesita fusi?n de datos (<) o no (>=)

if(!debug_mode)
    sink("models_main.log", append = TRUE)
t_ini<-as.POSIXct(Sys.time(),tz=TZ,origin="1970-01-01");
t_ini_madrid<-t_ini
attr(t_ini_madrid, "tzone") <- "Europe/Madrid"
cat(paste0("\n\n>>------------------- INI models_main ",t_ini," UTC (",t_ini_madrid," Madrid ) ----------------------->>\n"))

#Basic dependency
if(!exists("dependencyLoader")){
    if(!file.exists('functions_common/dependencyLoader.R')) return(list(error=TRUE,data=NULL,msg="Missing dependency function: functions_common/dependencyLoader.R"));
    source('functions_common/dependencyLoader.R')
}
dep<-dependencyLoader(c('RMySQL','parallel','doParallel',
                        'functions_common/getModelsToBeCreated.R',
                        'brain3.R',
                        'functions_common/db_query.R',
                        'functions_common/ranking_by_id_walm.R',
                        'functions_common/need_fusion.R',
                        'functions_common/get_most_unhealthy.R',
                        'functions_common/create_model.R',
                        'functions_common/close_protocol.R'))

if(dep$error)  return(list(error=TRUE,data=NULL,msg=paste0(iam,":on call dependencyLoader\n",dep$msg)))
brain<-brain3

if(force_regenerate) #Force generate the model even there are created models.
    r2<-db_query(query=paste0("UPDATE ",table_cast_config," SET creation_date_ini=NULL,creation_date_end=NULL,creation_model_path=\"windfarms/\",creation_log_path=\"windfarms/\",creation_error=0 WHERE `type`='",type,"' AND creation_enable=1;"),db_config=db_config_data);

models<-getModelsToBeCreated(table_cast_config,force_regenerate,type=type,db_config=db_config_data)

if(models$error) {
    output_msg <- models$msg
    close_protocol(output_msg, iam, debug_mode, TZ = "UTC")
    stop(output_msg);
}

if(nrow(models$data)<=0) {
    output_msg <- "\nNo models to be generated, check DB.\n"
    close_protocol(output_msg, iam, debug_mode, TZ = "UTC")
    stop(output_msg);
}

n_models<-nrow(models$data)
if(n_models>0){
    #------- Parallel setup ----------
    if(parallel&&!debug_mode){
        cores<-floor(parallel:::detectCores()/1.5) #Model in memory uses about 700-800MB,1.5 is corrector factor.
        cl <- makePSOCKcluster(min(cores,n_models),outfile='models_main.log')
        setDefaultCluster(cl)
        clusterExport(cl=cl,c('brain','create_model','db_query','TZ','ranking_by_id_walm','need_fusion','models','daily_alarms_threshold','debug_mode','force_regenerate','parallel','table_cast_config','type','table_filter_config','table_cast_park_dic','date_time_name','db_config_data','db_config_realtime'))
        #------- End parallel setup --------
        rs<-parSapply(cl, 1:nrow(models$data), function(i) create_model(brain=brain,m=models$data[i,],daily_alarms_threshold=daily_alarms_threshold,debug_mode=debug_mode,force_regenerate=force_regenerate,parallel=parallel,table_cast_config=table_cast_config,type=type,table_filter_config=table_filter_config,table_cast_park_dic=table_cast_park_dic,date_time_name=date_time_name,db_config_data=db_config_data,db_config_realtime=db_config_realtime))
        stopCluster(cl)
    }else{
        for (i in 1:nrow(models$data)) {#Old for loop before parallel
            create_model(brain=brain,m=models$data[i,],daily_alarms_threshold=daily_alarms_threshold,debug_mode=debug_mode,force_regenerate=force_regenerate,parallel=parallel,table_cast_config=table_cast_config,type=type,table_filter_config=table_filter_config,table_cast_park_dic=table_cast_park_dic,date_time_name=date_time_name,db_config_data=db_config_data,db_config_realtime=db_config_realtime)
        }
    }
    
}else{
    cat("No models to be generated, check DB")
}

output_msg <- "\nDONE.\n"
close_protocol(output_msg, iam, debug_mode, TZ = "UTC")