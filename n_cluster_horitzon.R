n_cluster_horitzon<-function(alarm,clusters,alarms_date_time,horizon){
    days_before<-as.POSIXct(as.numeric(alarms_date_time)-(horizon*24*60*60),origin='1970-01-01')
    date_between<-alarm$date_time[alarm$date_time >= days_before & alarm$date_time <= alarms_date_time]
    #current_clusters<-data.frame(cluster=clusters[alarm$date_time >= days_before & alarm$date_time <= alarms_date_time],date_time=date_between)
    current_clusters<-data.frame(cluster=clusters,date_time=alarm$date_time)
    
    current_clusters$date_time<-as.Date(format(current_clusters$date_time, '%Y-%m-%d'))
    cluster_count<-table(current_clusters)
    cluster_count<-as.data.frame(cluster_count)
    cluster_count$cluster<-as.numeric(cluster_count$cluster)-1
    cluster_count$date_time<-as.Date(cluster_count$date_time)
    cluster_count$Freq<-cluster_count$Freq
    cluster_count$alarm_date_time<-alarms_date_time
    cluster_count$days_before<-days_before
    cluster_count$horizon<-horizon
    cluster_count$generated_date<-as.POSIXct(cluster_count$date_time-((cluster_count$cluster+1)),origin='1970-01-01')
    return(list(error=FALSE,data=cluster_count,msg="ok"))
}