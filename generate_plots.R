#require(devtools)
#devtools::install_github("richardjtelford/ggbiplot", ref = "experimental")

#library(ggbiplot)

Sys.setenv(TZ='UTC')
db_config<- data.frame(user='user',
                       password='password',
                       dbname='yourHistoricalBD',
                       host='127.0.0.1',
                       port=3306)
if(!exists("dependencyLoader")){
    if(!file.exists('functions_common/dependencyLoader.R')) return(list(error=TRUE,data=NULL,msg=paste0("\n",iam,":Missing dependency function: functions_common/dependencyLoader.R")));
    source('functions_common/dependencyLoader.R')
}

if(!exists("createFile.R")){
    if(!file.exists('functions_common/createFile.R')) return(list(error=TRUE,data=NULL,msg=paste0("\n",iam,":Missing dependency function: functions_common/createFile.R")));
    source('functions_common/createFile.R')
}

if(!exists("dirname2")){
    if(!file.exists('functions_common/dirname2.R')) return(list(error=TRUE,data=NULL,msg=paste0("\n",iam,":Missing dependency function: functions_common/dirname2.R")));
    source('functions_common/dirname2.R')
}

# Sources
libraries<-c('RMySQL','plyr','dplyr','rmarkdown','ggplot','grid','gridExtra')
sources<-paste0("functions_common/",
                c('load_wtdata.R','close_protocol.R','filter_custom.R'))
dep<-dependencyLoader(c(libraries,sources))
if(dep$error)  return(list(error=TRUE,data=NULL,msg=paste0("\n",iam,":on call dependencyLoader\n",dep$msg)))

debug_mode<-TRUE

#2014->2015
dates<-matrix(c(ini=1388534400,ini=1420070400,end=1420070399,end=1451606399),nrow=2,ncol=2)
#dates<-matrix(c(ini=1388534400,ini=1420070400,end=1388634400,end=1420170400),nrow=2,ncol=2)

seconds_to_aggregate<-600
template_file<-"park_pca.Rmd"

#izco
parks<-data.frame(
    ld_id=seq(167,216),
     wp_id=20,
        wp_code='Izco',
        fault='gbox1',
        array_id_walm='',
        include_variables='alarm,AngPitch_avg,CmdWTG_avg,ContEnerActiva_avg,ContLineaOk_avg,ContTurbinaOk_avg,FrecGen_avg,FrecRed_avg,Grd_Est2_avg,Grd_Est3_avg,IndTurb_avg,IndTurbNorma_avg,Pot_avg,PotEstator_avg,PotRotor_avg,PresGH_avg,SobreVelMec_avg,SPPitch_avg,SPVelRotor_avg,TempAceiteGH_avg,TempAceiteMultip_avg,TempAmb_avg,TempCojLA_avg,TempCojLOA_avg,TempGen_avg,TempGond_avg,TempMultip_avg,TempRadiad_avg,TempTrafo1_avg,TempTrafo2_avg,TempTrafo3_avg,TensGen_avg,TensRed_avg,Top_Alm3_avg,Top_Alm4_avg,TotPotReact_avg,VelGenGround_avg,VelGenTop_avg,VelPitch_avg,VelRotor_avg,VelViento_avg,AngPitch_min,CmdWTG_min,ContEnerActiva_min,ContLineaOk_min,ContTurbinaOk_min,FrecGen_min,FrecRed_min,Grd_Est2_min,Grd_Est3_min,IndTurb_min,IndTurbNorma_min,Pot_min,PotEstator_min,PotRotor_min,PresGH_min,SobreVelMec_min,SPPitch_min,SPVelRotor_min,TempAceiteGH_min,TempAceiteMultip_min,TempAmb_min,TempCojLA_min,TempCojLOA_min,TempGen_min,TempGond_min,TempMultip_min,TempRadiad_min,TempTrafo1_min,TempTrafo2_min,TempTrafo3_min,TensGen_min,TensRed_min,Top_Alm3_min,Top_Alm4_min,TotPotReact_min,VelGenGround_min,VelGenTop_min,VelPitch_min,VelRotor_min,VelViento_min,AngPitch_max,CmdWTG_max,ContEnerActiva_max,ContLineaOk_max,ContTurbinaOk_max,FrecGen_max,FrecRed_max,Grd_Est2_max,Grd_Est3_max,IndTurb_max,IndTurbNorma_max,Pot_max,PotEstator_max,PotRotor_max,PresGH_max,SobreVelMec_max,SPPitch_max,SPVelRotor_max,TempAceiteGH_max,TempAceiteMultip_max,TempAmb_max,TempCojLA_max,TempCojLOA_max,TempGen_max,TempGond_max,TempMultip_max,TempRadiad_max,TempTrafo1_max,TempTrafo2_max,TempTrafo3_max,TensGen_max,TensRed_max,Top_Alm3_max,Top_Alm4_max,TotPotReact_max,VelGenGround_max,VelGenTop_max,VelPitch_max,VelRotor_max,VelViento_max',
        exclude_variables='model,fake_data,n1,alarm_all,alarm_block_code,alarm_all_block_code,ot,ot_all,ot_block_code,ot_all_block_code',
        array_ot='',
 target='alarm',stringsAsFactors=FALSE)
#parks<-ifelse(exists("parks"),rbind(parks,izco),izco)

#moncayuelo
# moncay<-data.frame(
#     ld_id=seq(135,166),
#     wp_id=21,
#     wp_code='Moncay',
#     fault='gbox1',
#     array_id_walm="1806,794,1823,732,1848,1839",
#     array_ot="10067,10068",
#     target='ot',stringsAsFactors=FALSE)
# parks<-ifelse(exists("parks"),rbind(parks,moncay),moncay)

park_list<-unique(parks$wp_id)
for(pid in 1:length(park_list)){
    for(date in 1:nrow(dates)){
        park_turbines<-parks[parks$wp_id==park_list[pid],]
        turbines<- list()
        pos<-1
        fault<-park_turbines$fault[1]
        target_name<-park_turbines$target_name[1]
        for(pos in 1:length(park_turbines$ld_id)){
            ld_id<-park_turbines$ld_id[pos]
            current_turbine<-park_turbines[pos,]
            cat(paste0("Turbine ",ld_id))
            rs  <-  load_wtdata(
                ld_id=ld_id,
                wp_id=current_turbine$wp_id,
                wp_code=current_turbine$wp_code,
                fault=current_turbine$fault,
                array_id_walm=current_turbine$array_id_walm,
                array_ot=current_turbine$array_ot,
                power_condition='',
                include_variables=current_turbine$include_variables,
                exclude_variables=current_turbine$exclude_variables,
                unix_timestamp_ini=dates[date,1],
                unix_timestamp_end=dates[date,2],
                freq_dat_med_min=10,
                seconds_to_aggregate=seconds_to_aggregate,
                seconds_offset=0,
                date_time_name='date_time',
                target=current_turbine$target,
                table_cast_park_dic='1_cast_park_table_dic',
                table_filter_config='1_filter_config',
                filter='frange,fq3,fclean,fnzv',
                filter_exclude=paste("date_time,ld_id,alarm,alarm_block_code,alarm_all,alarm_all_block_code,ot,ot_block_code,ot_all,ot_all_block_code,n1,weekly_n1,weekly_power",sep=","),
                update_filter_ranges=FALSE,
                db_config=db_config)
            if(rs$error) {
                output_msg <- paste0("\n",iam,":on call load_wtdata\n\t",rs$msg)
                close_protocol(output_msg, iam, debug_mode)
                return(list(error=TRUE,data=NULL,msg=output_msg))
            }
            wtdata<-rs$data$wtdata
            vars<-names(wtdata[,-which(names(wtdata) %in% c("date_time","ld_id","alarm"))])
            year<-format(as.POSIXct(dates[date,1],origin='1970-01-01',TZ='UTC'),"%Y")
            #Uncommet for plot variable distributions
            for(var in vars){
                currentVar<-na.omit(wtdata[,c(var,"date_time")]);
                datetime<-as.POSIXct(currentVar[,"date_time"]);
                currentVarOnly<-as.numeric(currentVar[,var]);
                
                plot1<-qplot(currentVarOnly, geom="histogram",xlab = var,na.rm=TRUE,bins = 60)
                
                plot2<-ggplot(currentVar, aes(x=datetime,y=currentVarOnly) ,na.rm=TRUE) +
                    labs(title = "Time plot")+
                    labs(x ="Date" )+
                    labs(y =var )+
                    geom_line()
                
                g <- arrangeGrob(plot1, plot2,nrow=2) #generates g
                ggsave(filename=paste0("/home/alex/plots/",var,"_",ld_id,"_",year,".png"), g, width = 9, height = 13, dpi = 300) #saves g
            }
            
        }
    }
}
