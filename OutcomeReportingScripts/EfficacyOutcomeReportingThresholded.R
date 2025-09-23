#EfficacyOutcomeReportingThresholded.R

#SummarizeChange.R

timer.start<-Sys.time()

#scripts and important reference layers are in the github repo
loc.scripts<-"D:/GitRepos/BattlesLabRepos/Task-Force-Action-Plan-Support/"
#large files are on local (large) hard drives, not dropbox or github
loc.data<-"D:/GIS_Large_Files/"
#output sent to Dropbox so it can be accessed from anywhere while big runs are going
loc.output<-"D:/DropboxFiles/Dropbox/Professional/UCB_Battles/ActionPlanSupport/"

setwd(loc.output)
#this is where the list lives of activities that pertain to which policy questions
source(paste(loc.scripts,"FunctionLibraries/ActivityList.R",sep=""))
#all the functions used to do various outcome reporting and some scenario modeling calcs
source(paste(loc.scripts,"FunctionLibraries/SummarizeChange_functions.R",sep=""))

reference.rast<-rast(paste(loc.data,"CECS_Data/CECS_CAWide_Vulner_TreeDieoff_SPI-2_2020_V250614.tif",sep=""))

# CECS layers all have the same CRS and extent
cecs.rast<-rast(paste(loc.data,"CECS_Data/CECS_CAWide_Vulner_TreeDieoff_SPI-2_2020_V250614.tif",sep=""))
# whp is the same projection as state boundaries/TF regions, WHR/veg classifications
whp.rast <- rast(paste(loc.data,"PriorityLayers/whp_classified_20240906.tif",sep="")) 

############### GLOBAL PARAMETERS ###################

#date stamp of this set of results - appended to all outputs to avoid overwriting older versions
datetime<-"2025Sept22_thresholded"

#ending year of water year

start.year<-"2020"
end.year<-"2024"

#water year
start.y<-paste(start.year,"-09-30",sep="")
end.y<-paste(end.year,"-10-01",sep="")

#metrics<-c( "FlameLengthWUI",
#			"FlameLengthLandscape",
#			"FlameLengthUtilities",
#			"FlameLengthRoads",
#			"DroughtVulnerability", 
#			"Shrub-GrassRatio")

metrics<-c( "GrassProportion")


#these should match the metrics and are for making sure
#to read in the correctly aggregated vector which was
#filtered for the appropriate treatment types
#policy.target<-c("WildlandFireRisk",
#			"WildlandFireRisk",
#			"WildlandFireRisk",
#			"WildlandFireRisk",
#			"ForestHealth",
#			"ShrublandHealth")

policy.target<-c("ShrublandHealth")

############ END GLOBAL PARAMETERS #################


##########################################################
##########################################################
########### READ IN AND PROCESS RASTERS ##############

calculate.metrics<-FALSE

if(calculate.metrics){
    rast.time<-system.time(source(paste(loc.scripts,"FunctionLibraries/CalculateEfficacyThresholdedLayers.R",sep="")))
	print(paste("Time to process and difference rasters: ",round(rast.time[[1]]/60)," minute(s)", sep=""))
}


########### END READ IN AND PROCESS RASTERS ##############
##########################################################
##########################################################


########### READ IN AND PROCESS VECTORS ##############


 #loop through all california, and the four regions
 boundary.shape<-c(paste(loc.scripts,"ReferenceFiles/CA_State.shp",sep=""),
                   paste(loc.scripts,"ReferenceFiles/Region_SouthernCA.shp",sep=""),
                   paste(loc.scripts,"ReferenceFiles/Region_Sierra.shp",sep=""),
                   paste(loc.scripts,"ReferenceFiles/Region_NorthernCA.shp",sep=""),
                   paste(loc.scripts,"ReferenceFiles/Region_CentralCoast.shp",sep=""))
 boundary.name<-c("CA","South","Sierra","North","Central")

nice.boundary.name<-c(
				  "All of California",
				  "Southern California",
				  "Sierra Nevada Region",
				  "Northern California",
				  "Central Coast Region")

# vect.shape<-c(paste(loc.scripts,"ReferenceFiles/HUC12.shp",sep=""))
# vect.name<-c("HUC12")

######################################
##         PREP VECTORS            ###
######################################


aggregate.vectors<-FALSE

#this recalculates all the possible aggregations of the vectors, needs redoing if summary unit
#changes, or regions/boundary vectors change, or new treatments/fires need to be used
#or if we change what the treatment type filters are
if(aggregate.vectors){
    vect.time<-system.time(source(paste(loc.scripts,"FunctionLibraries/CalculateAggregatedPatches.R",sep="")))
	print(paste("Time to process and aggregate vectors: ",round(vect.time[[1]]/60)," minute(s)", sep=""))
}


library("exactextractr")
library("sf")

fire.areas<-data.frame(Region=character(),Area_ac=numeric(),stringsAsFactors=FALSE)
treatment.areas<-data.frame(Region=character(),metric=character(),Area_ac=numeric(),stringsAsFactors=FALSE)
count<-1

#calculate areas of fires and treatments (for st_area, documentation says it uses units of the CRS if it's projected)
for(k in 1:length(boundary.name)){ #loop through the extents e.g. all CA or each region
	print(paste("Start ", boundary.name[k]," loop"))

	agg.fire.sf<-st_read(paste(loc.data,"IntermediateFiles/AggregatedVectors/Fires_",
				boundary.name[k],"_",start.year,"_",end.year,".shp",sep=""))
	agg.fire.proj.sf<-st_transform(agg.fire.sf, st_crs(cecs.rast))

	fire.areas[k,]<-c(boundary.name[k],st_area(agg.fire.proj.sf)*0.000247105 )

	for(i in 1:length(metrics)){
		#choose the correct metric
		metric.name<-metrics[i]
		print(metric.name)

			agg.treat.sf<-st_read(paste(loc.data,"IntermediateFiles/AggregatedVectors/Treatments_",
						boundary.name[k],"_",policy.target[i],"_",start.year,"_",end.year,".shp",sep=""))
			agg.treat.proj.sf<-st_transform(agg.treat.sf, st_crs(cecs.rast))

		treatment.areas[count,]<-c(boundary.name[k],metric.name,st_area(agg.treat.proj.sf)*0.000247105)
		count<-count+1

		}

	}

write.csv(fire.areas,"FireAreasByRegion_exclusive.csv")
write.csv(treatment.areas,"TreatmentAreasByRegionMetric_exclusive.csv")

for(k in 1:length(boundary.name)){ #loop through the extents e.g. all CA or each region
	print(paste("Start ", boundary.name[k]," loop"))

	boundary.sf<-st_read(boundary.shape[k])
	prepped.boundary.sf<-st_transform(boundary.sf, st_crs(cecs.rast))

#	zonal.summary.area.sf<-st_read(vect.shape)
#	zonal.summary.proj.sf<-st_transform(zonal.summary.area.sf, st_crs(cecs.rast))
#	prepped.zonal.summary.sf<-st_intersection(zonal.summary.proj.sf,prepped.boundary.sf)

	# agg.fire.huc.sf<-st_read(paste(loc.data,"IntermediateFiles/AggregatedVectors/Fires_",
	# 		boundary.name[k],"_",start.year,"_",end.year,"_HUC12.shp",sep=""))
	# agg.fire.huc.proj.sf<-st_transform(agg.fire.huc.sf, st_crs(cecs.rast))

	agg.fire.sf<-st_read(paste(loc.data,"IntermediateFiles/AggregatedVectors/Fires_",
				boundary.name[k],"_",start.year,"_",end.year,".shp",sep=""))
	agg.fire.proj.sf<-st_transform(agg.fire.sf, st_crs(cecs.rast))


	#############################################
	###  BEGIN LOOP THROUGH METRICS   ###########
	#############################################

	for(i in 1:length(metrics)){
		#choose the correct metric
		metric.name<-metrics[i]
		print(metric.name)

		#read in the appropriate raster(s)
		print(paste("Reading: ",loc.data,"IntermediateFiles/ThresholdedEfficacyLayers/Thresholded_",metric.name,"_",start.year,".tif",sep=""))
		bef.thr.rast<-rast(paste(loc.data,"IntermediateFiles/ThresholdedEfficacyLayers/Thresholded_",metric.name,"_",start.year,".tif",sep=""))
		print(paste("Reading: ",loc.data,"IntermediateFiles/ThresholdedEfficacyLayers/Thresholded_",metric.name,"_",end.year,".tif",sep=""))
		aft.thr.rast<-rast(paste(loc.data,"IntermediateFiles/ThresholdedEfficacyLayers/Thresholded_",metric.name,"_",end.year,".tif",sep=""))


		#read in vector files for summarizing/clipping

			agg.treat.sf<-st_read(paste(loc.data,"IntermediateFiles/AggregatedVectors/Treatments_",
						boundary.name[k],"_",policy.target[i],"_",start.year,"_",end.year,".shp",sep=""))
			agg.treat.proj.sf<-st_transform(agg.treat.sf, st_crs(cecs.rast))

			#'global' calcs that are at a whole-region level

			diffname<-paste(metric.name,start.year,end.year,sep="_")

			print(paste("Starting region-level proportion calcs for ",metric.name," in ",boundary.name[k],sep=""))
			all.global.results<-rbind(
				cbind(
				method="Global",
				metric=metric.name,
				boundary=boundary.name[k],
				fire.area=st_area(agg.fire.proj.sf)*0.000247105,
				treatment.area=st_area(agg.treat.proj.sf)*0.000247105,
				subset="WholeArea",
				before=exact_extract(bef.thr.rast,prepped.boundary.sf,fun="mean"),
				after=exact_extract(aft.thr.rast,prepped.boundary.sf,fun="mean")
				),
				cbind(
				method="Global",
				metric=metric.name,
				boundary=boundary.name[k],
				fire.area=st_area(agg.fire.proj.sf)*0.000247105,
				treatment.area=st_area(agg.treat.proj.sf)*0.000247105,
				subset="Treatments",
				before=exact_extract(bef.thr.rast,agg.treat.proj.sf,fun="mean"),
				after=exact_extract(aft.thr.rast,agg.treat.proj.sf,fun="mean")
				),
				cbind(
				method="Global",
				metric=metric.name,
				boundary=boundary.name[k],
				fire.area=st_area(agg.fire.proj.sf)*0.000247105,
				treatment.area=st_area(agg.treat.proj.sf)*0.000247105,
				subset="Fires",
				before=exact_extract(bef.thr.rast,agg.fire.proj.sf,fun="mean"),
				after=exact_extract(aft.thr.rast,agg.fire.proj.sf,fun="mean")
				)
			)

			all.global.results.df<-as.data.frame(all.global.results)
			all.global.results.df$percdiff<-(as.numeric(as.character(all.global.results.df$after))-
										 as.numeric(as.character(all.global.results.df$before)))/
										 as.numeric(as.character(all.global.results.df$before))

			print(paste("Writing to GlobalThresholdCalcOutput_",diffname,"_",boundary.name[k],"_",datetime,".csv",sep=""))
			write.table(all.global.results.df,paste("EfficacyResults/GlobalThresholdCalcOutput_",diffname,"_",boundary.name[k],"_",datetime,".csv",sep=""),
				sep = ",",quote = FALSE, col.names = TRUE, row.names = FALSE,na="NA") 


			# #huc-level results to look at

			# #have to read in the vector using sf instead of terra
			# agg.treat.huc.sf<-st_read(paste(loc.data,"IntermediateFiles/AggregatedVectors/Treatments_",
			# 			boundary.name[k],"_",policy.target[i],"_",start.year,"_",end.year,"_HUC12.shp",sep=""))
			# agg.treat.huc.proj.sf<-st_transform(agg.treat.huc.sf, st_crs(cecs.rast))

			# diffname<-paste(metric.name,start.year,end.year,sep="_")

			# print(paste("Starting huc-level proportion calcs for ",metric.name," in ",boundary.name[k],sep=""))
			#  all.zonal.results<-rbind(
			# cbind(
			# 	method=rep("Zonal",nrow(prepped.zonal.summary.sf)),
			#  	metric=rep(metric.name,nrow(prepped.zonal.summary.sf)),
			#  	boundary=rep(boundary.name[k],nrow(prepped.zonal.summary.sf)),
			#  	subset=rep("WholeArea",nrow(prepped.zonal.summary.sf)),
			#  	shapeID=prepped.zonal.summary.sf[,"huc12"],
			# 	before=exact_extract(bef.thr.rast,prepped.zonal.summary.sf,fun="mean"),
			# 	after=exact_extract(aft.thr.rast,prepped.zonal.summary.sf,fun="mean")),
			# cbind(
			# 	method=rep("Zonal",nrow(agg.fire.huc.proj.sf)),
			#  	metric=rep(metric.name,nrow(agg.fire.huc.proj.sf)),
			#  	boundary=rep(boundary.name[k],nrow(agg.fire.huc.proj.sf)),
			#  	subset=rep("Fire",nrow(agg.fire.huc.proj.sf)),
			#  	shapeID=agg.fire.huc.proj.sf[,"huc12"],
			# 	before=exact_extract(bef.thr.rast,agg.fire.huc.proj.sf,fun="mean"),
			# 	after=exact_extract(aft.thr.rast,agg.fire.huc.proj.sf,fun="mean")),
			# cbind(
			# 	method=rep("Zonal",nrow(agg.treat.huc.proj.sf)),
			#  	metric=rep(metric.name,nrow(agg.treat.huc.proj.sf)),
			#  	boundary=rep(boundary.name[k],nrow(agg.treat.huc.proj.sf)),
			#  	subset=rep("Treatments",nrow(agg.treat.huc.proj.sf)),
			#  	shapeID=agg.treat.huc.proj.sf[,"huc12"],
			# 	before=exact_extract(bef.thr.rast,agg.treat.huc.proj.sf,fun="mean"),
			# 	after=exact_extract(aft.thr.rast,agg.treat.huc.proj.sf,fun="mean"))
			# )

			#  all.zonal.results$before[all.zonal.results$before==0]<-NA
			# all.zonal.results$percdiff<-(all.zonal.results$after-all.zonal.results$before)/all.zonal.results$before

			# all.zonal.results.df<-as.data.frame(all.zonal.results)

 			#  print(paste("Writing to ZonalThresholdCalcOutput_",diffname,"_",boundary.name[k],"_",datetime,".csv",sep=""))
			#  write.table(as.data.frame(all.zonal.results.df),paste("ZonalThresholdCalcOutput_",diffname,"_",boundary.name[k],"_",datetime,".csv",sep=""),
			#  	sep = ",",quote = FALSE, col.names = TRUE, row.names = FALSE,na="NA")

			#  print(paste("Writing to ZonalThresholdCalcOutput_",diffname,"_",boundary.name[k],"_",datetime,".shp",sep=""))
			#  st_write(all.zonal.results, paste("EfficacyResults/ZonalThresholdCalcOutput_",diffname,"_",boundary.name[k],"_",datetime,".shp",sep=""))


	}

}


timer.end<-Sys.time()

time.total<-timer.end-timer.start
print(time.total)

		global.ca<-read.csv(paste("GlobalThresholdCalcOutput_",diffname,"_",boundary.name[1],"_",datetime,".csv",sep=""))
		global.sc<-read.csv(paste("GlobalThresholdCalcOutput_",diffname,"_",boundary.name[2],"_",datetime,".csv",sep=""))
		global.sn<-read.csv(paste("GlobalThresholdCalcOutput_",diffname,"_",boundary.name[3],"_",datetime,".csv",sep=""))
		global.nc<-read.csv(paste("GlobalThresholdCalcOutput_",diffname,"_",boundary.name[4],"_",datetime,".csv",sep=""))
		global.cc<-read.csv(paste("GlobalThresholdCalcOutput_",diffname,"_",boundary.name[5],"_",datetime,".csv",sep=""))

		all.global.results<-rbind(global.ca,global.sc,global.sn,global.nc,global.cc)
		print(all.global.results)


#################### DATA VISUALIZATIONS ###################

timer.start<-Sys.time()

datetimevis<-"2025Aug21_thresholded"

nice.metric.name<-c("Likely High Severity\nFire in WUI",
					"Likely High Severity\nFire Across Landscape",
					"Likely High Severity\nFire in Utility Corridors",
					"Likely High Severity\nFire in Road Corridors",
					"Imminent Forest\nMortality",
					"Grass-dominated\nShrublands")

require(tidyterra)
require(tidyr)

ca.vect<-vect(paste(loc.scripts,"ReferenceFiles/CA_State.shp",sep=""))
ca.cecs.vect<-check.crs.match(cecs.rast,ca.vect)

for(k in 1:length(boundary.name)){

	for(i in 1:length(metrics)){
		#choose the correct metric
		metric.name<-metrics[i]
		print(metric.name)

		diffname<-paste(metric.name,start.year,end.year,sep="_")

		global.result<-read.csv(paste("GlobalThresholdCalcOutput_",diffname,"_",boundary.name[k],"_",datetimevis,".csv",sep=""))

		global.result$subset[global.result$subset=="WholeArea"]<-"Region"
		global.result$subset[global.result$subset=="Fires"]<-"Fire\nFootprint"
		global.result$subset[global.result$subset=="Treatments"]<-"Treated\nAreas"
		global.result$subset<-factor(global.result$subset,c("Region","Treated\nAreas","Fire\nFootprint"))

		global.result$percent<-100*global.result$percdiff
		global.result$before.rnd<-paste("Initial\nProportion:\n",round(global.result$before,2))

		#this is your main result for the efficacy modeling
		plot.title<-paste("Percent Change in ",nice.metric.name[i], " (",nice.boundary.name[k],")", sep="")
		bar.plt<-ggplot(data=global.result, aes(x=subset,fill=subset,y=percent)) +
		  geom_bar(stat="identity")+
		  theme(legend.position="none")+
	      labs(title = plot.title,x = element_blank(), y = "Percent Difference 2020-2024")+
		  scale_fill_manual(values=c("#E9E5C3","#9C8F57","#9F2214"))#+
		  #geom_text(aes(label=before.rnd), vjust=-0.5, color="black", size=3.5)
		pltnm.b<-paste("EfficacyResults/PercentDiff_bar_", metric.name,"_",boundary.name[k],".png",sep="")
	  	ggsave(pltnm.b, units="in", width=4,height=3)

	  	# #this is just making maps and histograms for us to sort of take a closer look as needed
		# all.zonal.results.vect<-vect(paste("ZonalThresholdCalcOutput_",diffname,"_",boundary.name[k],"_",datetimevis,".shp",sep=""))

		# legend.title<-"Percent Difference"
		# plot.title<-paste(legend.title, " in ", metric.name, " (",boundary.name[k],")", sep="")

	    # ggplt <- ggplot()+
	    #    geom_spatvector(data=ca.cecs.vect, lwd=1)+
	    #    geom_spatvector(data=all.zonal.results.vect,aes(fill=percdff),lwd = 0,col=NA)+
	    #          scale_fill_viridis_c(na.value = "white") +
	    #    theme(text=element_text(size=12, family="Century Gothic"))+
	    #    theme (legend.text = element_text(size =12))+
	    #    theme (legend.title = element_text (size = 14))+
	    #    labs(title = plot.title, fill=legend.title)+
	    #    theme_void()
	   	# pltnm<-paste("EfficacyResults/PercentDiff_Map_", metric.name,"_",boundary.name[k],".png",sep="")
	 	# ggsave(pltnm)


		# hist.plt<-ggplot(data=all.zonal.results.vect, aes(x=percdff)) +
		#   geom_histogram()+
		#   facet_grid(subset~.)
	    #   labs(title = plot.title)+
		#   scale_fill_viridis_d(end = 0.8, begin = 0.2, direction=-1, option = "viridis")#+
		# pltnm.h<-paste("EfficacyResults/PercentDiff_hist_", metric.name,"_",boundary.name[k],".png",sep="")
		# ggsave(pltnm.h)

	}

}




########### END DATA VISUALIZATIONS ##############

timer.end<-Sys.time()

time.vis<-timer.end-timer.start
print(time.vis)


