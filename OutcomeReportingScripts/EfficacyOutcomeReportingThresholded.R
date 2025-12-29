#EfficacyOutcomeReportingThresholded.R

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
datetime<-"2025Dec26_NoFire"

#ending year of water year

start.year<-"2020"
end.year<-"2024"

#water year
start.y<-paste(start.year,"-09-30",sep="")
end.y<-paste(end.year,"-10-01",sep="")

metrics<-c( "FlameLengthWUI",
			"FlameLengthLandscape",
			"FlameLengthUtilities1000",
			"FlameLengthRoads",
			"DroughtVulnerability", 
			"GrassProportion-Roads",
			"GrassProportion-NonRoads",
			"BeneficialFireLandscape")

#these should match the metrics and are for making sure
#to read in the correctly aggregated vector which was
#filtered for the appropriate treatment types
policy.target<-c("WildlandFireRisk",
			"WildlandFireRisk",
			"WildlandFireRisk",
			"WildlandFireRisk",
			"ForestHealth",
			"ShrublandHealth",
			"ShrublandHealth",
			"WildlandFireRisk")

#these are for calculating the areas associated with each metric
 wui.cecs.rast<-rast(paste(loc.data,"WUIVegetationClassifications/FRAP24_WUIOnly_CECS.tif",sep=""))
 wui.cecs.rast[is.na(wui.cecs.rast)]<-0
 land.cecs.rast<-rast(paste(loc.data,"WUIVegetationClassifications/FRAP24_Landscape_CECS.tif",sep=""))
 land.cecs.rast[is.na(land.cecs.rast)]<-0
 forest.cecs.rast<-rast(paste(loc.data,"WUIVegetationClassifications/WHR13_RECLASS_FOREST_CECS.tif",sep=""))
 forest.cecs.rast.na<-forest.cecs.rast
 forest.cecs.rast[is.na(forest.cecs.rast)]<-0
 shrub.cecs.rast<-rast(paste(loc.data,"WUIVegetationClassifications/WHR13_RECLASS_SHRUB_CECS.tif",sep=""))
 shrub.cecs.rast.na<-shrub.cecs.rast
 shrub.cecs.rast[is.na(shrub.cecs.rast)]<-0
 road.buff.cecs.rast<-rast(paste(loc.data,"WUIVegetationClassifications/RoadBuffer_CECSproj.tif",sep=""))
 road.buff.cecs.rast[is.na(road.buff.cecs.rast)]<-0
 nonroad.buff.cecs.rast<-rast(paste(loc.data,"WUIVegetationClassifications/NonRoads_CECSproj.tif",sep=""))
 nonroad.buff.cecs.rast[is.na(nonroad.buff.cecs.rast)]<-0
 tran.buff.cecs.rast<-rast(paste(loc.data,"WUIVegetationClassifications/TransmissionLineBuffer1000_CECSproj.tif",sep=""))
 tran.buff.cecs.rast[is.na(tran.buff.cecs.rast)]<-0

#note that for flame length, only forest is really appropriate
spat.rast<-list(
				wui.cecs.rast*forest.cecs.rast,
				land.cecs.rast*forest.cecs.rast,
				tran.buff.cecs.rast*forest.cecs.rast,
				road.buff.cecs.rast*forest.cecs.rast,
				forest.cecs.rast,
				shrub.cecs.rast,
				shrub.cecs.rast*road.buff.cecs.rast,
				shrub.cecs.rast*nonroad.buff.cecs.rast,
				land.cecs.rast*forest.cecs.rast)


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

# For plots and print statements
 nice.boundary.name<-c(
 				  "All of California",
 				  "Southern California",
 				  "Sierra Nevada Region",
 				  "Northern California",
 				  "Central Coast Region")


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

###-------------------------------------------
### this section is to calculate areas of all treatments, statewide, only filtered by policy target/type of treatment

# policy.target<-c("WildlandFireRisk",
# 			"ForestHealth",
# 			"ShrublandHealth",
# 			"Habitat",
# 			"Water")

# metrics<-c( "FlameLengthLandscape",
# 			"DroughtVulnerability", 
# 			"GrassProportion",
# 			"CriticalHabitat",
# 			"DebrisFlow")


# # fire.areas<-data.frame(Region=character(),SpatialMask=character(),Area_ac=numeric(),stringsAsFactors=FALSE)
#  treatment.areas<-data.frame(Region=character(),metric=character(),Area_ac=numeric(),stringsAsFactors=FALSE)

#  count<-1
#  k=1

# # #calculate areas of fires and treatments (for st_area, documentation says it uses units of the CRS if it's projected)
# # for(k in 1:length(boundary.name)){ #loop through the extents e.g. all CA or each region
#  	print(paste("Start ", boundary.name[k]," loop"))

# # 	agg.fire.sf<-st_read(paste(loc.data,"IntermediateFiles/AggregatedVectors/Fires_",
# # 				boundary.name[k],"_",start.year,"_",end.year,".shp",sep=""))
# # 	agg.fire.proj.sf<-st_transform(agg.fire.sf, st_crs(cecs.rast))

# # 	fire.areas[k,]<-c(boundary.name[k],st_area(agg.fire.proj.sf)*0.000247105 )

#  	for(i in 1:length(metrics)){
#  		#choose the correct metric
#  		metric.name<-metrics[i]
#  		print(metric.name)

#  			agg.treat.sf<-st_read(paste(loc.data,"IntermediateFiles/AggregatedVectors/Treatments_",
#       			boundary.name[k],"_",policy.target[i],"_",start.year,"-present.shp",sep=""))
#  			agg.treat.proj.sf<-st_transform(agg.treat.sf, st_crs(cecs.rast))

#  		treatment.areas[count,]<-c(boundary.name[k],metric.name,st_area(agg.treat.proj.sf)*0.000247105)
#  		count<-count+1

#  		}

# # 	}

# # write.csv(fire.areas,"FireAreasByRegion_exclusive.csv")
#  write.csv(treatment.areas,"TreatmentAreasByRegionMetric_TargetedEffort.csv")
###-------------------------------------------

#mask that remove the fire footprint from treatments
nofire.rast<-rast(paste(loc.data,"WUIVegetationClassifications/NonFireFootprints_2020-2024_CECSproj.tif",sep=""))
#mask that is only forest disturbances (but not masked for only forest)
yesdist.f.rast<-rast(paste(loc.data,"WUIVegetationClassifications/ForestDisturbances_2021-2024_CECSproj.tif",sep=""))
#mask that is only shrubland disturbances (but not masked for only shrub)
yesdist.s.rast<-rast(paste(loc.data,"WUIVegetationClassifications/ShrubDisturbances_2021-2024_CECSproj.tif",sep=""))

#nofire, only disturbances - only forest
nofire.yesdist.f.rast<-nofire.rast*yesdist.f.rast*forest.cecs.rast.na
#nofire, only disturbances - only shrub
nofire.yesdist.s.rast<-nofire.rast*yesdist.s.rast*shrub.cecs.rast.na

for(k in 1:length(boundary.name)){ #loop through the extents e.g. all CA or each region
	print(paste("Start ", boundary.name[k]," loop"))

	boundary.sf<-st_read(boundary.shape[k])
	prepped.boundary.sf<-st_transform(boundary.sf, st_crs(cecs.rast))

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

		#read in the appropriate raster(s) (already masked for appropriate geographical subsets)
		print(paste("Reading: ",loc.data,"IntermediateFiles/ThresholdedEfficacyLayers/Thresholded_",metric.name,"_",start.year,".tif",sep=""))
		bef.thr.rast<-rast(paste(loc.data,"IntermediateFiles/ThresholdedEfficacyLayers/Thresholded_",metric.name,"_",start.year,".tif",sep=""))
		print(paste("Reading: ",loc.data,"IntermediateFiles/ThresholdedEfficacyLayers/Thresholded_",metric.name,"_",end.year,".tif",sep=""))
		aft.thr.rast<-rast(paste(loc.data,"IntermediateFiles/ThresholdedEfficacyLayers/Thresholded_",metric.name,"_",end.year,".tif",sep=""))

		disturbed.only<-FALSE

		if(policy.target[i]=="WildlandFireRisk"){
			bef.thr.rast<-bef.thr.rast*forest.cecs.rast.na
			aft.thr.rast<-aft.thr.rast*forest.cecs.rast.na
		}	

		# #if we want to only include disturbances seen in CECS remote sensing data
		# #then multiply by the no-fire, only disturbance layer (already also masked by ecosystem type)
		# if(disturbed.only==TRUE){
		# 	if(policy.target[i] %in% (c("WildlandFireRisk","ForestHealth")){
		# 		bef.thr.rast.tr<-bef.thr.rast*nofire.yesdist.f.rast
		# 		aft.thr.rast.tr<-aft.thr.rast*nofire.yesdist.f.rast
		# 	}
		# 	else if (policy.target[i]=="ShrublandHealth"){
		# 		bef.thr.rast.tr<-bef.thr.rast*nofire.yesdist.s.rast
		# 		aft.thr.rast.tr<-aft.thr.rast*nofire.yesdist.s.rast
		# 	}
		# #If you are only going to remove the fire footprints
		# }else if (disturbed.only==FALSE){
		# 	#just do forest only for wildland fire risk = flame length
		# 	#make sure to omit the fire footprints but only for treatment calcs
		# 	bef.thr.rast.tr<-bef.thr.rast*nofire.rast
		# 	aft.thr.rast.tr<-aft.thr.rast*nofire.rast
		# }

		 	bef.thr.rast.tr<-bef.thr.rast*nofire.rast
		 	aft.thr.rast.tr<-aft.thr.rast*nofire.rast


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
				area_ac=st_area(prepped.boundary.sf)*0.000247105*exact_extract(spat.rast[[i]],prepped.boundary.sf,fun="mean"),
				subset="WholeArea",
				before=exact_extract(bef.thr.rast,prepped.boundary.sf,fun="mean"),
				after=exact_extract(aft.thr.rast,prepped.boundary.sf,fun="mean")
				),
				cbind(
				method="Global",
				metric=metric.name,
				boundary=boundary.name[k],
				area_ac=st_area(agg.treat.proj.sf)*0.000247105*exact_extract(spat.rast[[i]],agg.treat.proj.sf,fun="mean"),
				subset="Treatments",
				before=exact_extract(bef.thr.rast.tr,agg.treat.proj.sf,fun="mean"),
				after=exact_extract(aft.thr.rast.tr,agg.treat.proj.sf,fun="mean")
				),
				cbind(
				method="Global",
				metric=metric.name,
				boundary=boundary.name[k],
				area_ac=st_area(agg.fire.proj.sf)*0.000247105*exact_extract(spat.rast[[i]],agg.fire.proj.sf,fun="mean"),
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

	}

}


timer.end<-Sys.time()

time.total<-timer.end-timer.start
print(time.total)


datetimevis<-"2025Dec26_NoFire"


#compiling all the efficacy results into one csv in order to manually do nice table formatting in a spreadsheet program
#manually created EfficacyOutputs.csv with "ls GlobalThr*2025Sept*csv > EfficacyOutputs.csv"
#efficacy.list<-read.csv("EfficacyResults/EfficacyOutputsNov17masked.csv",header=FALSE)
efficacy.list<-read.csv(paste("EfficacyResults/EfficacyOutputFiles_",datetimevis,".csv",sep=""),header=FALSE)

efficacy.results<-list()

for(i in 1:nrow(efficacy.list))
	efficacy.results[[i]]<-read.csv(paste("EfficacyResults/",efficacy.list[i,],sep=""))

efficacy.df<-do.call(rbind,efficacy.results)
efficacy.df$method<-factor(efficacy.df$method)
efficacy.df$metric<-factor(efficacy.df$metric)
efficacy.df$boundary<-factor(efficacy.df$boundary)
efficacy.df$subset<-factor(efficacy.df$subset)
efficacy.df$absdiff<-efficacy.df$after-efficacy.df$before

write.csv(efficacy.df,paste("EfficacyResults/AllEfficacyOutputs_",datetimevis,".csv",sep=""))

#################### DATA VISUALIZATIONS ###################

timer.start<-Sys.time()

nice.metric.name<-c("Likely High Severity Fire\nin WUI",
					"Likely High Severity Fire\nAcross Landscape",
					"Likely High Severity Fire\nin Utility Corridors",
					"Likely High Severity Fire\nin Road Corridors",
					"Imminent Forest Mortality",
					"Grass-dominated Shrublands",
					"Potentially Beneficial Fire\nAcross Landscape")


require(tidyterra)
require(tidyr)

ca.vect<-vect(paste(loc.scripts,"ReferenceFiles/CA_State.shp",sep=""))
ca.cecs.vect<-check.crs.match(cecs.rast,ca.vect)

# for(k in 1:length(boundary.name)){

# 	for(i in 1:length(metrics)){
# 		#choose the correct metric
# 		metric.name<-metrics[i]
# 		print(metric.name)

# 		diffname<-paste(metric.name,start.year,end.year,sep="_")

# 		global.result<-read.csv(paste("EfficacyResults/GlobalThresholdCalcOutput_",diffname,"_",boundary.name[k],"_",datetimevis,".csv",sep=""))

# 		global.result$subset[global.result$subset=="WholeArea"]<-"Region"
# 		global.result$subset[global.result$subset=="Fires"]<-"Fire\nFootprint"
# 		global.result$subset[global.result$subset=="Treatments"]<-"Treated\nAreas"
# 		global.result$subset<-factor(global.result$subset,c("Region","Treated\nAreas","Fire\nFootprint"))

# 		global.result$percent<-100*global.result$percdiff
# 		global.result$before.rnd<-paste("Initial\nProportion:\n",round(global.result$before,2))
# 		global.result$absdiff<-global.result$after-global.result$before

# 		#this is your main result for the efficacy modeling
# 		plot.title<-paste("Change in Proportion of\n",nice.metric.name[i], "\n(",nice.boundary.name[k],")", sep="")
# 		bar.plt<-ggplot(data=global.result, aes(x=subset,fill=subset,y=absdiff)) +
# 		  geom_bar(stat="identity")+
# 		  theme(legend.position="none")+
# 	      labs(title = plot.title,x = element_blank(), y = "Difference in Proportion 2020-2024")+
# 		  scale_fill_manual(values=c("#E9E5C3","#9C8F57","#9F2214"))#+
# 		  #geom_text(aes(label=before.rnd), vjust=-0.5, color="black", size=3.5)
# 		pltnm.b<-paste("EfficacyResults/AbsDiff_bar_", metric.name,"_",boundary.name[k],"_",datetime,".png",sep="")
# 	  	ggsave(pltnm.b, units="in", width=4,height=3)

# 	}

# }


diffname<-paste(metric.name,start.year,end.year,sep="_")

efficacy.df$subset<-as.character(efficacy.df$subset)
efficacy.df$subset[efficacy.df$subset=="WholeArea"]<-"Region"
efficacy.df$subset[efficacy.df$subset=="Fires"]<-"Fire Footprint"
efficacy.df$subset[efficacy.df$subset=="Treatments"]<-"Treated Areas"
efficacy.df$subset<-factor(efficacy.df$subset,c("Region","Treated Areas","Fire Footprint"))

efficacy.df$before.rnd<-paste("Initial\nProportion:\n",round(efficacy.df$before,2))

for(i in 1:length(metrics)){
	#choose the correct metric
	metric.name<-metrics[i]
	print(metric.name)

	#this is your main result for the efficacy modeling
	plot.title<-paste("Change in Proportion of ",nice.metric.name[i], sep="")
	bar.plt<-ggplot(data=efficacy.df[efficacy.df$metric==metric.name,], aes(x=subset,fill=subset,y=absdiff)) +
	  geom_bar(stat="identity")+
	  facet_grid(.~boundary)+
	  theme(legend.position="bottom",axis.text.x = element_blank(),axis.title.x= element_blank(),axis.ticks.x=element_blank())+
      labs(title = plot.title, fill = "", y = "Difference in Proportion 2020-2024")+
	  scale_fill_manual(values=c("#E9E5C3","#9C8F57","#9F2214"))#+
	  #geom_text(aes(label=before.rnd), vjust=-0.5, color="black", size=3.5)
	pltnm.b<-paste("EfficacyResults/AbsDiff_bar_", metric.name,"_",datetime,".png",sep="")
  	ggsave(pltnm.b, units="in", width=6,height=3)

}



for(i in 1:length(metrics)){
	#choose the correct metric
	metric.name<-metrics[i]
	print(metric.name)

	#this is your main result for the efficacy modeling
	plot.title<-paste("Change in Proportion of ",nice.metric.name[i], sep="")
	bar.plt<-ggplot(data=efficacy.df[efficacy.df$metric==metric.name,], aes(x=subset,fill=subset,y=percdiff)) +
	  geom_bar(stat="identity")+
	  facet_grid(.~boundary)+
	  theme(legend.position="bottom",axis.text.x = element_blank(),axis.title.x= element_blank(),axis.ticks.x=element_blank())+
      labs(title = plot.title, fill = "", y = "Difference in Proportion 2020-2024")+
	  scale_fill_manual(values=c("#E9E5C3","#9C8F57","#9F2214"))#+
	  #geom_text(aes(label=before.rnd), vjust=-0.5, color="black", size=3.5)
	pltnm.b<-paste("EfficacyResults/PercDiff_bar_", metric.name,"_",datetime,".png",sep="")
  	ggsave(pltnm.b, units="in", width=6,height=3)

}




########### END DATA VISUALIZATIONS ##############

timer.end<-Sys.time()

time.vis<-timer.end-timer.start
print(time.vis)


