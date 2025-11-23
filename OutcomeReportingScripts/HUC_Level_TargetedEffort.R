#HUC_Level_TargetedEffort.R


timer.start<-Sys.time()

#############################################################################
########### Read in scripts and set paramters/reference layers ##############
#############################################################################

datestamp<-"2025Nov17"

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


library("exactextractr")
library("sf")

#reference rasters for spatial reference of CRS and extent
# CECS layers all have the same CRS and extent
cecs.rast<-rast(paste(loc.data,"CECS_Data/CECS_CAWide_Vulner_TreeDieoff_SPI-2_2020_V250614.tif",sep=""))
# whp is the same projection as state boundaries/TF regions, WHR/veg classifications
whp.rast <- rast(paste(loc.data,"PriorityLayers/whp_classified_20240906.tif",sep="")) 

reference.rast<-rast(paste(loc.data,"CECS_Data/CECS_CAWide_Vulner_TreeDieoff_SPI-2_2020_V250614.tif",sep=""))

ca.vect<-vect(paste(loc.scripts,"ReferenceFiles/CA_State.shp",sep=""))
ca.whp.vect<-check.crs.match(whp.rast,ca.vect)
ca.cecs.vect<-check.crs.match(cecs.rast,ca.vect)

#these will be necessary for if you need to recalculate the polygons
#and for file naming conventions
start.year<-"2020"
end.year<-"2024"

#water year
start.y<-paste(start.year,"-09-30",sep="")
end.y<-paste(end.year,"-10-01",sep="")

#############################################################################
############  Bring in landscape mask, WHP layer                  ###########
#############################################################################
land.whp.rast<-rast(paste(loc.data,"WUIVegetationClassifications/FRAP24_Landscape_WHP.tif",sep=""))

whp.priority.rast<-rast(paste(loc.data,"PriorityLayers/FinalPriorityLayers/WHPpriority_WHP.tif",sep=""))

high.priority.rast<-land.whp.rast*whp.priority.rast

#############################################################################
######## set boundary, policy target, zonal layer, and patch layer ##########
#############################################################################

#boundary.shape<-c(paste(loc.scripts,"ReferenceFiles/Region_SouthernCA.shp",sep=""))
#boundary.name<-c("South")

 boundary.shape<-c(
                   paste(loc.scripts,"ReferenceFiles/Region_SouthernCA.shp",sep=""),
                   paste(loc.scripts,"ReferenceFiles/Region_Sierra.shp",sep=""),
                   paste(loc.scripts,"ReferenceFiles/Region_NorthernCA.shp",sep=""),
                   paste(loc.scripts,"ReferenceFiles/Region_CentralCoast.shp",sep=""))
 boundary.name<-c("South","Sierra","North","Central")


# #loop through all california, and the four regions
# boundary.shape<-c(paste(loc.scripts,"ReferenceFiles/CA_State.shp",sep=""))
# boundary.name<-c("CA")


policy.target<-c("WildlandFireRisk")

# just HUC12 for starters
zsum.shape<-c(paste(loc.scripts,"ReferenceFiles/HUC12.shp",sep=""))
zsum.name<-c("huc12")

patch.shape<-c(paste(loc.data,"ITS_2025_V2Nov14_Data/appended.gdb",sep=""))
patch.layer<-c("appended_poly")

#############################################################################
############ read in treatment data, filter, and aggregate by HUC ###########
#############################################################################

#note that I have both terra and sf versions here for potential comparison
#and to make sure I haven't missed any steps

# #can just read in the file if you've already calculated the aggregated version
# aggregate.vectors<-TRUE

# if(aggregate.vectors==TRUE){

	#read treatments (sf version)
	treat.sf<-st_read(patch.shape,patch.layer)
	prepped.treat.sf<-st_transform(treat.sf, st_crs(whp.rast))
	#read treatments (terra version)
	#read.time<-system.time(treat.vect<-read.and.check.crs.patch.vector(patch.shape[1],"Treatments",patch.layer[1],whp.rast))
	#print(paste("Time to read treatments: ",round(read.time[[1]]/60)," minute(s)", sep=""))

	#read zonal summary vector, check crs (sf version)
	zonal.summary.area.sf<-st_read(zsum.shape)
	zonal.summary.proj.sf<-st_transform(zonal.summary.area.sf, st_crs(whp.rast))
	#read zonal summary vector, check crs (terra version)
	#	read.time<-system.time(zonal.summary.area.vect<-read.vector.and.check.crs(whp.rast,zsum.shape,zsum.name))
	#	print(paste("Time to read zonal summary area: ",round(read.time[[1]]/60)," minute(s)", sep=""))

#i=1

HUC.proportions.by.region<-list()

	for(i in 1:length(boundary.name)){
		print(boundary.name[i])

		#read and prepare boundary vector (sf version)
		boundary.sf<-st_read(boundary.shape[i])
		prepped.boundary.sf<-st_transform(boundary.sf, st_crs(whp.rast))
		#read and prepare boundary vector (terra version)
		#prepped.boundary.vect<-read.and.prepare.boundary.vector(boundary.shape[i],boundary.name[i],whp.rast)

		if(boundary.name[i]=="CA"){
			#prepped.boundary.vect$Region<-"AllCA"
			prepped.boundary.sf$Region<-"AllCA"
		}	

		k=1
#		for(k in 1:length(policy.target)){
		  	print(policy.target[k])

		  	#filter.treatments should be agnostic of the type of vector object and should work for sf or terra
		    filter.time<-system.time(treat.filt.te.vect<-filter.treatments(treat.sf,policy.target[k],start.y,"present"))
		    print(paste("Time to filter treatments (targeted effort): ",round(filter.time[[1]]/60)," minute(s)", sep=""))
		    treat.filt.te.vect<-treat.filt.te.vect[,c()]

		    # clip zonal summary polygons to boundary (sf version)
			prepped.zonal.summary.sf<-st_intersection(zonal.summary.proj.sf,prepped.boundary.sf)
			prepped.zonal.summary.sf$HUCareaac<-st_area(prepped.zonal.summary.sf)*0.000247105 
			# clip zonal summary polygons to boundary (terra version)
			# prep.time<-system.time(prepped.zonal.summary.area.vect<-crop.vector.by.boundary.and.recalc.area(prepped.boundary.vect,boundary.name[i],zonal.summary.area.vect,zsum.name))
			# print(paste("Time to prep zonal summary area: ",round(prep.time[[1]]/60)," minute(s)", sep=""))
			prepped.zonal.summary.sf<-prepped.zonal.summary.sf[,(c("name","huc12","Region","HUCareaac"))]
			prepped.zonal.summary.sf$HUCareaac<-as.numeric(prepped.zonal.summary.sf$HUCareaac)

			#intersect and aggregate treatments at HUC level (sf version)
			zonal.treat.intersect<-st_intersection(treat.filt.te.vect,prepped.zonal.summary.sf)
			#have to use terra for the aggregate function, not implemented in sf as of oct 2025 I think
			zonal.treat.vect<-terra::vect(zonal.treat.intersect)
			zonal.treat.agg.vect<-aggregate(zonal.treat.vect,by=zsum.name,dissolve=TRUE)
			writeVector(zonal.treat.agg.vect,paste(loc.data,"IntermediateFiles/AggregatedVectors/Treatments_",
				zsum.name,"_",boundary.name[i],"_",policy.target[k],"_",start.year,"-present.shp",sep=""),overwrite=TRUE)

			#intersect and aggregate treatments at HUC level (terra-only version)		
			#agg.time<-system.time(agg.treatments.huc.vect<-intersect.and.aggregate.vectors(
			#	prepped.zonal.summary.area.vect,zsum.name,treat.filt.ef.vect,"Treatments","HUC12","huc12",
			#	prepped.boundary.vect,boundary.name[i]))
			#print(paste("Time to aggregate treatments by HUC12 (efficacy): ",round(agg.time[[1]]/60)," minute(s)", sep=""))
			#writeVector(agg.treatments.huc.vect,paste(loc.data,"IntermediateFiles/AggregatedVectors/Treatments_",
			#	boundary.name[i],"_",policy.target[k],"_",start.year,"_",end.year,"_HUC12.shp",sep=""),overwrite=TRUE)

			zonal.treat.agg.sf<-st_read(paste(loc.data,"IntermediateFiles/AggregatedVectors/Treatments_",zsum.name,"_",
				boundary.name[i],"_",policy.target[k],"_",start.year,"-present.shp",sep=""))

			zonal.treat.agg.sf$treatareaac<-as.numeric(st_area(zonal.treat.agg.sf)*0.000247105 )
			zonal.treat.agg.sf$treat_proportion<-
				zonal.treat.agg.sf$treatareaac/as.numeric(zonal.treat.agg.sf$mean_HUCar)

			plot(zonal.treat.agg.sf$mean_HUCar,zonal.treat.agg.sf$treat_proportion)

	#############################################################################
	##### Do calculations by HUC   ##############################################
	#############################################################################

	#use exactextractr to get the area of high WHP in each HUC, just the mean
	#use vector operations to calculate the area of the treatments within the HUCs (above) and merge and divide

	prepped.zonal.summary.sf$highpriority_proportion<-exact_extract(high.priority.rast,prepped.zonal.summary.sf,fun="mean")

	zonal.df<-as.data.frame(prepped.zonal.summary.sf)
	treat.df<-as.data.frame(zonal.treat.agg.sf)

	compiled.data<-merge(zonal.df, treat.df, by.x=zsum.name,by.y=zsum.name,all.x=TRUE)

	plot(compiled.data$treat_proportion~compiled.data$highpriority_proportion,
		xlab="Proportion of HUC WHP 4 or 5",
		ylab="Proportion of HUC Treated for Wildfire Risk",
		main="All HUC 12s in CA Treated vs High Priority")

	compiled.data.nona<-compiled.data[!is.na(compiled.data$treat_proportion),]
	compiled.data.nona<-compiled.data.nona[!is.na(compiled.data.nona$highpriority_proportion),]

	print(compiled.data.nona[compiled.data.nona$treat_proportion>0.60 & compiled.data.nona$highpriority_proportion>0.60,
		c("name.x","Region.x","HUCareaac","treatareaac","highpriority_proportion","treat_proportion")])
	compiled.data.nona<-compiled.data.nona[,
		c("huc12","name.x","Region.x","HUCareaac","treatareaac","highpriority_proportion","treat_proportion")]

	write.csv(compiled.data.nona,paste("HUC12_Treated_HighPriority",boundary.name[i],".csv"))

			HUC.proportions.by.region[[i]]<-compiled.data.nona

	}


#}
	

timer.end<-Sys.time()

time.total<-timer.end-timer.start
print(time.total)

compiled.data.df<-do.call(rbind,HUC.proportions.by.region)
compiled.data.df$highpri_rounded<-round(compiled.data.df$highpriority_proportion,1)
compiled.data.df$treat_rounded<-round(compiled.data.df$treat_proportion,1)
compiled.data.df$combined<-compiled.data.df$treat_proportion*compiled.data.df$highpriority_proportion

	write.csv(compiled.data.df,paste("HUC12_Treated_HighPriority_allRegions.csv"))



    plot.title<-paste("HUC12s Treated vs High Priority for ",policy.target, sep="")
    plt<-ggplot(data=compiled.data.df, aes(x=highpriority_proportion,color=Region.x,y=treat_proportion)) +
      geom_point(stat="identity")+
          theme(legend.position="bottom")+
        labs(title = plot.title, color="Region", 
        	x = "Proportion of HUC WHP 4 or 5",
        	y = "Proportion of HUC Treated for Wildfire Risk")+
      scale_color_manual(values=viridis(4))#+
    pltnm.b<-paste("HUCProportions_", policy.target,"_",datestamp,".png",sep="")
      ggsave(pltnm.b, units="in", width=6,height=6)

#export a shapefile for each of the four best-in-region HUCs

SC.sf<-zonal.summary.proj.sf[zonal.summary.proj.sf$huc12=="180703050801",] #Upper Pine Valley Creek
NC.sf<-zonal.summary.proj.sf[zonal.summary.proj.sf$huc12=="180101010307",] #Rock Creek
CC.sf<-zonal.summary.proj.sf[zonal.summary.proj.sf$huc12=="180600050302",] #Middle Branch Huerhuero Creek
SN.sf<-zonal.summary.proj.sf[zonal.summary.proj.sf$huc12=="180201560103",] #Little Antelope Creek

st_write(NC.sf,"NC_BestHUC.shp")
st_write(SC.sf,"SC_BestHUC.shp")
st_write(CC.sf,"CC_BestHUC.shp")
st_write(SN.sf,"SN_BestHUC.shp")

###########################
###########################
###########################


#ending year of water year

start.year<-"2020"
end.year<-"2024"

#water year
start.y<-paste(start.year,"-09-30",sep="")
end.y<-paste(end.year,"-10-01",sep="")

metrics<-c(
			"FlameLengthLandscape",
			"BeneficialFireLandscape")

#these should match the metrics and are for making sure
#to read in the correctly aggregated vector which was
#filtered for the appropriate treatment types
policy.target<-c("WildlandFireRisk",
			"WildlandFireRisk")

land.cecs.rast<-rast(paste(loc.data,"WUIVegetationClassifications/FRAP24_Landscape_CECS.tif",sep=""))
land.cecs.rast[is.na(land.cecs.rast)]<-0

spat.rast<-list(
				land.cecs.rast,
				land.cecs.rast)

######## END GLOBAL PARAMETERS #################


########### READ IN AND PROCESS VECTORS ##############

 boundary.shape<-c("NC_BestHUC.shp",
 					"SC_BestHUC.shp",
 					"CC_BestHUC.shp",
 					"SN_BestHUC.shp")
 boundary.name<-c("North","South","Central","Sierra")

 nice.boundary.name<-c(
 				  "Rock Creek - North",
 				  "Upper Pine Valley Creek - South",
 				  "Middle Branch Huerhuero Creek - Central",
 				  "Little Antelope Creek - Sierra")


######################################
##         PREP VECTORS            ###
######################################


# patch.shape<-c(paste(loc.data,"ITS_2025_V2Nov14_Data/appended.gdb",sep=""),
# 			   paste(loc.data,"FireFootprints/fire24_1.gdb",sep=""))
# patch.layer<-c("appended_poly",
# 				"firep24_1")

# # read.time<-system.time(zonal.summary.area.vect<-read.vector.and.check.crs(whp.rast,zsum.shape,zsum.name))
# # print(paste("Time to read zonal summary area: ",round(read.time[[1]]/60)," minute(s)", sep=""))

# read.time<-system.time(treat.vect<-read.and.check.crs.patch.vector(patch.shape[1],"Treatments",patch.layer[1],whp.rast))
# print(paste("Time to read treatments: ",round(read.time[[1]]/60)," minute(s)", sep=""))
# read.time<-system.time(fire.vect<-read.and.check.crs.patch.vector(patch.shape[2],"Fires",patch.layer[2],whp.rast))
# print(paste("Time to read fires: ",round(read.time[[1]]/60)," minute(s)", sep=""))

# for(i in 1:length(boundary.name)){
# 	print(boundary.name[i])
#     prepped.boundary.vect<-read.and.prepare.boundary.vector(boundary.shape[i],boundary.name[i],whp.rast)
# 	filt.time<-system.time(fire.filt.vect<-filter.fires(fire.vect,start.y,end.y))
# 	print(paste("Time to filter fires by time range (efficacy): ",round(filt.time[[1]]/60)," minute(s)", sep=""))

# 	#also intersect and aggregate fires
# #	agg.time<-system.time(agg.fires.region.vect<-intersect.and.aggregate.vectors(
# #		prepped.boundary.vect,boundary.name[i],fire.filt.vect,"Fires","HUC","name",
# #		prepped.boundary.vect,boundary.name[i]))
# #	print(paste("Time to aggregate fires by region (efficacy): ",round(agg.time[[1]]/60)," minute(s)", sep=""))
# #	writeVector(agg.fires.region.vect,paste(loc.data,"IntermediateFiles/AggregatedVectors/Fires_",
# #		boundary.name[i],"_",start.year,"_",end.year,".shp",sep=""),overwrite=TRUE)

# 	for(k in 1:length(policy.target)){
# 	  	print(policy.target[k])

# 	    filter.time<-system.time(treat.filt.te.vect<-filter.treatments(treat.vect,policy.target[k],start.y,"present"))
# 	    print(paste("Time to filter treatments (targeted effort): ",round(filter.time[[1]]/60)," minute(s)", sep=""))

# 	    filter.time<-system.time(treat.filt.ef.vect<-filter.treatments(treat.vect,policy.target[k],start.y,end.y))
# 	    print(paste("Time to filter treatments (efficacy): ",round(filter.time[[1]]/60)," minute(s)", sep=""))

# 		#"Regions/HUC12" is the display name for how patches are being aggregated, and "Region/huc12" is the column name 
# 		agg.time<-system.time(agg.treatments.region.vect<-intersect.and.aggregate.vectors(
# 			prepped.boundary.vect,boundary.name[i],treat.filt.te.vect,"Treatments","HUC","name",
# 			prepped.boundary.vect,boundary.name[i]))
# 		print(paste("Time to aggregate treatments by region (targeted effort): ",round(agg.time[[1]]/60)," minute(s)", sep=""))
# 		writeVector(agg.treatments.region.vect,paste(loc.data,"IntermediateFiles/AggregatedVectors/Treatments_",
# 			boundary.name[i],"_",policy.target[k],"_",start.year,"-present.shp",sep=""),overwrite=TRUE)

# 		#for efficacy
# 		if(policy.target[k] %in% c("WildlandFireRisk","ShrublandHealth","ForestHealth")){

# 			#recalculate for the earlier end time to match CECS water year
# 			#"Regions/HUC12" is the display name for how patches are being aggregated, and "Region/huc12" is the column name 
# 			agg.time<-system.time(agg.treatments.region.vect<-intersect.and.aggregate.vectors(
# 				prepped.boundary.vect,boundary.name[i],treat.filt.ef.vect,"Treatments","Regions","Region",
# 				prepped.boundary.vect,boundary.name[i]))
# 			print(paste("Time to aggregate treatments by region (efficacy): ",round(agg.time[[1]]/60)," minute(s)", sep=""))
# 			writeVector(agg.treatments.region.vect,paste(loc.data,"IntermediateFiles/AggregatedVectors/Treatments_",
# 				boundary.name[i],"_",policy.target[k],"_",start.year,"_",end.year,".shp",sep=""),overwrite=TRUE)
# 		}

# 	}

# }



library("exactextractr")
library("sf")

for(k in 1:length(boundary.name)){ #loop through the extents e.g. all CA or each region
	print(paste("Start ", boundary.name[k]," loop"))

	boundary.sf<-st_read(boundary.shape[k])
	prepped.boundary.sf<-st_transform(boundary.sf, st_crs(cecs.rast))

#	agg.fire.sf<-st_read(paste(loc.data,"IntermediateFiles/AggregatedVectors/Fires_",
#				boundary.name[k],"_",start.year,"_",end.year,".shp",sep=""))
#	agg.fire.proj.sf<-st_transform(agg.fire.sf, st_crs(cecs.rast))


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

		#TODO: instead of reading in the masks and filling in 0s for NAs, could just take one of these rasters
		# and make any non-na pixel a 1, and any NA pixel a 0, and it would be the same and also tighter

		#read in vector files for summarizing/clipping

#			agg.treat.sf<-st_read(paste(loc.data,"IntermediateFiles/AggregatedVectors/Treatments_",
#						boundary.name[k],"_",policy.target[i],"_",start.year,"_",end.year,".shp",sep=""))
#			agg.treat.proj.sf<-st_transform(agg.treat.sf, st_crs(cecs.rast))

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
				)#,
				# cbind(
				# method="Global",
				# metric=metric.name,
				# boundary=boundary.name[k],
				# area_ac=st_area(agg.treat.proj.sf)*0.000247105*exact_extract(spat.rast[[i]],agg.treat.proj.sf,fun="mean"),
				# subset="Treatments",
				# before=exact_extract(bef.thr.rast,agg.treat.proj.sf,fun="mean"),
				# after=exact_extract(aft.thr.rast,agg.treat.proj.sf,fun="mean")
				# ),
				# cbind(
				# method="Global",
				# metric=metric.name,
				# boundary=boundary.name[k],
				# area_ac=st_area(agg.fire.proj.sf)*0.000247105*exact_extract(spat.rast[[i]],agg.fire.proj.sf,fun="mean"),
				# subset="Fires",
				# before=exact_extract(bef.thr.rast,agg.fire.proj.sf,fun="mean"),
				# after=exact_extract(aft.thr.rast,agg.fire.proj.sf,fun="mean")
				# )
			)

			all.global.results.df<-as.data.frame(all.global.results)
			all.global.results.df$percdiff<-(as.numeric(as.character(all.global.results.df$after))-
										 as.numeric(as.character(all.global.results.df$before)))/
										 as.numeric(as.character(all.global.results.df$before))

			print(paste("Writing to GlobalThresholdCalcOutput_",diffname,"_",boundary.name[k],"_",datestamp,".csv",sep=""))
			write.table(all.global.results.df,paste("EfficacyResults/GlobalThresholdCalcOutput_",diffname,"_",boundary.name[k],"_",datestamp,".csv",sep=""),
				sep = ",",quote = FALSE, col.names = TRUE, row.names = FALSE,na="NA") 


	}

}


#compiling all the efficacy results into one csv in order to manually do nice table formatting in a spreadsheet program
#manually created EfficacyOutputs.csv with "ls GlobalThr*2025Sept*csv > EfficacyOutputs.csv"
efficacy.list<-read.csv("EfficacyResults/EfficacyOutputsNov17.csv",header=FALSE)

efficacy.results<-list()

for(i in 1:nrow(efficacy.list))
	efficacy.results[[i]]<-read.csv(paste("EfficacyResults/",efficacy.list[i,],sep=""))

efficacy.df<-do.call(rbind,efficacy.results)
efficacy.df$method<-factor(efficacy.df$method)
efficacy.df$metric<-factor(efficacy.df$metric)
efficacy.df$boundary<-factor(efficacy.df$boundary)
efficacy.df$subset<-factor(efficacy.df$subset)
efficacy.df$absdiff<-efficacy.df$after-efficacy.df$before

write.csv(efficacy.df,"EfficacyResults/AllEfficacyOutputsSingleHUCs.csv")

#################### DATA VISUALIZATIONS ###################

timer.start<-Sys.time()

datestampvis<-datestamp

nice.metric.name<-c(
					"Likely High Severity Fire\nAcross Landscape",
					"Potentially Beneficial Fire\nAcross Landscape")

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

		global.result<-read.csv(paste("EfficacyResults/GlobalThresholdCalcOutput_",diffname,"_",boundary.name[k],"_",datestampvis,".csv",sep=""))

		global.result$subset[global.result$subset=="WholeArea"]<-"Region"
		global.result$subset[global.result$subset=="Fires"]<-"Fire\nFootprint"
		global.result$subset[global.result$subset=="Treatments"]<-"Treated\nAreas"
		global.result$subset<-factor(global.result$subset,c("Region","Treated\nAreas","Fire\nFootprint"))

		global.result$percent<-100*global.result$percdiff
		global.result$before.rnd<-paste("Initial\nProportion:\n",round(global.result$before,2))
		global.result$absdiff<-global.result$after-global.result$before

		#this is your main result for the efficacy modeling
		plot.title<-paste("Change in Proportion of\n",nice.metric.name[i], "\n(",nice.boundary.name[k],")", sep="")
		bar.plt<-ggplot(data=global.result, aes(x=subset,fill=subset,y=absdiff)) +
		  geom_bar(stat="identity")+
		  theme(legend.position="none")+
	      labs(title = plot.title,x = element_blank(), y = "Difference in Proportion 2020-2024")+
		  scale_fill_manual(values=c("#E9E5C3","#9C8F57","#9F2214"))#+
		  #geom_text(aes(label=before.rnd), vjust=-0.5, color="black", size=3.5)
		pltnm.b<-paste("EfficacyResults/AbsDiff_bar_", metric.name,"_",boundary.name[k],".png",sep="")
	  	ggsave(pltnm.b, units="in", width=4,height=3)

	}

}


diffname<-paste(metric.name,start.year,end.year,sep="_")

efficacy.df$subset<-as.character(efficacy.df$subset)
efficacy.df$subset[efficacy.df$subset=="WholeArea"]<-"WholeHUC"
efficacy.df$subset[efficacy.df$subset=="Fires"]<-"Fire Footprint"
efficacy.df$subset[efficacy.df$subset=="Treatments"]<-"Treated Areas"
efficacy.df$subset<-factor(efficacy.df$subset,c("WholeHUC","Treated Areas","Fire Footprint"))

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
	pltnm.b<-paste("EfficacyResults/AbsDiff_bar_", metric.name,"BestHUCOnly.png",sep="")
  	ggsave(pltnm.b, units="in", width=6,height=3)

}





