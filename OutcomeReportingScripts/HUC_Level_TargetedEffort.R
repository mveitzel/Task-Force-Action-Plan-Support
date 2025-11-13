#HUC_Level_TargetedEffort.R


timer.start<-Sys.time()

#############################################################################
########### Read in scripts and set paramters/reference layers ##############
#############################################################################

datestamp<-"2025Nov03"

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

boundary.shape<-c(paste(loc.scripts,"ReferenceFiles/Region_SouthernCA.shp",sep=""))
boundary.name<-c("South")

policy.target<-c("WildlandFireRisk")

# just HUC12 for starters
zsum.shape<-c(paste(loc.scripts,"ReferenceFiles/HUC12.shp",sep=""))
zsum.name<-c("huc12")

patch.shape<-c(paste(loc.data,"ITS_2025Aug16_Data/appended.gdb",sep=""))
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

i=1
#	for(i in 1:length(boundary.name)){
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

		    # clip zonal summary polygons to boundary (sf version)
			prepped.zonal.summary.sf<-st_intersection(zonal.summary.proj.sf,prepped.boundary.sf)
			prepped.zonal.summary.sf$HUCareaac<-st_area(prepped.zonal.summary.sf)*0.000247105 
			# clip zonal summary polygons to boundary (terra version)
			# prep.time<-system.time(prepped.zonal.summary.area.vect<-crop.vector.by.boundary.and.recalc.area(prepped.boundary.vect,boundary.name[i],zonal.summary.area.vect,zsum.name))
			# print(paste("Time to prep zonal summary area: ",round(prep.time[[1]]/60)," minute(s)", sep=""))

			#intersect and aggregate treatments at HUC level (sf version)
			zonal.treat.intersect<-st_intersection(treat.filt.te.vect,prepped.zonal.summary.sf)
			#have to use terra for the aggregate function, not implemented in sf as of oct 2024 I think
			zonal.treat.vect<-terra::vect(zonal.treat.intersect)
			zonal.treat.agg.vect<-aggregate(zonal.treat.vect,by=zsum.name,dissolve=TRUE)
			writeVector(zonal.treat.agg.vect,paste(loc.data,"IntermediateFiles/AggregatedVectors/Treatments_",zsum.name,"_",
				boundary.name[i],"_",policy.target[k],"_",start.year,"-present.shp",sep=""),overwrite=TRUE)

			#intersect and aggregate treatments at HUC level (terra-only version)		
			#agg.time<-system.time(agg.treatments.huc.vect<-intersect.and.aggregate.vectors(
			#	prepped.zonal.summary.area.vect,zsum.name,treat.filt.ef.vect,"Treatments","HUC12","huc12",
			#	prepped.boundary.vect,boundary.name[i]))
			#print(paste("Time to aggregate treatments by HUC12 (efficacy): ",round(agg.time[[1]]/60)," minute(s)", sep=""))
			#writeVector(agg.treatments.huc.vect,paste(loc.data,"IntermediateFiles/AggregatedVectors/Treatments_",
			#	boundary.name[i],"_",policy.target[k],"_",start.year,"_",end.year,"_HUC12.shp",sep=""),overwrite=TRUE)

			zonal.treat.agg.sf<-st_read(paste(loc.data,"IntermediateFiles/AggregatedVectors/Treatments_",zsum.name,"_",
				boundary.name[i],"_",policy.target[k],"_",start.year,"-present.shp",sep=""))

			zonal.treat.agg.sf$treatareaac<-st_area(zonal.treat.agg.sf)*0.000247105 
			zonal.treat.agg.sf$treat_proportion<-zonal.treat.agg.sf$treatareaac/as.numeric(zonal.treat.agg.sf$HUCareaac)

#		}
#	}
	# } else {
	# 	#agg.treatments.huc.vect<-st_read()
	# }

#############################################################################
##### Do calculations by HUC   ##############################################
#############################################################################

#use exactextractr to get the area of high WHP in each HUC, just the mean
#use vector operations to calculate the area of the treatments within the HUCs and merge and divide

prepped.zonal.summary.sf$highpriority_proportion<-exact_extract(high.priority.rast,prepped.zonal.summary.sf,fun="mean")

zonal.df<-as.data.frame(prepped.zonal.summary.sf)
treat.df<-as.data.frame(zonal.treat.agg.sf)

compiled.data<-merge(zonal.df, treat.df, by.x=zsum.name,by.y=zsum.name,all.x=TRUE)

compiled.data$treat_proportion_numeric<-as.numeric(compiled.data$treat_proportion)

plot(compiled.data$treat_proportion_numeric,compiled.data$highpriority_proportion)

compiled.data.nona<-compiled.data[!is.na(compiled.data$treat_proportion_numeric),]

compiled.data.nona[compiled.data.nona$treat_proportion_numeric>0.20,c("name.x","Region","HUCareaac.y","treatareaac","highpriority_proportion","treat_proportion_numeric")]

write.csv(compiled.data.nona,"HUC12_Treated_HighPriority.csv")

# #set up the data frame for the outputs
# targeted.effort.results<-data.frame(Boundary=character(),PolicyTarget=character(),Metric=character(),
#                                     MaskName=character(),AreaType=character(),
#                                     PriorityArea=numeric(),TotalTreatmentArea=numeric(),
#                                     ProportionOfTreatments=numeric(),stringsAsFactors=FALSE)

# count<-1
# for(i in 1:length(boundary.name)){

#     # #---------------- WHP, Landscape ---------------------------#

#     policy.target<-"WildlandFireRisk"
#     metric.name<-"WildfireHazardPotentialLandscape"
#     mask.name<-"Landscape"
#     r.rast<-whp.rast
#     m.rast<-land.whp.rast
#     p.rast<-whp.priority.rast
#     rast.calcs.time<-system.time(count<-rasterize.mask.calculate.proportions(1,p.rast,metric.name,policy.target,boundary.name[i],boundary.shape[i],r.rast,mask.name,m.rast))
#     print(paste("Total time to do raster math for ",policy.target," using ",metric.name," in ",mask.name,": ",round(rast.calcs.time[[1]]/60)," minute(s)", sep=""))
#     print(count)


# }

timer.end<-Sys.time()

time.total<-timer.end-timer.start
print(time.total)







