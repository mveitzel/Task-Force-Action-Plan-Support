#TestingNewFunctions.R

	loc.scripts<-"D:/GitRepos/BattlesLabRepos/Task-Force-Action-Plan-Support/"
	loc.data<-"D:/GIS_Large_Files/"
	loc.output<-"D:/DropboxFiles/Dropbox/Professional/UCB_Battles/ActionPlanSupport"

	setwd(loc.output)
	source(paste(loc.scripts,"FunctionLibraries/SummarizeChange_functions.R",sep=""))

	boundary.shape<-c(paste(loc.scripts,"ReferenceFiles/Region_Sierra.shp",sep=""))
	boundary.name<-c("Sierra")
	reference.rast<-rast(paste(loc.data,"CECS_Data/CECS_CAWide_Vulner_TreeDieoff_SPI-2_2020_V250418.tif",sep=""))

	vect.shape<-c(paste(loc.scripts,"ReferenceFiles/HUC12.shp",sep=""))
	vect.name<-c("HUC12")


	prepped.boundary.vect<-read.and.prepare.boundary.vector(boundary.shape,boundary.name,reference.rast)
	zonal.summary.area.vect<-read.vector.and.check.crs(prepped.boundary.vect,vect.shape,vect.name)
	prepped.zonal.summary.area.vect<-crop.vector.by.boundary.and.recalc.area(prepped.boundary.vect,boundary.name,zonal.summary.area.vect,vect.name)

#checking to confirm that there's a big 1:1 line with hucs that haven't been clipped
#and a bunch of hucs that have smaller areas in post_crop_area_ha than they did before
# plot(prepped.zonal.summary.area.vect$huc12_area,prepped.zonal.summary.area.vect$post_crop_area_ha)

#patch.name<-c("Treatments","Fires")
#patch.shape<-c(paste(loc.data,"ITT_2024_Data/Interagency Tracking System.gdb",sep=""),paste(loc.data,"FireFootprints/fire23_1.gdb",sep=""))
#patch.layer<-c("Treat_n_harvests_polygons2023_20240911","firep23_1")

	patch.name<-c("Treatments","Fires")
	patch.shape<-c(paste(loc.data,"ITS_2025Jul25_Polygons/appended.gdb",sep=""),paste(loc.data,"FireFootprints/fire24_1.gdb",sep=""))
	patch.layer<-c("appended_poly","firep24_1")


	treatments<-read.and.check.crs.patch.vector(patch.shape[1],patch.name[1],patch.layer[1],prepped.boundary.vect)
	fires<-read.and.check.crs.patch.vector(patch.shape[2],patch.name[2],patch.layer[2],prepped.boundary.vect)

	agg.name<-"Regions"
	agg.code<-"Region"

	#note that using the sierra region had the side effect of clipping because everything else was null, I think? I wasn't expecting that
	#of course, using the zonal summary area, I've already clipped that so it makes sense
	#in general, where there's no overlap, and also no overlap = that code doesn't appear in the dissolve argument (the column name)
	#it throws out the 'zeroes'.  Will have to decide how to handle that, I think it's largely fine because we aren't going to do 
	#histograms quite how we did it before.  I think HUC12 is only for mapping, maybe histograms/boxplots for reality checks but not for display
	agg.fires.vect.region<-intersect.and.aggregate.vectors(
			prepped.boundary.vect,boundary.name,fires,patch.name[2],"Regions","Region",prepped.boundary.vect,boundary.name)
	#agg.fires.vect.huc<-intersect.and.aggregate.vectors(
	#		prepped.zonal.summary.area.vect,vect.name,fires,patch.name[2],"HUC12","huc12",prepped.boundary.vect,boundary.name)
	agg.treatments.vect.region<-intersect.and.aggregate.vectors(
			prepped.boundary.vect,boundary.name,treatments,patch.name[1],"Regions","Region",prepped.boundary.vect,boundary.name)
	#agg.treatments.vect.huc<-intersect.and.aggregate.vectors(
	#		prepped.zonal.summary.area.vect,vect.name,treatments,patch.name[1],"HUC12","huc12",prepped.boundary.vect,boundary.name)

	metric.name<-"Drought Vulnerability"

	huc_DV<-summarize.pixels.in.area.of.interest(reference.rast,metric.name,prepped.zonal.summary.area.vect,vect.name,"zonal")

	# oops I accidentally used the non-prepped version that wasn't cropped
	#      user     system    elapsed 
	#24.3666667  0.5361667 24.9105000 
	#is the timing for the whole state.  Not bad!
	#     user    system   elapsed 
	#6.9768333 0.1421667 7.1306667 
	#is the timing for just the sierras


region_DV<-summarize.pixels.in.area.of.interest(reference.rast,metric.name,agg.treatments.vect.region,"Treatments","global")


	sub.rast<-subset.raster(reference.rast,metric.name,NA,NA,prepped.boundary.vect,boundary.name)
	global.avg<-as.numeric(global(sub.rast,"mean",na.rm=TRUE))

	global.sum.vect<-prepped.boundary.vect
	global.sum.vect[,metric.name]<-global.avg




#sketching out the function that will do all the things


	#date stamp of this set of results - appended to all outputs to avoid overwriting older versions
	datetime<-"2025Aug4"

	#ending year of water year
	before.year<-2020
	after.year<-2023

	metric.name<-"Drought Vulnerability"
	vint<-"250418"
	metric.code<-"Vulner_TreeDieoff_SPI-2"
	#xlabel<-"Change in Runoff"
	#index, so no conversion factor
	conversion<-NA
	#read in the rasters, convert them from CECS 'units', and difference them final minus initial (after minus before)	
	#generate CECS file names
	before.yr.name<-generate.CECS.filename(metric.code,before.year,vint)
	after.yr.name<-generate.CECS.filename(metric.code,after.year,vint)

	before.rast<-read.in.raster(loc.data,before.yr.name,metric.name)
	after.rast<-read.in.raster(loc.data,after.yr.name,metric.name)

	before.proj.rast<-check.crs.match(reference.rast,before.rast)
	after.proj.rast<-check.crs.match(reference.rast,after.rast)

	before.prepped.rast<-multiply.conversion.factor(metric.name,before.proj.rast,conversion)
	after.prepped.rast<-multiply.conversion.factor(metric.name,after.proj.rast,conversion)

	diff<-diff.rasters(before.yr.name,before.prepped.rast,after.yr.name,after.prepped.rast,metric.name)


	whole.summary.area<-summarize.pixels.in.area.of.interest(diff,metric.name,prepped.zonal.summary.area.vect,vect.name,"zonal")
	treatments<-summarize.pixels.in.area.of.interest(diff,metric.name,agg.treatments.vect.huc,patch.name[1],"zonal")
	fires<-summarize.pixels.in.area.of.interest(diff,metric.name,agg.fires.vect.huc,patch.name[2],"zonal")

# zonal summary polys
#      user    system   elapsed 
# 9.3341667 0.3913333 9.6680000 
# treatments
#    user  system elapsed 
#  0.8200  0.0425  0.8590 
# fires
#      user    system   elapsed 
# 3.3078333 0.1958333 3.4911667 

#comparing sf and terra for vector area calculation:

vect.terra<-vect(paste(loc.data,"IntermediateFiles/AggregatedVectors/Treatments_CA_ForestHealth_2020_2024.shp",sep=""))
terra.time<-system.time(vect.terra.area<-expanse(vect.terra))

library("sf")

vect.sf<-st_read(paste(loc.data,"IntermediateFiles/AggregatedVectors/Treatments_CA_ForestHealth_2020_2024.shp",sep=""))
sf.time<-system.time(vect.sf.area<-st_area(vect.sf))


hist(treat.vect$ACTIVITY_END,breaks=50)
hist(treat.vect$ACTIVITY_END[treat.vect$ACTIVITY_END>"2020-10-01"],breaks=50)

hist(as.Date(treat.vect$ACTIVITY_START),breaks=50)
hist(as.Date(treat.vect$ACTIVITY_START)[as.Date(treat.vect$ACTIVITY_START)>"2020-10-01"],breaks=50)

treat.vect$duration<-as.Date(treat.vect$ACTIVITY_END)-as.Date(treat.vect$ACTIVITY_START)
hist(as.numeric(as.character(treat.vect$duration[as.Date(treat.vect$ACTIVITY_START)>"2020-10-01" &
	as.Date(treat.vect$ACTIVITY_END)<"2024-09-30"],breaks=50)))

sum(as.Date(treat.vect$ACTIVITY_START)>"2020-10-01" &
	as.Date(treat.vect$ACTIVITY_END)<"2024-09-30",na.rm=TRUE)
#about 31 K records

sum(as.Date(treat.vect$ACTIVITY_START)>"2020-10-01" &
	as.Date(treat.vect$ACTIVITY_END)<"2024-09-30" & treat.vect$duration<=0,na.rm=TRUE)
#13662 have negative durations

sum(as.Date(treat.vect$ACTIVITY_START)>"2020-10-01" &
	as.Date(treat.vect$ACTIVITY_END)<"2024-09-30" & treat.vect$duration>365,na.rm=TRUE)
#4878 have durations longer than a year 

summary(as.numeric(as.character(treat.vect$duration[as.Date(treat.vect$ACTIVITY_START)>"2020-10-01" &
	as.Date(treat.vect$ACTIVITY_END)<"2024-09-30"])))

