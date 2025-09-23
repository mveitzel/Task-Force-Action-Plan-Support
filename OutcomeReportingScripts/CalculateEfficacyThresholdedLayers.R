#CalculateEfficacyThresholdedLayers.R



  # #---------- Drought Vulnerability read in and process---------*


timer.start<-Sys.time()
	vint<-"250614"
	metric<-"Vulner_TreeDieoff_SPI-2"
	xlabel<-"Average decrease in Drought Vulnerability"
	conversion<-NA

	before.yr.name<-generate.CECS.filename(metric,start.year,vint)
	after.yr.name<-generate.CECS.filename(metric,end.year,vint)

	before.rast<-read.in.raster(loc.data,before.yr.name,metrics[5])
	after.rast<-read.in.raster(loc.data,after.yr.name,metrics[5])

	before.proj.rast<-check.crs.match(reference.rast,before.rast)
	after.proj.rast<-check.crs.match(reference.rast,after.rast)

	print("Begin thresholding")
	# John Battles compared the distribution of drought vulnerability values to relative Stand Density Index Max values, and 
	#found that the "Imminent Mortality" range (>= 60 rSDImax) is the top 14.5%.  So the top 14.5% of DV values correspond to
	#a threshold of 7310
	dv.before.rast<-before.proj.rast
	dv.before.rast[dv.before.rast < 7310]<-0
	dv.before.rast[dv.before.rast >= 7310]<-1
	print("Drought Vulnerability thresholded: before")
  
	dv.after.rast<-after.proj.rast
	dv.after.rast[dv.after.rast < 7310]<-0
	dv.after.rast[dv.after.rast >= 7310]<-1
	print("Drought Vulnerability thresholded: after")

	forest.cecs.rast<-rast(paste(loc.data,"WUIVegetationClassifications/WHR13_RECLASS_FOREST_CECS.tif",sep=""))
	dv.aft.masked.rast<-dv.after.rast*forest.cecs.rast
	dv.bef.masked.rast<-dv.before.rast*forest.cecs.rast
	print("Raster masked for forest")

	print(paste("Writing IntermediateFiles/ThresholdedEfficacyLayers/Thresholded_",metrics[5],"_",start.year,".tif",sep=""))
	writeRaster(dv.bef.masked.rast,paste(loc.data,"IntermediateFiles/ThresholdedEfficacyLayers/Thresholded_",metrics[5],"_",start.year,".tif",sep=""),overwrite=TRUE)
	print(paste("Writing IntermediateFiles/ThresholdedEfficacyLayers/Thresholded_",metrics[5],"_",end.year,".tif",sep=""))
	writeRaster(dv.aft.masked.rast,paste(loc.data,"IntermediateFiles/ThresholdedEfficacyLayers/Thresholded_",metrics[5],"_",end.year,".tif",sep=""),overwrite=TRUE)

timer.end<-Sys.time()

time.total<-timer.end-timer.start
print(time.total)


  # #---------- Flame Length read in and process---------*

timer.start<-Sys.time()

	vint<-"250614"
	metric<-"Fire_FlamMap_FL"
	xlabel<-"Average decrease in flame length (ft)"
	#'units are 0.01 m' so divide by 100, but want ft so multiply by 3.28084
	conversion<-(0.0328084)

	before.yr.name<-generate.CECS.filename(metric,start.year,vint)
	after.yr.name<-generate.CECS.filename(metric,end.year,vint)

	before.rast<-read.in.raster(loc.data,before.yr.name,metrics[4])
	after.rast<-read.in.raster(loc.data,after.yr.name,metrics[4])

	before.proj.rast<-check.crs.match(reference.rast,before.rast)
	after.proj.rast<-check.crs.match(reference.rast,after.rast)

	print("Begin thresholding")
	# Everyone seems to agree that flame length above 8 feet is likely to be a high-severity or hard to control fire
	#convert from meters to feet and undo the storage multiplicative factor:
	#"Units are FL 0.01 m" -> *3.28084/100
	fl.before.rast<-before.proj.rast*0.0328084
	fl.before.rast[fl.before.rast<8]<-0
	fl.before.rast[fl.before.rast >= 8 ]<-1
	print("Flame Length thresholded: before")
  
	fl.after.rast<-after.proj.rast*0.0328084
	fl.after.rast[fl.after.rast<8]<-0
	fl.after.rast[fl.after.rast >= 8 ]<-1
	print("Flame Length thresholded: after")
 
	#	mask for roads
	road.buff.cecs.rast<-rast(paste(loc.data,"WUIVegetationClassifications/RoadBuffer_CECSproj.tif",sep=""))
	road.aft.masked.rast<-fl.after.rast*road.buff.cecs.rast
	road.bef.masked.rast<-fl.before.rast*road.buff.cecs.rast
	print("Raster masked for roads")

	print(paste("Writing IntermediateFiles/ThresholdedEfficacyLayers/Thresholded_",metrics[4],"_",start.year,".tif",sep=""))
	writeRaster(road.bef.masked.rast,paste(loc.data,"IntermediateFiles/ThresholdedEfficacyLayers/Thresholded_",metrics[4],"_",start.year,".tif",sep=""),overwrite=TRUE)
	print(paste("Writing IntermediateFiles/ThresholdedEfficacyLayers/Thresholded_",metrics[4],"_",end.year,".tif",sep=""))
	writeRaster(road.aft.masked.rast,paste(loc.data,"IntermediateFiles/ThresholdedEfficacyLayers/Thresholded_",metrics[4],"_",end.year,".tif",sep=""),overwrite=TRUE)


	#	mask for utilities
	tran.buff.cecs.rast<-rast(paste(loc.data,"WUIVegetationClassifications/TransmissionLineBuffer_CECSproj.tif",sep=""))
	tran.aft.masked.rast<-fl.after.rast*tran.buff.cecs.rast
	tran.bef.masked.rast<-fl.before.rast*tran.buff.cecs.rast
	print("Raster masked for Utilities")

	print(paste("Writing IntermediateFiles/ThresholdedEfficacyLayers/Thresholded_",metrics[3],"_",start.year,".tif",sep=""))
	writeRaster(tran.bef.masked.rast,paste(loc.data,"IntermediateFiles/ThresholdedEfficacyLayers/Thresholded_",metrics[3],"_",start.year,".tif",sep=""),overwrite=TRUE)
	print(paste("Writing IntermediateFiles/ThresholdedEfficacyLayers/Thresholded_",metrics[3],"_",end.year,".tif",sep=""))
	writeRaster(tran.aft.masked.rast,paste(loc.data,"IntermediateFiles/ThresholdedEfficacyLayers/Thresholded_",metrics[3],"_",end.year,".tif",sep=""),overwrite=TRUE)


    # 'mask' for whole landscape
	land.cecs.rast<-rast(paste(loc.data,"WUIVegetationClassifications/FRAP24_Landscape_CECS.tif",sep=""))
	land.aft.masked.rast<-fl.after.rast*land.cecs.rast
	land.bef.masked.rast<-fl.before.rast*land.cecs.rast
	print("Raster masked for Landscape")

	print(paste("Writing IntermediateFiles/ThresholdedEfficacyLayers/Thresholded_",metrics[2],"_",start.year,".tif",sep=""))
	writeRaster(land.bef.masked.rast,paste(loc.data,"IntermediateFiles/ThresholdedEfficacyLayers/Thresholded_",metrics[2],"_",start.year,".tif",sep=""),overwrite=TRUE)
	print(paste("Writing IntermediateFiles/ThresholdedEfficacyLayers/Thresholded_",metrics[2],"_",end.year,".tif",sep=""))
	writeRaster(land.aft.masked.rast,paste(loc.data,"IntermediateFiles/ThresholdedEfficacyLayers/Thresholded_",metrics[2],"_",end.year,".tif",sep=""),overwrite=TRUE)

 
    #mask for WUI
	wui.cecs.rast<-rast(paste(loc.data,"WUIVegetationClassifications/FRAP24_WUIOnly_CECS.tif",sep=""))
	wui.aft.masked.rast<-fl.after.rast*wui.cecs.rast
	wui.bef.masked.rast<-fl.before.rast*wui.cecs.rast
	print("Raster masked for WUI")

	print(paste("Writing IntermediateFiles/ThresholdedEfficacyLayers/Thresholded_",metrics[1],"_",start.year,".tif",sep=""))
	writeRaster(wui.bef.masked.rast,paste(loc.data,"IntermediateFiles/ThresholdedEfficacyLayers/Thresholded_",metrics[1],"_",start.year,".tif",sep=""),overwrite=TRUE)
	print(paste("Writing IntermediateFiles/ThresholdedEfficacyLayers/Thresholded_",metrics[1],"_",end.year,".tif",sep=""))
	writeRaster(wui.aft.masked.rast,paste(loc.data,"IntermediateFiles/ThresholdedEfficacyLayers/Thresholded_",metrics[1],"_",end.year,".tif",sep=""),overwrite=TRUE)


timer.end<-Sys.time()

time.total<-timer.end-timer.start
print(time.total)


#---------- SHRUB-GRASS RATIO CALCS -------------------#

timer.start<-Sys.time()

	vint<-"250418"
	metric.shrub<-"Veg_ShrubFrac"
	metric.grass<-"Veg_HerbFrac"
	xlabel<-"Average decrease in Shrub-Grass Ratio"
	conversion<-NA #the conversion factor for the individual proportions are 1/10000, but we're doing a ratio so they cancel

	#filenaming convention changed for the CONUS CECS runs
	before.yr.shrub.name<-"CECS_Data/ShrubCover_2020_WestCoast.tif"
	after.yr.shrub.name<-"CECS_Data/ShrubCover_2024_WestCoast.tif"
	before.yr.grass.name<-"CECS_Data/HerbCover_2020_WestCoast.tif"
	after.yr.grass.name<-"CECS_Data/HerbCover_2024_WestCoast.tif"

	before.shrub.west.rast<-read.in.raster(loc.data,before.yr.shrub.name,metrics[6])
	after.shrub.west.rast<-read.in.raster(loc.data,after.yr.shrub.name,metrics[6])
	before.grass.west.rast<-read.in.raster(loc.data,before.yr.grass.name,metrics[6])
	after.grass.west.rast<-read.in.raster(loc.data,after.yr.grass.name,metrics[6])

	#need to clip to CA
	before.shrub.cr.rast<-crop(before.shrub.west.rast,reference.rast)
	before.shrub.rast<-mask(before.shrub.cr.rast,reference.rast)

	after.shrub.cr.rast<-crop(after.shrub.west.rast,reference.rast)
	after.shrub.rast<-mask(after.shrub.cr.rast,reference.rast)

	before.grass.cr.rast<-crop(before.grass.west.rast,reference.rast)
	before.grass.rast<-mask(before.grass.cr.rast,reference.rast)

	after.grass.cr.rast<-crop(after.grass.west.rast,reference.rast)
	after.grass.rast<-mask(after.grass.cr.rast,reference.rast)

	#where the grass proportion is zero, we don't want to divide by it.
	#but we don't want to substitute an arbitrarily small value that will mess up
	#the mean values.  So find the minimum value that isn't zero
	# if there's a problem with the grass data, we don't want it to mess up our
	#calculations, but also don't want to throw out a shrub value
	mask.before.grass.rast<-before.grass.rast
	mask.before.grass.rast[before.grass.rast<=0]<-NA
	bef.grass.min<-as.numeric(global(mask.before.grass.rast,"min",na.rm=TRUE))
	#and do the same for the 'after' grass raster, though likely it's the same
	mask.after.grass.rast<-after.grass.rast
	mask.after.grass.rast[after.grass.rast<=0]<-NA
	aft.grass.min<-as.numeric(global(mask.after.grass.rast,"min",na.rm=TRUE))

	before.grass.prepped.rast<-before.grass.rast
	before.grass.prepped.rast[before.grass.prepped.rast==0]<-bef.grass.min
	after.grass.prepped.rast<-after.grass.rast
	after.grass.prepped.rast[after.grass.prepped.rast==0]<-aft.grass.min

	#make the numbers that are negative into NAs though
	before.grass.prepped.rast[before.grass.prepped.rast<0]<-NA
	after.grass.prepped.rast[after.grass.prepped.rast<0]<-NA


	# also there are negative values in the shrub layer, so we will replace those with NA values.
	#If there's a problem with the shrub data, we just don't want to report on it.
	before.shrub.prepped.rast<-before.shrub.rast
	before.shrub.prepped.rast[before.shrub.prepped.rast<0]<-NA
	after.shrub.prepped.rast<-after.shrub.rast
	after.shrub.prepped.rast[after.shrub.prepped.rast<0]<-NA

	# "Units are % multiplied by 100, so a tree cover value of 9000 corresponds to 90% tree cover."
	# so as long as max values are below 10000, we're okay.  a quick 'global' check verified that.
	#now it should be safe to divide by the grass layers
	#and neither of them will have weird negative values
	before.rast<-before.shrub.prepped.rast/before.grass.prepped.rast
	after.rast<-after.shrub.prepped.rast/after.grass.prepped.rast

	before.proj.rast<-check.crs.match(reference.rast,before.rast)
	after.proj.rast<-check.crs.match(reference.rast,after.rast)


	print("Begin thresholding")
	# "Units are % multiplied by 100, so a tree cover value of 9000 corresponds to 90% tree cover."
	# so as long as max values are below 10000, we're okay.  a quick 'global' check verified that.
	# also, once we've done a ratio we don't care anymore about the multiplicative conversion factor.
	# We just apply our threshold that we want the plot to be majority shrub, so shrub/grass is at least 1.
	sh.before.rast<-before.proj.rast
	sh.before.rast[sh.before.rast<=1]<-1
	sh.before.rast[sh.before.rast > 1 ]<-0
	print("Shrub-Grass ratio thresholded: before")
  
	sh.after.rast<-after.proj.rast
	sh.after.rast[sh.after.rast<=1]<-1
	sh.after.rast[sh.after.rast > 1 ]<-0
	print("Shrub-Grass ratio thresholded: after")
 
	#	mask for shrubs
	shrub.cecs.rast<-rast(paste(loc.data,"WUIVegetationClassifications/WHR13_RECLASS_SHRUB_CECS.tif",sep=""))
	shrub.aft.masked.rast<-sh.after.rast*shrub.cecs.rast
	shrub.bef.masked.rast<-sh.before.rast*shrub.cecs.rast
	print("Raster masked for shrub")

	print(paste("Writing IntermediateFiles/ThresholdedEfficacyLayers/Thresholded_",metrics[6],"_",start.year,".tif",sep=""))
	writeRaster(shrub.bef.masked.rast,paste(loc.data,"IntermediateFiles/ThresholdedEfficacyLayers/Thresholded_",metrics[6],"_",start.year,".tif",sep=""),overwrite=TRUE)
	print(paste("Writing IntermediateFiles/ThresholdedEfficacyLayers/Thresholded_",metrics[6],"_",end.year,".tif",sep=""))
	writeRaster(shrub.aft.masked.rast,paste(loc.data,"IntermediateFiles/ThresholdedEfficacyLayers/Thresholded_",metrics[6],"_",end.year,".tif",sep=""),overwrite=TRUE)


timer.end<-Sys.time()

time.total<-timer.end-timer.start
print(time.total)



#---------- GRASS PROPORTION CALCS -------------------#

timer.start<-Sys.time()

	vint<-"250418"
	metric.grass<-"Veg_HerbFrac"
	#xlabel<-"Average decrease in Shrub-Grass Ratio"
	conversion<-NA #the conversion factor for the individual proportions are 1/10000, but we're doing a ratio so they cancel

	#filenaming convention changed for the CONUS CECS runs
	before.yr.grass.name<-"CECS_Data/HerbCover_2020_WestCoast.tif"
	after.yr.grass.name<-"CECS_Data/HerbCover_2024_WestCoast.tif"

	before.grass.west.rast<-read.in.raster(loc.data,before.yr.grass.name,metrics[1])
	after.grass.west.rast<-read.in.raster(loc.data,after.yr.grass.name,metrics[1])

	#need to clip to CA
	before.grass.cr.rast<-crop(before.grass.west.rast,reference.rast)
	before.grass.rast<-mask(before.grass.cr.rast,reference.rast)

	after.grass.cr.rast<-crop(after.grass.west.rast,reference.rast)
	after.grass.rast<-mask(after.grass.cr.rast,reference.rast)

	# "Units are % multiplied by 100, so a tree cover value of 9000 corresponds to 90% tree cover."
	# so as long as max values are below 10000, we're okay.  a quick 'global' check verified that.
	
	before.proj.rast<-check.crs.match(reference.rast,before.grass.rast)
	after.proj.rast<-check.crs.match(reference.rast,after.grass.rast)


	print("Begin thresholding")
	# "Units are % multiplied by 100, so a tree cover value of 9000 corresponds to 90% tree cover."
	# so as long as max values are below 10000, we're okay.  a quick 'global' check verified that.
	# Talking with Emma Underwood, Nicole Molinari, and Alexandra Syphard, we settled on a simple measure
	# of grass proportion being greater than or equal to 50 % in an area that should otherwise be shrub
	# just as a simple metric of shrubland intactness.
	sh.before.rast<-before.proj.rast
	sh.before.rast[sh.before.rast<=5000]<-1
	sh.before.rast[sh.before.rast > 5000 ]<-0
	print("Grass proportion thresholded: before")
  
	sh.after.rast<-after.proj.rast
	sh.after.rast[sh.after.rast<=5000]<-1
	sh.after.rast[sh.after.rast > 5000 ]<-0
	print("Grass proportion thresholded: after")
 
	#	mask for shrubs
	shrub.cecs.rast<-rast(paste(loc.data,"WUIVegetationClassifications/WHR13_RECLASS_SHRUB_CECS.tif",sep=""))
	shrub.aft.masked.rast<-sh.after.rast*shrub.cecs.rast
	shrub.bef.masked.rast<-sh.before.rast*shrub.cecs.rast
	print("Raster masked for shrub")

	print(paste("Writing IntermediateFiles/ThresholdedEfficacyLayers/Thresholded_",metrics[1],"_",start.year,".tif",sep=""))
	writeRaster(shrub.bef.masked.rast,paste(loc.data,"IntermediateFiles/ThresholdedEfficacyLayers/Thresholded_",metrics[1],"_",start.year,".tif",sep=""),overwrite=TRUE)
	print(paste("Writing IntermediateFiles/ThresholdedEfficacyLayers/Thresholded_",metrics[1],"_",end.year,".tif",sep=""))
	writeRaster(shrub.aft.masked.rast,paste(loc.data,"IntermediateFiles/ThresholdedEfficacyLayers/Thresholded_",metrics[1],"_",end.year,".tif",sep=""),overwrite=TRUE)


timer.end<-Sys.time()

time.total<-timer.end-timer.start
print(time.total)

