#CalculateMetricDiffs.R

#this section contains the specifics for each metric, including
#the CECS conversion factor and the actual function calls to 
# the raster differencing calculation

#---------- FLAME LENGTH (WUI) RASTER CALCS -------------------#

	vint<-"250614"
	metric<-"Fire_FlamMap_FL"
	xlabel<-"Average decrease in flame length (ft)"
	#'units are 0.01 m' so divide by 100, but want ft so multiply by 3.28084
	conversion<-(0.0328084)

	before.yr.name<-generate.CECS.filename(metric,start.year,vint)
	after.yr.name<-generate.CECS.filename(metric,end.year,vint)

	before.rast<-read.in.raster(loc.data,before.yr.name,metrics[1])
	after.rast<-read.in.raster(loc.data,after.yr.name,metrics[1])

	before.proj.rast<-check.crs.match(reference.rast,before.rast)
	after.proj.rast<-check.crs.match(reference.rast,after.rast)

	diff.rast<-diff.rasters(start.year,before.proj.rast,end.year,after.proj.rast,metrics[1])

	#	mask for WUI
	# do I want to use subset.raster here?
	wui.cecs.rast<-rast(paste(loc.data,"WUIVegetationClassifications/FRAP24_WUIOnly_CECS.tif",sep=""))
	diff.masked.rast<-diff.rast*wui.cecs.rast
	print("Raster masked for WUI")

	diff<-multiply.conversion.factor(metrics[1],diff.masked.rast,conversion)

	print(paste(loc.data,"Writing IntermediateFiles/DiffRasters/Diff_",metrics[1],".tif",sep=""))
	writeRaster(diff,paste(loc.data,"IntermediateFiles/DiffRasters/Diff_",metrics[1],".tif",sep=""))

#------------ end flame length (WUI) raster calcs -----------#

#---------- FLAME LENGTH (LANDSCAPE) RASTER CALCS -------------------#

	vint<-"250614"
	metric<-"Fire_FlamMap_FL"
	xlabel<-"Average decrease in flame length (ft)"
	#'units are 0.01 m' so divide by 100, but want ft so multiply by 3.28084
	conversion<-(0.0328084)

	before.yr.name<-generate.CECS.filename(metric,start.year,vint)
	after.yr.name<-generate.CECS.filename(metric,end.year,vint)

	before.rast<-read.in.raster(loc.data,before.yr.name,metrics[2])
	after.rast<-read.in.raster(loc.data,after.yr.name,metrics[2])

	before.proj.rast<-check.crs.match(reference.rast,before.rast)
	after.proj.rast<-check.crs.match(reference.rast,after.rast)

	diff.rast<-diff.rasters(start.year,before.proj.rast,end.year,after.proj.rast,metrics[2])

	#	mask for landscape
	# do I want to use subset.raster here?
	land.cecs.rast<-rast(paste(loc.data,"WUIVegetationClassifications/FRAP24_Landscape_CECS.tif",sep=""))
	diff.masked.rast<-diff.rast*land.cecs.rast
	print("Raster masked for Landscape")

	diff<-multiply.conversion.factor(metrics[2],diff.masked.rast,conversion)

	print(paste(loc.data,"Writing IntermediateFiles/DiffRasters/Diff_",metrics[2],".tif",sep=""))
	writeRaster(diff,paste(loc.data,"IntermediateFiles/DiffRasters/Diff_",metrics[2],".tif",sep=""),overwrite=TRUE)

#------------ end flame length (Wildland) raster calcs -----------#

#---------- FLAME LENGTH (UTILITIES) RASTER CALCS -------------------#

	vint<-"250614"
	metric<-"Fire_FlamMap_FL"
	xlabel<-"Average decrease in flame length (ft)"
	#'units are 0.01 m' so divide by 100, but want ft so multiply by 3.28084
	conversion<-(0.0328084)

	before.yr.name<-generate.CECS.filename(metric,start.year,vint)
	after.yr.name<-generate.CECS.filename(metric,end.year,vint)

	before.rast<-read.in.raster(loc.data,before.yr.name,metrics[3])
	after.rast<-read.in.raster(loc.data,after.yr.name,metrics[3])

	before.proj.rast<-check.crs.match(reference.rast,before.rast)
	after.proj.rast<-check.crs.match(reference.rast,after.rast)

	diff.rast<-diff.rasters(start.year,before.proj.rast,end.year,after.proj.rast,metrics[3])

	#	mask for utilities
	# do I want to use subset.raster here?
	tran.buff.cecs.rast<-rast(paste(loc.data,"WUIVegetationClassifications/TransmissionLineBuffer_CECSproj.tif",sep=""))
	diff.masked.rast<-diff.rast*tran.buff.cecs.rast
	print("Raster masked for Utilities")

	diff<-multiply.conversion.factor(metrics[3],diff.masked.rast,conversion)
	print(paste(loc.data,"Writing IntermediateFiles/DiffRasters/Diff_",metrics[3],".tif",sep=""))
	writeRaster(diff,paste(loc.data,"IntermediateFiles/DiffRasters/Diff_",metrics[3],".tif",sep=""))


#------------ end flame length (Utilities) raster calcs -----------#


#---------- FLAME LENGTH (ROADS) RASTER CALCS -------------------#

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

	diff.rast<-diff.rasters(start.year,before.proj.rast,end.year,after.proj.rast,metrics[4])

	#	mask for roads
	# do I want to use subset.raster here?
	road.buff.cecs.rast<-rast(paste(loc.data,"WUIVegetationClassifications/RoadBuffer_CECSproj.tif",sep=""))
	diff.masked.rast<-diff.rast*road.buff.cecs.rast
	print("Raster masked for roads")

	diff<-multiply.conversion.factor(metrics[4],diff.masked.rast,conversion)
	print(paste(loc.data,"Writing IntermediateFiles/DiffRasters/Diff_",metrics[4],".tif",sep=""))
	writeRaster(diff,paste(loc.data,"IntermediateFiles/DiffRasters/Diff_",metrics[4],".tif",sep=""))


#------------ end flame length (Roads) raster calcs -----------#


#---------- DROUGHT VULNERABILITY RASTER CALCS -------------------#

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

	diff.rast<-diff.rasters(start.year,before.proj.rast,end.year,after.proj.rast,metrics[5])

	#	mask for forest
	# do I want to use subset.raster here?
	forest.cecs.rast<-rast(paste(loc.data,"WUIVegetationClassifications/WHR13_RECLASS_FOREST_CECS.tif",sep=""))
	diff.masked.rast<-diff.rast*forest.cecs.rast
	print("Raster masked for forest")

	diff<-multiply.conversion.factor(metrics[5],diff.masked.rast,conversion)
	print(paste(loc.data,"Writing IntermediateFiles/DiffRasters/Diff_",metrics[5],".tif",sep=""))
	writeRaster(diff,paste(loc.data,"IntermediateFiles/DiffRasters/Diff_",metrics[5],".tif",sep=""))


#------------ end drought vulnerability raster calcs -----------#



#---------- SHRUB-GRASS RATIO CALCS -------------------#

	vint<-"250418"
	metric.shrub<-"Veg_ShrubFrac"
	metric.grass<-"Veg_HerbFrac"
	xlabel<-"Average decrease in Shrub-Grass Ratio"
	conversion<-NA #the conversion factor for the individual proportions are 1/10000, but we're doing a ratio so they cancel

#	before.yr.shrub.name<-generate.CECS.filename(metric.shrub,start.year,vint)
#	after.yr.shrub.name<-generate.CECS.filename(metric.shrub,end.year,vint)
#	before.yr.grass.name<-generate.CECS.filename(metric.grass,start.year,vint)
#	after.yr.grass.name<-generate.CECS.filename(metric.grass,end.year,vint)

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
	# so as long as max values are below 10000, we're okay.  a quick 'global' check verifies that.

	#now it should be safe to divide by the grass layers
	#and neither of them will have weird negative values
	before.rast<-before.shrub.prepped.rast/before.grass.prepped.rast
	after.rast<-after.shrub.prepped.rast/after.grass.prepped.rast

	#just checking on how many of these produce infinite values based on division by 0
	#before pulling the trick with replacing with lowest nonzero value, it was about 0.3-0.4%
 	#global(is.infinite(after.rast),"sum")/global(after.rast,"notNA")
 	#global(is.infinite(before.rast),"sum")/global(after.rast,"notNA")

	before.proj.rast<-check.crs.match(reference.rast,before.rast)
	after.proj.rast<-check.crs.match(reference.rast,after.rast)

	diff.rast<-diff.rasters(start.year,before.proj.rast,end.year,after.proj.rast,metrics[6])

	#   mask for shrub
	shrub.cecs.rast<-rast(paste(loc.data,"WUIVegetationClassifications/WHR13_RECLASS_SHRUB_CECS.tif",sep=""))
	diff.masked.rast<-diff.rast*shrub.cecs.rast
	print("Raster masked for shrub")

	diff<-multiply.conversion.factor(metrics[6],diff.masked.rast,conversion)
	print(paste(loc.data,"Writing IntermediateFiles/DiffRasters/Diff_",metrics[6],".tif",sep=""))
	writeRaster(diff,paste(loc.data,"IntermediateFiles/DiffRasters/Diff_",metrics[6],".tif",sep=""),overwrite=TRUE)


#------------ end shrub-grass ratio calcs -----------#



