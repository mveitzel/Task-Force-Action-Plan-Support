#CalculateWUI_Veg_Masks.R


# for rasters that are getting projected onto each other for consistency, 
# the naming convention is dataorigin.<resamp/proj/etc>.projectiondata.rast
# so for CECS data that is getting put into the projection of WHP data,
# the name is CECS.<proj/resamp/etc>.WHP.rast

###---------------------State Boundary --------------------###

#First let's get the state boundary from the Task Force regions.

# regions.vect<-vect(paste(loc.scripts,"ReferenceFiles/TaskForceRegions_20250722.shp",sep=""))
# state.vect<-aggregate(regions.vect)
# #there are some small mismatches with the region boundaries, so get rid of those 'holes':
# state_clean.vect<-fillHoles(state.vect)
# writeVector(state_clean.vect,paste(loc.scripts,"ReferenceFiles/CA_State_TF.shp",sep=""))


###--------------------- WUI LAYERS-----------------------###

# raw WUI layer, doesn't match anything else and needs to be projected to WHP/WHR and CECS worlds respectively
wui_FRAP.rast<-rast(paste(loc.data,"WUIVegetationClassifications/WUI24_extract.tif",sep=""))

#reclass first
#For the FRAP layer via SIG, 2=intermix, 1=interface, and 3=influence
# Per Forest, only use intermix and interface
wui.reclass.rast<-wui_FRAP.rast %in% c(2,1)
wui.only.rast<-wui.reclass.rast
wui.only.rast[wui.reclass.rast]<-1
wui.only.rast[wui.only.rast!=1]<-NA

#Create a version of the wui layer that matches WHP and ecosystem masps (WHR)
whp.rast <- rast(paste(loc.data,"PriorityLayers/whp_classified_20240906.tif",sep=""))
wui.proj.whp.rast<-check.crs.match(whp.rast,wui.only.rast,"near")
writeRaster(wui.proj.whp.rast,paste(loc.scripts,"ReferenceFiles/FRAP24_WUIOnly_WHP.tif",sep=""))

#create a version of the wui layer that matches CECS (to make wildland and wui layer for CECS layers)
cecs.rast<-rast(paste(loc.data,"CECS_Data/CECS_CAWide_Vulner_TreeDieoff_SPI-2_2020_V250614.tif",sep=""))
wui.proj.cecs.rast<-check.crs.match(cecs.rast,wui.only.rast,"near")
writeRaster(wui.proj.cecs.rast,paste(loc.data,"WUIVegetationClassifications/FRAP24_WUIOnly_CECS.tif",sep=""))


###------------- WILDLAND LAYERS -------------------------###

#reclassify first
#pull out just the non ag and urban areas from CECS (which are wildland and wui)
non.ag.urban.class.cecs.rast<-!is.na(cecs.rast)
#pull out the 'other' category from FRAP (code 0, which is ag, urban, and wildland)
# per Forest, include influence in with the wildland
other.class.wui.rast<-wui_FRAP.rast %in% c(0,3)


non.ag.urban.class.whp.rast<-check.crs.match(whp.rast,non.ag.urban.class.cecs.rast,"near")
other.class.whp.rast<-check.crs.match(whp.rast,other.class.wui.rast,"near")

# combine the other and the non-ag-urban, and you should get just wildland
wild.whp.rast<-(other.class.whp.rast)*(non.ag.urban.class.whp.rast)
#make a new raster and pull out only the wildland (set everything else to NA), and export
wild.only.whp.rast<-wild.whp.rast
wild.only.whp.rast[wild.whp.rast!=1]<-NA
writeRaster(wild.only.whp.rast,paste(loc.scripts,"ReferenceFiles/FRAP24_WildlandOnly_WHP.tif",sep=""))


# And make a version where you use the CECS-projected WUI and CECS unprojected to make its own wildland stratification
other.class.cecs.rast<-check.crs.match(cecs.rast,other.class.wui.rast,"near")

wild.cecs.rast<-(other.class.cecs.rast)*(non.ag.urban.class.cecs.rast)
#make a new raster and pull out only the wildland (set everything else to NA), and export
wild.only.cecs.rast<-wild.cecs.rast
wild.only.cecs.rast[wild.cecs.rast!=1]<-NA
writeRaster(wild.only.cecs.rast,paste(loc.data,"WUIVegetationClassifications/FRAP24_WildlandOnly_CECS.tif",sep=""))

#landscape is wui and wildland, which is just the CECS NA mask
non.ag.urban.class.cecs.rast[non.ag.urban.class.cecs.rast==0]<-NA
non.ag.urban.class.whp.rast[non.ag.urban.class.whp.rast==0]<-NA

writeRaster(non.ag.urban.class.cecs.rast,paste(loc.data,"WUIVegetationClassifications/FRAP24_Landscape_CECS.tif",sep=""),overwrite=TRUE)
writeRaster(non.ag.urban.class.whp.rast,paste(loc.data,"WUIVegetationClassifications/FRAP24_Landscape_WHP.tif",sep=""),overwrite=TRUE)

###---------- WHR (forest/shrub layers) ------------------###

## the ecosystem (WHR) layers are fine for any calculations but CECS
## make a version for CECS

## ecosystem layers reclassified from Wildlife Habitat Relationships CALFIRE dataset
forest.rast<-rast(paste(loc.scripts,"ReferenceFiles/WHR13_RECLASS_FOREST.tif",sep=""))
forest.proj.rast<-check.crs.match(cecs.rast,forest.rast,"near")
  forest.proj.rast<-mask(forest.proj.rast,ca.cecs.vect)
writeRaster(forest.proj.rast,paste(loc.data,"WUIVegetationClassifications/WHR13_RECLASS_FOREST_CECS.tif",sep=""))
  #For some reason WHR extends beyond CA boundaries  
  forest.rast<-mask(forest.rast,ca.whp.vect)
writeRaster(forest.rast,paste(loc.scripts,"ReferenceFiles/WHR13_RECLASS_FOREST.tif",sep=""))

shrub.rast<-rast(paste(loc.scripts,"ReferenceFiles/WHR13_RECLASS_SHRUB.tif",sep=""))
shrub.proj.rast<-check.crs.match(cecs.rast,shrub.rast,"near")
  shrub.proj.rast<-mask(shrub.proj.rast,ca.cecs.vect)
writeRaster(shrub.proj.rast,paste(loc.data,"WUIVegetationClassifications/WHR13_RECLASS_SHRUB_CECS.tif",sep=""))
  #For some reason WHR extends beyond CA boundaries  
  shrub.rast<-mask(shrub.rast,ca.whp.vect)
writeRaster(shrub.rast,paste(loc.scripts,"ReferenceFiles/WHR13_RECLASS_SHRUB.tif",sep=""))

grass.rast<-rast(paste(loc.data,"WUIVegetationClassifications/WHR13_RECLASS_GRASS.tif",sep=""))
grass.proj.rast<-check.crs.match(cecs.rast,grass.rast,"near")
  grass.proj.rast<-mask(grass.proj.rast,ca.cecs.vect)
writeRaster(grass.proj.rast,paste(loc.data,"WUIVegetationClassifications/WHR13_RECLASS_GRASS_CECS.tif",sep=""),overwrite=TRUE)
  #For some reason WHR extends beyond CA boundaries  
  grass.rast<-mask(grass.rast,ca.whp.vect)
writeRaster(grass.rast,paste(loc.data,"WUIVegetationClassifications/WHR13_RECLASS_GRASS.tif",sep=""),overwrite=TRUE)

wood.rast<-rast(paste(loc.data,"WUIVegetationClassifications/WHR13_RECLASS_WOODLAND.tif",sep=""))
wood.proj.rast<-check.crs.match(cecs.rast,wood.rast,"near")
  wood.proj.rast<-mask(wood.proj.rast,ca.cecs.vect)
writeRaster(wood.proj.rast,paste(loc.data,"WUIVegetationClassifications/WHR13_RECLASS_WOODLAND_CECS.tif",sep=""),overwrite=TRUE)
  #For some reason WHR extends beyond CA boundaries  
  wood.rast<-mask(wood.rast,ca.whp.vect)
writeRaster(wood.rast,paste(loc.data,"WUIVegetationClassifications/WHR13_RECLASS_WOODLAND.tif",sep=""),overwrite=TRUE)


##########

# #---------------- Powerlines + Roads -------------------------#
#buffering by 500 ft on either side

SDGE.vect<-vect(paste(loc.data,"PriorityLayers/SDGE_2023_Q2NonConfidential.gdb",sep=""),layer="SDGE_TransmissionLine_2023_Q2")
#Note that SDGE also has "SDGE_PrimaryDistributionLine_2023_Q2"
SCE.vect<-vect(paste(loc.data,"PriorityLayers/SCE_ICA_TransmissionLines.shp",sep=""))
PGE.vect<-vect(paste(loc.data,"PriorityLayers/TransmissionLines_upTo_115kV.shp",sep=""))
#gisdata-caltrans.opendata.arcgis.com/datasets/cf4982ddf16c4c9ca7242364c94c7ad6_0/about
#updated 2024
road.vect<-vect(paste(loc.data,"PriorityLayers/Public_Road_Functional_Classification.shp",sep=""))


SDGE.cecs.vect<-check.crs.match(cecs.rast,SDGE.vect)
SCE.cecs.vect<-check.crs.match(cecs.rast,SCE.vect)
PGE.cecs.vect<-check.crs.match(cecs.rast,PGE.vect)
road.cecs.vect<-check.crs.match(cecs.rast,road.vect)

tran.cecs.vect<-union(SDGE.cecs.vect,SCE.cecs.vect)
tran.cecs.vect<-union(tran.cecs.vect,PGE.cecs.vect)
#rdtr.cecs.vect<-union(tran.cecs.vect,road.cecs.vect)


#roads, CECS CRS
road.buff.cecs.vect<-buffer(road.cecs.vect,width=500*0.3048)
road.buff.cecs.simp.vect<-aggregate(road.buff.cecs.vect)
road.buff.cecs.proj.vect<-check.crs.match(cecs.rast,road.buff.cecs.simp.vect)
writeVector(road.buff.cecs.proj.vect,paste(loc.data,"WUIVegetationClassifications/RoadBuffer_CECSproj.shp",sep=""),overwrite=TRUE)

#because rasterizing the entire Functional Road Classification dataset is
#too long to compute in a reasonable amount of time, we'll clip things
#for each region, and then stick them back together afterwards.

road.buff.cecs.proj.vect<-vect(paste(loc.data,"WUIVegetationClassifications/RoadBuffer_CECSproj.shp",sep=""))
#ca.vect<-vect(paste(loc.scripts,"ReferenceFiles/CA_State.shp",sep=""))
#ca.proj.vect<-check.crs.match(road.buff.cecs.proj.vect,ca.vect)
#road.crop.vect<-crop(road.buff.cecs.proj.vect,ca.proj.vect)

# #this takes very little time, and is 10-15% of the whole area
# test.vect<-vect(paste(loc.scripts,"ReferenceFiles/TCSI_boundary.shp",sep=""))
# test.proj.vect<-check.crs.match(cecs.rast,test.vect)
# test.crop.rast<-crop(cecs.rast,test.proj.vect)
# test.crop.vect<-crop(road.crop.vect,test.proj.vect)
# road.test.crop.rast<-rasterize(test.crop.vect,test.crop.rast)
#> dim(test.crop.rast)/dim(cecs.rast)
#[1] 0.1054222 0.1435667 1.0000000
#>

#now try it with a region (Sierra takes 10.5 min)
timer.start<-Sys.time()
#pull in a region
region.vect<-vect(paste(loc.scripts,"ReferenceFiles/Region_Sierra.shp",sep=""))
#project it to CECS or WHP
region.proj.vect<-check.crs.match(cecs.rast,region.vect)
#crop the raster (cecs or whp) to that region
region.crop.rast<-crop(cecs.rast,region.proj.vect)
#crop the road/transmission lines to that region
region.crop.vect<-crop(road.buff.cecs.proj.vect,region.proj.vect)
#then rasterize
road.test.crop.rast<-rasterize(region.crop.vect,region.crop.rast)
#and save in a new object
road.sierra.rast<-road.test.crop.rast
timer.end<-Sys.time()
time.total<-timer.end-timer.start
print(time.total)
#dim(test.crop.rast)/dim(cecs.rast)
#[1] 0.6090444 0.4848000 1.0000000
#> 


#now try it with a region (5 min for socal)
timer.start<-Sys.time()
#pull in a region
region.vect<-vect(paste(loc.scripts,"ReferenceFiles/Region_SouthernCA.shp",sep=""))
#project it to CECS or WHP
region.proj.vect<-check.crs.match(cecs.rast,region.vect)
#crop the raster (cecs or whp) to that region
region.crop.rast<-crop(cecs.rast,region.proj.vect)
#crop the road/transmission lines to that region
region.crop.vect<-crop(road.buff.cecs.proj.vect,region.proj.vect)
#then rasterize
road.test.crop.rast<-rasterize(region.crop.vect,region.crop.rast)
#and save in a new object
road.south.rast<-road.test.crop.rast
timer.end<-Sys.time()
time.total<-timer.end-timer.start
print(time.total)


#now try it with a region (9.5 min for Northern CA)
timer.start<-Sys.time()
#pull in a region
region.vect<-vect(paste(loc.scripts,"ReferenceFiles/Region_NorthernCA.shp",sep=""))
#project it to CECS or WHP
region.proj.vect<-check.crs.match(cecs.rast,region.vect)
#crop the raster (cecs or whp) to that region
region.crop.rast<-crop(cecs.rast,region.proj.vect)
#crop the road/transmission lines to that region
region.crop.vect<-crop(road.buff.cecs.proj.vect,region.proj.vect)
#then rasterize
road.test.crop.rast<-rasterize(region.crop.vect,region.crop.rast)
#and save in a new object
road.north.rast<-road.test.crop.rast
timer.end<-Sys.time()
time.total<-timer.end-timer.start
print(time.total)


#now try it with a region (I think Central took something like 9 min)
timer.start<-Sys.time()
#pull in a region
region.vect<-vect(paste(loc.scripts,"ReferenceFiles/Region_CentralCoast.shp",sep=""))
#project it to CECS or WHP
region.proj.vect<-check.crs.match(cecs.rast,region.vect)
#crop the raster (cecs or whp) to that region
region.crop.rast<-crop(cecs.rast,region.proj.vect)
#crop the road/transmission lines to that region
region.crop.vect<-crop(road.buff.cecs.proj.vect,region.proj.vect)
#then rasterize
road.test.crop.rast<-rasterize(region.crop.vect,region.crop.rast)
#and save in a new object
road.central.rast<-road.test.crop.rast
timer.end<-Sys.time()
time.total<-timer.end-timer.start
print(time.total)

#road.central.ext.rast<-extend(road.central.rast,cecs.rast)
#road.sierra.ext.rast<-extend(road.sierra.rast,cecs.rast)
#road.south.ext.rast<-extend(road.south.rast,cecs.rast)
#road.north.ext.rast<-extend(road.north.rast,cecs.rast)

road.allstate.cecs.rast<-mosaic(road.central.rast,road.sierra.rast,road.south.rast,road.north.rast,fun="max")

writeRaster(road.allstate.cecs.rast,paste(loc.data,"WUIVegetationClassifications/RoadBuffer_CECSproj.tif",sep=""),overwrite=TRUE)

road.allstate.cecs.rast<-rast(paste(loc.data,"WUIVegetationClassifications/RoadBuffer_CECSproj.tif",sep=""))
road.crop.cecs.rast<-crop(road.allstate.cecs.rast,cecs.rast)
road.cropext.cecs.rast<-extend(road.crop.cecs.rast,cecs.rast)
writeRaster(road.cropext.cecs.rast,paste(loc.data,"WUIVegetationClassifications/RoadBuffer_CECSproj.tif",sep=""),overwrite=TRUE)


#this would probably be faster than regions but need to figure out how to implement loop
#because the regions seem to work okay we'll do that.
# huc8.vect<-vect(paste(loc.scripts,"ReferenceFiles/HUC8.shp",sep=""))
# huc8.proj.vect<-check.crs.match(cecs.rast,huc8.vect)
# huc8.crop.vect<-crop(huc8.proj.vect,ca.proj.vect)
# test.vect<-huc8.crop.vect[1]
# test.proj.vect<-check.crs.match(cecs.rast,test.vect)
# test.crop.rast<-crop(cecs.rast,test.proj.vect)
# test.crop.vect<-crop(road.crop.vect,test.proj.vect)
# road.test.crop.rast<-rasterize(test.crop.vect,test.crop.rast)


# #---------------- Just transmission lines (for WHP, flame length, and shrubs) ----#
#500 foot buffer but function expects meters (CECS CRS)
tran.buff.cecs.vect<-buffer(tran.cecs.vect,width=500*0.3048)
tran.buff.cecs.simp.vect<-aggregate(tran.buff.cecs.vect)
tran.buff.cecs.proj.vect<-check.crs.match(cecs.rast,tran.buff.cecs.simp.vect)
writeVector(tran.buff.cecs.proj.vect,paste(loc.data,"WUIVegetationClassifications/TransmissionLinesBuffer_CECSproj.shp",sep=""),overwrite=TRUE)

tran.buff.cecs.proj.rast<-rasterize(tran.buff.cecs.proj.vect,cecs.rast)
writeRaster(tran.buff.cecs.proj.rast,paste(loc.data,"WUIVegetationClassifications/TransmissionLineBuffer_CECSproj.tif",sep=""),overwrite=TRUE)
  #transmission lines run past the state boundary too
  tran.buff.cecs.rast<-mask(tran.buff.cecs.proj.rast,ca.cecs.vect)
writeRaster(tran.buff.cecs.rast,paste(loc.data,"WUIVegetationClassifications/TransmissionLineBuffer_CECSproj.tif",sep=""),overwrite=TRUE)


# #---------------- do roads and utilities for WHP projection----------#

SDGE.whp.vect<-check.crs.match(whp.rast,SDGE.vect)
SCE.whp.vect<-check.crs.match(whp.rast,SCE.vect)
PGE.whp.vect<-check.crs.match(whp.rast,PGE.vect)
road.whp.vect<-check.crs.match(whp.rast,road.vect)

tran.whp.vect<-union(SDGE.whp.vect,SCE.whp.vect)
tran.whp.vect<-union(tran.whp.vect,PGE.whp.vect)
#rdtr.whp.vect<-union(tran.whp.vect,road.whp.vect)

#for WHP calcs, WHP CRS
road.buff.whp.vect<-buffer(road.whp.vect,width=500*0.3048)
road.buff.whp.simp.vect<-aggregate(road.buff.whp.vect)
road.buff.whp.proj.vect<-check.crs.match(whp.rast,road.buff.whp.simp.vect)
writeVector(road.buff.whp.proj.vect,paste(loc.data,"WUIVegetationClassifications/RoadBuffer_WHPproj.shp",sep=""),overwrite=TRUE)


road.buff.whp.proj.vect<-vect(paste(loc.data,"WUIVegetationClassifications/RoadBuffer_WHPproj.shp",sep=""),overwrite=TRUE)


#now try it with a region (Sierra takes 17 min)
timer.start<-Sys.time()
#pull in a region
region.vect<-vect(paste(loc.scripts,"ReferenceFiles/Region_Sierra.shp",sep=""))
#project it to CECS or WHP
region.proj.vect<-check.crs.match(whp.rast,region.vect)
#crop the raster (cecs or whp) to that region
region.crop.rast<-crop(whp.rast,region.proj.vect)
#crop the road/transmission lines to that region
region.crop.vect<-crop(road.buff.whp.proj.vect,region.proj.vect)
#then rasterize
road.test.crop.rast<-rasterize(region.crop.vect,region.crop.rast)
#and save in a new object
road.sierra.rast<-road.test.crop.rast
timer.end<-Sys.time()
time.total<-timer.end-timer.start
print(time.total)
#dim(test.crop.rast)/dim(whp.rast)
#[1] 0.6090444 0.4848000 1.0000000
#> 


#now try it with a region (5 min for socal)
timer.start<-Sys.time()
#pull in a region
region.vect<-vect(paste(loc.scripts,"ReferenceFiles/Region_SouthernCA.shp",sep=""))
#project it to CECS or WHP
region.proj.vect<-check.crs.match(whp.rast,region.vect)
#crop the raster (cecs or whp) to that region
region.crop.rast<-crop(whp.rast,region.proj.vect)
#crop the road/transmission lines to that region
region.crop.vect<-crop(road.buff.whp.proj.vect,region.proj.vect)
#then rasterize
road.test.crop.rast<-rasterize(region.crop.vect,region.crop.rast)
#and save in a new object
road.south.rast<-road.test.crop.rast
timer.end<-Sys.time()
time.total<-timer.end-timer.start
print(time.total)


#now try it with a region (9.8 min for Northern CA)
timer.start<-Sys.time()
#pull in a region
region.vect<-vect(paste(loc.scripts,"ReferenceFiles/Region_NorthernCA.shp",sep=""))
#project it to CECS or WHP
region.proj.vect<-check.crs.match(whp.rast,region.vect)
#crop the raster (cecs or whp) to that region
region.crop.rast<-crop(whp.rast,region.proj.vect)
#crop the road/transmission lines to that region
region.crop.vect<-crop(road.buff.whp.proj.vect,region.proj.vect)
#then rasterize
road.test.crop.rast<-rasterize(region.crop.vect,region.crop.rast)
#and save in a new object
road.north.rast<-road.test.crop.rast
timer.end<-Sys.time()
time.total<-timer.end-timer.start
print(time.total)


#now try it with a region (7.9 min)
timer.start<-Sys.time()
#pull in a region
region.vect<-vect(paste(loc.scripts,"ReferenceFiles/Region_CentralCoast.shp",sep=""))
#project it to CECS or WHP
region.proj.vect<-check.crs.match(whp.rast,region.vect)
#crop the raster (cecs or whp) to that region
region.crop.rast<-crop(whp.rast,region.proj.vect)
#crop the road/transmission lines to that region
region.crop.vect<-crop(road.buff.whp.proj.vect,region.proj.vect)
#then rasterize
road.test.crop.rast<-rasterize(region.crop.vect,region.crop.rast)
#and save in a new object
road.central.rast<-road.test.crop.rast
timer.end<-Sys.time()
time.total<-timer.end-timer.start
print(time.total)

road.allstate.whp.rast<-mosaic(road.central.rast,road.sierra.rast,road.south.rast,road.north.rast,fun="max")

writeRaster(road.allstate.whp.rast,paste(loc.data,"WUIVegetationClassifications/RoadBuffer_WHPproj.tif",sep=""),overwrite=TRUE)

road.allstate.whp.rast<-rast(paste(loc.data,"WUIVegetationClassifications/RoadBuffer_WHPproj.tif",sep=""))
road.crop.whp.rast<-crop(road.allstate.whp.rast,whp.rast)
road.cropext.whp.rast<-extend(road.crop.whp.rast,whp.rast)
writeRaster(road.cropext.whp.rast,paste(loc.data,"WUIVegetationClassifications/RoadBuffer_WHPproj.tif",sep=""),overwrite=TRUE)

# ------- just transmission lines, WHP CRS

#for WHP calcs (WHP CRS)
tran.buff.whp.vect<-buffer(tran.whp.vect,width=500*0.3048)
tran.buff.whp.simp.vect<-aggregate(tran.buff.whp.vect)
tran.buff.whp.proj.vect<-check.crs.match(whp.rast,tran.buff.whp.simp.vect)
writeVector(tran.buff.whp.proj.vect,paste(loc.data,"WUIVegetationClassifications/TransmissionLineBuffer_WHPproj.shp",sep=""),overwrite=TRUE)
tran.buff.whp.proj.rast<-rasterize(tran.buff.whp.proj.vect,whp.rast)
writeRaster(tran.buff.whp.proj.rast,paste(loc.data,"WUIVegetationClassifications/TransmissionLineBuffer_WHPproj.tif",sep=""),overwrite=TRUE)
  #transmission lines run past the state boundary too
  tran.buff.whp.rast<-mask(tran.buff.whp.proj.rast,ca.whp.vect)
writeRaster(tran.buff.whp.rast,paste(loc.data,"WUIVegetationClassifications/TransmissionLineBuffer_WHPproj.tif",sep=""),overwrite=TRUE)

############################################################

## this code is me trying to work out the best way to reproject and resample things


# #now comparing WHR (forest and shrub), WHP and WUI layers (all from CALFIRE)

# forest.rast<-rast(paste(loc.scripts,"ReferenceFiles/WHR13_RECLASS_FOREST.tif",sep=""))
# shrub.rast<-rast(paste(loc.scripts,"ReferenceFiles/WHR13_RECLASS_SHRUB.tif",sep=""))

# #now let's look at the WHP layer
# whp.rast <- rast(paste(loc.data,"PriorityLayers/whp_classified_20240906.tif",sep=""))
# #identical to the shrub/forest layers which are WHR
# #and compare with the TF boundary - identical with the shrub/forest and WHP layers.


# wui_FRAP.rast<-rast(paste(loc.data,"WUIVegetationClassifications/WUI24_extract.tif",sep=""))
# #from terra's 'crs' command, here are the differences between the WHR (first) and WUI (second) projections:
# #(1)
# #ELLIPSOID[\"GRS 1980\",6378137,298.257222101,\n
# #ELLIPSOID[\"GRS 1980\",6378137,298.257222101004,\n
# #(2)
# #CONVERSION[\"California Albers\",\n
# # CONVERSION[\"Albers Equal Area\",\n  
# #(3)
# #AXIS[\"easting (X)\",east,\n
# # AXIS[\"easting\",east,\n 
# #(4)
# # AXIS[\"northing (Y)\",north,\n 
# #AXIS[\"northing\",north,\n 
# #(5)
# #USAGE[\n        SCOPE[\"State-wide spatial data management.\"],\n        AREA[\"United States (USA) - California.\"],\n        BBOX[32.53,-124.45,42.01,-114.12]],\n 

# #I determine these are the same CRS, and the WHR metadata is more complete so I will use that one.
# #looking at them in QGIS, though, there is a 1.4 m vertical offset and a 9.3 m horizontal offset.
# #also, I do think that the crs bascically matches, because I can use the crosshairs in QGIS's measurement tool 
# #and that implies they're aligned rotationally at least.  But let's go ahead and reproject it anyway.
# wui_FRAP_proj.rast<-check.crs.match(forest.rast,wui_FRAP.rast)
# writeRaster(wui_FRAP_proj.rast,paste(loc.data,"WUIVegetationClassifications/WUI24_extract_WHRproj.tif",sep=""))
# #this did not fix it,as expected this is basically visually identical, but is still offset from the pixels in WHR


# #looking at the actual pixels, the WHP matches the WHR but not the WUI layer
# #WHR/forest/shrub is actually the same as the WHP layer.  But need to get the WUI layer to match.
# #the extent is smaller, and the origin is different, for the WUI layer

# #so let's try a resample and crop/extend

# wui.resamp.rast<-resample(wui_FRAP_proj.rast,whp.rast,method="near",threads=TRUE)
# writeRaster(wui.resamp.rast,paste(loc.data,"WUIVegetationClassifications/WUI24_extract_resamp.tif",sep=""))
# #and now they visually match, and the origin matches and so does the extent

# #testing raster math operation:

# test<-wui.resamp.rast*whp.rast
# #yes, this works.  just resample is good enough.


# #and finally check against CECS

# cecs.rast<-rast(paste(loc.data,"CECS_Data/CECS_CAWide_Vulner_TreeDieoff_SPI-2_2020_V250614.tif",sep=""))
# cecs.proj.rast<-check.crs.match(whp.rast,cecs.rast)
# cecs.resamp.rast<-resample(cecs.proj.rast,whp.rast,method="bilinear",threads=TRUE)

# test<-cecs.resamp.rast*whp.rast
# #yes, this works too!!!



# #WUI (once it's appropriately resampled) is about the size of the state boundary
# #WHP is slightly larger than the state boundary
# #forest/shrub/WHR is slightly larger than state boundary
# #so we're still going to clip everything by the new state boundary

#####################################

## this code was one of many workarounds trying to get stuff to work by vectorizing

#  WHR layers

# these have been subset by our team from the larger WHR 13 classification


# ## ecosystem layers reclassified from Wildlife Habitat Relationships CALFIRE dataset
# forest.rast<-rast(paste(loc.scripts,"ReferenceFiles/WHR13_RECLASS_FOREST.tif",sep=""))
# forest.proj.rast<-check.crs.match(reference.rast,forest.rast)
# forest.proj.vect<-as.polygons(forest.proj.rast)
# forest.CECSproj.vect<-check.crs.match(reference.rast,forest.proj.vect)
# writeVector(forest.CECSproj.vect,paste(loc.data,"WUIVegetationClassifications/WHR13_RECLASS_FOREST.shp",sep=""))

# shrub.rast<-rast(paste(loc.scripts,"ReferenceFiles/WHR13_RECLASS_SHRUB.tif",sep=""))
# shrub.proj.rast<-check.crs.match(reference.rast,shrub.rast)
# shrub.proj.vect<-as.polygons(shrub.proj.rast)
# shrub.CECSproj.vect<-check.crs.match(reference.rast,shrub.proj.vect)
# writeVector(shrub.CECSproj.vect,paste(loc.data,"WUIVegetationClassifications/WHR13_RECLASS_SHRUB.shp",sep=""))



# #I had to do some crazy stuff to pull out just the WUI classification from the FRAP layer SIG gave me
# #I could not get the raster to match with CECS rasters so I vectorized the WUI/non-WUI rasters

# #"5 Class WUI raster, developed by SIG. 0 = everything else, 1 = influence, 
# #2 = intermix, 3 = interface, 4 = urban, 5 = wildland

# # wui_FRAP.rast<-rast(paste(loc.data,"WUIVegetationClassifications/WUI24_extract.tif",sep=""))

# ## WILDLAND MASK IS NON-WUI, NON-AG, NON-URBAN

# #technically the crs matches, but the extent doesn't, even when cropping.
# #probably a datum mismatch that the reprojection algorithms aren't fixing
# #but at least start it out in the right projection in general
# wui_FRAP.proj.rast<-check.crs.match(reference.rast,wui_FRAP.rast)
# #pull out just the non ag and urban areas from CECS (which are wildland and wui)
# non.ag.urban.class.rast<-!is.na(reference.rast) #reference rast is a CECS layer
# #pull out the 'other' category from FRAP (which is ag, urban, and wildland)
# #(FRAP and/or SIG has combined wildland with ag and urban, in code '0')
# other.class.rast<-wui_FRAP.proj.rast==0
# #take it to vector, check crs/reproject, pull out only the "1" values
# other.class.vect<-as.polygons(other.class.rast)
# other.class.proj.vect<-check.crs.match(reference.rast,other.class.vect)
# other.class.only.vect<-other.class.proj.vect[other.class.proj.vect$Band_1==1,]
# #then rerasterize it using the reference rast for extent, etc
# other.class.proj.rast<-rasterize(other.class.only.vect,reference.rast)
# #then find the raster intersection of wildland+wui and ag+urban+wildland, which is wildland
# wild.rast<-(other.class.proj.rast)*(non.ag.urban.class.rast)
# #make a new raster and pull out only the wildland (set everything else to NA), and export
# wild.only.rast<-wild.rast
# wild.only.rast[wild.rast!=1]<-NA
# writeRaster(wild.only.rast,paste(loc.scripts,"ReferenceFiles/FRAP24_WildlandOnly.tif",sep=""))

# ## WUI MASK IS INTERFACE, INTERMIX, AND INFLUENCE AS PER CALFIRE'S DEFINITION

# #For the FRAP layer via SIG, 2=intermix, 1=interface, and 3=influence
# wui.class.rast<-wui_FRAP.proj.rast %in% c(2,1,3)
# #pull the same trick to fix the projection by going to vector and back to raster again
# wui.class.vect<-as.polygons(wui.class.rast)
# wui.class.proj.vect<-check.crs.match(reference.rast,wui.class.vect)
# wui.class.only.vect<-wui.class.proj.vect[wui.class.proj.vect$Band_1==1,]
# #then rerasterize it using the reference rast for extent, etc
# wui.class.proj.rast<-rasterize(wui.class.only.vect,reference.rast)

# wui.rast<-wui.class.proj.rast
# wui.rast[wui.rast!=1]<-NA
# writeRaster(wui.rast,paste(loc.scripts,"ReferenceFiles/FRAP24_WUIOnly.tif",sep=""))


# #FOR WHP, ALSO HAVING TROUBLE WITH PROJECTIONS SO TAKE THE NEWLY 
# #CREATED WILDLAND MASK AND PROJECT IT BACK (VIA VECTORIZING AND 
# #RERASTERIZING) TO THE WHP COORDINATE SYSTEM
# whp.rast <- rast(paste(loc.data,"PriorityLayers/whp_classified_20240906.tif",sep=""))

# wild.vect<-as.polygons(wild.only.rast)
# wild.proj.vect<-check.crs.match(whp.rast,wild.vect)
# wild.proj.rast<-rasterize(wild.proj.vect,whp.rast) #this step takes a long time - use whp.rast for the extent
# writeRaster(wild.proj.rast,paste(loc.data,"WUIVegetationClassifications/FRAP24_WildlandOnly_WHPproj.tif",sep=""))

# #FOR WHP FOR THE WUI CALC, JUST DON'T DO ANY REPROJECTION
# #USE THE ORIGINAL LAYER TO RECLASSIFY, MAKE THE NA MASK, 
# #AND EXPORT TO BE USED
# wui.proj.reclass.rast<-wui_FRAP.rast %in% c(2,1,3)
# wui.proj.rast<-wui.proj.reclass.rast
# wui.proj.rast[wui.proj.rast!=1]<-NA
# #TODO***Do I want it to be 1 rather than TRUE?
# writeRaster(wui.proj.rast,paste(loc.data,"WUIVegetationClassifications/FRAP24_WUIOnly_WHPproj.tif",sep=""))

###########################################

######## these are various failed attempts
### to do things with WUI and WHP vectorized
#### things to deal with various projection issues

### the below was an attempt to make a vector WUI mask, but it didn't turn out well
###  keeping the code here for posterity but not recommended to run it


# #In order to assign NA to the unwanted pixels, I have selected the 'other'
# #pixels via not-in c(2,1,3), then assigned NA to them, then exported as polygons
# #so the naming follows the final product, not the intermediate ones
# wui.wui.class.rast<-!(wui_FRAP %in% c(2,1,3))
# wui.wui.rast<-wui.wui.class.rast
# wui.wui.rast[wui.wui.class.rast]<-NA
# wui.wui.vect<-as.polygons(wui.wui.rast,aggregate=TRUE)

# #Same logic as above, selecting the wui pixels in order to assign them
# #NA values, and exporting polygons of those.  Naming follows the final product.
# wui.other.class.rast<-(wui_FRAP %in% c(2,1,3))
# wui.other.rast<-wui.other.class.rast
# wui.other.rast[wui.other.class.rast]<-NA
# wui.other.vect<-as.polygons(wui.other.rast,aggregate=TRUE)

# #recreate CECS mask to establish what is ag and urban
# ag.urban.class.rast<-!is.na(reference.rast)
# ag.urban.rast<-ag.urban.class.rast
# ag.urban.rast[ag.urban.class.rast]<-NA
# ag.urban.vect<-as.polygons(ag.urban.rast,aggregate=TRUE)

# ag.urban.vect$keep<-FALSE

# ag.urban.vect<-check.crs.match(reference.rast,ag.urban.vect)
# wui.other.vect<-check.crs.match(reference.rast,wui.other.vect)
# wui.wui.vect<-check.crs.match(reference.rast,wui.wui.vect)

# wui.wild.intersect.vect<-intersect(ag.urban.vect,wui.other.vect)
# wui.wild.union.vect<-union(ag.urban.vect,wui.other.vect)

# wui.wild.vect<-subset(wui.wild.vect,is.na(wui.wild.vect$keep))

# writeVector(wui.wui.vect,"VectorFiles/FRAP24_WUI.shp",overwrite=TRUE)
# writeVector(wui.wild.vect,"VectorFiles/FRAP24_Wild.shp",overwrite=TRUE)

##################
## also storing these old attempts to make vectorized versions of WHP classes

    # for the whp, we expect to only ever use hazard classes 4 and 5, so we made a vector version of the raster with just those classes
    # kept this code for posterity, it didn't run well on LC's computer but might run better elsewhere
    # whp.rast <- rast("/Users/laurencox/Documents/Task Force/Scenario Modeling/Scenario Modeling Results/tx_criteria/whp_classified_20240906.tif")
    # plot(whp)
    # whp.rast.priority<-whp.rast %in% c(4,5)
    # plot(whp.rast.priority)
    # Manipulate the shapefiles before entering them into the functions. We need to standardize the column name (attribute). 
    # whp.vect<-as.polygons(whp.rast.priority,aggregate=TRUE)
    # plot(whp.vect)

    #whp.vect<-vect("/Users/laurencox/Downloads/whp_high/whp_high.shp")
    ## aggregate is the terra version of Arc's 'dissolve'
    #whp.vect.singlepolygon<-aggregate(whp.vect,dissolve=TRUE)

    # #for fire risk, we also need to read in and stratify by the WUI classes
    # WUI.classes #note that we need one of these that has all classes separately since MV's combines WUI classes ***TODO
    # # for now use MV's two-class solution (WUI vs Wildland?)

    # #intersect is the equivalent of Union in Arc
    # whp.WUI.intersect<-intersect(whp.vect.singlepolygon,WUI.classes)

    # #might want to separate out the different WUI classes and wildland, so this is really more like 2-5 of our targeted effort metrics


# # 2024 Wildfire Hazard Potential layer from CALFIRE
# whp.rast <- rast(paste(loc.data,"PriorityLayers/whp_classified_20240906.tif",sep=""))
# #check CRS
# whp.proj.rast<-check.crs.match(reference.rast,whp.rast)
# # WHP classes 4 (High) and 5 (Very High)
# whp.priority.rast<-whp.proj.rast %in% c(4,5)
# ##vectorize because the rasters' extents don't match
# #whp.priority.vect<-as.polygons(whp.priority.rast)
# #whp.priority.proj.vect<-check.crs.match(reference.rast,whp.priority.vect)
# #whp.pri.rast<-rasterize(whp.priority.proj.vect,reference.rast)
# whp.priority.crop.rast<-crop(whp.priority.rast,reference.rast)

# whp.priority.rast*wild.rast

# wild.proj.rast<-check.crs.match(whp.rast,wild.rast)
# wild.proj.rast.crop<-crop(wild.proj.rast,whp.rast)

