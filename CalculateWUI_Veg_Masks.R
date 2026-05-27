#CalculateWUI_Veg_Masks.R


# for rasters that are getting projected onto each other for consistency, 
# the naming convention is dataorigin.<resamp/proj/etc>.projectiondata.rast
# so for CECS data that is getting put into the projection of WHP data,
# the name is CECS.<proj/resamp/etc>.WHP.rast


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

road.allstate.cecs.rast<-mosaic(road.central.rast,road.sierra.rast,road.south.rast,road.north.rast,fun="max")

writeRaster(road.allstate.cecs.rast,paste(loc.data,"WUIVegetationClassifications/RoadBuffer_CECSproj.tif",sep=""),overwrite=TRUE)

road.allstate.cecs.rast<-rast(paste(loc.data,"WUIVegetationClassifications/RoadBuffer_CECSproj.tif",sep=""))
road.crop.cecs.rast<-crop(road.allstate.cecs.rast,cecs.rast)
road.cropext.cecs.rast<-extend(road.crop.cecs.rast,cecs.rast)
writeRaster(road.cropext.cecs.rast,paste(loc.data,"WUIVegetationClassifications/RoadBuffer_CECSproj.tif",sep=""),overwrite=TRUE)


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


#1000 foot buffer but function expects meters (CECS CRS)
tran.buff.cecs.vect<-buffer(tran.cecs.vect,width=1000*0.3048)
tran.buff.cecs.simp.vect<-aggregate(tran.buff.cecs.vect)
tran.buff.cecs.proj.vect<-check.crs.match(cecs.rast,tran.buff.cecs.simp.vect)
writeVector(tran.buff.cecs.proj.vect,paste(loc.data,"WUIVegetationClassifications/TransmissionLinesBuffer1000_CECSproj.shp",sep=""),overwrite=TRUE)

tran.buff.cecs.proj.rast<-rasterize(tran.buff.cecs.proj.vect,cecs.rast)
writeRaster(tran.buff.cecs.proj.rast,paste(loc.data,"WUIVegetationClassifications/TransmissionLineBuffer1000_CECSproj.tif",sep=""),overwrite=TRUE)
  #transmission lines run past the state boundary too
  tran.buff.cecs.rast<-mask(tran.buff.cecs.proj.rast,ca.cecs.vect)
writeRaster(tran.buff.cecs.rast,paste(loc.data,"WUIVegetationClassifications/TransmissionLineBuffer1000_CECSproj.tif",sep=""),overwrite=TRUE)

# #---------------- do roads and utilities for WHP projection----------#

SDGE.whp.vect<-check.crs.match(whp.rast,SDGE.vect)
SCE.whp.vect<-check.crs.match(whp.rast,SCE.vect)
PGE.whp.vect<-check.crs.match(whp.rast,PGE.vect)
road.whp.vect<-check.crs.match(whp.rast,road.vect)

tran.whp.vect<-union(SDGE.whp.vect,SCE.whp.vect)
tran.whp.vect<-union(tran.whp.vect,PGE.whp.vect)

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

tran.buff.whp.vect<-buffer(tran.whp.vect,width=1000*0.3048)
tran.buff.whp.simp.vect<-aggregate(tran.buff.whp.vect)
tran.buff.whp.proj.vect<-check.crs.match(whp.rast,tran.buff.whp.simp.vect)
writeVector(tran.buff.whp.proj.vect,paste(loc.data,"WUIVegetationClassifications/TransmissionLineBuffer1000_WHPproj.shp",sep=""),overwrite=TRUE)
tran.buff.whp.proj.rast<-rasterize(tran.buff.whp.proj.vect,whp.rast)
writeRaster(tran.buff.whp.proj.rast,paste(loc.data,"WUIVegetationClassifications/TransmissionLineBuffer1000_WHPproj.tif",sep=""),overwrite=TRUE)
  #transmission lines run past the state boundary too
  tran.buff.whp.rast<-mask(tran.buff.whp.proj.rast,ca.whp.vect)
writeRaster(tran.buff.whp.rast,paste(loc.data,"WUIVegetationClassifications/TransmissionLineBuffer1000_WHPproj.tif",sep=""),overwrite=TRUE)


###--------------------- FOREST+UTILITIES, FOREST+ROADS, FOREST+WUI -----------------------###

 forest.cecs.rast<-rast(paste(loc.data,"WUIVegetationClassifications/WHR13_RECLASS_FOREST_CECS.tif",sep=""))
 road.buff.cecs.rast<-rast(paste(loc.data,"WUIVegetationClassifications/RoadBuffer_CECSproj.tif",sep=""))
 tran.buff.cecs.rast<-rast(paste(loc.data,"WUIVegetationClassifications/TransmissionLineBuffer1000_CECSproj.tif",sep=""))
 wui.cecs.rast<-rast(paste(loc.data,"WUIVegetationClassifications/FRAP24_WUIOnly_CECS.tif",sep=""))

forestroad.cecs.rast<- road.buff.cecs.rast*forest.cecs.rast
forestroad.cecs.rast[forestroad.cecs.rast==0]<-NA
foresttran.cecs.rast<- tran.buff.cecs.rast*forest.cecs.rast
foresttran.cecs.rast[foresttran.cecs.rast==0]<-NA
forestwui.cecs.rast<- wui.cecs.rast*forest.cecs.rast
forestwui.cecs.rast[forestwui.cecs.rast==0]<-NA

writeRaster(forestroad.cecs.rast,paste(loc.data,"WUIVegetationClassifications/Forest_Road_CECSproj.tif",sep=""),overwrite=TRUE)
writeRaster(foresttran.cecs.rast,paste(loc.data,"WUIVegetationClassifications/Forest_Utilities_CECSproj.tif",sep=""),overwrite=TRUE)
writeRaster(forestwui.cecs.rast,paste(loc.data,"WUIVegetationClassifications/Forest_WUI_CECSproj.tif",sep=""),overwrite=TRUE)


###--------------------- NON-FIRE MASK -----------------------###

#this is only for efficacy. so it will be the time-limited vector file, for the whole state
start.year<-2020
end.year<-2024

fire.footprint.vect<-vect(paste(loc.data,"IntermediateFiles/AggregatedVectors/Fires_CA_",start.year,"_",end.year,".shp",sep=""))
fire.footprint.vect<-check.crs.match(cecs.rast,fire.footprint.vect)
fire.footprint.rast<-rasterize(fire.footprint.vect,cecs.rast)
writeRaster(fire.footprint.rast,paste(loc.data,"WUIVegetationClassifications/FireFootprints_2020-2024_CECSproj.tif",sep=""),overwrite=TRUE)

nofire.footprint.rast<-fire.footprint.rast
nofire.footprint.rast[is.na(nofire.footprint.rast)]<-0
nofire.footprint.rast[nofire.footprint.rast==1]<-NA
nofire.footprint.rast[nofire.footprint.rast==0]<-1
writeRaster(nofire.footprint.rast,paste(loc.data,"WUIVegetationClassifications/NonFireFootprints_2020-2024_CECSproj.tif",sep=""),overwrite=TRUE)


###--------------------- CANOPY DISTURBANCE MASK - FOREST -----------------------###

#read in all of the CECS layers for forest disturbance
#These are calendar year, where the effects layers (flame length, drought vulnerability) are water years
#

years<-2021:2024

disturbance.layers<- paste(loc.data,"CECS_Data/CECS_CAWide_Veg_TreeDist_",years,"_V250418.tif",sep="")

#disturb.rasts<-list()
disturb.accumulated.rast<-cecs.rast
values(disturb.accumulated.rast)<-0

#note that it takes the raster band name from the reference layer
for(i in 1:length(disturbance.layers)){
  disturb.rast<-rast(disturbance.layers[i])
  disturb.accumulated.rast<-disturb.accumulated.rast+disturb.rast
}

#just going to assume that 0 means no change.  Not sure what the negatives mean,
# but let's count them as not disturbed since we aren't sure what they mean.

disturb.mask.rast<-disturb.accumulated.rast
disturb.mask.rast[disturb.mask.rast<=0]<-NA
disturb.mask.rast[disturb.mask.rast>0]<-1

writeRaster(disturb.mask.rast,paste(loc.data,"WUIVegetationClassifications/ForestDisturbances_2021-2024_CECSproj.tif",sep=""),overwrite=TRUE)


###--------------------- SHRUB DISTURBANCE MASK -----------------------###

#read in all of the CECS layers for shrub disturbance
#These are calendar year, where the effects layers (flame length, drought vulnerability) are water years
#

years<-2021:2024

disturbance.layers<- paste(loc.data,"CECS_Data/CECS_CAWide_Veg_ShrubDist_",years,"_V250418.tif",sep="")

#disturb.rasts<-list()
disturb.accumulated.rast<-cecs.rast
values(disturb.accumulated.rast)<-0

#note that it takes the raster band name from the reference layer
for(i in 1:length(disturbance.layers)){
  disturb.rast<-rast(disturbance.layers[i])
  disturb.accumulated.rast<-disturb.accumulated.rast+disturb.rast
}

#just going to assume that 0 means no change.  Not sure what the negatives mean,
# but let's count them as not disturbed since we aren't sure what they mean.

disturb.mask.rast<-disturb.accumulated.rast
disturb.mask.rast[disturb.mask.rast<=0]<-NA
disturb.mask.rast[disturb.mask.rast>0]<-1

writeRaster(disturb.mask.rast,paste(loc.data,"WUIVegetationClassifications/ShrubDisturbances_2021-2024_CECSproj.tif",sep=""),overwrite=TRUE)

## No Fire and also only the canopy disturbances

 forest.cecs.rast<-rast(paste(loc.data,"WUIVegetationClassifications/WHR13_RECLASS_FOREST_CECS.tif",sep=""))
 shrub.cecs.rast<-rast(paste(loc.data,"WUIVegetationClassifications/WHR13_RECLASS_SHRUB_CECS.tif",sep=""))

#mask that removes the fire footprint from treatments
nofire.rast<-rast(paste(loc.data,"WUIVegetationClassifications/NonFireFootprints_2020-2024_CECSproj.tif",sep=""))
#mask that is only forest disturbances (but not masked for only forest)
yesdist.f.rast<-rast(paste(loc.data,"WUIVegetationClassifications/ForestDisturbances_2021-2024_CECSproj.tif",sep=""))
#mask that is only shrubland disturbances (but not masked for only shrub)
yesdist.s.rast<-rast(paste(loc.data,"WUIVegetationClassifications/ShrubDisturbances_2021-2024_CECSproj.tif",sep=""))
#nofire, only disturbances - only forest
nofire.yesdist.f.rast<-nofire.rast*yesdist.f.rast*forest.cecs.rast
writeRaster(nofire.yesdist.f.rast,paste(loc.data,"WUIVegetationClassifications/ForestDisturbanceNoFire_2021-2024_CECSproj.tif",sep=""),overwrite=TRUE)
#nofire, only disturbances - only shrub
nofire.yesdist.s.rast<-nofire.rast*yesdist.s.rast*shrub.cecs.rast
writeRaster(nofire.yesdist.s.rast,paste(loc.data,"WUIVegetationClassifications/ShrubDisturbancesNoFire_2021-2024_CECSproj.tif",sep=""),overwrite=TRUE)



### ---------making a 'nonroad' mask for shrub calculations

 road.buff.cecs.rast<-rast(paste(loc.data,"WUIVegetationClassifications/RoadBuffer_CECSproj.tif",sep=""))

 #flip it so nonroads are zero, then 1, and roads are NA
 nonroad.buff.cecs.rast<-road.buff.cecs.rast
 nonroad.buff.cecs.rast[is.na(nonroad.buff.cecs.rast)]<-0
 nonroad.buff.cecs.rast[nonroad.buff.cecs.rast==1]<-NA
 nonroad.buff.cecs.rast[nonroad.buff.cecs.rast==0]<-1
 
 #keep one with NA for roads, 1 for nonroads
 nonroad.buff.cecs.rast.na<-nonroad.buff.cecs.rast
writeRaster(nonroad.buff.cecs.rast.na,paste(loc.data,"WUIVegetationClassifications/NonRoads_CECSproj.tif",sep=""),overwrite=TRUE)

 #this one is zero for roads, 1 for nonroads
 nonroad.buff.cecs.rast[is.na(nonroad.buff.cecs.rast)]<-0
writeRaster(nonroad.buff.cecs.rast,paste(loc.data,"WUIVegetationClassifications/NonRoads_Filled_CECSproj.tif",sep=""),overwrite=TRUE)

# Combo shrub-road
 shrub.cecs.rast<-rast(paste(loc.data,"WUIVegetationClassifications/WHR13_RECLASS_SHRUB_CECS.tif",sep=""))
shrub.road.rast<-road.buff.cecs.rast*shrub.cecs.rast
shrub.road.rast[shrub.road.rast==0]<-NA
writeRaster(shrub.road.rast,paste(loc.data,"WUIVegetationClassifications/Shrub_Road_CECSproj.tif",sep=""),overwrite=TRUE)

#combo shrub-no road
shrub.nonroad.rast<-nonroad.buff.cecs.rast*shrub.cecs.rast
shrub.nonroad.rast[shrub.nonroad.rast==0]<-NA
writeRaster(shrub.nonroad.rast,paste(loc.data,"WUIVegetationClassifications/Shrub_NonRoad_CECSproj.tif",sep=""),overwrite=TRUE)




############################################################
