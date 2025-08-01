#CalculateWUI_Veg_Masks.R

#  WHR layers

# these have been subset by our team from the larger WHR 13 classification


## ecosystem layers reclassified from Wildlife Habitat Relationships CALFIRE dataset
forest.rast<-rast(paste(loc.scripts,"ReferenceFiles/WHR13_RECLASS_FOREST.tif",sep=""))
forest.proj.rast<-check.crs.match(reference.rast,forest.rast)
forest.proj.vect<-as.polygons(forest.proj.rast)
forest.CECSproj.vect<-check.crs.match(reference.rast,forest.proj.vect)
writeVector(forest.CECSproj.vect,paste(loc.data,"WUIVegetationClassifications/WHR13_RECLASS_FOREST.shp",sep=""))

shrub.rast<-rast(paste(loc.scripts,"ReferenceFiles/WHR13_RECLASS_SHRUB.tif",sep=""))
shrub.proj.rast<-check.crs.match(reference.rast,shrub.rast)
shrub.proj.vect<-as.polygons(shrub.proj.rast)
shrub.CECSproj.vect<-check.crs.match(reference.rast,shrub.proj.vect)
writeVector(shrub.CECSproj.vect,paste(loc.data,"WUIVegetationClassifications/WHR13_RECLASS_SHRUB.shp",sep=""))



#I had to do some crazy stuff to pull out just the WUI classification from the FRAP layer SIG gave me
#I could not get the raster to match with CECS rasters so I vectorized the WUI/non-WUI rasters

#"5 Class WUI raster, developed by SIG. 0 = everything else, 1 = influence, 
#2 = intermix, 3 = interface, 4 = urban, 5 = wildland

wui_FRAP.rast<-rast(paste(loc.data,"WUIVegetationClassifications/WUI24_extract.tif",sep=""))

## WILDLAND MASK IS NON-WUI, NON-AG, NON-URBAN

#technically the crs matches, but the extent doesn't, even when cropping.
#probably a datum mismatch that the reprojection algorithms aren't fixing
#but at least start it out in the right projection in general
wui_FRAP.proj.rast<-check.crs.match(reference.rast,wui_FRAP.rast)
#pull out just the non ag and urban areas from CECS (which are wildland and wui)
non.ag.urban.class.rast<-!is.na(reference.rast) #reference rast is a CECS layer
#pull out the 'other' category from FRAP (which is ag, urban, and wildland)
#(FRAP and/or SIG has combined wildland with ag and urban, in code '0')
other.class.rast<-wui_FRAP.proj.rast==0
#take it to vector, check crs/reproject, pull out only the "1" values
other.class.vect<-as.polygons(other.class.rast)
other.class.proj.vect<-check.crs.match(reference.rast,other.class.vect)
other.class.only.vect<-other.class.proj.vect[other.class.proj.vect$Band_1==1,]
#then rerasterize it using the reference rast for extent, etc
other.class.proj.rast<-rasterize(other.class.only.vect,reference.rast)
#then find the raster intersection of wildland+wui and ag+urban+wildland, which is wildland
wild.rast<-(other.class.proj.rast)*(non.ag.urban.class.rast)
#make a new raster and pull out only the wildland (set everything else to NA), and export
wild.only.rast<-wild.rast
wild.only.rast[wild.rast!=1]<-NA
writeRaster(wild.only.rast,paste(loc.scripts,"ReferenceFiles/FRAP24_WildlandOnly.tif",sep=""))

## WUI MASK IS INTERFACE, INTERMIX, AND INFLUENCE AS PER CALFIRE'S DEFINITION

#For the FRAP layer via SIG, 2=intermix, 1=interface, and 3=influence
wui.class.rast<-wui_FRAP.proj.rast %in% c(2,1,3)
#pull the same trick to fix the projection by going to vector and back to raster again
wui.class.vect<-as.polygons(wui.class.rast)
wui.class.proj.vect<-check.crs.match(reference.rast,wui.class.vect)
wui.class.only.vect<-wui.class.proj.vect[wui.class.proj.vect$Band_1==1,]
#then rerasterize it using the reference rast for extent, etc
wui.class.proj.rast<-rasterize(wui.class.only.vect,reference.rast)

wui.rast<-wui.class.proj.rast
wui.rast[wui.rast!=1]<-NA
writeRaster(wui.rast,paste(loc.scripts,"ReferenceFiles/FRAP24_WUIOnly.tif",sep=""))


#FOR WHP, ALSO HAVING TROUBLE WITH PROJECTIONS SO TAKE THE NEWLY 
#CREATED WILDLAND MASK AND PROJECT IT BACK (VIA VECTORIZING AND 
#RERASTERIZING) TO THE WHP COORDINATE SYSTEM
whp.rast <- rast(paste(loc.data,"PriorityLayers/whp_classified_20240906.tif",sep=""))

wild.vect<-as.polygons(wild.only.rast)
wild.proj.vect<-check.crs.match(whp.rast,wild.vect)
wild.proj.rast<-rasterize(wild.proj.vect,whp.rast) #this step takes a long time - use whp.rast for the extent
writeRaster(wild.proj.rast,paste(loc.data,"WUIVegetationClassifications/FRAP24_WildlandOnly_WHPproj.tif",sep=""))

#FOR WHP FOR THE WUI CALC, JUST DON'T DO ANY REPROJECTION
#USE THE ORIGINAL LAYER TO RECLASSIFY, MAKE THE NA MASK, 
#AND EXPORT TO BE USED
wui.proj.reclass.rast<-wui_FRAP.rast %in% c(2,1,3)
wui.proj.rast<-wui.proj.reclass.rast
wui.proj.rast[wui.proj.rast!=1]<-NA
#TODO***Do I want it to be 1 rather than TRUE?
writeRaster(wui.proj.rast,paste(loc.data,"WUIVegetationClassifications/FRAP24_WUIOnly_WHPproj.tif",sep=""))


##########

# #---------------- Powerlines + Roads -------------------------#
#buffering by 500 ft on either side

SDGE.vect<-vect(paste(loc.data,"PriorityLayers/SDGE_2023_Q2NonConfidential.gdb",sep=""),layer="SDGE_TransmissionLine_2023_Q2")
#Note that SDGE also has "SDGE_PrimaryDistributionLine_2023_Q2"
SDGE.proj.vect<-check.crs.match(reference.rast,SDGE.vect)
SCE.vect<-vect(paste(loc.data,"PriorityLayers/SCE_ICA_TransmissionLines.shp",sep=""))
SCE.proj.vect<-check.crs.match(reference.rast,SCE.vect)
PGE.vect<-vect(paste(loc.data,"PriorityLayers/TransmissionLines_upTo_115kV.shp",sep=""))
PGE.proj.vect<-check.crs.match(reference.rast,PGE.vect)
road.vect<-vect(paste(loc.data,"PriorityLayers/OSM_majorRoads_CA_2022.shp",sep=""))
road.proj.vect<-check.crs.match(reference.rast,road.vect)

tran.vect<-union(SDGE.proj.vect,SCE.proj.vect)
tran.vect<-union(tran.vect,PGE.proj.vect)
rdtr.vect<-union(tran.vect,road.proj.vect)


#roads and transmission lines, CECS CRS
rdtr.buff.vect<-buffer(rdtr.vect,width=500*0.3048)
writeVector(rdtr.buff.vect,paste(loc.data,"WUIVegetationClassifications/RoadTransmissionLineBuffer_CECSproj.shp",sep=""))
#for WHP calcs, WHP CRS
rdtr.buff.proj.vect<-check.crs.match(whp.rast,rdtr.buff.vect)
writeVector(rdtr.buff.proj.vect,paste(loc.data,"WUIVegetationClassifications/RoadTransmissionLineBuffer_WHPproj.shp",sep=""))


# #---------------- Just Roads (for WHP, flame length, and shrubs) ----#
#500 foot buffer but function expects meters (CECS CRS)
road.buff.vect<-buffer(road.proj.vect,width=500*0.3048)
writeVector(road.buff.vect,paste(loc.data,"WUIVegetationClassifications/RoadBuffer_CECSproj.shp",sep=""))
#for WHP calcs (WHP CRS)
road.buff.proj.vect<-check.crs.match(whp.rast,road.buff.vect)
writeVector(road.buff.proj.vect,paste(loc.data,"WUIVegetationClassifications/RoadBuffer_WHPproj.shp",sep=""))



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

