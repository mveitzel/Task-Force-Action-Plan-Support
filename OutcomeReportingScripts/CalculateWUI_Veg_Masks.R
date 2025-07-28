#CalculateWUI_Veg_Masks.R

#I had to do some crazy stuff to pull out just the WUI classification from the FRAP layer SIG gave me
#I could not get the raster to match with CECS rasters so I vectorized the WUI/non-WUI rasters

#"5 Class WUI raster, developed by SIG. 0 = everything else, 1 = influence, 
#2 = intermix, 3 = interface, 4 = urban, 5 = wildland

wui_FRAP.rast<-rast(paste(loc.data,"WUIVegetationClassifications/WUI24_extract.tif",sep=""))

## WILDLAND MASK IS NON-WUI, NON-AG, NON-URBAN

#technically the crs matches, but the extent doesn't, even when cropping.
#but at least start it out in the right projection in general
wui_FRAP.proj.rast<-check.crs.match(reference.rast,wui_FRAP.rast)
#pull out just the non ag and urban areas from CECS (which are wildland and wui)
non.ag.urban.class.rast<-!is.na(reference.rast)
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




###########################################

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

