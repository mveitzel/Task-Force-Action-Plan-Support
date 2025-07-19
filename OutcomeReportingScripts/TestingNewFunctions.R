#TestingNewFunctions.R

loc.scripts<-"D:/GitRepos/BattlesLabRepos/Task-Force-Action-Plan-Support/"
loc.data<-"D:/GIS_Large_Files/CECS_Data/"
loc.output<-"D:/DropboxFiles/Dropbox/Professional/UCB_Battles/ActionPlanSupport"

setwd(loc.output)
source(paste(loc.scripts,"FunctionLibraries/SummarizeChange_functions.R",sep=""))

boundary.shape<-c(paste(loc.scripts,"VectorFiles/Region_Sierra.shp",sep=""))
boundary.name<-c("Sierra")
reference.rast<-rast(paste(loc.data,"CECS_CAWide_Vulner_TreeDieoff_SPI-2_2020_V250418.tif",sep=""))

vect.shape<-c(paste(loc.scripts,"VectorFiles/HUC12.shp",sep=""))
vect.name<-c("HUC12")


prepped.boundary.vect<-read.and.prepare.boundary.vector(boundary.shape,boundary.name,reference.rast)

prepped.zonal.summary.area.vect<-crop.vector.by.boundary.vector(prepped.boundary.vect, boundary.name,vect.shape,vect.name)

#checking to confirm that there's a big 1:1 line with hucs that haven't been clipped
#and a bunch of hucs that have smaller areas in post_crop_area_ha than they did before
# plot(prepped.zonal.summary.area.vect$huc12_area,prepped.zonal.summary.area.vect$post_crop_area_ha)
 

