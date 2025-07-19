#TestingNewFunctions.R

loc.scripts<-"D:/GitRepos/BattlesLabRepos/Task-Force-Action-Plan-Support/"
loc.data<-"D:/GIS_Large_Files/"
loc.output<-"D:/DropboxFiles/Dropbox/Professional/UCB_Battles/ActionPlanSupport"

setwd(loc.output)
source(paste(loc.scripts,"FunctionLibraries/SummarizeChange_functions.R",sep=""))

boundary.shape<-c(paste(loc.scripts,"VectorFiles/Region_Sierra.shp",sep=""))
boundary.name<-c("Sierra")
reference.rast<-rast(paste(loc.data,"CECS_Data/CECS_CAWide_Vulner_TreeDieoff_SPI-2_2020_V250418.tif",sep=""))

vect.shape<-c(paste(loc.scripts,"VectorFiles/HUC12.shp",sep=""))
vect.name<-c("HUC12")


prepped.boundary.vect<-read.and.prepare.boundary.vector(boundary.shape,boundary.name,reference.rast)
zonal.summary.area.vect<-read.vector.and.check.crs(prepped.boundary.vect,vect.shape,vect.name)
prepped.zonal.summary.area.vect<-crop.vector.by.boundary.and.recalc.area(prepped.boundary.vect,boundary.name,zonal.summary.area.vect,vect.name)

#checking to confirm that there's a big 1:1 line with hucs that haven't been clipped
#and a bunch of hucs that have smaller areas in post_crop_area_ha than they did before
# plot(prepped.zonal.summary.area.vect$huc12_area,prepped.zonal.summary.area.vect$post_crop_area_ha)

patch.name<-c("Treatments","Fires")
patch.shape<-c(paste(loc.data,"ITT_2024_Data/Interagency Tracking System.gdb",sep=""),paste(loc.data,"FireFootprints/fire23_1.gdb",sep=""))
patch.layer<-c("Treat_n_harvests_polygons2023_20240911","firep23_1")

#treatments<-read.and.check.crs.patch.vector(patch.shape[1],patch.name[1],patch.layer[1],prepped.boundary.vect)
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
agg.fires.vect.huc<-intersect.and.aggregate.vectors(
		prepped.zonal.summary.area.vect,vect.name,fires,patch.name[2],"HUC12","huc12",prepped.boundary.vect,boundary.name)
$plot(agg.fires.vect.huc$,)

