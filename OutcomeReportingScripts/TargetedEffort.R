loc.scripts<-"D:/GitRepos/BattlesLabRepos/Task-Force-Action-Plan-Support/"
loc.data<-"D:/GIS_Large_Files/"
loc.output<-"D:/DropboxFiles/Dropbox/Professional/UCB_Battles/ActionPlanSupport"

setwd(loc.output)
source(paste(loc.scripts,"FunctionLibraries/SummarizeChange_functions.R",sep=""))

boundary.shape<-c(paste(loc.scripts,"VectorFiles/CA.shp",sep=""))
boundary.name<-c("CA")
reference.rast<-rast(paste(loc.data,"CECS_Data/CECS_CAWide_Vulner_TreeDieoff_SPI-2_2020_V250418.tif",sep=""))

prepped.boundary.vect<-read.and.prepare.boundary.vector(boundary.shape,boundary.name,reference.rast)


# PREP TREATMENT dataset

patch.name<-c("Treatments")
patch.shape<-c(paste(loc.data,"ITT_2024_Data/Interagency Tracking System.gdb",sep=""))
patch.layer<-c("Treat_n_harvests_polygons2023_20240911")

treat.vect<-read.and.check.crs.patch.vector(patch.shape[1],patch.name[1],patch.layer[1],prepped.boundary.vect)

#filter the treatment types for the appropriate policy goal/topic
#function to do this, hard code in a list of treatment type names
#also filter for time range?  This will be helpful for outcome reporting, but not as important for targeted effort
#so have an 'NA' option for the end date

activity.list<-

#aggregate the treatments into one big polygon
act.filt<-subset(act,act$ACTIVITY_D %in%)
act.agg<-aggregate(act,)

#".code" means the column name in the shapefile
read.in.activities.and.priority.areas <- function(activity.shape, activity.code, priority.shape,priority.code){
  intersected.layers<-aggregate(activity.shape,by=activity.code)
}

output<-read.in.activities.and.priority.areas(act,"ACTIVITY_D",whp,"")



#PRIORITY LAYER(S) PREP
#reading in a layer that will indicate high priority areas

#WHP

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


# Drought Vulnerability

dv.rast<-rast("D:\\GIS_Large_Files\\CECS_Data\\CECS_CAWide_Vulner_TreeDieoff_SPI-2_2020_V250418.tif")
dv.rast.priority<-dv>1000
#dv.vect<-as.polygons(dv.rast.priority,aggregate=TRUE)#what if we didn't aggregate first?



#also need to check crses


