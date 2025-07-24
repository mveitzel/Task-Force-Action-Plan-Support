loc.scripts<-"D:/GitRepos/BattlesLabRepos/Task-Force-Action-Plan-Support/"
loc.data<-"D:/GIS_Large_Files/"
loc.output<-"D:/DropboxFiles/Dropbox/Professional/UCB_Battles/ActionPlanSupport/"

setwd(loc.output)
source(paste(loc.scripts,"FunctionLibraries/SummarizeChange_functions.R",sep=""))

boundary.shape<-c(paste(loc.scripts,"VectorFiles/CA_State.shp",sep=""))
boundary.name<-c("CA")
reference.rast<-rast(paste(loc.data,"CECS_Data/CECS_CAWide_Vulner_TreeDieoff_SPI-2_2020_V250418.tif",sep=""))

prepped.boundary.vect<-read.and.prepare.boundary.vector(boundary.shape,boundary.name,reference.rast)

####################################################################
# PREP TREATMENT DATASET                                           #
####################################################################

patch.name<-c("Treatments")
patch.shape<-c(paste(loc.data,"ITT_2024_Data/Interagency Tracking System.gdb",sep=""))
patch.layer<-c("Treat_n_harvests_polygons2023_20240911")

treat.vect<-read.and.check.crs.patch.vector(patch.shape[1],patch.name[1],patch.layer[1],prepped.boundary.vect)
treat.prep.vect<-crop.vector.by.boundary.and.recalc.area(prepped.boundary.vect,boundary.name,treat.vect,patch.name[1])

start<-"2020-09-30"
end<-"2023-10-01"


# #***TODO: something is going wrong with the first four, fire risk related metrics
# #getting the different areas in ha of the different policy targets
# areas<-list()
# for(i in 1:length(activity.list)){
#   temp<-treat.subs.vect<-filter.patches(treat.prep.vect,names(activity.list)[i],start,NA)
#   temp2<-aggregate(treat.subs.vect)
#   areas[[names(activity.list)[i]]]<-expanse(temp2,unit="ha")
# }

# areas*2.47105

####################################################################
# PREP STRATIFICATION LAYERS (WUI/Wildland and Ecosystem Type)   ###
####################################################################
#I had to do some crazy stuff to pull out just the WUI classification from the FRAP layer SIG gave me
#I could not get the raster to match with CECS rasters so I vectorized the WUI/non-WUI rasters

#"5 Class WUI raster, developed by SIG. 0 = everything else, 1 = influence, 
#2 = intermix, 3 = interface, 4 = urban, 5 = wildland

# wui_FRAP<-rast("WUI24_extract.tif")
# #For the FRAP layer via SIG, 2=intermix, 1=interface, and 3=influence
# wui.urb<-wui_FRAP %in% c(2,1,3) #should I include 4 = urban?
# # and 5 = wildland 
# wui.wild<-wui_FRAP == 5
# wui.urb.poly<-as.polygons(wui.urb,aggregate=TRUE)
# wui.wild.poly<-as.polygons(wui.wild,aggregate=TRUE)
# wui.urb.poly.proj<-check.crs.match(rasters$before,wui.urb.poly)
# wui.wild.poly.proj<-check.crs.match(rasters$before,wui.wild.poly)
# wui.urb.poly.proj<-wui.urb.poly.proj[wui.urb.poly.proj$Band_1==1,]
# wui.wild.poly.proj<-wui.wild.poly.proj[wui.wild.poly.proj$Band_1==1,]
# writeVector(wui.urb.poly.proj,"VectorFiles/FRAP24_WUI.shp",overwrite=TRUE)
# writeVector(wui.wild.poly.proj,"VectorFiles/FRAP24_Wild.shp",overwrite=TRUE)

wui.urb.vect<-vect(paste(loc.scripts,"VectorFiles/FRAP24_WUI.shp",sep=""))
wui.wild.vect<-vect(paste(loc.scripts,"VectorFiles/FRAP24_WUI.shp",sep=""))



####################################################################
############# PRIORITY LAYERS CALCS      ###########################
####################################################################

#reading in each layer that will indicate high priority areas
#needs to be separately scripted because each one has a slightly different way to be 
#thresholded or recoded in order to do the crosstab

#---------------- WHP, Wildland ---------------------------#


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


#---------------- Drought Vulnerability, forest -------------------------#

#prep treatment layer
policy.target<-"Forest Health"
treat.subs.vect<-filter.patches(treat.prep.vect,policy.target,start,NA)
treat.strat.vect

treat.rast<-rasterize(treat.strat.vect,reference.rast)



dv.rast<-rast("D:\\GIS_Large_Files\\CECS_Data\\CECS_CAWide_Vulner_TreeDieoff_SPI-2_2020_V250418.tif")
dv.rast.priority<-dv.rast> 10000 
#dv.rast.pri<-subst(dv.rast.priority,FALSE,NA)


#Do crosstab


crosstab.time<- system.time(dv.treat<-crosstab(c(dv.rast.priority,treat.rast)) )
crosstab.time/60
#    user   system  elapsed 
#15.82867  1.52350 17.40600 

result<-as.data.frame(dv.treat)

result$area<-result$Freq*30*30*0.000247105

prop.dv.pri<-result$area[result$CECS_CAWide_Vulner_TreeDieoff_SPI.2_2020_V250418==1]/sum(result$area)




#also need to check crses


