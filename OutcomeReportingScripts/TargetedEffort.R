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
patch.shape<-c(paste(loc.data,"ITS_2025Jul25_Polygons/appended.gdb",sep=""))
patch.layer<-c("appended_poly")

treat.vect<-read.and.check.crs.patch.vector(patch.shape[1],patch.name[1],patch.layer[1],prepped.boundary.vect)
treat.prep.vect<-crop.vector.by.boundary.and.recalc.area(prepped.boundary.vect,boundary.name,treat.vect,patch.name[1])

start.year<-"2020"
end.year<-"2024"

start<-paste(start.year,"-09-30",sep="")
end<-paste(end.year,"-10-01",sep="")

 #getting the different areas in ha of the different policy targets
 areas<-list()
 treat.filt.vect<-list()
 for(i in 1:length(activity.list)){
   temp<-filter.patches(treat.prep.vect,names(activity.list)[i],start,NA)
   treat.filt.vect[[names(activity.list)[i]]]<-aggregate(temp)
    #convert to acres
   areas[[names(activity.list)[i]]]<-expanse(treat.filt.vect[[names(activity.list)[i]]],unit="ha")*2.47105
 }


####################################################################
# PREP STRATIFICATION LAYERS (WUI/Wildland and Ecosystem Type)   ###
####################################################################

#Does one need to recalculate vegetation and wui masks?
recalculate.masks<-FALSE

if(recalculate.masks){
  #source the file that has all the raster and vector calculations to create
  #veg and wui/wildland masks
  source(paste(loc.scripts,"FunctionLibraries/CalculateWUI_Veg_Masks.R",sep=""))
} else {
  #read in the raster files
  wui.urb.rast<-vect(paste(loc.scripts,"ReferenceFiles/FRAP24_WUI.tif",sep=""))
  wui.wild.rast<-rast(paste(loc.scripts,"ReferenceFiles/FRAP24_Wild.tif",sep=""))

}


## add ecosystem layers here, which are rasters

####################################################################
############# PRIORITY LAYERS CALCS      ###########################
####################################################################

#reading in each layer that will indicate high priority areas
#needs to be separately scripted because each one has a slightly different way to be 
#thresholded or recoded in order to do the crosstab

#---------------- WHP, Wildland ---------------------------#

policy.target<-"Wildland Fire Risk"
#treatment type/activity type filter
treat.subs.vect<-filter.patches(treat.prep.vect,policy.target,start,NA)

#subset treatments for wildland only (spatial subset)

treat.rast<-rasterize(treat.strat.vect,reference.rast)

#read in WHP raster
#recode for 4 & 5 vs other

#then do crosstab between recoded WHP raster and filtered/subsetted treatments

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


#---------------- WHP, WUI ---------------------------#


#---------------- Drought Vulnerability, forest -------------------------#

#prep treatment layer
policy.target<-"Forest Health"
treat.subs.vect<-filter.patches(treat.prep.vect,policy.target,start,NA)
treat.strat.vect<-

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


