####################################################################
### Set locations and source functions and other inputs      #######
####################################################################

timer.start<-Sys.time()

datestamp<-"2025Nov14"

#scripts and important reference layers are in the github repo
loc.scripts<-"D:/GitRepos/BattlesLabRepos/Task-Force-Action-Plan-Support/"
#large files are on local (large) hard drives, not dropbox or github
loc.data<-"D:/GIS_Large_Files/"
#output sent to Dropbox so it can be accessed from anywhere while big runs are going
loc.output<-"D:/DropboxFiles/Dropbox/Professional/UCB_Battles/ActionPlanSupport/"

setwd(loc.output)
#this is where the list lives of activities that pertain to which policy questions
source(paste(loc.scripts,"FunctionLibraries/ActivityList.R",sep=""))
#all the functions used to do various outcome reporting and some scenario modeling calcs
source(paste(loc.scripts,"FunctionLibraries/SummarizeChange_functions.R",sep=""))

#reference rasters for spatial reference of CRS and extent
# CECS layers all have the same CRS and extent
cecs.rast<-rast(paste(loc.data,"CECS_Data/CECS_CAWide_Vulner_TreeDieoff_SPI-2_2020_V250614.tif",sep=""))
# whp is the same projection as state boundaries/TF regions, WHR/veg classifications
whp.rast <- rast(paste(loc.data,"PriorityLayers/whp_classified_20240906.tif",sep="")) 

reference.rast<-rast(paste(loc.data,"CECS_Data/CECS_CAWide_Vulner_TreeDieoff_SPI-2_2020_V250614.tif",sep=""))

ca.vect<-vect(paste(loc.scripts,"ReferenceFiles/CA_State.shp",sep=""))
ca.whp.vect<-check.crs.match(whp.rast,ca.vect)
ca.cecs.vect<-check.crs.match(cecs.rast,ca.vect)


#######################################################################
# PREP STRATIFICATION LAYERS (WUI/landscape, Ecosystem, utility/roads) #
#######################################################################

#Does one need to recalculate vegetation, road/utility, and wui masks?
recalculate.masks<-FALSE

if(recalculate.masks){
  #source the file that has all the raster and vector calculations to create
  #veg, utility, road, and wui/landscape masks
  mask.time<-system.time(source(paste(loc.scripts,"FunctionLibraries/CalculateWUI_Veg_Masks.R",sep="")))
  print(paste("Time to recalculate masks: ",round(mask.time[[1]]/60)," minute(s)", sep=""))
}

#some object names are different, so go ahead and reread them in even if you did recreate them
#read in the raster files
wui.whp.rast<-rast(paste(loc.scripts,"ReferenceFiles/FRAP24_WUIOnly_WHP.tif",sep=""))
wild.whp.rast<-rast(paste(loc.scripts,"ReferenceFiles/FRAP24_WildlandOnly_WHP.tif",sep=""))
#the CECS-compatible one probably won't get used much by the whole group, so keeping those
#files locally rather than in the GitHub repo.  if it needs to be recalculated, use recalculate.masks<-TRUE
wild.cecs.rast<-rast(paste(loc.data,"WUIVegetationClassifications/FRAP24_WildlandOnly_CECS.tif",sep=""))
wui.cecs.rast<-rast(paste(loc.data,"WUIVegetationClassifications/FRAP24_WUIOnly_CECS.tif",sep=""))

land.cecs.rast<-rast(paste(loc.data,"WUIVegetationClassifications/FRAP24_Landscape_CECS.tif",sep=""))
land.whp.rast<-rast(paste(loc.data,"WUIVegetationClassifications/FRAP24_Landscape_WHP.tif",sep=""))

#read in the veg classification masks (Forest and Shrub from CALFIRE WHR reclassifications, same crs as WHP)
forest.whp.rast<-rast(paste(loc.scripts,"ReferenceFiles/WHR13_RECLASS_FOREST.tif",sep=""))
shrub.whp.rast<-rast(paste(loc.scripts,"ReferenceFiles/WHR13_RECLASS_SHRUB.tif",sep=""))

shrub.whp.rast<-crop(shrub.whp.rast,whp.rast)
forest.whp.rast<-crop(forest.whp.rast,whp.rast)

#read in the CECS-projected forest and shrub classifications
forest.cecs.rast<-rast(paste(loc.data,"WUIVegetationClassifications/WHR13_RECLASS_FOREST_CECS.tif",sep=""))
shrub.cecs.rast<-rast(paste(loc.data,"WUIVegetationClassifications/WHR13_RECLASS_SHRUB_CECS.tif",sep=""))
#and grass and woodland, for completeness
grass.whp.rast<-rast(paste(loc.data,"WUIVegetationClassifications/WHR13_RECLASS_GRASS.tif",sep=""))
wood.whp.rast<-rast(paste(loc.data,"WUIVegetationClassifications/WHR13_RECLASS_WOODLAND.tif",sep=""))
grass.cecs.rast<-rast(paste(loc.data,"WUIVegetationClassifications/WHR13_RECLASS_GRASS_CECS.tif",sep=""))
wood.cecs.rast<-rast(paste(loc.data,"WUIVegetationClassifications/WHR13_RECLASS_WOODLAND_CECS.tif",sep=""))

grass.whp.rast<-crop(grass.whp.rast,whp.rast)
wood.whp.rast<-crop(wood.whp.rast,whp.rast)


#read in the road and transmission line buffers
tran.buff.cecs.rast<-rast(paste(loc.data,"WUIVegetationClassifications/TransmissionLineBuffer_CECSproj.tif",sep=""))
tran.buff.whp.rast<-rast(paste(loc.data,"WUIVegetationClassifications/TransmissionLineBuffer_WHPproj.tif",sep=""))
#note that these are just for calculating the shrub priority layer
road.buff.cecs.rast<-rast(paste(loc.data,"WUIVegetationClassifications/RoadBuffer_CECSproj.tif",sep=""))
road.buff.whp.rast<-rast(paste(loc.data,"WUIVegetationClassifications/RoadBuffer_WHPproj.tif",sep=""))


####################################################################
#     READ IN AND RECODE PRIORITY LAYERS                         ###
####################################################################

#reading in each layer that will indicate high priority areas
#needs to be separately scripted because each one has a slightly different way to be 
#thresholded or recoded in order to do the crosstab, and some are vector and some are raster

#This process uses up a lot of memory, so we'll put this in a flag as well to either
#recalculate them, or read them in as rasters
recalculate.priority.layers<-FALSE

if(recalculate.priority.layers){
  #recalculate all the different priority layers
  pri.time<-system.time(source(paste(loc.scripts,"FunctionLibraries/CalculatePriorityLayers.R",sep="")))
  print(paste("Time to recalculate priority layers: ",round(pri.time[[1]]/60)," minute(s)", sep=""))
}
  #read in rasters
  whp.priority.rast<-rast(paste(loc.data,"PriorityLayers/FinalPriorityLayers/WHPpriority_WHP.tif",sep=""))
  dv.priority.rast<-rast(paste(loc.data,"PriorityLayers/FinalPriorityLayers/DroughtVulnerabilityPriority_CECS.tif",sep=""))
  fl.priority.rast<-rast(paste(loc.data,"PriorityLayers/FinalPriorityLayers/FlameLengthPriority_CECS.tif",sep=""))
  cr.priority.rast<-rast(paste(loc.data,"PriorityLayers/FinalPriorityLayers/CriticalHabitatPriority_WHP.tif",sep=""))
  hy.priority.rast<-rast(paste(loc.data,"PriorityLayers/FinalPriorityLayers/HydropowerPriority_WHP.tif",sep=""))
  de.priority.rast<-rast(paste(loc.data,"PriorityLayers/FinalPriorityLayers/DebrisFlowPriority_WHP.tif",sep=""))
  sh.priority.rast<-rast(paste(loc.data,"PriorityLayers/FinalPriorityLayers/ShrubRoadPriority_WHP.tif",sep=""))


plots<-FALSE

if(plots){
  png("PriorityLayerImages/WildfireHazardPotential.png",width=5.5,height=6, units="in",res=150)
  plot(whp.priority.rast, main="Wildfire Hazard Potential 4 (high) or 5 (very high)",col=c("#E9E5C3","#5A3B00"))
  polys(ca.whp.vect,border="#5A3B00")
  dev.off()
  png("PriorityLayerImages/DroughtVulnerability.png",width=5.5,height=6, units="in",res=150)
  plot(dv.priority.rast, main="Drought Vulnerability index 7,310 or greater",col=c("#E9E5C3","#5A3B00"))
  polys(ca.cecs.vect,border="#5A3B00")
  dev.off()
  png("PriorityLayerImages/FlameLength.png",width=5.5,height=6, units="in",res=150)
  plot(fl.priority.rast, main="Flame Length (FLAMMAP), 8 ft or greater",col=c("#E9E5C3","#5A3B00"))
  polys(ca.cecs.vect,border="#5A3B00")
  dev.off()
  png("PriorityLayerImages/CriticalHabitat.png",width=5.5,height=6, units="in",res=150)
  plot(cr.priority.rast, main="ACE Species Diversity rating 4 or 5",col=c("#E9E5C3","#5A3B00"))
  polys(ca.whp.vect,border="#5A3B00")
  dev.off()
  png("PriorityLayerImages/Hydropower.png",width=5.5,height=6, units="in",res=150)
  plot(hy.priority.rast, main="Watersheds feeding powerhouses\n30 KW and greater",col=c("#E9E5C3","#5A3B00"))
  polys(ca.whp.vect,border="#5A3B00")
  dev.off()
  png("PriorityLayerImages/DebrisFlow.png",width=5.5,height=6, units="in",res=150)
  plot(de.priority.rast, main="Debris Flow likelihood top 20% of risk",col=c("#E9E5C3","#5A3B00"))
  polys(ca.whp.vect,border="#5A3B00")
  dev.off()
  png("PriorityLayerImages/ShrubRoad.png",width=5.5,height=6, units="in",res=150)
  plot(sh.priority.rast, main="Shrub veg type within 500 ft of CALTRANS roads",col=c("#E9E5C3","#5A3B00"))
  polys(ca.whp.vect,border="#5A3B00")
  dev.off()

  png("MaskImages/Forest.png",width=5.5,height=6, units="in",res=150)
  plot(forest.whp.rast, main="CALFIRE WHR Forest",col="#5A3B00")
  polys(ca.whp.vect,border="#5A3B00")
  dev.off()
  png("MaskImages/Shrub.png",width=5.5,height=6, units="in",res=150)
  plot(shrub.whp.rast, main="CALFIRE WHR Shrub",col="#5A3B00")
  polys(ca.whp.vect,border="#5A3B00")
  dev.off()
  png("MaskImages/Grass.png",width=5.5,height=6, units="in",res=150)
  plot(grass.whp.rast, main="CALFIRE WHR Grass",col="#5A3B00")
  polys(ca.whp.vect,border="#5A3B00")
  dev.off()
  png("MaskImages/Woodland.png",width=5.5,height=6, units="in",res=150)
  plot(wood.whp.rast, main="CALFIRE WHR Woodland",col="#5A3B00")
  polys(ca.whp.vect,border="#5A3B00")
  dev.off()
#  png("MaskImages/Wildland.png",width=5.5,height=6, units="in",res=150)
#  plot(wild.whp.rast, main="Wildland: CALFIRE 'Influence & Other' + CECS 'Other'",col="#5A3B00")
#  polys(ca.whp.vect,border="#5A3B00")
#  dev.off()
  png("MaskImages/Landscape.png",width=5.5,height=6, units="in",res=150)
  plot(land.whp.rast, main="Landscape: NOT agriculture, urban, and water",col="#5A3B00")
  polys(ca.whp.vect,border="#5A3B00")
  dev.off()
  png("MaskImages/WUI.png",width=5.5,height=6, units="in",res=150)
  plot(wui.whp.rast, main="CALFIRE WUI footprint (interface,intermix)",col="#5A3B00")
  polys(ca.whp.vect,border="#5A3B00")
  dev.off()
  png("MaskImages/TransmissionLine.png",width=5.5,height=6, units="in",res=150)
  plot(tran.buff.whp.rast, main="Utility Transmission lines",col="#5A3B00")
  polys(ca.whp.vect,border="#5A3B00")
  dev.off()
  png("MaskImages/Road.png",width=5.5,height=6, units="in",res=150)
  plot(road.buff.whp.rast, main="CALTRANS roads",col="#5A3B00")
  polys(ca.whp.vect,border="#5A3B00")
  dev.off()

}

conditions.mask.areas<-FALSE

if(conditions.mask.areas){
  current.conditions<-data.frame(PolicyTarget=character(),Priority=character(), Source=character(), Threshold=character(),
        Area=numeric(),stringsAsFactors=FALSE)
  temp<-whp.priority.rast
  temp[is.na(temp)]<-0
  current.conditions[1,]<-c("Fire Risk","Wildfire Hazard Potential", "CALFIRE FRAP","Hazard Category 4 or 5", 
        as.numeric(global(temp,"sum")*30*30*0.000247105))
  temp<-fl.priority.rast
  temp[is.na(temp)]<-0
  current.conditions[2,]<-c("Fire Risk","Flame Length", "CECS","Flame Length > 8 ft",
        as.numeric(global(temp,"sum")*30*30*0.000247105))
  temp<-dv.priority.rast
  temp[is.na(temp)]<-0
  current.conditions[3,]<-c("Forest Health","Drought Vulnerability", "CECS","Vulnerability Index >=7,310",
        as.numeric(global(temp,"sum")*30*30*0.000247105))
  current.conditions[4,]<-c("Shrubland Health","Road Proximity","CALFIRE WHR & CALTRANS Roads","500 ft road buffer in shrublands", 
        as.numeric(global(sh.priority.rast,"sum")*30*30*0.000247105))
  current.conditions[5,]<-c("Water","Hydropower","Bales & Guo","Watersheds w/powerhouses >= 30KW",
        as.numeric(global(hy.priority.rast,"sum")*30*30*0.000247105))
  temp<-de.priority.rast
  temp[is.na(temp)]<-0
  current.conditions[6,]<-c("Water","Debris Flow Potential","CGS","Watersheds with risk in the top 40%", 
        as.numeric(global(temp,"sum")*30*30*0.000247105))
  temp<-cr.priority.rast
  temp[is.na(temp)]<-0
  current.conditions[7,]<-c("Habitat","Critical Habitat","CDFW ACE","Species Diversity Category 4 or 5",
        as.numeric(global(temp,"sum")*30*30*0.000247105))

  write.csv(current.conditions,paste("CurrentConditions",datestamp,".csv",sep=""))

  mask.areas<-data.frame(Mask=character(),CRS=character(),Area=numeric(),stringsAsFactors=FALSE)
  mask.areas[1,]<-c("Forest","CECS",as.numeric(global(forest.cecs.rast,"notNA")*30*30*0.000247105))
  mask.areas[2,]<-c("Shrub","CECS",as.numeric(global(shrub.cecs.rast,"notNA")*30*30*0.000247105))
  mask.areas[3,]<-c("Woodland","CECS",as.numeric(global(wood.cecs.rast,"notNA")*30*30*0.000247105))
  mask.areas[4,]<-c("Grass","CECS",as.numeric(global(grass.cecs.rast,"notNA")*30*30*0.000247105))
  mask.areas[5,]<-c("WUI","CECS",as.numeric(global(wui.cecs.rast,"notNA")*30*30*0.000247105))
  mask.areas[6,]<-c("Landscape","CECS",as.numeric(global(land.cecs.rast,"notNA")*30*30*0.000247105))
  mask.areas[7,]<-c("Utility","CECS",as.numeric(global(tran.buff.cecs.rast,"notNA")*30*30*0.000247105))
  mask.areas[8,]<-c("Road","CECS",as.numeric(global(road.buff.cecs.rast,"notNA")*30*30*0.000247105))
  mask.areas[9,]<-c("Forest","WHP",as.numeric(global(forest.whp.rast,"notNA")*30*30*0.000247105))
  mask.areas[10,]<-c("Shrub","WHP",as.numeric(global(shrub.whp.rast,"notNA")*30*30*0.000247105))
  mask.areas[11,]<-c("Woodland","WHP",as.numeric(global(wood.whp.rast,"notNA")*30*30*0.000247105))
  mask.areas[12,]<-c("Grass","WHP",as.numeric(global(grass.whp.rast,"notNA")*30*30*0.000247105))
  mask.areas[13,]<-c("WUI","WHP",as.numeric(global(wui.whp.rast,"notNA")*30*30*0.000247105))
  mask.areas[14,]<-c("Landscape","WHP",as.numeric(global(land.whp.rast,"notNA")*30*30*0.000247105))
  mask.areas[15,]<-c("Utility","WHP",as.numeric(global(tran.buff.whp.rast,"notNA")*30*30*0.000247105))
  mask.areas[16,]<-c("Road","WHP",as.numeric(global(road.buff.whp.rast,"notNA")*30*30*0.000247105))
  
  write.csv(mask.areas,paste("MaskAreas_",datestamp,".csv",sep=""))
}

####################################################################
# PREP BOUNDARY LAYERS (Statewide and Task Force Regions)        ###
####################################################################

# boundary.shape<-c(
#                   paste(loc.scripts,"ReferenceFiles/Region_Sierra.shp",sep=""),
#                   paste(loc.scripts,"ReferenceFiles/Region_NorthernCA.shp",sep=""),
#                   paste(loc.scripts,"ReferenceFiles/Region_CentralCoast.shp",sep=""))
# boundary.name<-c("Sierra","North","Central")


# #loop through the four regions
#  boundary.shape<-c(paste(loc.scripts,"ReferenceFiles/Region_SouthernCA.shp",sep=""),
#                    paste(loc.scripts,"ReferenceFiles/Region_Sierra.shp",sep=""),
#                    paste(loc.scripts,"ReferenceFiles/Region_NorthernCA.shp",sep=""),
#                    paste(loc.scripts,"ReferenceFiles/Region_CentralCoast.shp",sep=""))
#  boundary.name<-c("South","Sierra","North","Central")

#loop through all california, and the four regions
boundary.shape<-c(paste(loc.scripts,"ReferenceFiles/CA_State.shp",sep=""),
                  paste(loc.scripts,"ReferenceFiles/Region_Sierra.shp",sep=""),
                  paste(loc.scripts,"ReferenceFiles/Region_NorthernCA.shp",sep=""),
                  paste(loc.scripts,"ReferenceFiles/Region_SouthernCA.shp",sep=""),
                  paste(loc.scripts,"ReferenceFiles/Region_CentralCoast.shp",sep=""))
boundary.name<-c("CA","Sierra","North","South","Central")

#boundary.shape<-c(paste(loc.scripts,"ReferenceFiles/CA_State.shp",sep=""))
#boundary.name<-c("CA")

#boundary.shape<-c(paste(loc.scripts,"ReferenceFiles/Region_SouthernCA.shp",sep=""))
#boundary.name<-c("South")


#these will be necessary for if you need to recalculate the polygons
#and for file naming conventions
start.year<-"2020"
end.year<-"2024"

#water year
start.y<-paste(start.year,"-09-30",sep="")
end.y<-paste(end.year,"-10-01",sep="")


aggregate.vectors<-FALSE

#this recalculates all the possible aggregations of the vectors, needs redoing if summary unit
#changes, or regions/boundary vectors change, or new treatments/fires need to be used
#or if we change what the treatment type filters are
if(aggregate.vectors){
    vect.time<-system.time(source(paste(loc.scripts,"FunctionLibraries/CalculateAggregatedPatches.R",sep="")))
  print(paste("Time to process and aggregate vectors: ",round(vect.time[[1]]/60)," minute(s)", sep=""))
}

#######################################################
######  start loop through boundary layers ############
#######################################################

#set up the data frame for the outputs
targeted.effort.results<-data.frame(Boundary=character(),PolicyTarget=character(),Metric=character(),
                                    MaskName=character(),AreaType=character(),
                                    PriorityArea=numeric(),TotalTreatmentArea=numeric(),
                                    ProportionOfTreatments=numeric(),stringsAsFactors=FALSE)

count<-1
for(i in 1:length(boundary.name)){

    ####################################################################
    ############# CROSSTAB CALCULATIONS      ###########################
    ####################################################################

    #---------------- High-Risk Shrubs -------------------------#

    policy.target<-"ShrublandHealth"
    metric.name<-"ShrubVulnerability"
    mask.name<-"Shrub"
    r.rast<-whp.rast
    m.rast<-shrub.whp.rast
    p.rast<-sh.priority.rast

    rast.calcs.time<-system.time(count<-rasterize.mask.calculate.proportions(1,p.rast,metric.name,policy.target,boundary.name[i],boundary.shape[i],r.rast,mask.name,m.rast))
    print(paste("Total time to do raster math for ",policy.target," using ",metric.name," in ",mask.name,": ",round(rast.calcs.time[[1]]/60)," minute(s)", sep=""))
    print(count)
    #---------------- Drought Vulnerability, forest -------------------------#

    policy.target<-"ForestHealth"
    metric.name<-"DroughtVulnerability"
    mask.name<-"Forest"
    r.rast<-cecs.rast
    m.rast<-forest.cecs.rast
    p.rast<-dv.priority.rast

    rast.calcs.time<-system.time(count<-rasterize.mask.calculate.proportions(1,p.rast,metric.name,policy.target,boundary.name[i],boundary.shape[i],r.rast,mask.name,m.rast))
    print(paste("Total time to do raster math for ",policy.target," using ",metric.name," in ",mask.name,": ",round(rast.calcs.time[[1]]/60)," minute(s)", sep=""))
    print(count)
    # #---------------- WHP, Landscape ---------------------------#

    policy.target<-"WildlandFireRisk"
    metric.name<-"WildfireHazardPotentialLandscape"
    mask.name<-"Landscape"
    r.rast<-whp.rast
    m.rast<-land.whp.rast
    p.rast<-whp.priority.rast
    rast.calcs.time<-system.time(count<-rasterize.mask.calculate.proportions(1,p.rast,metric.name,policy.target,boundary.name[i],boundary.shape[i],r.rast,mask.name,m.rast))
    print(paste("Total time to do raster math for ",policy.target," using ",metric.name," in ",mask.name,": ",round(rast.calcs.time[[1]]/60)," minute(s)", sep=""))
    print(count)
    #-----------------FLAME LENGTH, landscape--------------#
    policy.target<-"WildlandFireRisk"
    metric.name<-"FlameLengthAbove8FtLandscape"
    mask.name<-"Landscape"
    r.rast<-cecs.rast
    m.rast<-land.cecs.rast
    p.rast<-fl.priority.rast

    rast.calcs.time<-system.time(count<-rasterize.mask.calculate.proportions(1,p.rast,metric.name,policy.target,boundary.name[i],boundary.shape[i],r.rast,mask.name,m.rast))
    print(paste("Total time to do raster math for ",policy.target," using ",metric.name," in ",mask.name,": ",round(rast.calcs.time[[1]]/60)," minute(s)", sep=""))
    print(count) 
    # #---------------- WHP, WUI ---------------------------#

    policy.target<-"WildlandFireRisk"
    metric.name<-"WildfireHazardPotentialWUI"
    mask.name<-"WUI"
    r.rast<-whp.rast
    m.rast<-wui.whp.rast
    p.rast<-whp.priority.rast
    rast.calcs.time<-system.time(count<-rasterize.mask.calculate.proportions(1,p.rast,metric.name,policy.target,boundary.name[i],boundary.shape[i],r.rast,mask.name,m.rast))
    print(paste("Total time to do raster math for ",policy.target," using ",metric.name," in ",mask.name,": ",round(rast.calcs.time[[1]]/60)," minute(s)", sep=""))
    print(count)
    #-----------------FLAME LENGTH, WUI--------------#
    policy.target<-"WildlandFireRisk"
    metric.name<-"FlameLengthAbove8FtWUI"
    mask.name<-"WUI"
    r.rast<-cecs.rast
    m.rast<-wui.cecs.rast
    p.rast<-fl.priority.rast

    rast.calcs.time<-system.time(count<-rasterize.mask.calculate.proportions(1,p.rast,metric.name,policy.target,boundary.name[i],boundary.shape[i],r.rast,mask.name,m.rast))
    print(paste("Total time to do raster math for ",policy.target," using ",metric.name," in ",mask.name,": ",round(rast.calcs.time[[1]]/60)," minute(s)", sep=""))
    print(count)
    # #---------------- WHP, Roads ---------------------------#

    policy.target<-"WildlandFireRisk"
    metric.name<-"WildfireHazardPotentialRoads"
    mask.name<-"Roads"
    r.rast<-whp.rast
    m.rast<-road.buff.whp.rast
    p.rast<-whp.priority.rast
    rast.calcs.time<-system.time(count<-rasterize.mask.calculate.proportions(1,p.rast,metric.name,policy.target,boundary.name[i],boundary.shape[i],r.rast,mask.name,m.rast))
    print(paste("Total time to do raster math for ",policy.target," using ",metric.name," in ",mask.name,": ",round(rast.calcs.time[[1]]/60)," minute(s)", sep=""))
    print(count)
    #-----------------FLAME LENGTH, roads--------------#
    policy.target<-"WildlandFireRisk"
    metric.name<-"FlameLengthAbove8FtRoads"
    mask.name<-"Roads"
    r.rast<-cecs.rast
    m.rast<-road.buff.cecs.rast
    p.rast<-fl.priority.rast

    rast.calcs.time<-system.time(count<-rasterize.mask.calculate.proportions(1,p.rast,metric.name,policy.target,boundary.name[i],boundary.shape[i],r.rast,mask.name,m.rast))
    print(paste("Total time to do raster math for ",policy.target," using ",metric.name," in ",mask.name,": ",round(rast.calcs.time[[1]]/60)," minute(s)", sep=""))
    print(count)

    # #---------------- WHP, Utilities ---------------------------#

    policy.target<-"WildlandFireRisk"
    metric.name<-"WildfireHazardPotentialUtilities"
    mask.name<-"Utilities"
    r.rast<-whp.rast
    m.rast<-tran.buff.whp.rast
    p.rast<-whp.priority.rast
    rast.calcs.time<-system.time(count<-rasterize.mask.calculate.proportions(1,p.rast,metric.name,policy.target,boundary.name[i],boundary.shape[i],r.rast,mask.name,m.rast))
    print(paste("Total time to do raster math for ",policy.target," using ",metric.name," in ",mask.name,": ",round(rast.calcs.time[[1]]/60)," minute(s)", sep=""))
    print(count)
    #-----------------FLAME LENGTH, Utilities --------------#
    policy.target<-"WildlandFireRisk"
    metric.name<-"FlameLengthAbove8FtUtilities"
    mask.name<-"Utilities"
    r.rast<-cecs.rast
    m.rast<-tran.buff.cecs.rast
    p.rast<-fl.priority.rast

    rast.calcs.time<-system.time(count<-rasterize.mask.calculate.proportions(1,p.rast,metric.name,policy.target,boundary.name[i],boundary.shape[i],r.rast,mask.name,m.rast))
    print(paste("Total time to do raster math for ",policy.target," using ",metric.name," in ",mask.name,": ",round(rast.calcs.time[[1]]/60)," minute(s)", sep=""))
    print(count)
   # #---------------- WHP, Forest ---------------------------#

    policy.target<-"WildlandFireRisk"
    metric.name<-"WildfireHazardPotentialForest"
    mask.name<-"Forest"
    r.rast<-whp.rast
    m.rast<-forest.whp.rast
    p.rast<-whp.priority.rast
    rast.calcs.time<-system.time(count<-rasterize.mask.calculate.proportions(1,p.rast,metric.name,policy.target,boundary.name[i],boundary.shape[i],r.rast,mask.name,m.rast))
    print(paste("Total time to do raster math for ",policy.target," using ",metric.name," in ",mask.name,": ",round(rast.calcs.time[[1]]/60)," minute(s)", sep=""))
    print(count)
   # #---------------- WHP, Shrub ---------------------------#

    policy.target<-"WildlandFireRisk"
    metric.name<-"WildfireHazardPotentialShrub"
    mask.name<-"Shrub"
    r.rast<-whp.rast
    m.rast<-shrub.whp.rast
    p.rast<-whp.priority.rast
    rast.calcs.time<-system.time(count<-rasterize.mask.calculate.proportions(1,p.rast,metric.name,policy.target,boundary.name[i],boundary.shape[i],r.rast,mask.name,m.rast))
    print(paste("Total time to do raster math for ",policy.target," using ",metric.name," in ",mask.name,": ",round(rast.calcs.time[[1]]/60)," minute(s)", sep=""))
    print(count)

   # #---------------- WHP, grass ---------------------------#

    policy.target<-"WildlandFireRisk"
    metric.name<-"WildfireHazardPotentialGrass"
    mask.name<-"Grass"
    r.rast<-whp.rast
    m.rast<-grass.whp.rast
    p.rast<-whp.priority.rast
    rast.calcs.time<-system.time(count<-rasterize.mask.calculate.proportions(1,p.rast,metric.name,policy.target,boundary.name[i],boundary.shape[i],r.rast,mask.name,m.rast))
    print(paste("Total time to do raster math for ",policy.target," using ",metric.name," in ",mask.name,": ",round(rast.calcs.time[[1]]/60)," minute(s)", sep=""))
    print(count)
   # #---------------- WHP, woodland ---------------------------#

    policy.target<-"WildlandFireRisk"
    metric.name<-"WildfireHazardPotentialWoodland"
    mask.name<-"Woodland"
    r.rast<-whp.rast
    m.rast<-wood.whp.rast
    p.rast<-whp.priority.rast
    rast.calcs.time<-system.time(count<-rasterize.mask.calculate.proportions(1,p.rast,metric.name,policy.target,boundary.name[i],boundary.shape[i],r.rast,mask.name,m.rast))
    print(paste("Total time to do raster math for ",policy.target," using ",metric.name," in ",mask.name,": ",round(rast.calcs.time[[1]]/60)," minute(s)", sep=""))
    print(count)

    #---------------- Critical Habitat -------------------------#

    policy.target<-"Habitat"
    metric.name<-"CriticalHabitat"
    mask.name<-"Landscape"
    r.rast<-whp.rast
    m.rast<-land.whp.rast
    p.rast<-cr.priority.rast
    rast.calcs.time<-system.time(count<-rasterize.mask.calculate.proportions(1,p.rast,metric.name,policy.target,boundary.name[i],boundary.shape[i],r.rast,mask.name,m.rast))
    print(paste("Total time to do raster math for ",policy.target," using ",metric.name," in ",mask.name,": ",round(rast.calcs.time[[1]]/60)," minute(s)", sep=""))
    print(count)
    #---------------- Hydropower -------------------------#

    #there aren't any overlaps of hydropower watersheds in south
    if(boundary.name[i] %in% c("CA","Sierra","North","Central")){
      policy.target<-"Water"
      metric.name<-"Hydropower"
      mask.name<-"Landscape"
      r.rast<-whp.rast
      m.rast<-land.whp.rast
      p.rast<-hy.priority.rast
      rast.calcs.time<-system.time(count<-rasterize.mask.calculate.proportions(1,p.rast,metric.name,policy.target,boundary.name[i],boundary.shape[i],r.rast,mask.name,m.rast))
      print(paste("Total time to do raster math for ",policy.target," using ",metric.name," in ",mask.name,": ",round(rast.calcs.time[[1]]/60)," minute(s)", sep=""))
      print(count)
    }
    #---------------- Debris Flow Risk -------------------------#

    policy.target<-"Water"
    metric.name<-"DebrisFlow"
    mask.name<-"Landscape"
    r.rast<-whp.rast
    m.rast<-land.whp.rast
    p.rast<-de.priority.rast
    rast.calcs.time<-system.time(count<-rasterize.mask.calculate.proportions(1,p.rast,metric.name,policy.target,boundary.name[i],boundary.shape[i],r.rast,mask.name,m.rast))
    print(paste("Total time to do raster math for ",policy.target," using ",metric.name," in ",mask.name,": ",round(rast.calcs.time[[1]]/60)," minute(s)", sep=""))
    print(count)
    #---------------- acres of fuel breaks -------------------------#
      ### Manually, aspatially calculate

}

timer.end<-Sys.time()

time.total<-timer.end-timer.start
print(time.total)


#data vis

#all.targeted.effort<-read.csv("TargetedEffortResults/TargetedEffortResults_2025Aug20_AllRegions.csv")
#all.targeted.effort<-read.csv("TargetedEffortResults_2025Sep15_forVis.csv")
#all.targeted.effort<-read.csv("TargetedEffortResults_2025Sep28_forVis.csv")
all.targeted.effort<-read.csv("TargetedEffortResults_2025Nov14_forVis.csv")

metrics<-levels(factor(all.targeted.effort$Metric))

regions<-levels(factor(all.targeted.effort$Boundary))

nice.boundary.name<-c(
          "All of California",
          "Central Coast Region",
          "Northern California",
          "Sierra Nevada Region",
          "Southern California")



# for(k in 1:length(regions)){

#   for(i in 1:length(metrics)){
#     #choose the correct metric
#     metric.name<-metrics[i]
#     print(metric.name)

#    all.targeted.effort$AreaType[all.targeted.effort$AreaType=="TotalArea"]<-"Background\nProportion"
#    all.targeted.effort$AreaType[all.targeted.effort$AreaType=="Treatments"]<-"Proportion\n in Treatments"

#    targeted.effort.sub<-all.targeted.effort[all.targeted.effort$Metric==metrics[i] & all.targeted.effort$Boundary==regions[k],]

#     plot.title<-paste("Proportion of Priority Areas\n(",metric.name,")\nin ",nice.boundary.name[k], sep="")
#     bar.plt<-ggplot(data=targeted.effort.sub, aes(x=AreaType,fill=AreaType,y=ProportionOfTreatments)) +
#       geom_bar(stat="identity")+
#       theme(legend.position="none")+
#         labs(title = plot.title,x = element_blank(), y = "Proportion of Priority Areas")+
#       scale_fill_manual(values=c("#E9E5C3","#5A3B00"))#+
#     pltnm.b<-paste("TargetedEffortResults/RelativeProportion_bar_", metric.name,"_",regions[k],"_",datetimevis,".png",sep="")
#       ggsave(pltnm.b, units="in", width=4,height=3)

#   }

# }

nice.metric.name<-c(
                      "Critical Habitat", 
                      "High Debris Flow Areas",
                      "Drought-Vulnerable Forests",
                      "High FlameLength (Whole Landscape)",
                      "High Flame Length (Near Roads)",
                      "High Flame Length (Near Utilities)",
                      "High Flame Length (in WUI)",          
                      "Hydropower Watersheds",
                      "Shrublands near Roads",
                      "High Wildfire Hazard Potential, Forest",
                      "High Wildfire Hazard Potential, Grassland",
                      "High Wildfire Hazard Potential, Whole Landscape",
                      "High Wildfire Hazard Potential, Near Roads",
                      "High Wildfire Hazard Potential, Shrubland",
                      "High Wildfire Hazard Potential, Near Utilities",
                      "High Wildfire Hazard Potential, Woodland",
                      "High Wildfire Hazard Potential, in WUI"
  )

  for(i in 1:length(metrics)){
    #choose the correct metric
    metric.name<-metrics[i]
    print(metric.name)

   all.targeted.effort$AreaType[all.targeted.effort$AreaType=="TotalArea"]<-"Background Proportion"
   all.targeted.effort$AreaType[all.targeted.effort$AreaType=="Treatments"]<-"Proportion  in Treatments"

   targeted.effort.sub<-all.targeted.effort[all.targeted.effort$Metric==metrics[i],]

    plot.title<-paste("Proportion of ",nice.metric.name[i], sep="")
    bar.plt<-ggplot(data=targeted.effort.sub, aes(x=AreaType,fill=AreaType,y=ProportionOfTreatments)) +
      geom_bar(stat="identity")+
      theme(legend.position="none")+
      facet_grid(.~Boundary)+
          theme(legend.position="bottom",axis.text.x = element_blank(),axis.title.x= element_blank(),axis.ticks.x=element_blank())+
        labs(title = plot.title, fill="", y = "Proportion of Priority Areas")+
      scale_fill_manual(values=c("#E9E5C3","#5A3B00"))#+
    pltnm.b<-paste("TargetedEffortResults/RelativeProportion_bar_", metric.name,"_",datestamp,".png",sep="")
      ggsave(pltnm.b, units="in", width=6,height=3)

  }

