####################################################################
### Set locations and source functions and other inputs      #######
####################################################################

timer.start<-Sys.time()

datestamp<-"2025Aug10"

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

#######################################################################
# PREP STRATIFICATION LAYERS (WUI/Wildland, Ecosystem, utility/roads) #
#######################################################################

#Does one need to recalculate vegetation and wui masks?
recalculate.masks<-FALSE

if(recalculate.masks){
  #source the file that has all the raster and vector calculations to create
  #veg and wui/wildland masks
  #right now this is just the wui/wildland masks
  mask.time<-system.time(source(paste(loc.scripts,"FunctionLibraries/CalculateWUI_Veg_Masks.R",sep="")))
  print(paste("Time to recalculate masks: ",mask.time[[1]]/60, sep=""))

} else {
  #read in the raster files
  wui.whp.rast<-rast(paste(loc.scripts,"ReferenceFiles/FRAP24_WUIOnly_WHP.tif",sep=""))
  wild.whp.rast<-rast(paste(loc.scripts,"ReferenceFiles/FRAP24_WildlandOnly_WHP.tif",sep=""))
  #the CECS-compatible one probably won't get used much by the whole group, so keeping those
  #files locally rather than in the GitHub repo.  if it needs to be recalculated, use recalculate.masks<-TRUE
  wild.cecs.rast<-rast(paste(loc.data,"WUIVegetationClassifications/FRAP24_WildlandOnly_CECS.tif",sep=""))
  wui.cecs.rast<-rast(paste(loc.data,"WUIVegetationClassifications/FRAP24_WUIOnly_CECS.tif",sep=""))

  #read in the veg classification masks (Forest and Shrub from CALFIRE WHR reclassifications, same crs as WHP)
  forest.whp.rast<-rast(paste(loc.scripts,"ReferenceFiles/WHR13_RECLASS_FOREST.tif",sep=""))
  shrub.whp.rast<-rast(paste(loc.scripts,"ReferenceFiles/WHR13_RECLASS_SHRUB.tif",sep=""))
  #read in the CECS-projected forest and shrub classifications
  forest.cecs.rast<-rast(paste(loc.data,"WUIVegetationClassifications/WHR13_RECLASS_FOREST_CECS.tif",sep=""))
  shrub.cecs.rast<-rast(paste(loc.data,"WUIVegetationClassifications/WHR13_RECLASS_SHRUB_CECS.tif",sep=""))

  #read in the road and road+transmission line buffers
  rdtr.buff.cecs.vect<-vect(paste(loc.data,"WUIVegetationClassifications/RoadTransmissionLineBuffer_CECSproj.shp",sep=""))
  rdtr.buff.whp.vect<-vect(paste(loc.data,"WUIVegetationClassifications/RoadTransmissionLineBuffer_WHPproj.shp",sep=""))
  road.buff.cecs.vect<-vect(paste(loc.data,"WUIVegetationClassifications/RoadBuffer_CECSproj.shp",sep=""))
  road.buff.whp.vect<-vect(paste(loc.data,"WUIVegetationClassifications/RoadBuffer_WHPproj.shp",sep=""))
}


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
  print(paste("Time to recalculate priority layers: ",pri.time[[1]]/60, sep=""))
}else{
  #read in rasters
  whp.priority.rast<-rast(paste(loc.data,"PriorityLayers/FinalPriorityLayers/WHPpriority_WHP.tif",sep=""))
  dv.priority.rast<-rast(paste(loc.data,"PriorityLayers/FinalPriorityLayers/DroughtVulnerabilityPriority_CECS.tif",sep=""))
  fl.priority.rast<-rast(paste(loc.data,"PriorityLayers/FinalPriorityLayers/FlameLengthPriority_CECS.tif",sep=""))
  cr.priority.rast<-rast(paste(loc.data,"PriorityLayers/FinalPriorityLayers/CriticalHabitatPriority_WHP.tif",sep=""))
  hy.priority.rast<-rast(paste(loc.data,"PriorityLayers/FinalPriorityLayers/HydropowerPriority_WHP.tif",sep=""))
  de.priority.rast<-rast(paste(loc.data,"PriorityLayers/FinalPriorityLayers/DebrisFlowPriority_WHP.tif",sep=""))
  sh.priority.rast<-rast(paste(loc.data,"PriorityLayers/FinalPriorityLayers/ShrubRoadPriority_WHP.tif",sep=""))
}

plots<-TRUE

if(plots){
  png("PriorityLayerImages/WildfireHazardPotential.png",width=5.5,height=6, units="in",res=150)
  plot(whp.priority.rast, main="CALFIRE Wildfire Hazard Potential\n4 (high) or 5 (very high)")
  dev.off()
  png("PriorityLayerImages/DroughtVulnerability.png",width=5.5,height=6, units="in",res=150)
  plot(dv.priority.rast, main="CECS Drought Vulnerability index 10,000 or greater")
  dev.off()
  png("PriorityLayerImages/FlameLength.png",width=5.5,height=6, units="in",res=150)
  plot(fl.priority.rast, main="CECS Flame Length (FLAMMAP), 8 ft or greater")
  dev.off()
  png("PriorityLayerImages/CriticalHabitat.png",width=5.5,height=6, units="in",res=150)
  plot(cr.priority.rast, main="CDFW ACE Species Diversity rating 4 or 5")
  dev.off()
  png("PriorityLayerImages/Hydropower.png",width=5.5,height=6, units="in",res=150)
  plot(hy.priority.rast, main="Bales & Guo Watersheds feeding powerhouses\n30 KW and greater")
  dev.off()
  png("PriorityLayerImages/DebrisFlow.png",width=5.5,height=6, units="in",res=150)
  plot(de.priority.rast, main="CGS Debris Flow likelihood top 20% of risk")
  dev.off()
  png("PriorityLayerImages/ShrubRoad.png",width=5.5,height=6, units="in",res=150)
  plot(sh.priority.rast, main="CALFIRE WHR Shrub type within 500 ft of CALTRANS roads")
  dev.off()

  png("MaskImages/Forest.png",width=5.5,height=6, units="in",res=150)
  plot(forest.whp.rast, main="CALFIRE WHR Forest (WHP crs)")
  dev.off()
  png("MaskImages/Shrub.png",width=5.5,height=6, units="in",res=150)
  plot(shrub.whp.rast, main="CALFIRE WHR Shrub (WHP crs)")
  dev.off()
  png("MaskImages/Wildland.png",width=5.5,height=6, units="in",res=150)
  plot(wild.whp.rast, main="Wildland: CALFIRE 'Other' + CECS 'Other' (WHP crs)")
  dev.off()
  png("MaskImages/WUI.png",width=5.5,height=6, units="in",res=150)
  plot(wui.whp.rast, main="CALFIRE WUI footprint (interface,intermix, influence, WHP crs)")
  dev.off()
  png("MaskImages/RoadTransmissionLine.png",width=5.5,height=6, units="in",res=150)
  plot(rdtr.buff.whp.vect, col="blue", main="OSM roads + Utility Transmission lines (WHP crs)")
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
  current.conditions[3,]<-c("Forest Health","Drought Vulnerability", "CECS","Vulnerability Index >10,000",
        as.numeric(global(temp,"sum")*30*30*0.000247105))
  current.conditions[4,]<-c("Shrubland Health","Road Proximity","CALFIRE WHR & OSM Roads","500 ft road buffer in shrublands", 
        as.numeric(global(sh.priority.rast,"sum")*30*30*0.000247105))
  current.conditions[5,]<-c("Water","Hydropower","Bales & Guo","Watersheds w/powerhouses >= 30KW",
        as.numeric(global(hy.priority.rast,"sum")*30*30*0.000247105))
  temp<-de.priority.rast
  temp[is.na(temp)]<-0
  current.conditions[6,]<-c("Water","CGS","Debris Flow Potential","Watersheds with risk in the top 40%", 
        as.numeric(global(temp,"sum")*30*30*0.000247105))
  temp<-cr.priority.rast
  temp[is.na(temp)]<-0
  current.conditions[7,]<-c("Habitat","Critical Habitat","CDFW ACE","Species Diversity Category 4 or 5",
        as.numeric(global(temp,"sum")*30*30*0.000247105))

  write.csv(current.conditions,paste("CurrentConditions",datestamp,".csv",sep=""))

  mask.areas<-data.frame(Mask=character(),CRS=character(),Area=numeric(),stringsAsFactors=FALSE)
  mask.areas[1,]<-c("Forest","CECS",as.numeric(global(forest.cecs.rast,"notNA")*30*30*0.000247105))
  mask.areas[2,]<-c("Shrub","CECS",as.numeric(global(shrub.cecs.rast,"notNA")*30*30*0.000247105))
  mask.areas[3,]<-c("WUI","CECS",as.numeric(global(wui.cecs.rast,"notNA")*30*30*0.000247105))
  mask.areas[4,]<-c("Wildland","CECS",as.numeric(global(wild.cecs.rast,"notNA")*30*30*0.000247105))
  mask.areas[5,]<-c("Utility/Road","CECS",expanse(rdtr.buff.cecs.vect)*0.000247105)
  mask.areas[6,]<-c("Forest","WHP",as.numeric(global(forest.whp.rast,"notNA")*30*30*0.000247105))
  mask.areas[7,]<-c("Shrub","WHP",as.numeric(global(shrub.whp.rast,"notNA")*30*30*0.000247105))
  mask.areas[8,]<-c("WUI","WHP",as.numeric(global(wui.whp.rast,"notNA")*30*30*0.000247105))
  mask.areas[9,]<-c("Wildland","WHP",as.numeric(global(wild.whp.rast,"notNA")*30*30*0.000247105))
  mask.areas[10,]<-c("Utility/Road","WHP",expanse(rdtr.buff.whp.vect)*0.000247105) #these seem wildly off
  #mask.areas[11,]<-c("FRAP Other","CECS",) #do these when you get a chance to rerun the make veg masks code
  #mask.areas[12,]<-c("FRAP Other","WHP",) #do these when you get a chance to rerun the make veg masks code

  write.csv(mask.areas,paste("MaskAreas_",datestamp,".csv",sep=""))
}

####################################################################
# PREP BOUNDARY LAYERS (Statewide and Task Force Regions)        ###
####################################################################


#loop through all california, and the four regions
# boundary.shape<-c(paste(loc.scripts,"ReferenceFiles/CA_State.shp",sep=""),
#                   paste(loc.scripts,"ReferenceFiles/Region_Sierra.shp",sep=""),
#                   paste(loc.scripts,"ReferenceFiles/Region_NorthernCA.shp",sep=""),
#                   paste(loc.scripts,"ReferenceFiles/Region_SouthernCA.shp",sep=""),
#                   paste(loc.scripts,"ReferenceFiles/Region_CentralCoast.shp",sep=""))
# boundary.name<-c("CA","Sierra","North","South","Central")

boundary.shape<-c(paste(loc.scripts,"ReferenceFiles/CA_State.shp",sep=""))
boundary.name<-c("CA")


#read in patch layer in order to then clip as needed in the loop
patch.name<-c("Treatments")
patch.shape<-c(paste(loc.data,"ITS_2025Jul25_Polygons/appended.gdb",sep=""))
patch.layer<-c("appended_poly")
#just leave ref.rast out and it won't reproject it.
#for treatment vectors, we'll be projecting them per dataset

#treat.vect<-read.and.check.crs.patch.vector(patch.shape[1],patch.name[1],patch.layer[1]) 
#with no reprojection, it's in whp
read.time<-system.time(treat.vect<-read.and.check.crs.patch.vector(patch.shape[1],patch.name[1],patch.layer[1],whp.rast))
print(paste("Time to read in treatments: ",read.time[[1]]/60, sep=""))


#######################################################
######  start loop through boundary layers ############
#######################################################

#set up the data frame for the outputs
targeted.effort.results<-data.frame(Boundary=character(),PolicyTarget=character(),Metric=character(),
                                    PriorityArea=numeric(),TotalTreatmentArea=numeric(),
                                    ProportionOfTreatments=numeric(),stringsAsFactors=FALSE)

count<-1
for(i in 1:length(boundary.name)){

    prepped.boundary.whp.vect<-read.and.prepare.boundary.vector(boundary.shape[i],boundary.name[i],whp.rast)

    ####################################################################
    # PREP TREATMENT DATASET FOR BOUNDARY LAYER                        #
    ####################################################################

    #remember for CECS layers to reproject the treatment vectors
    crop.time<-system.time(treat.prep.vect<-crop.vector.by.boundary.and.recalc.area(prepped.boundary.whp.vect,boundary.name[i],treat.vect,patch.name[1]))
    print(paste("Time to crop and recalc areas of treatments: ",crop.time[[1]]/60, sep=""))
    start.year<-"2020"
    end.year<-"2024"

    #water year
    start<-paste(start.year,"-09-30",sep="")
    end<-paste(end.year,"-10-01",sep="")

    calculate.areas<-FALSE

    if(calculate.areas){
       #getting the different areas in ha of the different policy targets
       areas<-list()
       perims<-list()
       treat.filt.vect<-list()
       for(i in 1:length(activity.list)){
         temp<-filter.patches(treat.prep.vect,names(activity.list)[i],start,NA)
         treat.filt.vect[[names(activity.list)[i]]]<-aggregate(temp) #aggregated here
          #convert to acres
         areas[[names(activity.list)[i]]]<-expanse(treat.filt.vect[[names(activity.list)[i]]],unit="ha")*2.47105
         # perims[[names(activity.list)[i]]]<-perim(treat.filt.vect[[names(activity.list)[i]]])#in m
       }
      treat.areas<-as.data.frame(unlist(areas))
      treat.areas$PolicyTarget<-rownames(treat.areas)
      names(treat.areas)<-c("Area_ac","PolicyTarget")
      rownames(treat.areas)<-NULL
      write.csv(treat.areas,paste("TreatmentAreasByPolicyTarget_",start,"_",end,"_",datestamp,".csv",sep=""))
      # treat.perims<-as.data.frame(unlist(perims))
      # treat.perims$PolicyTarget<-rownames(treat.perims)
      # names(treat.perims)<-c("Perim_m","PolicyTarget")
      # rownames(treat.perims)<-NULL
      # write.csv(treat.perims,paste("TreatmentPerimsByPolicyTarget_",start,"_",end,"_",datestamp,".csv",sep=""))

    }


    ####################################################################
    ############# CROSSTAB CALCULATIONS      ###########################
    ####################################################################


    #---------------- High-Risk Shrubs -------------------------#

    policy.target<-"Shrubland Health"
    metric.name<-"ShrubVulnerability"
    #subset treatments by policy objective
    filter.time<-system.time(treat.subs.vect<-filter.patches(treat.prep.vect,policy.target,start,NA))
    print(paste("Time to filter treatments: ",filter.time[[1]]/60, sep=""))
    #check CRS match with WHP projection
    treat.proj.vect<-check.crs.match(whp.rast,treat.subs.vect,"near")
    #rasterize treatment layer first
    rasterize.time<-system.time(treat.rast<-rasterize(treat.proj.vect,shrub.whp.rast)) #just use shrub extent to start with
    print(paste("Time to rasterize treatments: ",rasterize.time[[1]]/60, sep=""))


    #then stratify as necessary - subset treatments for shrub only (spatial subset)
    treat.strat.rast<-treat.rast*shrub.whp.rast #using the WHP projection version
        
    #Do crosstab
    crosstb.result<-crosstab.calc(sh.priority.rast, metric.name,treat.strat.rast,policy.target , boundary.name[i])
    targeted.effort.results[count,]<-c(boundary.name[i],policy.target,metric.name,crosstb.result)
    write.csv(targeted.effort.results,paste("TargetedEffortResults_",datestamp,".csv",sep=""))
    count<-count+1

    #---------------- Drought Vulnerability, forest -------------------------#

    policy.target<-"Forest Health"
    metric.name<-"DroughtVulnerability"
    #subset treatments by policy objective
    filter.time<-system.time(treat.subs.vect<-filter.patches(treat.prep.vect,policy.target,start,NA))
    print(paste("Time to filter treatments: ",filter.time[[1]]/60, sep=""))
    #check CRS match with CECS projection
    treat.proj.vect<-check.crs.match(cecs.rast,treat.subs.vect,"near")
    #rasterize treatment layer first
    rasterize.time<-system.time(treat.rast<-rasterize(treat.proj.vect,cecs.rast))
    print(paste("Time to rasterize treatments: ",rasterize.time[[1]]/60, sep=""))
    #then stratify as necessary - subset treatments for forest only (spatial subset)
    treat.strat.rast<-treat.rast*forest.cecs.rast #using the CECS projection version
    
    #Do crosstab
    crosstab.result<-crosstab.calc(dv.priority.rast, metric.name,treat.strat.rast,policy.target , boundary.name[i])
    targeted.effort.results[count,]<-c(boundary.name[i],policy.target,metric.name,crosstab.result)
    write.csv(targeted.effort.results,paste("TargetedEffortResults_",datestamp,".csv",sep=""))
    count<-count+1

    # #---------------- WHP, Wildland ---------------------------#

    policy.target<-"Wildland Fire Risk"
    metric.name<-"WildfireHazardPotentialWildland"
    #treatment type/activity type filter
    filter.time<-system.time(treat.subs.vect<-filter.patches(treat.prep.vect,policy.target,start,NA))
    print(paste("Time to filter treatments: ",filter.time[[1]]/60, sep=""))
    #check CRS match with WHP projection
    treat.proj.vect<-check.crs.match(whp.rast,treat.subs.vect,"near")
    #rasterize treatment layer first
    rasterize.time<-system.time(treat.rast<-rasterize(treat.proj.vect,whp.rast))
    print(paste("Time to rasterize treatments: ",rasterize.time[[1]]/60, sep=""))
    #then stratify as necessary - subset treatments for wildland only (spatial subset)
    treat.strat.rast<-treat.rast*wild.whp.rast #using the WHP projection version

    #Do crosstab
    crosstab.result<-crosstab.calc(whp.priority.rast, metric.name,treat.strat.rast,policy.target , boundary.name[i])
    targeted.effort.results[count,]<-c(boundary.name[i],policy.target,metric.name,crosstab.result)
    write.csv(targeted.effort.results,paste("TargetedEffortResults_",datestamp,".csv",sep=""))
    count<-count+1

    #-----------------FLAME LENGTH, WILDLAND--------------#
    policy.target<-"Wildland Fire Risk"
    metric.name<-"FlameLengthAbove8FtWildland"
    #treatment type/activity type filter
    filter.time<-system.time(treat.subs.vect<-filter.patches(treat.prep.vect,policy.target,start,NA))
    print(paste("Time to filter treatments: ",filter.time[[1]]/60, sep=""))
    #check CRS match with CECS projection 
    treat.proj.vect<-check.crs.match(cecs.rast,treat.subs.vect,"near")
    #rasterize treatment layer first
    rasterize.time<-system.time(treat.rast<-rasterize(treat.proj.vect,cecs.rast))
    print(paste("Time to rasterize treatments: ",rasterize.time[[1]]/60, sep=""))
    #then stratify as necessary - subset treatments for wildland only (spatial subset)
    treat.strat.rast<-treat.rast*wild.cecs.rast #using the CECS projection version

    #Do crosstab
    crosstab.result<-crosstab.calc(fl.priority.rast, metric.name,treat.strat.rast,policy.target , boundary.name[i])
    targeted.effort.results[count,]<-c(boundary.name[i],policy.target,metric.name,crosstab.result)
    write.csv(targeted.effort.results,paste("TargetedEffortResults_",datestamp,".csv",sep=""))
    count<-count+1

 
    #---------------- WHP, WUI ---------------------------#

    policy.target<-"WUI Fire Risk"
    metric.name<-"WildfireHazardPotentialWUI"
    #treatment type/activity type filter
    filter.time<-system.time(treat.subs.vect<-filter.patches(treat.prep.vect,policy.target,start,NA))
    print(paste("Time to filter treatments: ",filter.time[[1]]/60, sep=""))
    #check CRS match with WHP projection
    treat.proj.vect<-check.crs.match(whp.rast,treat.subs.vect,"near")
    #rasterize treatment layer first
    rasterize.time<-system.time(treat.rast<-rasterize(treat.proj.vect,whp.rast))
    print(paste("Time to rasterize treatments: ",rasterize.time[[1]]/60, sep=""))
    #then stratify as necessary - subset treatments for wui only (spatial subset)
    treat.strat.rast<-treat.rast*wui.whp.rast #using the WHP projection version

    #Do crosstab
    crosstab.result<-crosstab.calc(whp.priority.rast, metric.name,treat.strat.rast,policy.target , boundary.name[i])
    targeted.effort.results[count,]<-c(boundary.name[i],policy.target,metric.name,crosstab.result)
    write.csv(targeted.effort.results,paste("TargetedEffortResults_",datestamp,".csv",sep=""))
    count<-count+1

    #-----------------FLAME LENGTH, WUI--------------#
    policy.target<-"WUI Fire Risk"
    metric.name<-"FlameLengthAbove8FtWUI"
    #treatment type/activity type filter
    filter.time<-system.time(treat.subs.vect<-filter.patches(treat.prep.vect,policy.target,start,NA))
    print(paste("Time to filter treatments: ",filter.time[[1]]/60, sep=""))
    #check CRS match with CECS projection
    treat.proj.vect<-check.crs.match(cecs.rast,treat.subs.vect,"near")
    #rasterize treatment layer first
    rasterize.time<-system.time(treat.rast<-rasterize(treat.proj.vect,cecs.rast))
    print(paste("Time to rasterize treatments: ",rasterize.time[[1]]/60, sep=""))
    #then stratify as necessary - subset treatments for wui only (spatial subset)
    treat.strat.rast<-treat.rast*wui.cecs.rast #using the CECS projection version

    #Do crosstab
    crosstab.result<-crosstab.calc(fl.priority.rast, metric.name,treat.strat.rast,policy.target , boundary.name[i])
    targeted.effort.results[count,]<-c(boundary.name[i],policy.target,metric.name,crosstab.result)
    write.csv(targeted.effort.results,paste("TargetedEffortResults_",datestamp,".csv",sep=""))
    count<-count+1


    #---------------- WHP, Utility corridors ---------------------------#

    policy.target<-"Fire Risk in Utility Corridors"
    metric.name<-"WildfireHazardPotentialUtility"
    #treatment type/activity type filter
    filter.time<-system.time(treat.subs.vect<-filter.patches(treat.prep.vect,policy.target,start,NA))
    print(paste("Time to filter treatments: ",filter.time[[1]]/60, sep=""))
    #check CRS match with WHP projection
    treat.proj.vect<-check.crs.match(whp.rast,treat.subs.vect,"near")
    #subset by utility corridors
    treat.strat.vect<-intersect(treat.proj.vect,rdtr.buff.whp.vect) #use the whp layer version
    #rasterize treatment layer
    rasterize.time<-system.time(treat.strat.rast<-rasterize(treat.strat.vect,whp.rast))
    print(paste("Time to rasterize treatments: ",rasterize.time[[1]]/60, sep=""))
  
    #Do crosstab
    crosstab.result<-crosstab.calc(whp.priority.rast, metric.name,treat.strat.rast,policy.target , boundary.name[i])
    targeted.effort.results[count,]<-c(boundary.name[i],policy.target,metric.name,crosstab.result)
    write.csv(targeted.effort.results,paste("TargetedEffortResults_",datestamp,".csv",sep=""))
    count<-count+1

    #-----------------FLAME LENGTH, Utility corridors --------------#
    policy.target<-"Fire Risk in Utility Corridors"
    metric.name<-"FlameLengthAbove8FtUtilities"
    #treatment type/activity type filter
    filter.time<-system.time(treat.subs.vect<-filter.patches(treat.prep.vect,policy.target,start,NA))
    print(paste("Time to filter treatments: ",filter.time[[1]]/60, sep=""))
    #check CRS match with CECS projection
    treat.proj.vect<-check.crs.match(cecs.rast,treat.subs.vect,"near")
    rdtr.buff.proj.cecs.vect<-check.crs.match(cecs.rast,rdtr.buff.cecs.vect,"near")
    #subset by utility corridors
    treat.strat.vect<-intersect(treat.proj.vect,rdtr.buff.proj.cecs.vect) #use the CECS layer version
    #rasterize treatment layer 
    rasterize.time<-system.time(treat.strat.rast<-rasterize(treat.strat.vect,cecs.rast))
    print(paste("Time to rasterize treatments: ",rasterize.time[[1]]/60, sep=""))

    #Do crosstab
    crosstab.result<-crosstab.calc(fl.priority.rast, metric.name,treat.strat.rast,policy.target , boundary.name[i])
    targeted.effort.results[count,]<-c(boundary.name[i],policy.target,metric.name,crosstab.result)
    write.csv(targeted.effort.results,paste("TargetedEffortResults_",datestamp,".csv",sep=""))
    count<-count+1

    #---------------- Critical Habitat -------------------------#

    policy.target<-"Habitat"
    metric.name<-"CriticalHabitat"
    #subset treatments by policy objective
    filter.time<-system.time(treat.subs.vect<-filter.patches(treat.prep.vect,policy.target,start,NA))
    print(paste("Time to filter treatments: ",filter.time[[1]]/60, sep=""))
    #rasterize treatment layer
    rasterize.time<-system.time(treat.strat.rast<-rasterize(treat.subs.vect,whp.rast))
    print(paste("Time to rasterize treatments: ",rasterize.time[[1]]/60, sep=""))
    
    #Do crosstab
    crosstab.result<-crosstab.calc(cr.priority.rast, metric.name,treat.strat.rast,policy.target , boundary.name[i])
    targeted.effort.results[count,]<-c(boundary.name[i],policy.target,metric.name,crosstab.result)
    write.csv(targeted.effort.results,paste("TargetedEffortResults_",datestamp,".csv",sep=""))
    count<-count+1

    #---------------- Hydropower -------------------------#

    policy.target<-"Water"
    metric.name<-"Hydropower"
    #subset treatments by policy objective
    filter.time<-system.time(treat.subs.vect<-filter.patches(treat.prep.vect,policy.target,start,NA))
    print(paste("Time to filter treatments: ",filter.time[[1]]/60, sep=""))
    #rasterize treatment layer
    rasterize.time<-system.time(treat.strat.rast<-rasterize(treat.subs.vect,whp.rast))
    print(paste("Time to rasterize treatments: ",rasterize.time[[1]]/60, sep=""))
    
    #Do crosstab
    crosstab.result<-crosstab.calc(hy.priority.rast, metric.name,treat.strat.rast,policy.target , boundary.name[i])
    targeted.effort.results[count,]<-c(boundary.name[i],policy.target,metric.name,crosstab.result)
    write.csv(targeted.effort.results,paste("TargetedEffortResults_",datestamp,".csv",sep=""))
    count<-count+1

    #---------------- Debris Flow Risk -------------------------#

    policy.target<-"Water"
    metric.name<-"DebrisFlow"
    #subset treatments by policy objective
    filter.time<-system.time(treat.subs.vect<-filter.patches(treat.prep.vect,policy.target,start,NA))
    print(paste("Time to filter treatments: ",filter.time[[1]]/60, sep=""))
    #rasterize treatment layer
    rasterize.time<-system.time(treat.strat.rast<-rasterize(treat.subs.vect,whp.rast))
    print(paste("Time to rasterize treatments: ",rasterize.time[[1]]/60, sep=""))
    
    #Do crosstab
    crosstab.result<-crosstab.calc(de.priority.rast, metric.name,treat.strat.rast,policy.target , boundary.name[i])
    targeted.effort.results[count,]<-c(boundary.name[i],policy.target,metric.name,crosstab.result)
    write.csv(targeted.effort.results,paste("TargetedEffortResults_",datestamp,".csv",sep=""))
    count<-count+1

    #---------------- acres of fuel breaks -------------------------#
      ### Manually, aspatially calculate?

}

timer.end<-Sys.time()

time.total<-timer.end-timer.start
print(time.total)

