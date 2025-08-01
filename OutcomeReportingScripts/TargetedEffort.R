####################################################################
### Set locations and source functions and other inputs      #######
####################################################################

timer.start<-Sys.time()

datestamp<-"2025Jul31"

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
#we use a CECS layer as a reference raster for spatial reference of CRS and extent
reference.rast<-rast(paste(loc.data,"CECS_Data/CECS_CAWide_Vulner_TreeDieoff_SPI-2_2020_V250614.tif",sep=""))
#make sure this is here because it has a different CRS we need to work from as well
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
  source(paste(loc.scripts,"FunctionLibraries/CalculateWUI_Veg_Masks.R",sep=""))
} else {
  #read in the raster files
  wui.rast<-rast(paste(loc.scripts,"ReferenceFiles/FRAP24_WUIOnly.tif",sep=""))
  wild.rast<-rast(paste(loc.scripts,"ReferenceFiles/FRAP24_WildlandOnly.tif",sep=""))
  #there is a problem with the projections for the CALFIRE products
  #Probably a datum change that isn't being applied correctly
  #So WHP layers will be used as the reference rather than CECS
  #which means the "WildlandOnly" mask (which used the FRAP WUI layer 
  #and also one of the CECS layers) needs to be reprojected back
  #into the coordinate system of the WHP.  This will get done in the same
  #script as the other wildland/wui masks, which shouldn't need to be done
  #very frequently.
  #the net result is that there are two wildland and wui masks in each projection to read in
  #the WHP-compatible one probably won't get used much by the whole group, so keeping those
  #files locally rather than in the GitHub repo.  if it needs to be recalculated, use recalculate.masks<-TRUE
  wild.proj.rast<-rast(paste(loc.data,"WUIVegetationClassifications/FRAP24_WildlandOnly_WHPproj.tif",sep=""))
  wui.proj.rast<-rast(paste(loc.data,"WUIVegetationClassifications/FRAP24_WUIOnly_WHPproj.tif",sep=""))

  #read in the veg classification masks (Forest and Shrub from CALFIRE WHR reclassifications)
  forest.vect<-vect(paste(loc.data,"WUIVegetationClassifications/WHR13_RECLASS_FOREST.shp",sep=""))
  shrub.vect<-vect(paste(loc.data,"WUIVegetationClassifications/WHR13_RECLASS_SHRUB.shp",sep=""))

  #read in the road and road+transmission line buffers
  rdtr.buff.vect<-vect(paste(loc.data,"WUIVegetationClassifications/RoadTransmissionLineBuffer_CECSproj.shp",sep=""))
  rdtr.buff.proj.vect<-vect(paste(loc.data,"WUIVegetationClassifications/RoadTransmissionLineBuffer_WHPproj.shp",sep=""))
  road.buff.vect<-vect(paste(loc.data,"WUIVegetationClassifications/RoadBuffer_CECSproj.shp",sep=""))
  road.buff.proj.vect<-vect(paste(loc.data,"WUIVegetationClassifications/RoadBuffer_WHPproj.shp",sep=""))
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
  source(paste(loc.scripts,"FunctionLibraries/CalculatePriorityLayers.R",sep=""))
}else{
  #read in rasters
  whp.priority.rast<-rast(paste(loc.data,"PriorityLayers/FinalPriorityLayers/WHPpriority_CALFIREproj.tif",sep=""))
  dv.priority.rast<-rast(paste(loc.data,"PriorityLayers/FinalPriorityLayers/DroughtVulnerabilityPriority_CECSproj.tif",sep=""))
  fl.priority.rast<-rast(paste(loc.data,"PriorityLayers/FinalPriorityLayers/FlameLengthPriority_CECSproj.tif",sep=""))
  cr.pri.rast<-rast(paste(loc.data,"PriorityLayers/FinalPriorityLayers/CriticalHabitatPriority_CECSproj.tif",sep=""))
  hy.pri.rast<-rast(paste(loc.data,"PriorityLayers/FinalPriorityLayers/HydropowerPriority_CECSproj.tif",sep=""))
  de.pri.rast<-rast(paste(loc.data,"PriorityLayers/FinalPriorityLayers/DebrisFlowPriority_CECSproj.tif",sep=""))
  sh.pri.rast<-rast(paste(loc.data,"PriorityLayers/FinalPriorityLayers/AtRiskShrubsPriority_CECSproj.tif",sep=""))
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
treat.vect<-read.and.check.crs.patch.vector(patch.shape[1],patch.name[1],patch.layer[1],reference.rast)


#######################################################
######  start loop through boundary layers ############
#######################################################

#set up the data frame for the outputs
targeted.effort.results<-data.frame(Boundary=character(),PolicyTarget=character(),Metric=character(),
                                    PriorityArea=numeric(),TotalTreatmentArea=numeric(),
                                    ProportionOfTreatments=numeric(),stringsAsFactors=FALSE)

for(i in 1:length(boundary.name)){

    prepped.boundary.vect<-read.and.prepare.boundary.vector(boundary.shape[i],boundary.name[i],reference.rast)

    ####################################################################
    # PREP TREATMENT DATASET FOR BOUNDARY LAYER                        #
    ####################################################################

    treat.prep.vect<-crop.vector.by.boundary.and.recalc.area(prepped.boundary.vect,boundary.name[i],treat.vect,patch.name[1])
    count<-1

    start.year<-"2020"
    end.year<-"2024"

    #water year
    start<-paste(start.year,"-09-30",sep="")
    end<-paste(end.year,"-10-01",sep="")

    calculate.areas<-FALSE

    if(calculate.areas){
       #getting the different areas in ha of the different policy targets
       areas<-list()
       treat.filt.vect<-list()
       for(i in 1:length(activity.list)){
         temp<-filter.patches(treat.prep.vect,names(activity.list)[i],start,NA)
         treat.filt.vect[[names(activity.list)[i]]]<-aggregate(temp)
          #convert to acres
         areas[[names(activity.list)[i]]]<-expanse(treat.filt.vect[[names(activity.list)[i]]],unit="ha")*2.47105
       }
      treat.areas<-as.data.frame(unlist(areas))
      treat.areas$PolicyTarget<-rownames(treat.areas)
      names(treat.areas)<-c("Area_ac","PolicyTarget")
      rownames(treat.areas)<-NULL
      write.csv(treat.areas,paste("TreatmentAreasByPolicyTarget_",start,"_",end,".csv",sep=""))
    }


    ####################################################################
    ############# CROSSTAB CALCULATIONS      ###########################
    ####################################################################

    # #---------------- WHP, Wildland ---------------------------#

    policy.target<-"Wildland Fire Risk"
    metric.name<-"WildfireHazardPotentialWildland"
    #treatment type/activity type filter
    treat.subs.vect<-filter.patches(treat.prep.vect,policy.target,start,NA)
    #check CRS match with whp projection (only for layers using WHP layer)
    treat.proj.vect<-check.crs.match(whp.rast,treat.subs.vect)
    #rasterize treatment layer first
    treat.rast<-rasterize(treat.proj.vect,whp.rast)
    #then stratify as necessary - subset treatments for wildland only (spatial subset)
    treat.strat.rast<-treat.rast*wild.proj.rast #using the WHP projection version

    #Do crosstab
    targeted.effort.results[count,]<-c(boundary.name[i],policy.target,metric.name,
      crosstab.calc(whp.priority.rast, metric.name,treat.strat.rast,policy.target , boundary.name[i]))
    write.csv(targeted.effort.results,paste("TargetedEffortResults_",datestamp,".csv",sep=""),append=TRUE)
    count<-count+1

    #-----------------FLAME LENGTH, WILDLAND--------------#
    policy.target<-"Wildland Fire Risk"
    metric.name<-
    #treatment type/activity type filter
    treat.subs.vect<-filter.patches(treat.prep.vect,policy.target,start,NA)
    #check CRS match with whp projection (only for layers using WHP layer)
    treat.proj.vect<-check.crs.match(reference.rast,treat.subs.vect)
    #rasterize treatment layer first
    treat.rast<-rasterize(treat.proj.vect,reference.rast)
    #then stratify as necessary - subset treatments for wui only (spatial subset)
    treat.strat.rast<-treat.rast*wild.rast #using the CECS projection version

    #Do crosstab
    targeted.effort.results[count,]<-c(boundary.name[i],policy.target,metric.name,
      crosstab.calc(fl.priority.rast, metric.name,treat.strat.rast,policy.target , boundary.name[i]))
    write.csv(targeted.effort.results,paste("TargetedEffortResults_",datestamp,".csv",sep=""),append=TRUE)
    count<-count+1

 
    #---------------- WHP, WUI ---------------------------#

    policy.target<-"WUI Fire Risk"
    metric.name<-"WildfireHazardPotentialWUI"
    #treatment type/activity type filter
    treat.subs.vect<-filter.patches(treat.prep.vect,policy.target,start,NA)
    #check CRS match with whp projection (only for layers using WHP layer)
    treat.proj.vect<-check.crs.match(whp.rast,treat.subs.vect)
    #rasterize treatment layer first
    treat.rast<-rasterize(treat.proj.vect,whp.rast)
    #then stratify as necessary - subset treatments for wui only (spatial subset)
    treat.strat.rast<-treat.rast*wui.proj.rast #using the WHP projection version

    #Do crosstab
    targeted.effort.results[count,]<-c(boundary.name[i],policy.target,metric.name
      crosstab.calc(whp.priority.rast, metric.name,treat.strat.rast,policy.target , boundary.name[i]))
    write.csv(targeted.effort.results,paste("TargetedEffortResults_",datestamp,".csv",sep=""),append=TRUE)
    count<-count+1

    #-----------------FLAME LENGTH, WUI--------------#
    policy.target<-"WUI Fire Risk"
    metric.name<-"FlameLengthAbove8FtWUI"
    #treatment type/activity type filter
    treat.subs.vect<-filter.patches(treat.prep.vect,policy.target,start,NA)
    #check CRS match with whp projection (only for layers using WHP layer)
    treat.proj.vect<-check.crs.match(reference.rast,treat.subs.vect)
    #rasterize treatment layer first
    treat.rast<-rasterize(treat.proj.vect,reference.rast)
    #then stratify as necessary - subset treatments for wui only (spatial subset)
    treat.strat.rast<-treat.rast*wui.rast #using the CECS projection version

    #Do crosstab
    targeted.effort.results[count,]<-c(boundary.name[i],policy.target,,metric.name,
      crosstab.calc(fl.priority.rast, metric.name,treat.strat.rast,policy.target , boundary.name[i]))
    write.csv(targeted.effort.results,paste("TargetedEffortResults_",datestamp,".csv",sep=""),append=TRUE)
    count<-count+1

    #---------------- Drought Vulnerability, forest -------------------------#

#currently the forest mask is giving me problems

    policy.target<-"Forest Health"
    metric.name<-"DroughtVulnerability"
    #subset treatments by policy objective
    treat.subs.vect<-filter.patches(treat.prep.vect,policy.target,start,NA)
    #use vector mask for forest before rasterizing
    forest.CECSproj.vect<-check.crs.match(treat.subs.vect,forest.vect)
    treat.strat.vect<-intersect(treat.subs.vect,forest.CECSproj.vect)
    #rasterize treatment layer first
    treat.strat.rast<-rasterize(treat.strat.vect,reference.rast)
    
    #Do crosstab
    targeted.effort.results[count,]<-c(boundary.name[i],policy.target,metric.name,
      crosstab.calc(dv.priority.rast, metric.name,treat.strat.rast,policy.target , boundary.name[i]))
    write.csv(targeted.effort.results,paste("TargetedEffortResults_",datestamp,".csv",sep=""),append=TRUE)
    count<-count+1


    #---------------- WHP, Utility corridors ---------------------------#

    policy.target<-"Fire Risk in Utility Corridors"
    metric.name<-"WildfireHazardPotentialUtility"
    #treatment type/activity type filter
    treat.subs.vect<-filter.patches(treat.prep.vect,policy.target,start,NA)
    #check CRS match with whp projection (only for layers using WHP layer)
    treat.proj.vect<-check.crs.match(whp.rast,treat.subs.vect)
    #subset by utility corridors
    treat.strat.vect<-intersect(treat.proj.vect,rdtr.buff.proj.vect) #use the whp layer version
    #rasterize treatment layer
    treat.strat.rast<-rasterize(treat.strat.vect,whp.rast)
  
    #Do crosstab
    targeted.effort.results[count,]<-c(boundary.name[i],policy.target,metric.name,
      crosstab.calc(whp.priority.rast, metric.name,treat.strat.rast,policy.target , boundary.name[i]))
    write.csv(targeted.effort.results,paste("TargetedEffortResults_",datestamp,".csv",sep=""),append=TRUE)
    count<-count+1

    #-----------------FLAME LENGTH, Utility corridors --------------#
    policy.target<-"Fire Risk in Utility Corridors"
    metric.name<-"FlameLengthAbove8FtUtilities"
    #treatment type/activity type filter
    treat.subs.vect<-filter.patches(treat.prep.vect,policy.target,start,NA)
    #check CRS match with whp projection (only for layers using WHP layer)
    treat.proj.vect<-check.crs.match(reference.rast,treat.subs.vect)
    #subset by utility corridors
    treat.strat.vect<-intersect(treat.proj.vect,rdtr.buff.vect) #use the CECS layer version
    #rasterize treatment layer 
    treat.strat.rast<-rasterize(treat.strat.vect,reference.rast)

    #Do crosstab
    targeted.effort.results[count,]<-c(boundary.name[i],policy.target,metric.name,
      crosstab.calc(fl.priority.rast, metric.name,treat.strat.rast,policy.target , boundary.name[i]))
    write.csv(targeted.effort.results,paste("TargetedEffortResults_",datestamp,".csv",sep=""),append=TRUE)
    count<-count+1

    #---------------- Critical Habitat -------------------------#

    policy.target<-"Habitat"
    metric.name<-"CriticalHabitat"
    #subset treatments by policy objective
    treat.subs.vect<-filter.patches(treat.prep.vect,policy.target,start,NA)
    #rasterize treatment layer
    treat.strat.rast<-rasterize(treat.subs.vect,reference.rast)
    
    #Do crosstab
    targeted.effort.results[count,]<-c(boundary.name[i],policy.target,metric.name,
      crosstab.calc(cr.pri.rast, metric.name,treat.strat.rast,policy.target , boundary.name[i]))
    write.csv(targeted.effort.results,paste("TargetedEffortResults_",datestamp,".csv",sep=""),append=TRUE)
    count<-count+1

    #---------------- Hydropower -------------------------#

    policy.target<-"Water"
    metric.name<-"Hydropower"
    #subset treatments by policy objective
    treat.subs.vect<-filter.patches(treat.prep.vect,policy.target,start,NA)
    #rasterize treatment layer
    treat.strat.rast<-rasterize(treat.subs.vect,reference.rast)
    
    #Do crosstab
    targeted.effort.results[count,]<-c(boundary.name[i],policy.target,metric.name,
      crosstab.calc(hy.pri.rast, metric.name,treat.strat.rast,policy.target , boundary.name[i]))
    write.csv(targeted.effort.results,paste("TargetedEffortResults_",datestamp,".csv",sep=""),append=TRUE)
    count<-count+1

    #---------------- Debris Flow Risk -------------------------#

    policy.target<-"Water"
    metric.name<-"DebrisFlow"
    #subset treatments by policy objective
    treat.subs.vect<-filter.patches(treat.prep.vect,policy.target,start,NA)
    #rasterize treatment layer
    treat.strat.rast<-rasterize(treat.subs.vect,reference.rast)
    
    #Do crosstab
    targeted.effort.results[count,]<-c(boundary.name[i],policy.target,metric.name,
      crosstab.calc(de.pri.rast, metric.name,treat.strat.rast,policy.target , boundary.name[i]))
    write.csv(targeted.effort.results,paste("TargetedEffortResults_",datestamp,".csv",sep=""),append=TRUE)
    count<-count+1

    #---------------- High-Risk Shrubs -------------------------#

    policy.target<-"Shrubland Health"
    metric.name<-"ShrubVulnerability"
    #subset treatments by policy objective
    treat.subs.vect<-filter.patches(treat.prep.vect,policy.target,start,NA)
  
    #use vector mask for shrub before rasterizing
    shrub.CECSproj.vect<-check.crs.match(treat.subs.vect,shrub.vect)
    treat.strat.vect<-intersect(treat.subs.vect,shrub.CECSproj.vect)
    #rasterize treatment layer
    treat.strat.rast<-rasterize(treat.strat.vect,reference.rast)
        
    #Do crosstab
    targeted.effort.results[count,]<-c(boundary.name[i],policy.target,metric.name,
      crosstab.calc(sh.pri.rast, metric.name,treat.strat.rast,policy.target , boundary.name[i]))
    write.csv(targeted.effort.results,paste("TargetedEffortResults_",datestamp,".csv",sep=""),append=TRUE)
    count<-count+1


    #---------------- acres of fuel breaks -------------------------#
      ### Manually, aspatially calculate?

}

timer.end<-Sys.time()

time.total<-timer.end-timer.start
print(time.total)

