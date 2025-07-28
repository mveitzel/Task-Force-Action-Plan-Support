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
reference.rast<-rast(paste(loc.data,"CECS_Data/CECS_CAWide_Vulner_TreeDieoff_SPI-2_2020_V250418.tif",sep=""))


#will loop through all california, and the four regions
boundary.shape<-c(paste(loc.scripts,"ReferenceFiles/CA_State.shp",sep=""),
                  paste(loc.scripts,"ReferenceFiles/Region_Sierra.shp",sep=""),
                  paste(loc.scripts,"ReferenceFiles/Region_NorthernCA.shp",sep=""),
                  paste(loc.scripts,"ReferenceFiles/Region_SouthernCA.shp",sep=""),
                  paste(loc.scripts,"ReferenceFiles/Region_CentralCoast.shp",sep=""))
boundary.name<-c("CA","Sierra","North","South","Central")

#start loop here

targeted.effort.results<-data.frame(Boundary=character(),PolicyTarget=character(),ProportionOfTreatments=numeric(),stringsAsFactors=FALSE)

for(i in 1:length(boundary.name)){

    prepped.boundary.vect<-read.and.prepare.boundary.vector(boundary.shape[i],boundary.name[i],reference.rast)

    ####################################################################
    # PREP TREATMENT DATASET                                           #
    ####################################################################

    patch.name<-c("Treatments")
    patch.shape<-c(paste(loc.data,"ITS_2025Jul25_Polygons/appended.gdb",sep=""))
    patch.layer<-c("appended_poly")

    treat.vect<-read.and.check.crs.patch.vector(patch.shape[1],patch.name[1],patch.layer[1],prepped.boundary.vect)
    treat.prep.vect<-crop.vector.by.boundary.and.recalc.area(prepped.boundary.vect,boundary.name[i],treat.vect,patch.name[1])

    start.year<-"2020"
    end.year<-"2024"

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
    # PREP STRATIFICATION LAYERS (WUI/Wildland and Ecosystem Type)   ###
    ####################################################################

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

    }


    ## add ecosystem layers here

    #TODO*** need to add this in

    ####################################################################
    ############# PRIORITY LAYERS CALCS      ###########################
    ####################################################################

    #reading in each layer that will indicate high priority areas
    #needs to be separately scripted because each one has a slightly different way to be 
    #thresholded or recoded in order to do the crosstab

    # #---------------- WHP, Wildland ---------------------------#

    # policy.target<-"Wildland Fire Risk"
    # #treatment type/activity type filter
    # treat.subs.vect<-filter.patches(treat.prep.vect,policy.target,start,NA)

    # #subset treatments for wildland only (spatial subset)

    # treat.rast<-rasterize(treat.strat.vect,reference.rast)

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

    #           prep treatment layer
    policy.target<-"Forest Health"
    #subset treatments by policy objective
    treat.subs.vect<-filter.patches(treat.prep.vect,policy.target,start,NA)
    #rasterize treatment layer first
    treat.rast<-rasterize(treat.subs.vect,reference.rast)
    #then stratify as necessary - for wildland, first, then for forest only
    treat.strat.rast<-treat.rast*wild.rast
    #treat.strat.rast<-treat.strat.rast*forest.rast

    #           read in the priority layer source
    # CECS drought vulnerability, in Oct 2020 before treatments started in January
    dv.rast<-rast("D:\\GIS_Large_Files\\CECS_Data\\CECS_CAWide_Vulner_TreeDieoff_SPI-2_2020_V250418.tif")
    #check CRS
    dv.proj.rast<-check.crs.match(dv.rast,reference.rast)

    ##          Set any thresholds, add any buffers, etc

    # the layer was vetted using the 2012-2014 drought, trees that did die then had a drought
    # vulnerability higher than 10,000
    dv.priority.rast<-dv.proj.rast> 10000 


    #Do crosstab

    targeted.effort.results[i,]<-c(boundary.name[i],policy.target,
      crosstab.calc(dv.priority.rast, "DroughtVulnerability",treat.strat.rast,policy.target , boundary.name[i])
      )
    write.csv(targeted.effort.results,"TargetedEffortResults.csv",append=TRUE)

    #---------------- Powerlines, Roads -------------------------#

#buffering by 500 ft on either side


    #---------------- Critical Habitat -------------------------#

#buffering by 1.5 miles (as in 'influence' WUI calc)

    #---------------- Hydropower -------------------------#



    #---------------- Debris Flow Risk -------------------------#

#thresholding by 50% to match underlying modeling assumptions

    #---------------- High-Risk Shrubs -------------------------#

#still need to get this, buffer with 1.5 miles?


    #---------------- acres of fuel breaks -------------------------#
    ### Manually, aspatially calculate?

}

