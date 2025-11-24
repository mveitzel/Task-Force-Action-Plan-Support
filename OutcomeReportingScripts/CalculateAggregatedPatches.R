#CalculateAggregatedPatches.R

#This script creates the vector files and saves them in an intermediate folder so that it doesn't need to be rerun
# For Targeted Effort, there are 5 policy goals and therefore filters to pass treatments through
# For efficacy, only three.
#there are 5 boundary areas - CA and the four TF regions
#And for Efficacy, we'll do this for the fires too (but just per region, no difference in policy goal)
#For efficacy, we'll also want HUC12 summaries

#use WHP CRS for this, they're all vectors and can be crs checked before running analyses

#loop through all california, and the four regions
 boundary.shape<-c(paste(loc.scripts,"ReferenceFiles/CA_State.shp",sep=""),
                   paste(loc.scripts,"ReferenceFiles/Region_Sierra.shp",sep=""),
                   paste(loc.scripts,"ReferenceFiles/Region_NorthernCA.shp",sep=""),
                   paste(loc.scripts,"ReferenceFiles/Region_SouthernCA.shp",sep=""),
                   paste(loc.scripts,"ReferenceFiles/Region_CentralCoast.shp",sep=""))
 boundary.name<-c("CA","Sierra","North","South","Central")


#policy.target<-c("WildlandFireRisk","ShrublandHealth","ForestHealth","Habitat","Water")
#note that all the Fire Risk policy targets use the same list of activities regardless of the location
# i.e. utilities, roads, landscape, wildland, wui.  So will filter treatments for this once.

#only for canopy/noncanopy
policy.target<-c("Canopy", "NonCanopy")


#enable this if you want to make a csv of the relative areas of each policy target in each boundary shape
calculate.areas<-TRUE

# zsum.shape<-c(paste(loc.scripts,"ReferenceFiles/HUC12.shp",sep=""))
# zsum.name<-c("HUC12")

patch.shape<-c(paste(loc.data,"ITS_2025_V2Nov14_Data/appended.gdb",sep=""),
			   paste(loc.data,"FireFootprints/fire24_1.gdb",sep=""))
patch.layer<-c("appended_poly",
				"firep24_1")

# read.time<-system.time(zonal.summary.area.vect<-read.vector.and.check.crs(whp.rast,zsum.shape,zsum.name))
# print(paste("Time to read zonal summary area: ",round(read.time[[1]]/60)," minute(s)", sep=""))

read.time<-system.time(treat.vect<-read.and.check.crs.patch.vector(patch.shape[1],"Treatments",patch.layer[1],whp.rast))
print(paste("Time to read treatments: ",round(read.time[[1]]/60)," minute(s)", sep=""))
read.time<-system.time(fire.vect<-read.and.check.crs.patch.vector(patch.shape[2],"Fires",patch.layer[2],whp.rast))
print(paste("Time to read fires: ",round(read.time[[1]]/60)," minute(s)", sep=""))



for(i in 1:length(boundary.name)){
	print(boundary.name[i])

    prepped.boundary.vect<-read.and.prepare.boundary.vector(boundary.shape[i],boundary.name[i],whp.rast)
	if(boundary.name[i]=="CA"){
		prepped.boundary.vect$Region<-"AllCA"
	}	

    if(calculate.areas){
       #getting the different areas in ha of the different policy targets
       areas<-list()
       treat.filt.vect<-list()
    }

	# prep.time<-system.time(prepped.zonal.summary.area.vect<-crop.vector.by.boundary.and.recalc.area(prepped.boundary.vect,boundary.name[i],zonal.summary.area.vect,zsum.name))
	# print(paste("Time to prep zonal summary area: ",round(prep.time[[1]]/60)," minute(s)", sep=""))

	#prep fires
	filt.time<-system.time(fire.filt.vect<-filter.fires(fire.vect,start.y,end.y))
	print(paste("Time to filter fires by time range (efficacy): ",round(filt.time[[1]]/60)," minute(s)", sep=""))

	#also intersect and aggregate fires
	agg.time<-system.time(agg.fires.region.vect<-intersect.and.aggregate.vectors(
		prepped.boundary.vect,boundary.name[i],fire.filt.vect,"Fires","Regions","Region",
		prepped.boundary.vect,boundary.name[i]))
	print(paste("Time to aggregate fires by region (efficacy): ",round(agg.time[[1]]/60)," minute(s)", sep=""))
	writeVector(agg.fires.region.vect,paste(loc.data,"IntermediateFiles/AggregatedVectors/Fires_",
		boundary.name[i],"_",start.year,"_",end.year,".shp",sep=""),overwrite=TRUE)

	# #also intersect and aggregate fires at HUC level
	# agg.time<-system.time(agg.fires.huc.vect<-intersect.and.aggregate.vectors(
	# 	prepped.zonal.summary.area.vect,zsum.name,fire.vect,"Fires","HUC12","huc12",
	# 	prepped.boundary.vect,boundary.name[i]))
	# print(paste("Time to aggregate fires by HUC12 (efficacy): ",round(agg.time[[1]]/60)," minute(s)", sep=""))
	# writeVector(agg.fires.huc.vect,paste(loc.data,"IntermediateFiles/AggregatedVectors/Fires_",
	# 	boundary.name[i],"_",start.year,"_",end.year,"_HUC12.shp",sep=""),overwrite=TRUE)


	for(k in 1:length(policy.target)){
	  	print(policy.target[k])

	    filter.time<-system.time(treat.filt.te.vect<-filter.treatments(treat.vect,policy.target[k],start.y,"present"))
	    print(paste("Time to filter treatments (targeted effort): ",round(filter.time[[1]]/60)," minute(s)", sep=""))

	    filter.time<-system.time(treat.filt.ef.vect<-filter.treatments(treat.vect,policy.target[k],start.y,end.y))
	    print(paste("Time to filter treatments (efficacy): ",round(filter.time[[1]]/60)," minute(s)", sep=""))


	  if(calculate.areas) {
         temp<-treat.filt.te.vect
         treat.filt.vect[[policy.target[k] ]]<-aggregate(temp) #aggregated here
          #convert to acres
         areas[[policy.target[k]]]<-expanse(treat.filt.vect[[policy.target[k] ]],unit="ha")*2.47105
       }

		#"Regions/HUC12" is the display name for how patches are being aggregated, and "Region/huc12" is the column name 
		agg.time<-system.time(agg.treatments.region.vect<-intersect.and.aggregate.vectors(
			prepped.boundary.vect,boundary.name[i],treat.filt.te.vect,"Treatments","Regions","Region",
			prepped.boundary.vect,boundary.name[i]))
		print(paste("Time to aggregate treatments by region (targeted effort): ",round(agg.time[[1]]/60)," minute(s)", sep=""))
		writeVector(agg.treatments.region.vect,paste(loc.data,"IntermediateFiles/AggregatedVectors/Treatments_",
			boundary.name[i],"_",policy.target[k],"_",start.year,"-present.shp",sep=""),overwrite=TRUE)

		#for efficacy
		if(policy.target[k] %in% c("WildlandFireRisk","ShrublandHealth","ForestHealth","Canopy","NonCanopy")){

			#recalculate for the earlier end time to match CECS water year
			#"Regions/HUC12" is the display name for how patches are being aggregated, and "Region/huc12" is the column name 
			agg.time<-system.time(agg.treatments.region.vect<-intersect.and.aggregate.vectors(
				prepped.boundary.vect,boundary.name[i],treat.filt.ef.vect,"Treatments","Regions","Region",
				prepped.boundary.vect,boundary.name[i]))
			print(paste("Time to aggregate treatments by region (efficacy): ",round(agg.time[[1]]/60)," minute(s)", sep=""))
			writeVector(agg.treatments.region.vect,paste(loc.data,"IntermediateFiles/AggregatedVectors/Treatments_",
				boundary.name[i],"_",policy.target[k],"_",start.year,"_",end.year,".shp",sep=""),overwrite=TRUE)

			# #also intersect and aggregate treatments at HUC level
			# agg.time<-system.time(agg.treatments.huc.vect<-intersect.and.aggregate.vectors(
			# 	prepped.zonal.summary.area.vect,zsum.name,treat.filt.ef.vect,"Treatments","HUC12","huc12",
			# 	prepped.boundary.vect,boundary.name[i]))
			# print(paste("Time to aggregate treatments by HUC12 (efficacy): ",round(agg.time[[1]]/60)," minute(s)", sep=""))
			# writeVector(agg.treatments.huc.vect,paste(loc.data,"IntermediateFiles/AggregatedVectors/Treatments_",
			# 	boundary.name[i],"_",policy.target[k],"_",start.year,"_",end.year,"_HUC12.shp",sep=""),overwrite=TRUE)

		}

	}

	if(calculate.areas){
      treat.areas<-as.data.frame(unlist(areas))
      treat.areas$PolicyTarget<-rownames(treat.areas)
      names(treat.areas)<-c("Area_ac","PolicyTarget")
      rownames(treat.areas)<-NULL
      write.csv(treat.areas,paste("TreatmentAreasByPolicyTarget_",boundary.name[i],"_",start.y,"_",end.y,"_",datestamp,".csv",sep=""))
	}

}



