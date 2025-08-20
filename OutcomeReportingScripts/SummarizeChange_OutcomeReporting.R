#SummarizeChange.R

timer.start<-Sys.time()

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

reference.rast<-rast(paste(loc.data,"CECS_Data/CECS_CAWide_Vulner_TreeDieoff_SPI-2_2020_V250614.tif",sep=""))

# CECS layers all have the same CRS and extent
cecs.rast<-rast(paste(loc.data,"CECS_Data/CECS_CAWide_Vulner_TreeDieoff_SPI-2_2020_V250614.tif",sep=""))
# whp is the same projection as state boundaries/TF regions, WHR/veg classifications
whp.rast <- rast(paste(loc.data,"PriorityLayers/whp_classified_20240906.tif",sep="")) 

############### GLOBAL PARAMETERS ###################

#date stamp of this set of results - appended to all outputs to avoid overwriting older versions
datetime<-"2025Aug18"

#ending year of water year

start.year<-"2020"
end.year<-"2024"

#water year
start.y<-paste(start.year,"-09-30",sep="")
end.y<-paste(end.year,"-10-01",sep="")

metrics<-c( "FlameLengthWUI",
			"FlameLengthLandscape",
			"FlameLengthUtilities",
			"FlameLengthRoads",
			"DroughtVulnerability", 
			"Shrub-GrassRatio")

#these should match the metrics and are for making sure
#to read in the correctly aggregated vector which was
#filtered for the appropriate treatment types
policy.target<-c("WildlandFireRisk",
			"WildlandFireRisk",
			"WildlandFireRisk",
			"WildlandFireRisk",
			"ForestHealth",
			"ShrublandHealth")


############ END GLOBAL PARAMETERS #################


##########################################################
##########################################################
########### READ IN AND PROCESS RASTERS ##############

calculate.metrics<-FALSE

if(calculate.metrics){
    rast.time<-system.time(source(paste(loc.scripts,"FunctionLibraries/CalculateMetricDiffs.R",sep="")))
	print(paste("Time to process and difference rasters: ",round(rast.time[[1]]/60)," minute(s)", sep=""))
}


########### END READ IN AND PROCESS RASTERS ##############
##########################################################
##########################################################


########### READ IN AND PROCESS VECTORS ##############

# boundary.shape<-c(paste(loc.scripts,"ReferenceFiles/CA_State.shp",sep=""))
# boundary.name<-c("CA")

#boundary.shape<-c(paste(loc.scripts,"ReferenceFiles/Region_SouthernCA.shp",sep=""))
#boundary.name<-c("South")

 #loop through all california, and the four regions
 boundary.shape<-c(paste(loc.scripts,"ReferenceFiles/CA_State.shp",sep=""),
                   paste(loc.scripts,"ReferenceFiles/Region_SouthernCA.shp",sep=""),
                   paste(loc.scripts,"ReferenceFiles/Region_Sierra.shp",sep=""),
                   paste(loc.scripts,"ReferenceFiles/Region_NorthernCA.shp",sep=""),
                   paste(loc.scripts,"ReferenceFiles/Region_CentralCoast.shp",sep=""))
 boundary.name<-c("CA","South","Sierra","North","Central")

vect.shape<-c(paste(loc.scripts,"ReferenceFiles/HUC12.shp",sep=""))
vect.name<-c("HUC12")

######################################
##         PREP VECTORS            ###
######################################


aggregate.vectors<-FALSE

#this recalculates all the possible aggregations of the vectors, needs redoing if summary unit
#changes, or regions/boundary vectors change, or new treatments/fires need to be used
#or if we change what the treatment type filters are
if(aggregate.vectors){
    vect.time<-system.time(source(paste(loc.scripts,"FunctionLibraries/CalculateAggregatedPatches.R",sep="")))
	print(paste("Time to process and aggregate vectors: ",round(vect.time[[1]]/60)," minute(s)", sep=""))
}


for(k in 1:length(boundary.name)){ #loop through the extents e.g. all CA or each region
	print(paste("Start ", boundary.name[k]," loop"))

	prepped.boundary.vect<-read.and.prepare.boundary.vector(boundary.shape[k],boundary.name[k],reference.rast)

	if(boundary.name[k]=="CA"){
		prepped.boundary.vect$Region<-"AllCA"
	}	

	read.time<-system.time(zonal.summary.area.vect<-read.vector.and.check.crs(prepped.boundary.vect,vect.shape,vect.name))
	print(paste("Time to read zonal summary area: ",round(read.time[[1]]/60)," minute(s)", sep=""))
	prep.time<-system.time(prepped.zonal.summary.area.vect<-crop.vector.by.boundary.and.recalc.area(prepped.boundary.vect,boundary.name[k],zonal.summary.area.vect,vect.name))
	print(paste("Time to prep zonal summary area: ",round(prep.time[[1]]/60)," minute(s)", sep=""))

	#############################################
	###  BEGIN LOOP THROUGH METRICS   ###########
	#############################################

#	summary.method<-"global"
	summary.method<-"zonal"

	for(i in 1:length(metrics)){
		#choose the correct metric
		metric.name<-metrics[i]
		print(metric.name)

		#read in the appropriate raster(s)
		print(paste("Reading: ",loc.data,"PercDiff_",metric.name,".tif",sep=""))
		perc.diff.rast<-rast(paste(loc.data,"IntermediateFiles/DiffRasters/PercDiff_",metric.name,".tif",sep=""))
		print(paste("Reading: ",loc.data,"Init_",metric.name,".tif",sep=""))
		before.rast<-rast(paste(loc.data,"IntermediateFiles/DiffRasters/Init_",metric.name,".tif",sep=""))

		if(summary.method=="global"){

			library("exactextractr")
			library("sf")

			#SF versions

			#have to read in the vector using sf instead of terra
			agg.fire.sf<-st_read(paste(loc.data,"IntermediateFiles/AggregatedVectors/Fires_",
						boundary.name[k],"_",start.year,"_",end.year,".shp",sep=""))
			agg.fire.proj.sf<-st_transform(agg.fire.sf, st_crs(cecs.rast))

			#have to read in the vector using sf instead of terra
			agg.treat.sf<-st_read(paste(loc.data,"IntermediateFiles/AggregatedVectors/Treatments_",
						boundary.name[k],"_",policy.target[i],"_",start.year,"_",end.year,".shp",sep=""))
			agg.treat.proj.sf<-st_transform(agg.treat.sf, st_crs(cecs.rast))


			boundary.sf<-st_read(boundary.shape[k])
			prepped.boundary.sf<-st_transform(boundary.sf, st_crs(cecs.rast))

			########### END READ IN AND PROCESS VECTORS ##############

			################### BEGIN GLOBAL CALCS ######################

			diffname<-paste(metric.name,start.year,end.year,sep="_")

			print(paste("Starting global median calcs for ",metric.name," in ",boundary.name[k],sep=""))
			all.global.results<-rbind(
				cbind(
				method="Global",type="PercentDiff", metric=metric.name,boundary=boundary.name[k],subset="WholeArea",
				rasterAverage=exact_extract(perc.diff.rast,prepped.boundary.sf,fun="median")
				),
				cbind(
				method="Global",type="PercentDiff", metric=metric.name,boundary=boundary.name[k],subset="Treatments",
				rasterAverage=exact_extract(perc.diff.rast,agg.treat.proj.sf,fun="median")
				),
				cbind(
				method="Global",type="PercentDiff", metric=metric.name,boundary=boundary.name[k],subset="Fires",
				rasterAverage=exact_extract(perc.diff.rast,agg.fire.proj.sf,fun="median")
				),
				cbind(
				method="Global",type="InitialValues", metric=metric.name,boundary=boundary.name[k],subset="WholeArea",
				rasterAverage=exact_extract(before.rast,prepped.boundary.sf,fun="median")
				),
				cbind(
				method="Global",type="InitialValues", metric=metric.name,boundary=boundary.name[k],subset="Treatments",
				rasterAverage=exact_extract(before.rast,agg.treat.proj.sf,fun="median")
				),
				cbind(
				method="Global",type="InitialValues", metric=metric.name,boundary=boundary.name[k],subset="Fires",
				rasterAverage=exact_extract(before.rast,agg.fire.proj.sf,fun="median")
				)

			)

			print(paste("Writing to GlobalCalcOutput_",diffname,"_",boundary.name[k],"_",datetime,".csv",sep=""))
			write.table(all.global.results,paste("GlobalCalcOutput_",diffname,"_",boundary.name[k],"_",datetime,".csv",sep=""),
				sep = ",",quote = FALSE, col.names = TRUE, row.names = FALSE,na="NA") 

			################### END GLOBAL CALCS ######################


		}else if(summary.method=="zonal"){

			library("exactextractr")
			library("sf")


			#have to read in the vector using sf instead of terra
			agg.fire.huc.sf<-st_read(paste(loc.data,"IntermediateFiles/AggregatedVectors/Fires_",
						boundary.name[k],"_",start.year,"_",end.year,"_HUC12.shp",sep=""))
			agg.fire.huc.proj.sf<-st_transform(agg.fire.huc.sf, st_crs(cecs.rast))

			#have to read in the vector using sf instead of terra
			agg.treat.huc.sf<-st_read(paste(loc.data,"IntermediateFiles/AggregatedVectors/Treatments_",
						boundary.name[k],"_",policy.target[i],"_",start.year,"_",end.year,"_HUC12.shp",sep=""))
			agg.treat.huc.proj.sf<-st_transform(agg.treat.huc.sf, st_crs(cecs.rast))


			boundary.sf<-st_read(boundary.shape[k])
			prepped.boundary.sf<-st_transform(boundary.sf, st_crs(cecs.rast))

			zonal.summary.area.sf<-st_read(vect.shape)
			zonal.summary.proj.sf<-st_transform(zonal.summary.area.sf, st_crs(cecs.rast))
			prepped.zonal.summary.sf<-st_intersection(zonal.summary.proj.sf,prepped.boundary.sf)

			########### END READ IN AND PROCESS VECTORS ##############


			# #################### ZONAL CALCULATIONS #######################

			diffname<-paste(metric.name,start.year,end.year,sep="_")

			print(paste("Starting zonal median calcs for ",metric.name," in ",boundary.name[k],sep=""))
			 all.zonal.results<-rbind(
			cbind(
				method=rep("Zonal",nrow(prepped.zonal.summary.sf)),
			 	metric=rep(metric.name,nrow(prepped.zonal.summary.sf)),
			 	boundary=rep(boundary.name[k],nrow(prepped.zonal.summary.sf)),
			 	subset=rep("WholeArea",nrow(prepped.zonal.summary.sf)),
			 	shapeID=prepped.zonal.summary.sf[,"huc12"],
				PercentDiff=exact_extract(perc.diff.rast,prepped.zonal.summary.sf,fun="median"),
				Initial=exact_extract(before.rast,prepped.zonal.summary.sf,fun="median")),
			cbind(
				method=rep("Zonal",nrow(agg.fire.huc.proj.sf)),
			 	metric=rep(metric.name,nrow(agg.fire.huc.proj.sf)),
			 	boundary=rep(boundary.name[k],nrow(agg.fire.huc.proj.sf)),
			 	subset=rep("Fire",nrow(agg.fire.huc.proj.sf)),
			 	shapeID=agg.fire.huc.proj.sf[,"huc12"],
				PercentDiff=exact_extract(perc.diff.rast,agg.fire.huc.proj.sf,fun="median"),
				Initial=exact_extract(before.rast,agg.fire.huc.proj.sf,fun="median")),
			cbind(
				method=rep("Zonal",nrow(agg.treat.huc.proj.sf)),
			 	metric=rep(metric.name,nrow(agg.treat.huc.proj.sf)),
			 	boundary=rep(boundary.name[k],nrow(agg.treat.huc.proj.sf)),
			 	subset=rep("Treatments",nrow(agg.treat.huc.proj.sf)),
			 	shapeID=agg.treat.huc.proj.sf[,"huc12"],
				PercentDiff=exact_extract(perc.diff.rast,agg.treat.huc.proj.sf,fun="median"),
				Initial=exact_extract(before.rast,agg.treat.huc.proj.sf,fun="median"))
			)

 			 print(paste("Writing to ZonalCalcOutput_",diffname,"_",boundary.name[k],"_",datetime,".csv",sep=""))
			 write.table(as.data.frame(all.zonal.results),paste("ZonalCalcOutput_",diffname,"_",boundary.name[k],"_",datetime,".csv",sep=""),
			 	sep = ",",quote = FALSE, col.names = TRUE, row.names = FALSE,na="NA")

			 print(paste("Writing to GlobalCalcOutput_",diffname,"_",boundary.name[k],"_",datetime,".shp",sep=""))
			 st_write(all.zonal.results, paste("ZonalCalcOutput_",diffname,"_",boundary.name[k],"_",datetime,".shp",sep=""))

			# ################### END ZONAL CALCS ######################

		}



		#################### DATA VISUALIZATIONS ###################

				#---------------- Plots for whole summary area ---------------------#

				# area_type<-"SummaryUnit"

				# plot.results(dt.dff=zonal.results$zonalAll,	ttlestrng=titlestring,
				# 	xlbl=xlabel, metnm=metric.name, af.yr=after.year, bf.yr=before.year, sum.area=area_type,
				# 	sumIDnm=sum.Poly.name[j], lnd.clss=sub.set[i], dffnm=diffname, dttme=datetime,reg=crop.nm[k])

				# zonal.means[1,]<-c(metric.name,crop.nm[k],sum.Poly.name[j],sub.set[i],area_type,
				# 	mean(as.data.frame(zonal.results$zonalAll)[,diffname],na.rm=TRUE))
				# write.table(zonal.means,paste("ZonalMeansOutput_",diffname,"_",datetime,".csv",sep=""),
				# 			sep = ",",quote = FALSE, col.names = FALSE, row.names = FALSE,na="NA",append=TRUE)
				#count<-count+1

		#use Lauren's boxplot code
		#make HUC level maps of the differences


			########### END DATA VISUALIZATIONS ##############


	} # end loop through metrics (i)

	########### END INITIAL ANALYSIS AND DATA VIS ############
	##########################################################
	##########################################################
} #end boundary regions loop (k)

timer.end<-Sys.time()

time.total<-timer.end-timer.start
print(time.total)

