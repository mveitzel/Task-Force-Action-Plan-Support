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

reference.rast<-rast(paste(loc.data,"CECS_Data/CECS_CAWide_Vulner_TreeDieoff_SPI-2_2020_V250418.tif",sep=""))

############### GLOBAL PARAMETERS ###################

#date stamp of this set of results - appended to all outputs to avoid overwriting older versions
datetime<-"2025Aug12"

#ending year of water year

start.year<-"2020"
end.year<-"2024"

#water year
start<-paste(start.year,"-09-30",sep="")
end<-paste(end.year,"-10-01",sep="")

metrics<-c( "FlameLengthWUI",
			"FlameLengthLandscape",
			"FlameLengthUtilities",
			"FlameLengthRoads",
			"DroughtVulnerability", 
			"Shrub-GrassRatio")


############ END GLOBAL PARAMETERS #################


##########################################################
##########################################################
########### READ IN AND PROCESS RASTERS ##############

calculate.metrics<-FALSE

if(calculate.metrics){
    rast.time<-system.time(source(paste(loc.scripts,"FunctionLibraries/CalculateMetricDiffs.R",sep="")))
	print(paste("Time to process and difference rasters: ",round(rast.time[[1]]/60)," minutes", sep=""))
}


########### END READ IN AND PROCESS RASTERS ##############
##########################################################
##########################################################


########### READ IN AND PROCESS VECTORS ##############

# boundary.shape<-c(paste(loc.scripts,"ReferenceFiles/CA_State.shp",sep=""))
# boundary.name<-c("CA")

boundary.shape<-c(paste(paste(loc.scripts,"ReferenceFiles/Region_SouthernCA.shp",sep="")))
boundary.name<-c("South")


 #boundary.shape<-c(paste(loc.scripts,"ReferenceFiles/CA_State.shp",sep=""),
 #                  paste(loc.scripts,"ReferenceFiles/Region_SouthernCA.shp",sep=""))
 #boundary.name<-c("CA","South")

# #loop through all california, and the four regions
# boundary.shape<-c(paste(loc.scripts,"ReferenceFiles/CA_State.shp",sep=""),
#                   paste(loc.scripts,"ReferenceFiles/Region_SouthernCA.shp",sep=""),
#                   paste(loc.scripts,"ReferenceFiles/Region_Sierra.shp",sep=""),
#                   paste(loc.scripts,"ReferenceFiles/Region_NorthernCA.shp",sep=""),
#                   paste(loc.scripts,"ReferenceFiles/Region_CentralCoast.shp",sep=""))
# boundary.name<-c("CA","South","Sierra","North","Central")

#boundary.shape<-c(paste(loc.scripts,"ReferenceFiles/Region_Sierra.shp",sep=""))
#boundary.name<-c("Sierra")

vect.shape<-c(paste(loc.scripts,"ReferenceFiles/HUC12.shp",sep=""))
vect.name<-c("HUC12")

patch.name<-c("Treatments","Fires")
patch.shape<-c(paste(loc.data,"ITS_2025Aug16_Data/appended.gdb",sep=""),paste(loc.data,"FireFootprints/fire24_1.gdb",sep=""))
patch.layer<-c("appended_poly","firep24_1")

read.time<-system.time(treatments<-read.and.check.crs.patch.vector(patch.shape[1],patch.name[1],patch.layer[1],reference.rast))
print(paste("Time to read treatments: ",round(read.time[[1]]/60)," minutes", sep=""))
read.time<-system.time(fires<-read.and.check.crs.patch.vector(patch.shape[2],patch.name[2],patch.layer[2],reference.rast))
print(paste("Time to read fires: ",round(read.time[[1]]/60)," minutes", sep=""))

#these are necessary because to do the aggregation, you need to intersect both vectors, and then
# have a column name (agg.code) to do the aggregation/dissolve on.
agg.name<-c("Regions","HUC12")
agg.code<-c("Region","huc12")

######################################
##         PREP VECTORS            ###
######################################


for(k in 1:length(boundary.name)){ #loop through the extents e.g. all CA or each region
	print(paste("Start ", boundary.name[k]," loop"))

	prepped.boundary.vect<-read.and.prepare.boundary.vector(boundary.shape[k],boundary.name[k],reference.rast)
	if(boundary.name[k]=="CA"){
		prepped.boundary.vect$Region<-"AllCA"
	}	

#	read.time<-system.time(zonal.summary.area.vect<-read.vector.and.check.crs(prepped.boundary.vect,vect.shape,vect.name))
#	print(paste("Time to read zonal summary area: ",round(read.time[[1]]/60)," minutes", sep=""))
#	prep.time<-system.time(prepped.zonal.summary.area.vect<-crop.vector.by.boundary.and.recalc.area(prepped.boundary.vect,boundary.name[k],zonal.summary.area.vect,vect.name))
#	print(paste("Time to prep zonal summary area: ",round(prep.time[[1]]/60)," minutes", sep=""))

#in theory, here I should also filter treatments and therefore the aggregating vectors should be inside the loop
#but it takes a very long time

	agg.time<-system.time(agg.fires.vect.region<-intersect.and.aggregate.vectors(
		prepped.boundary.vect,boundary.name[k],fires,patch.name[2],agg.name[1],agg.code[1],prepped.boundary.vect,boundary.name[k]))
	print(paste("Time to aggregate fires by region: ",round(agg.time[[1]]/60)," minutes", sep=""))
#	agg.time<-system.time(agg.fires.vect.huc<-intersect.and.aggregate.vectors(
#		prepped.zonal.summary.area.vect,vect.name,fires,patch.name[2],agg.name[2],agg.code[2],prepped.boundary.vect,boundary.name[k]))
#	print(paste("Time to aggregate fires by HUC12: ",round(agg.time[[1]]/60)," minutes", sep=""))
	agg.time<-system.time(agg.treatments.vect.region<-intersect.and.aggregate.vectors(
		prepped.boundary.vect,boundary.name[k],treatments,patch.name[1],agg.name[1],agg.code[1],prepped.boundary.vect,boundary.name[k]))
	print(paste("Time to aggregate treatments by region: ",round(agg.time[[1]]/60)," minutes", sep=""))
#	agg.time<-system.time(agg.treatments.vect.huc<-intersect.and.aggregate.vectors(
#		prepped.zonal.summary.area.vect,vect.name,treatments,patch.name[1],agg.name[2],agg.code[2],prepped.boundary.vect,boundary.name[k]))
#	print(paste("Time to aggregate treatments by HUC12: ",round(agg.time[[1]]/60)," minutes", sep=""))


	########### END READ IN AND PROCESS VECTORS ##############



	#############################################
	###  BEGIN LOOP THROUGH METRICS   ###########
	#############################################

	for(i in 1:length(metrics)){
		#choose the correct metric
		metric.name<-metrics[i]
		#read in the appropriate raster
		print(paste("Reading: ",loc.data,"Diff_",metric.name,".tif",sep=""))
		diff.rast<-rast(paste(loc.data,"IntermediateRasters/DiffRasters/Diff_",metric.name,".tif",sep=""))


		# #################### ZONAL CALCULATIONS #######################

		# whole.summary.area.zonal<-summarize.pixels.in.area.of.interest(
		# 						diff,metric.name,prepped.zonal.summary.area.vect,vect.name,"zonal",diffname)
		# treatments.zonal<-summarize.pixels.in.area.of.interest(
		# 						diff,metric.name,agg.treatments.vect.huc,patch.name[1],"zonal",diffname)
		# fires.zonal<-summarize.pixels.in.area.of.interest(
		# 						diff,metric.name,agg.fires.vect.huc,patch.name[2],"zonal",diffname)

		# diffname<-paste(metric.name,start.year,end.year,sep="_")

		# #this is the output that will have the zonal mean for each spatial summary unit (e.g. HUC)
		# #within the extent (boundary, e.g. Task Force region)
		# #and then also includes the hucID (or other ID number for the individual spatial summary units)
		# #and its area. the hucAverage is the actual value for the zonal average.

		# all.zonal.results<-rbind(
		# 	cbind(
		# 	method=rep("Zonal",nrow(whole.summary.area.zonal)),
		# 	metric=rep(metric.name,nrow(whole.summary.area.zonal)),
		# 	boundary=rep(boundary.name[k],nrow(whole.summary.area.zonal)),
		# 	subset=rep("WholeArea",nrow(whole.summary.area.zonal)),
		# 	shapeID=as.data.frame(whole.summary.area.zonal)[,agg.code[2]],
		# 	rasterAverage=as.data.frame(whole.summary.area.zonal)[,diffname]
		# 	),
		# 	cbind(
		# 	method=rep("Zonal",nrow(treatments.zonal)),
		# 	metric=rep(metric.name,nrow(treatments.zonal)),
		# 	boundary=rep(boundary.name[k],nrow(treatments.zonal)),
		# 	subset=rep(patch.name[1],nrow(treatments.zonal)),
		# 	shapeID=as.data.frame(treatments.zonal)[,agg.code[2]],
		# 	rasterAverage=as.data.frame(treatments.zonal)[,diffname]
		# 	),
		# 	cbind(
		# 	method=rep("Zonal",nrow(fires.zonal)),
		# 	metric=rep(metric.name,nrow(fires.zonal)),
		# 	boundary=rep(boundary.name[k],nrow(fires.zonal)),
		# 	subset=rep(patch.name[2],nrow(fires.zonal)),
		# 	shapeID=as.data.frame(fires.zonal)[,agg.code[2]],
		# 	rasterAverage=as.data.frame(fires.zonal)[,diffname]
		# 	)
		# )


		# write.table(all.zonal.results,paste("RawZonalCalcOutput_",diffname,"_",boundary.name[k],"_",datetime,".csv",sep=""),
		# 	sep = ",",quote = FALSE, col.names = TRUE, row.names = FALSE,na="NA") 

		# ################### END ZONAL CALCS ######################


		################### BEGIN GLOBAL CALCS ######################

		diffname<-paste(metric.name,start.year,end.year,sep="_")

		global.summary.treatments<-summarize.pixels.in.area.of.interest(
								diff,metric.name,agg.treatments.vect.region,patch.name[1],"global",diffname)
		global.summary.fires<-summarize.pixels.in.area.of.interest(
								diff,metric.name,agg.fires.vect.region,patch.name[2],"global",diffname)
		global.summary.wholearea<-summarize.pixels.in.area.of.interest(
								diff,metric.name,prepped.boundary.vect,boundary.name[k],"global",diffname)

		all.global.results<-rbind(
			cbind(
			method=rep("Global",nrow(global.summary.wholearea)),
			metric=rep(metric.name,nrow(global.summary.wholearea)),
			boundary=rep(boundary.name[k],nrow(global.summary.wholearea)),
			subset=rep("WholeArea",nrow(global.summary.wholearea)),
			rasterAverage=as.data.frame(global.summary.wholearea)[,diffname]
			),
			cbind(
			method=rep("Global",nrow(global.summary.treatments)),
			metric=rep(metric.name,nrow(global.summary.treatments)),
			boundary=rep(boundary.name[k],nrow(global.summary.treatments)),
			subset=rep(patch.name[1],nrow(global.summary.treatments)),
			rasterAverage=as.data.frame(global.summary.treatments)[,diffname]
			),
			cbind(
			method=rep("Global",nrow(global.summary.fires)),
			metric=rep(metric.name,nrow(global.summary.fires)),
			boundary=rep(boundary.name[k],nrow(global.summary.fires)),
			subset=rep(patch.name[2],nrow(global.summary.fires)),
			rasterAverage=as.data.frame(global.summary.fires)[,diffname]
			)
		)

		write.table(all.global.results,paste("GlobalCalcOutput_",diffname,"_",boundary.name[k],"_",datetime,".csv",sep=""),
			sep = ",",quote = FALSE, col.names = TRUE, row.names = FALSE,na="NA") 


		################### END GLOBAL CALCS ######################


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

