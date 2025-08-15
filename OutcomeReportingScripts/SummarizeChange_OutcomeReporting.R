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

metrics<-c(
			#"DroughtVulnerability"
			#, 
			#"FlameLengthWUI"
			#,
			#"FlameLengthWildland"
			#,
			"FlameLengthUtilities"
			,
			"Shrub-GrassRatio"
			)

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
patch.shape<-c(paste(loc.data,"ITS_2025Jul25_Polygons/appended.gdb",sep=""),paste(loc.data,"FireFootprints/fire24_1.gdb",sep=""))
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
		metric.name<-metrics[i]
		############ END GLOBAL PARAMETERS #################


		#this section contains the specifics for each metric, including
		#the CECS conversion factor and the actual function calls to 
		# the raster differencing calculation

		##########################################################
		##########################################################
		########### READ IN AND PROCESS RASTERS ##############




		#---------- FLAME LENGTH (WUI) RASTER CALCS -------------------#

		if(metric.name=="FlameLengthWUI"){
			vint<-"250614"
			metric<-"Fire_FlamMap_FL"
			xlabel<-"Average decrease in flame length (ft)"
			#'units are 0.01 m' so divide by 100, but want ft so multiply by 3.28084
			conversion<-(0.0328084)

			before.yr.name<-generate.CECS.filename(metric,start.year,vint)
			after.yr.name<-generate.CECS.filename(metric,end.year,vint)

			before.rast<-read.in.raster(loc.data,before.yr.name,metric.name)
			after.rast<-read.in.raster(loc.data,after.yr.name,metric.name)

			before.proj.rast<-check.crs.match(reference.rast,before.rast)
			after.proj.rast<-check.crs.match(reference.rast,after.rast)

			diff.rast<-diff.rasters(start.year,before.proj.rast,end.year,after.proj.rast,metric.name)

			#	mask for WUI
			# do I want to use subset.raster here?
			wui.cecs.rast<-rast(paste(loc.data,"WUIVegetationClassifications/FRAP24_WUIOnly_CECS.tif",sep=""))
			diff.masked.rast<-diff.rast*wui.cecs.rast
			print("Raster masked for WUI")

			diff<-multiply.conversion.factor(metric.name,diff.masked.rast,conversion)
		}

		#------------ end flame length (WUI) raster calcs -----------#

		#---------- FLAME LENGTH (WILDLAND) RASTER CALCS -------------------#

		if(metric.name=="FlameLengthWildland"){
			vint<-"250614"
			metric<-"Fire_FlamMap_FL"
			xlabel<-"Average decrease in flame length (ft)"
			#'units are 0.01 m' so divide by 100, but want ft so multiply by 3.28084
			conversion<-(0.0328084)

			before.yr.name<-generate.CECS.filename(metric,start.year,vint)
			after.yr.name<-generate.CECS.filename(metric,end.year,vint)

			before.rast<-read.in.raster(loc.data,before.yr.name,metric.name)
			after.rast<-read.in.raster(loc.data,after.yr.name,metric.name)

			before.proj.rast<-check.crs.match(reference.rast,before.rast)
			after.proj.rast<-check.crs.match(reference.rast,after.rast)

			diff.rast<-diff.rasters(start.year,before.proj.rast,end.year,after.proj.rast,metric.name)

			#	mask for WUI
			# do I want to use subset.raster here?
			wild.cecs.rast<-rast(paste(loc.data,"WUIVegetationClassifications/FRAP24_WildlandOnly_CECS.tif",sep=""))
			diff.masked.rast<-diff.rast*wild.cecs.rast
			print("Raster masked for Wildland")

			diff<-multiply.conversion.factor(metric.name,diff.masked.rast,conversion)
		}

		#------------ end flame length (Wildland) raster calcs -----------#

		#---------- FLAME LENGTH (UTILITIES) RASTER CALCS -------------------#

		if(metric.name=="FlameLengthUtilities"){
			vint<-"250614"
			metric<-"Fire_FlamMap_FL"
			xlabel<-"Average decrease in flame length (ft)"
			#'units are 0.01 m' so divide by 100, but want ft so multiply by 3.28084
			conversion<-(0.0328084)

			before.yr.name<-generate.CECS.filename(metric,start.year,vint)
			after.yr.name<-generate.CECS.filename(metric,end.year,vint)

			before.rast<-read.in.raster(loc.data,before.yr.name,metric.name)
			after.rast<-read.in.raster(loc.data,after.yr.name,metric.name)

			before.proj.rast<-check.crs.match(reference.rast,before.rast)
			after.proj.rast<-check.crs.match(reference.rast,after.rast)

			diff.rast<-diff.rasters(start.year,before.proj.rast,end.year,after.proj.rast,metric.name)

			#	mask for WUI
			# do I want to use subset.raster here?
			rdtr.buff.cecs.rast<-rast(paste(loc.data,"WUIVegetationClassifications/RoadTransmissionLineBuffer_CECSproj.tif",sep=""))
			diff.masked.rast<-diff.rast*rdtr.buff.cecs.rast
			print("Raster masked for Utilities")

			diff<-multiply.conversion.factor(metric.name,diff.masked.rast,conversion)
		}

		#------------ end flame length (Utilities) raster calcs -----------#


		#---------- DROUGHT VULNERABILITY RASTER CALCS -------------------#

		if(metric.name=="DroughtVulnerability"){
			vint<-"250614"
			metric<-"Vulner_TreeDieoff_SPI-2"
			xlabel<-"Average decrease in Drought Vulnerability"
			conversion<-NA

			before.yr.name<-generate.CECS.filename(metric,start.year,vint)
			after.yr.name<-generate.CECS.filename(metric,end.year,vint)

			before.rast<-read.in.raster(loc.data,before.yr.name,metric.name)
			after.rast<-read.in.raster(loc.data,after.yr.name,metric.name)

			before.proj.rast<-check.crs.match(reference.rast,before.rast)
			after.proj.rast<-check.crs.match(reference.rast,after.rast)

			diff.rast<-diff.rasters(start.year,before.proj.rast,end.year,after.proj.rast,metric.name)

			#	mask for forest
			# do I want to use subset.raster here?
			forest.cecs.rast<-rast(paste(loc.data,"WUIVegetationClassifications/WHR13_RECLASS_FOREST_CECS.tif",sep=""))
			diff.masked.rast<-diff.rast*forest.cecs.rast
			print("Raster masked for forest")

			diff<-multiply.conversion.factor(metric.name,diff.masked.rast,conversion)

		}

		#------------ end drought vulnerability raster calcs -----------#



		#---------- SHRUB-GRASS RATIO CALCS -------------------#

		if(metric.name=="Shrub-GrassRatio"){
			vint<-"250418"
			metric.shrub<-"Veg_ShrubFrac"
			metric.grass<-"Veg_HerbFrac"
			xlabel<-"Average decrease in Shrub-Grass Ratio"
			conversion<-NA #the conversion factor for the individual proportions are 1/10000, but we're doing a ratio so they cancel

			before.yr.shrub.name<-generate.CECS.filename(metric.shrub,start.year,vint)
			after.yr.shrub.name<-generate.CECS.filename(metric.shrub,end.year,vint)
			before.yr.grass.name<-generate.CECS.filename(metric.grass,start.year,vint)
			after.yr.grass.name<-generate.CECS.filename(metric.grass,end.year,vint)

			before.shrub.rast<-read.in.raster(loc.data,before.yr.shrub.name,metric.name)
			after.shrub.rast<-read.in.raster(loc.data,after.yr.shrub.name,metric.name)
			before.grass.rast<-read.in.raster(loc.data,before.yr.grass.name,metric.name)
			after.grass.rast<-read.in.raster(loc.data,after.yr.grass.name,metric.name)

			#where the grass proportion is zero, we don't want to divide by it.
			#but we don't want to substitute an arbitrarily small value that will mess up
			#the mean values.  So find the minimum value that isn't zero
			mask.before.grass.rast<-before.grass.rast
			mask.before.grass.rast[before.grass.rast<=0]<-NA
			bef.grass.min<-as.numeric(global(mask.before.grass.rast,"min",na.rm=TRUE))
			#and do the same for the 'after' grass raster, though likely it's the same
			mask.after.grass.rast<-after.grass.rast
			mask.after.grass.rast[after.grass.rast<=0]<-NA
			aft.grass.min<-as.numeric(global(mask.after.grass.rast,"min",na.rm=TRUE))

			before.grass.prepped.rast<-before.grass.rast
			before.grass.prepped.rast[before.grass.prepped.rast<=0]<-bef.grass.min
			after.grass.prepped.rast<-after.grass.rast
			after.grass.prepped.rast[after.grass.prepped.rast<=0]<-aft.grass.min

			#now it should be safe to divide by the grass layers
			before.rast<-before.shrub.rast/before.grass.prepped.rast
			after.rast<-after.shrub.rast/after.grass.prepped.rast

			#just checking on how many of these produce infinite values based on division by 0
			#before pulling the trick with replacing with lowest nonzero value, it was about 0.3-0.4%
		 	#global(is.infinite(after.rast),"sum")/global(after.rast,"notNA")
		 	#global(is.infinite(before.rast),"sum")/global(after.rast,"notNA")

			before.proj.rast<-check.crs.match(reference.rast,before.rast)
			after.proj.rast<-check.crs.match(reference.rast,after.rast)

			diff.rast<-diff.rasters(start.year,before.proj.rast,end.year,after.proj.rast,metric.name)

			#   mask for shrub
			shrub.cecs.rast<-rast(paste(loc.data,"WUIVegetationClassifications/WHR13_RECLASS_SHRUB_CECS.tif",sep=""))
			diff.masked.rast<-diff.rast*shrub.cecs.rast

			diff<-multiply.conversion.factor(metric.name,diff.masked.rast,conversion)

		}

		#------------ end shrub-grass ratio calcs -----------#



		########### END READ IN AND PROCESS RASTERS ##############
		##########################################################
		##########################################################



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

