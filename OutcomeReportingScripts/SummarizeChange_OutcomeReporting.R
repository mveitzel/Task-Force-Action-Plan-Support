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

##  This code is designed for each metric's analysis to be run in a separate R
##  instance, uncommenting the particular metric for each run, selecting all, and 
##  pasting into an R console, or using 'source' to run the entire script
##  This code makes histograms and maps, as well as outputting both the means for 
##  each metric and the actual values for each HUC (or other spatial summary unit)
##  in csv files.  A separate script creates the bar charts showing the means, as 
##  seen in the October 10th 2024 Task Force meeting presentation.

##  This version of the code is simplified just to focus on differencing rasters
##  and calculating and displaying zonal means.  Summaries are done only for entire
##  spatial summary units (e.g. HUCs) and not summarized for treatments or fires within them.

############### GLOBAL PARAMETERS ###################

#date stamp of this set of results - appended to all outputs to avoid overwriting older versions
datetime<-"2025Aug06"

#ending year of water year

start.year<-"2020"
end.year<-"2024"

#water year
start<-paste(start.year,"-09-30",sep="")
end<-paste(end.year,"-10-01",sep="")

#uncomment whichever metric you want to run, and then copy/paste
#or save and source the entire file to run this given metric
#alternatively add to the script a loop that runs through metrics
#choose metric
metric.name<-"Drought Vulnerability"
#metric.name<-"Flame Length WUI"
#metric.name<-"Flame Length Wildland"
#metric.name<-"Flame Length Utility"
#metric.name<-"Shrub:Grass Ratio"

############ END GLOBAL PARAMETERS #################


#this section contains the specifics for each metric, including
#the CECS conversion factor and the actual function calls to 
# the raster differencing calculation

##########################################################
##########################################################
########### READ IN AND PROCESS RASTERS ##############




#---------- FLAME LENGTH (WUI) RASTER CALCS -------------------#

if(metric.name=="Flame Length WUI"){
	vint<-"250614"
	metric<-"Fire_FlamMap_FL"
	xlabel<-"Average decrease in flame length (ft)"
	#'units are 0.01 m' so divide by 100, but want ft so multiply by 3.28084
	conversion<-(0.0328084)

	before.yr.filename<-generate.CECS.filename(metric,start.year,vint)
	after.yr.filename<-generate.CECS.filename(metric,end.year,vint)

	before.rast<-read.in.raster(loc.data,before.yr.name,metric.name)
	after.rast<-read.in.raster(loc.data,after.yr.name,metric.name)

	before.proj.rast<-check.crs.match(reference.rast,before.rast)
	after.proj.rast<-check.crs.match(reference.rast,after.rast)

	#	mask for WUI
	#forest.cecs.rast<-rast(paste(loc.data,"WUIVegetationClassifications/WHR13_RECLASS_FOREST_CECS.tif",sep=""))

	#before.masked.rast<-before.proj.rast*forest.cecs.rast
	#after.masked.rast<-after.proj.rast*forest.cecs.rast

	before.prepped.rast<-multiply.conversion.factor(metric.name,before.masked.rast,conversion)
	after.prepped.rast<-multiply.conversion.factor(metric.name,after.masked.rast,conversion)

	diff<-diff.rasters(start.year,before.prepped.rast,end.year,after.prepped.rast,metric.name)

}

#------------ end flame length (WUI) raster calcs -----------#

#repeat thrice for wildland and utilities

#---------- DROUGHT VULNERABILITY RASTER CALCS -------------------#

if(metric.name=="Drought Vulnerability"){
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

	#	mask for forest
	# do I want to use subset.raster here?
	forest.cecs.rast<-rast(paste(loc.data,"WUIVegetationClassifications/WHR13_RECLASS_FOREST_CECS.tif",sep=""))
	print("Raster masked for forest")

	before.masked.rast<-before.proj.rast*forest.cecs.rast
	after.masked.rast<-after.proj.rast*forest.cecs.rast

	before.prepped.rast<-multiply.conversion.factor(metric.name,before.masked.rast,conversion)
	after.prepped.rast<-multiply.conversion.factor(metric.name,after.masked.rast,conversion)

	diff<-diff.rasters(start.year,before.prepped.rast,end.year,after.prepped.rast,metric.name)

}

#------------ end drought vulnerability raster calcs -----------#



#---------- SHRUB:GRASS RATIO CALCS -------------------#

if(metric.name=="Shrub:Grass Ratio"){
	vint<-"250418"
	metric.shrub<-"Veg_ShrubFrac"
	metric.grass<-"Veg_HerbFrac"
	xlabel<-"Average decrease in Shrub:Grass Ratio"
	conversion<-NA

	before.yr.shrub.name<-generate.CECS.filename(metric.shrub,start.year,vint)
	after.yr.shrub.name<-generate.CECS.filename(metric.shrub,end.year,vint)
	before.yr.grass.name<-generate.CECS.filename(metric.grass,start.year,vint)
	after.yr.grass.name<-generate.CECS.filename(metric.grass,end.year,vint)

	before.shrub.rast<-read.in.raster(loc.data,before.yr.shrub.name,metric.name)
	after.shrub.rast<-read.in.raster(loc.data,after.yr.shrub.name,metric.name)
	before.grass.rast<-read.in.raster(loc.data,before.yr.grass.name,metric.name)
	after.grass.rast<-read.in.raster(loc.data,after.yr.grass.name,metric.name)

	before.rast<-before.shrub.rast/before.grass.rast
	after.rast<-after.shrub.rast/after.grass.rast

	before.proj.rast<-check.crs.match(reference.rast,before.rast)
	after.proj.rast<-check.crs.match(reference.rast,after.rast)

	#   mask for shrub
	shrub.cecs.rast<-rast(paste(loc.data,"WUIVegetationClassifications/WHR13_RECLASS_SHRUB_CECS.tif",sep=""))

	before.masked.rast<-before.proj.rast*shrub.cecs.rast
	after.masked.rast<-after.proj.rast*shrub.cecs.rast

	before.prepped.rast<-multiply.conversion.factor(metric.name,before.masked.rast,conversion)
	after.prepped.rast<-multiply.conversion.factor(metric.name,after.masked.rast,conversion)

	diff<-diff.rasters(start.year,before.prepped.rast,end.year,after.prepped.rast,metric.name)

}

#------------ end shrub:grass ratio calcs -----------#



########### END READ IN AND PROCESS RASTERS ##############
##########################################################
##########################################################



########### READ IN AND PROCESS VECTORS ##############

#loop through all california, and the four regions
boundary.shape<-c(paste(loc.scripts,"ReferenceFiles/CA_State.shp",sep=""),
                  paste(loc.scripts,"ReferenceFiles/Region_Sierra.shp",sep=""),
                  paste(loc.scripts,"ReferenceFiles/Region_NorthernCA.shp",sep=""),
                  paste(loc.scripts,"ReferenceFiles/Region_SouthernCA.shp",sep=""),
                  paste(loc.scripts,"ReferenceFiles/Region_CentralCoast.shp",sep=""))
boundary.name<-c("CA","Sierra","North","South","Central")

reference.rast<-rast(paste(loc.data,"CECS_Data/CECS_CAWide_Vulner_TreeDieoff_SPI-2_2020_V250418.tif",sep=""))

vect.shape<-c(paste(loc.scripts,"ReferenceFiles/HUC12.shp",sep=""))
vect.name<-c("HUC12")

patch.name<-c("Treatments","Fires")
patch.shape<-c(paste(loc.data,"ITS_2025Jul25_Polygons/appended.gdb",sep=""),paste(loc.data,"FireFootprints/fire24_1.gdb",sep=""))
patch.layer<-c("appended_poly","firep24_1")

treatments<-read.and.check.crs.patch.vector(patch.shape[1],patch.name[1],patch.layer[1],prepped.boundary.vect)
fires<-read.and.check.crs.patch.vector(patch.shape[2],patch.name[2],patch.layer[2],prepped.boundary.vect)

for(k in 1:length(boundary.name)){ #loop through the extents e.g. all CA or each region
	print(paste("Start ", boundary.name[k]," loop"))

	prepped.boundary.vect<-read.and.prepare.boundary.vector(boundary.shape[k],boundary.name[k],reference.rast)
	if(boundary.name[k]=="CA"){
		prepped.boundary.vect$Region<-"AllCA"
	}	

	zonal.summary.area.vect<-read.vector.and.check.crs(prepped.boundary.vect,vect.shape,vect.name)
	prepped.zonal.summary.area.vect<-crop.vector.by.boundary.and.recalc.area(prepped.boundary.vect,boundary.name[k],zonal.summary.area.vect,vect.name)

	#these are necessary because to do the aggregation, you need to intersect both vectors, and then
	# have a column name (agg.code) to do the aggregation/dissolve on.
	agg.name<-c("Regions","HUC12")
	agg.code<-c("Region","huc12")


	agg.fires.vect.region<-intersect.and.aggregate.vectors(
		prepped.boundary.vect,boundary.name,fires,patch.name[2],agg.name[1],agg.code[1],prepped.boundary.vect,boundary.name[k])
	agg.fires.vect.huc<-intersect.and.aggregate.vectors(
		prepped.zonal.summary.area.vect,vect.name,fires,patch.name[2],agg.name[2],agg.code[2],prepped.boundary.vect,boundary.name[k])
	agg.treatments.vect.region<-intersect.and.aggregate.vectors(
		prepped.boundary.vect,boundary.name,treatments,patch.name[1],agg.name[1],agg.code[1],prepped.boundary.vect,boundary.name[k])
	agg.treatments.vect.huc<-intersect.and.aggregate.vectors(
		prepped.zonal.summary.area.vect,vect.name,treatments,patch.name[1],agg.name[2],agg.code[2],prepped.boundary.vect,boundary.name[k])


	########### END READ IN AND PROCESS VECTORS ##############


	#################### ZONAL CALCULATIONS #######################

	whole.summary.area.zonal<-summarize.pixels.in.area.of.interest(
							diff,metric.name,prepped.zonal.summary.area.vect,vect.name,"zonal",diffname)
	treatments.zonal<-summarize.pixels.in.area.of.interest(
							diff,metric.name,agg.treatments.vect.huc,patch.name[1],"zonal",diffname)
	fires.zonal<-summarize.pixels.in.area.of.interest(
							diff,metric.name,agg.fires.vect.huc,patch.name[2],"zonal",diffname)

	diffname<-paste(metric.name,start.year,end.year,sep="_")

	#this is the output that will have the zonal mean for each spatial summary unit (e.g. HUC)
	#within the extent (boundary, e.g. Task Force region)
	#and then also includes the hucID (or other ID number for the individual spatial summary units)
	#and its area. the hucAverage is the actual value for the zonal average.

	all.zonal.results<-rbind(
		cbind(
		method=rep("Zonal",nrow(whole.summary.area.zonal)),
		metric=rep(metric.name,nrow(whole.summary.area.zonal)),
		boundary=rep(boundary.name[k],nrow(whole.summary.area.zonal)),
		subset=rep("WholeArea",nrow(whole.summary.area.zonal)),
		shapeID=as.data.frame(whole.summary.area.zonal)[,agg.code[2]],
		rasterAverage=as.data.frame(whole.summary.area.zonal)[,diffname]
		),
		cbind(
		method=rep("Zonal",nrow(treatments.zonal)),
		metric=rep(metric.name,nrow(treatments.zonal)),
		boundary=rep(boundary.name[k],nrow(treatments.zonal)),
		subset=rep(patch.name[1],nrow(treatments.zonal)),
		shapeID=as.data.frame(treatments.zonal)[,agg.code[2]],
		rasterAverage=as.data.frame(treatments.zonal)[,diffname]
		),
		cbind(
		method=rep("Zonal",nrow(fires.zonal)),
		metric=rep(metric.name,nrow(fires.zonal)),
		boundary=rep(boundary.name[k],nrow(fires.zonal)),
		subset=rep(patch.name[2],nrow(fires.zonal)),
		shapeID=as.data.frame(fires.zonal)[,agg.code[2]],
		rasterAverage=as.data.frame(fires.zonal)[,diffname]
		)
	)

	#initial one, set up the data frame.
	if(k==1){
		write.table(all.zonal.results,paste("RawZonalCalcOutput_",diffname,"_",datetime,".csv",sep=""),
			sep = ",",quote = FALSE, col.names = TRUE, row.names = FALSE,na="NA") 
	} else { #afterwards, just append it
		write.table(all.zonal.results,paste("RawZonalCalcOutput_",diffname,"_",datetime,".csv",sep=""),
			sep = ",",quote = FALSE, col.names = FALSE, row.names = FALSE,na="NA",append=TRUE)
	}

	################### END ZONAL CALCS ######################


	################### BEGIN GLOBAL CALCS ######################

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

	#initial one, set up the data frame.
	if(k==1){
		write.table(all.global.results,paste("GlobalCalcOutput_",diffname,"_",datetime,".csv",sep=""),
			sep = ",",quote = FALSE, col.names = TRUE, row.names = FALSE,na="NA") 
	} else { #afterwards, just append it
		write.table(all.global.results,paste("GlobalCalcOutput_",diffname,"_",datetime,".csv",sep=""),
			sep = ",",quote = FALSE, col.names = FALSE, row.names = FALSE,na="NA",append=TRUE)
	}


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


} #end boundary regions loop (k)

########### END INITIAL ANALYSIS AND DATA VIS ############
##########################################################
##########################################################

timer.end<-Sys.time()

time.total<-timer.end-timer.start
print(time.total)

