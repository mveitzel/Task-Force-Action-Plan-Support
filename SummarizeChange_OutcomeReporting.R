#SummarizeChange.R

library("ggplot2")
library("foreign")
library("viridis")
#library("sf")
library('terra')

source("SummarizeChange_functions.R")

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

setwd("D:/DropboxFiles/Dropbox/Professional/UCB_Battles/TreatmentEffectiveness")

#date stamp of this set of results - appended to all outputs to avoid overwriting older versions
datetime<-"2025Feb19"

#ending year of water year
before.year<-2020
after.year<-2023

#working folder location for the underlying data
#this analysis uses CECS data but any raster data
#should work, though if an underlying datum transformation
#needs to be done, just checking that the projection is 
#the same might not be enough
#also I am currently using CECS' version of Fveg for the
#vegetation classifications
loc<-"D:\\GIS_Large_Files\\CECS_Data\\"

#uncomment whichever metric you want to run, and then copy/paste
#or save and source the entire file to run this given metric
#alternatively add to the script a loop that runs through metrics
#choose metric
metricname<-"Runoff"
#metricname<-"Flame Length"
#metricname<-"Potential Species Richness"
#metricname<-"Possible Smoke Production"
#metricname<-"Live Carbon"

############ END GLOBAL PARAMETERS #################


#this section contains the specifics for each metric, including
#the CECS conversion factor and the actual function calls to 
# the raster differencing calculation

##########################################################
##########################################################
########### READ IN AND PROCESS RASTERS ##############


#-------- RUNOFF RASTER CALCS ---------#

if(metricname=="Runoff"){

	vint<-"240802"
	metric<-"WaterFlux_Runoff_SPI0"
	xlabel<-"Change in Runoff"
	#'units are mm/yr so no conversion factor
	conversion<-(1)

	#read in the rasters, convert them from CECS 'units', and difference them final minus initial (after minus before)
	
	#generate CECS file names
	before.yr.filename<-generate.CECS.filename(metric,before.year,vint)
	after.yr.filename<-generate.CECS.filename(metric,after.year,vint)

	rasters<-read.checkcrs.convert.and.diff.rasters(loc,before.year,before.yr.filename,after.year,after.yr.filename,metric,conversion)
	
	diffname<-paste(metric,before.year,after.year,sep="__")

}

#------ End runoff raster calcs ----------#



#---------- FLAME LENGTH RASTER CALCS -------------------#

if(metricname=="Flame Length"){
	vint<-"240802"
	metric<-"Fire_FlamMap_FL"
	xlabel<-"Average decrease in flame length (ft)"
	#'units are 0.01 m' so divide by 100, but want ft so multiply by 3.28084
	conversion<-(0.0328084)

	before.yr.filename<-generate.CECS.filename(metric,before.year,vint)
	after.yr.filename<-generate.CECS.filename(metric,after.year,vint)

	rasters<-read.checkcrs.convert.and.diff.rasters(loc,before.year,before.yr.filename,after.year,after.yr.filename,metric,conversion)

	diffname<-paste(metric,before.year,after.year,sep="__")

}

#------------ end flame length raster calcs -----------#


#-------- BIODIVERSITY RASTER CALCS ------------------#

if(metricname=="Potential Species Richness"){
	vint<-"240802"
	metric<-"Biodiv_WildlifeWeightedAll"
	xlabel<-"Change in potential species richness"
	#conversion factor - is there one for this variable?
	conversion<-1

	before.yr.filename<-generate.CECS.filename(metric,before.year,vint)
	after.yr.filename<-generate.CECS.filename(metric,after.year,vint)

	rasters<-read.checkcrs.convert.and.diff.rasters(loc,before.year,before.yr.filename,after.year,after.yr.filename,metric,conversion)

	diffname<-paste(metric,before.year,after.year,sep="__")

}

#------------ End biodiversity raster calcs ------------#


#-------- PM2.5 RASTER CALCS ------------------#

if(metricname=="Possible Smoke Production"){
	vint<-"240802"
	metric<-"Fire_FOFEM_PM25"
	xlabel<-"Change in possible smoke production"
	#units are lbs/acre so no conversion required
	conversion<-1

	before.yr.filename<-generate.CECS.filename(metric,before.year,vint)
	after.yr.filename<-generate.CECS.filename(metric,after.year,vint)

	rasters<-read.checkcrs.convert.and.diff.rasters(loc,before.year,before.yr.filename,after.year,after.yr.filename,metric,conversion)

	diffname<-paste(metric,before.year,after.year,sep="__")

}

#------------ End PM2.5 raster calcs ------------#


#----------------- CARBON RASTER CALCS -----------------#

if(metricname=="Live Carbon"){
	vint<-"240802"
	metric<-"ClimateForce_LiveAboveC"
	xlabel<-"Change in Live Carbon (Mg/ha)"
	#conversion factor - 'units are 10g/m^2' so divide by 10 to get Mg/ha
	conversion<-(1/10)

	before.yr.filename<-generate.CECS.filename(metric,before.year,vint)
	after.yr.filename<-generate.CECS.filename(metric,after.year,vint)

	rasters<-read.checkcrs.convert.and.diff.rasters(loc,before.year,before.yr.filename,after.year,after.yr.filename,metric,conversion)

	diffname<-paste(metric,before.year,after.year,sep="__")
}

#---------------- End carbon raster calcs ----------------------#


########### END READ IN AND PROCESS RASTERS ##############
##########################################################
##########################################################



########### READ IN AND PROCESS VECTORS ##############

###for the moment, we're removing this because we're not even doing ecosystems
# need to rerun this if you change the start year, so you get Mike's classifications for that year
# doing this outside the different huc levels

#masks<-create.subset.masks(rasters,before.year,vint,loc)

#I had to do some crazy stuff to pull out just the WUI classification from the FRAP layer SIG gave me
#so I'm coping that code in here if I need to do it again
#I could not get the raster to match with Mike's CECS rasters so I vectorized the WUI/non-WUI rasters

# wui_FRAP<-rast("WUI24_extract.tif")
# #For the FRAP layer via SIG, 2=intermix, 1=interface, and 3=influnce
# wui.urb<-wui_FRAP %in% c(2,1,3)
# # and 0 = notWUI (which includes urban and ag, but Mike has masked) = wildland
# wui.wild<-wui_FRAP == 0
# wui.urb.poly<-as.polygons(wui.urb,aggregate=TRUE)
# wui.wild.poly<-as.polygons(wui.wild,aggregate=TRUE)
# wui.urb.poly.proj<-check.crs.match(rasters$before,wui.urb.poly)
# wui.wild.poly.proj<-check.crs.match(rasters$before,wui.wild.poly)
# wui.urb.poly.proj<-wui.urb.poly.proj[wui.urb.poly.proj$Band_1==1,]
# wui.wild.poly.proj<-wui.wild.poly.proj[wui.wild.poly.proj$Band_1==1,]
# writeVector(wui.urb.poly.proj,"WUI_24_Urb_Poly_proj.shp",overwrite=TRUE)
# writeVector(wui.wild.poly.proj,"WUI_24_Wild_Poly_proj.shp",overwrite=TRUE)

wui.urb.poly.proj<-vect("WUI_24_Urb_Poly_proj.shp")
wui.wild.poly.proj<-vect("WUI_24_Wild_Poly_proj.shp")

#at some point might prefer this straight from the source, but for now just using this shps I made
# by downloading them from  
# https://gispublic.waterboards.ca.gov/portal/home/item.html?id=b6c1bab9acc148e7ac726e33c43402ee#overview
#huc4 seems to break memory allocation at the whole-summary-unit level, so I usually didn't run those
#this list of huc levels (or other spatial summary polygons) allows one to loop through different levels
#of spatial aggregation to do a sensitivity analysis, or to simply run it for different types of spatial
#summary polygons (e.g. HUCs and PODs).  You need to specify the file name (sum.Poly) and also the name 
# you'll want in the outputs (sum.Poly.name).  For October 10th, I used just the HUC10 summary level.

#sum.Poly<-c("HUC8.shp","HUC10.shp","HUC12.shp")
#sum.Poly.name<-c("huc8","huc10","huc12")
sum.Poly<-c("HUC10.shp")
sum.Poly.name<-c("huc10")

#I have manually exported each individual region from the task force's current boundaries
#This list will allow one to loop through multiple regions to do the entire analysis for.
#could be 'whole state' i.e. "AllCA" as below, or just a Task Force region, or a boundary
#like an RFFC grantee (e.g. Sierra Nevada Conservancy), or even something as small as a county
#as long as the extent of the analysis is bigger than the spatial summary unit (e.g. the HUC)
#you should be able to make it relatively small.  You'll just have fewer items to summarize (e.g.
#how many HUC12s are in Inyo County? that's how many items you'll have reporting on e.g. in the
#histograms and output datasets).
#for October 10th I only did the Task Force Sierra Region

#cropping<-c("CA_State.shp","Region_Sierra.shp", "Region_NorthernCA.shp", "Region_CentralCoast.shp", "Region_SouthernCA.shp")
#crop.nm<-c("AllCA","Sierra","North","Central","South")
cropping<-c("Region_Sierra.shp")
crop.nm<-c("Sierra")

#this will be one of the data outputs - a csv with just a mean for the histogram of each collection of HUCs (summary.unit), 
#each extent (crop.area), and each ecosystem type or WUI/non-WUI (land.class).  area.type refers to either
#the whole spatial summary unit (e.g. HUC), or the treatments within a HUC, or the fire footprints within the HUC
#but in this script, it will only ever be "summary unit"
zonal.means<-data.frame(metric=character(),crop.area=character(),summary.unit=character(),
	land.class=character(),area.type=character(),mean=numeric(),stringsAsFactors=FALSE)
#count<-1

for(k in 1:length(cropping)){ #loop through the extents e.g. all CA or each region
	print(paste("Start ", crop.nm[k]," loop"))
	for(j in 1:length(sum.Poly)){ #loop through different types of spatial summary unit (e.g. HUC levels)
		print(paste("Start ", sum.Poly.name[j]," loop"))

		#make sure you've run the rasters setup first, so you can grab the coordinate reference system
		#this needs to be run at this stage because it depends on the spatial summary unit (e.g. HUC level)
		raw.vectors<-read.in.and.process.vectors(crop.poly=cropping[k], 
			rstrs=rasters,sumPly=sum.Poly[j],sumPlyNm=sum.Poly.name[j])

		########### END READ IN AND PROCESS VECTORS ##############


		################ MASK/SUBSET BY ECOSYSTEM/WUI ################

		#here is where you might run the analysis for different parts of the state. For these analyses,
		#anything that is not, e.g., "forest" will be an NA value and not enter into the raster calculations
		#for the October 10th meeting, we did not stratify by ecosystem or WUI/non-WUI
		sub.set<-c("AllEcosystems")
		#sub.set<-c("Urban-WUI","AllEcosystems","Forest","Grassland","Shrubland","Wildland")

		#loop through Ecosystems

		for (i in 1:length(sub.set)) {
			print(paste("Start ", sub.set[i]," loop"))
			rasters.sub<-mask.subset.by.land.class(rasters,sub.set[i],masks)
	

			############ END MASK/SUBSET BY ECOSYSTEM/WUI ############


			#################### ZONAL CALCULATIONS #######################

			zonal.results<-zonal.calculations(rasters.sub,raw.vectors,prepped.vectors)

				#this is the output that will have the zonal mean for each spatial summary unit (e.g. HUC)
				#within the extent (cropping region, e.g. Task Force region), along with all the info about
				#what type of summary it is - what metric, what cropping region, what type of spatial summary
				#unit, what type of ecosystem/other mask was used (including none), and whether it is the entire
				#spatial summary unit (e.g. HUC), just fire footprints within that, or just treatments within that,
				#(in this script it is only ever the whole summary unit)
				#and then also includes the hucID (or other ID number for the individual spatial summary units)
				#and its area. the hucAverage is the actual value for the zonal average.
				all.zonal.results<-rbind(
					cbind(
					metric=rep(metricname,nrow(zonal.results$zonalAll)),
					crop.area=rep(crop.nm[k],nrow(zonal.results$zonalAll)),
					summary.unit=rep(sum.Poly.name[j],nrow(zonal.results$zonalAll)),
					land.class=rep(sub.set[i],nrow(zonal.results$zonalAll)),
					area.type=rep("SummaryUnit",nrow(zonal.results$zonalAll)),
					hucID=as.data.frame(zonal.results$zonalAll)[,sum.Poly.name[j]],
					hucArea=as.data.frame(zonal.results$zonalAll)$huc_area,
					hucAverage=as.data.frame(zonal.results$zonalAll)[,diffname]
					) 
				)
			#initial one, set up the data frame.
			if(i==1 & j==1 & k==1){
					write.table(all.zonal.results,paste("RawZonalCalcOutput_",diffname,"_",datetime,".csv",sep=""),
						sep = ",",quote = FALSE, col.names = TRUE, row.names = FALSE,na="NA") 
				} else { #afterwards, just append it
					write.table(all.zonal.results,paste("RawZonalCalcOutput_",diffname,"_",datetime,".csv",sep=""),
						sep = ",",quote = FALSE, col.names = FALSE, row.names = FALSE,na="NA",append=TRUE)
				}


			################### END ZONAL CALCS ######################


			#################### DATA VISUALIZATIONS ###################

			#---------------- Plots for whole summary area ---------------------#

			area_type<-"SummaryUnit"

			plot.results(dt.dff=zonal.results$zonalAll,	ttlestrng=titlestring,
				xlbl=xlabel, metnm=metricname, af.yr=after.year, bf.yr=before.year, sum.area=area_type,
				sumIDnm=sum.Poly.name[j], lnd.clss=sub.set[i], dffnm=diffname, dttme=datetime,reg=crop.nm[k])

			zonal.means[1,]<-c(metricname,crop.nm[k],sum.Poly.name[j],sub.set[i],area_type,
				mean(as.data.frame(zonal.results$zonalAll)[,diffname],na.rm=TRUE))
			write.table(zonal.means,paste("ZonalMeansOutput_",diffname,"_",datetime,".csv",sep=""),
						sep = ",",quote = FALSE, col.names = FALSE, row.names = FALSE,na="NA",append=TRUE)
			#count<-count+1


			########### END DATA VISUALIZATIONS ##############

		} # end ecosystem type/land classification type loop (i)

	} #end HUC scale loop (j)

} #end Cropped regions loop (k)

########### END INITIAL ANALYSIS AND DATA VIS ############
##########################################################
##########################################################

