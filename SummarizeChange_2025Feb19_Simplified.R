#SummarizeChange.R

library("ggplot2")
library("foreign")
library("viridis")
#library("sf")
library('terra')

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


##########################################################
##########################################################
#################### FUNCTIONS ######################

#function to read in before and after CECS layers
read.CECS.and.diff.rasters<-function(location,before.yr,after.yr,metric.name,vintage,conv){

	#this function expects a single pair of rasters to calculate a difference between

	#generate the CECS filenames for this metric, year, and version/vintage
	before.metric.nm<-paste("CECS_CAWide_",metric.name,"_",before.yr,"_V",vintage,".tif",sep="")
	after.metric.nm<-paste("CECS_CAWide_",metric.name,"_",after.yr,"_V",vintage,".tif",sep="")
	print(paste("Reading in ",before.metric.nm,sep=""))
	print(paste("Reading in ",after.metric.nm,sep=""))

	#read in the rasters
	before.met<-rast(paste(location,before.metric.nm,sep=""))
	after.met<-rast(paste(location,after.metric.nm,sep=""))

	#check that the Coordinate Reference System is the same
	#or set it to the before metric (the first argument)
	#(note that this may not handle datum transformations)
	after.met<-check.crs.match(before.met,after.met)

	#use the conversion factor (multiplicative) -- some CECS
	#metrics are in different units than are typically reported
	#or are multiplied by a factor of 100 or 1000 in order to be
	#stored as integers.  Look up the documentation to check what 
	#the conversion should be
	print(paste("Converting raster, multiplying 'before' (",before.yr,") ",metric.name, " by",conv),sep="")
	before.met<-before.met*conv
	print(paste("Converting raster, multiplying 'after' (",after.yr,") ",metric.name, " by",conv),sep="")
	after.met<-after.met*conv

	#calculate the difference
	dffname<-paste(metric,before.yr,after.yr,sep="__")

	#diff calc
	print(paste("Subtracting ",after.yr," from ",before.yr," for ",metric.name),sep="")
	delta.met<-after.met-before.met
	names(delta.met)<-dffname

	return(list(before=before.met,after=after.met,diff=delta.met))
}

#returns layers with both projected to dat1 (first argument)'s Coordinate Reference System
check.crs.match<-function(dat1,dat2){
	#check crs/projection is the same for both
	crs.match<-identical(crs(dat1),crs(dat2))
	if(crs.match){
		print(paste("Coordinate Reference System match? ",crs.match,sep=""))
	} else {
		print("Set second layer's CRS to first layer's CRS")
		dat2<-project(dat2,crs(dat1))
		crs.match<-identical(crs(dat1),crs(dat2))
		print(paste("Coordinate Reference System match? ",crs.match,sep=""))
	}
	return(dat2)
}


#Make the histogram plots
plot.results<-function(dt.dff,ttlestrng,xlbl,metnm,af.yr,bf.yr,sum.area,sumIDnm,lnd.clss,dffnm,dttme,reg){
	dt.dff.shp<-dt.dff
	dt.dff<-as.data.frame(dt.dff)
	
	ttlestrng<-paste(reg,"\n",metnm, " ",af.yr,"-",bf.yr," for ",sum.area," \nwithin each ",sumIDnm,
		"\n",lnd.clss," (mean ",round(mean(dt.dff[,dffnm],na.rm=TRUE),2), ")", sep="")		
	
	print(ttlestrng)
	plt<-ggplot(dt.dff,aes(x=dt.dff[,dffnm]))+
		geom_histogram()+
		geom_vline(xintercept = 0)+
		geom_vline(xintercept = mean(dt.dff[,dffnm],na.rm=TRUE), color = "blue", linewidth=1.5)+
		ggtitle(ttlestrng)+
		theme(text=element_text(size=10))+
		xlab(xlbl)
	plt
	hstnm<-paste(reg,dffnm,sumIDnm,lnd.clss,dttme,sum.area,".png",sep="_")
	ggsave(hstnm, units="in", width=4,height=2)

	#----- Make a map version of the whole area zonal calcs ------#
	if(sum.area=="SummaryUnit" && lnd.clss=="AllEcosystems"){
		png(paste(reg,dffnm,sumIDnm,lnd.clss,dttme,"map.png",sep="_"),width=5.5,height=6, units="in",res=150)
		plot(dt.dff.shp,dffnm,map.pal("viridis",10),main=ttlestrng)
		dev.off()
	}
}

#This function assumes you'll use another raster to subset 
#or mask out parts of your input raster
subset.raster<-function(input.raster,mask.raster){
	input.raster*mask.raster
}

#This function assumes you'll use a vector to subset or 
#mask out parts of your input raster
mask.rasters<-function(input.raster,mask.vector){
	mask(input.raster,mask.vector)
}

#This function reads in and processes the vector layers:
#crop.poly is the boundary polygon of your entire analysis region,
#whether that's the entire state, or a region, or a smaller area
#you need to read in the rasters (rstrs) to check the coordinate system
#sumPly is the spatial summary unit polygon (e.g. HUC), and the SumPlyNm 
#is the name of that column in the vector file (for example in the HUC 
#dataset, if it's a HUC10, the column name is 'huc10' and if it's a HUC12,
#the name is 'huc12')
read.in.and.process.vectors<-function(crop.poly,rstrs,sumPly,sumPlyNm){

	#Project, crop, calculate areas, and subset by date

	#-------------- State boundary for clipping ------------#
	#               (or region, as appropriate)             #

	cr.poly<-vect(crop.poly)
	#returns layers with both projected to first argument's CRS
	crop_poly_proj<-check.crs.match(rasters$before,cr.poly)
	print(paste(crop.poly," read in and processed.",sep=""))


	#---------------- Polygons to summarize over ------------#

	summary_poly<-vect(sumPly)
	#returns layers with both projected to first argument's CRS
	summary_poly_proj<-check.crs.match(rstrs$before,summary_poly)
	summary_ID_name<-sumPlyNm
	#crop to CA or regional boundary
	summary_poly_proj<-crop(summary_poly_proj,crop_poly_proj)
	#explicitly calculate the area of the HUC8
	summary_poly_proj$huc_area<-expanse(summary_poly_proj,unit="ha")
	print(paste(sumPly," read in and processed. Summary field: ",sumPlyNm,sep=""))

	return(list(boundary=crop_poly_proj,sumPoly=summary_poly_proj))

}


#This function actually does the zonal calculations for raster pixels that fall within
#the entire spatial summary area (e.g. HUC)
zonal.calculations<-function(rsters,rwVec,prepVec){

	 #----------- Zonal calcs for entire summary areas ------------#
	 #this takes a long time
	 summaryzonal.time<- system.time(zonal.stats.summarypoly<-zonal(rsters$diff,rwVec$sumPoly,fun="mean",as.polygons=TRUE,na.rm=TRUE) )
	print("Zonal stats calculated for whole summary unit (raw averages)")
	 print(summaryzonal.time/60)

		return(list(zonalAll=zonal.stats.summarypoly))
}


#this function assumes we want a 'current' vegetation classification
#for a given set of years - I'm choosing the 'before year' as the 
#one to base the classification on
#this is using SIG's crosswalk between CWHR type to a higher level
#aggregation of forestland, shrubland, and grassland
create.subset.masks<-function(rasts, b.yr,vintage,location){
	#Mike has a CECS layer with the Fveg codes
	CECSveg<-read.csv("CECS_Fveg_codes.csv",header=T)
	#SIG has a crosswalk between the Fveg codes and broad veg types they summarize in tabular form
	SIGveg<-read.csv("MAS 2 MASTER table - CWHR type crosswalk.csv",header=T)
	#this is the link between them
	crossw<-merge(CECSveg,SIGveg,by.x="WHR_Type",by.y="CWHR.abbreviation")

	tree.codes<-crossw$Code[crossw$Veg.type=="Tree"]
	shrub.codes<-crossw$Code[crossw$Veg.type=="Shrub"]
	grass.codes<-crossw$Code[crossw$Veg.type=="Grass"]

	#reading in Mike's layer to make sure that this is all aligned
	eco<-rast( paste(location,"CECS_CAWide_Veg_Fveg_",b.yr,"_V",vintage,".tif",sep=""),lyrs=1)
	eco<-check.crs.match(rasts$before,eco)

	#Now make the masks based on the codes
	grass.only<-eco %in% grass.codes
	print("Subset mask created for Grassland")
	tree.only<-eco %in% tree.codes
	print("Subset mask created for Forest")
	shrub.only<-eco %in% shrub.codes
	print("Subset mask created for Shrubland")
	#and set the '0's to NA
	grass.only[grass.only==0]<-NA
	tree.only[tree.only==0]<-NA
	shrub.only[shrub.only==0]<-NA
	print("Ecosystem subsets/masks created")

	return(list(tree.only=tree.only,shrub.only=shrub.only,grass.only=grass.only))

}

#This function does the subsetting of the rasters, leaving a new set of 
#rasters that only include pixels that are within one of the masks - one
#raster each for forest, shrub, grass, and WUI and non-WUI (wildland)
mask.subset.by.land.class<-function(rasts,sbst,msks){

	#save the 'all ecosystem' raster in one place
	rasters.all<-rasts

	#subset for each ecosystem
	if(sbst=="AllEcosystems"){
		rasts.subsetted<-rasters.all
		print(paste("Raster subset completed for ",sbst,sep=""))
	} else if (sbst=="Forest") {
		rasters.tree<-sapply(rasters.all,subset.raster,msks$tree.only)
		rasts.subsetted<-rasters.tree
		print(paste("Raster subset completed for ",sbst,sep=""))
	} else if (sbst=="Grassland"){
		rasters.grass<-sapply(rasters.all,subset.raster,msks$grass.only)
		rasts.subsetted<-rasters.grass
		print(paste("Raster subset completed for ",sbst,sep=""))
	} else if (sbst=="Shrubland"){
		rasters.shrub<-sapply(rasters.all,subset.raster,msks$shrub.only)
		rasts.subsetted<-rasters.shrub
		print(paste("Raster subset completed for ",sbst,sep=""))
	} else if (sbst=="Urban-WUI") {
		rasters.wuiurb<-sapply(rasters.all,mask.rasters,wui.urb.poly.proj)
		rasts.subsetted<-rasters.wuiurb
		print(paste("Raster subset completed for ",sbst,sep=""))
	} else if (sbst=="Wildland"){
		rasters.wuiwild<-sapply(rasters.all,mask.rasters,wui.wild.poly.proj)
		rasts.subsetted<-rasters.wuiwild
		print(paste("Raster subset completed for ",sbst,sep=""))
	}

	return(rasts.subsetted)
}

################ END FUNCTIONS #######################
##########################################################
##########################################################


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
	rasters<-read.CECS.and.diff.rasters(loc,before.year,after.year,metric,vint,conversion)

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

	rasters<-read.CECS.and.diff.rasters(loc,before.year,after.year,metric,vint,conversion)

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

	rasters<-read.CECS.and.diff.rasters(loc,before.year,after.year,metric,vint,conversion)

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

	rasters<-read.CECS.and.diff.rasters(loc,before.year,after.year,metric,vint,conversion)

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

	rasters<-read.CECS.and.diff.rasters(loc,before.year,after.year,metric,vint,conversion)

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

