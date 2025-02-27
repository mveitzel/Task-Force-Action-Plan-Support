#SummarizeChange.R

library("ggplot2")
library("foreign")
library("viridis")
#library("sf")
library('terra')

#Lauren makes a change
#########################################################
##########################################################
#################### FUNCTIONS ######################

#function to read in before and after rasters
#this function expects a single pair of rasters to calculate a difference between
#if 'conv' is NA, don't multiply by any factors
#beforename and aftername are just strings to go in print statements, etc
#before.filename and after.filename need to be actual filenames
#do specify the file path as 'location', e.g. use "getwd()" to pull your current
#location, or specify a different location
#TODO*** add a default argument for location to just be getwd()
read.checkcrs.convert.and.diff.rasters<-function(location,beforename,before.filename,aftername,after.filename,metr,conv){

	#read in the rasters
	print(paste("Reading in ",location,before.filename,sep=""))
	before.met<-rast(paste(location,before.filename,sep=""))
	print(paste("Reading in ",location,after.filename,sep=""))
	after.met<-rast(paste(location,after.filename,sep=""))

	#check that the Coordinate Reference System is the same
	#or set it to the before metric (the first argument)
	#(note that this may not handle datum transformations)
	after.met<-check.crs.match(before.met,after.met)

	#use a conversion factor for each raster if necessary
	if(!is.na(conv)){
		before.met.conv<-multiply.conversion.factor(beforename,before.met,metr,conv)
		after.met.conv<-multiply.conversion.factor(aftername,after.met,metr,conv)
		delta.met<-diff.rasters(beforename, before.met.conv,aftername,after.met.conv, metr)
	} else {
		print("No conversion factor applied")
		delta.met<-diff.rasters(beforename, before.met,aftername,after.met, metr)
	}
	return(list(before=before.met,after=after.met,diff=delta.met))
}

#Generate CECS filenames to read in rasters
generate.CECS.filename<-function(metrname,yearname,datavintage){
	#generate the CECS filenames for this metric, year, and version/vintage
	metr.nm<-paste("CECS_CAWide_",metrname,"_",yearname,"_V",datavintage,".tif",sep="")
	print(paste("Preparing to read in ",metr.nm,sep=""))
	return(metr.nm)
}

#use the conversion factor (multiplicative) -- some CECS and other
#metrics are in different units than are typically reported
#or are multiplied by a factor of 100 or 1000 in order to be
#stored as integers.  Look up the documentation to check what 
#the conversion should be
#expects a single rastrer
multiply.conversion.factor<-function(which.rast, met.rast, metname,conv.fact){
	print(paste("Converting raster, multiplying (",which.rast,") ",metname, " by",conv.fact),sep="")
	conv.rast<-met.rast*conv.fact
	return(conv.rast)
}

#function to diff two rasters - for scenario modeling, 'before' is a base
#case and 'after' is one of the scenarios; or 'before' is an earlier time 
#step and 'after' is a later time step.  For outcome reporting, 'before' is
#before treatments, and 'after' is after treatments.
#'before' is the name, and 'before.met' is the actual raster
diff.rasters<-function(before,before.rast,after,after.rast,metric){

	#this function expects a single pair of rasters to calculate a difference between
	#set the layer name 
	dffname<-paste(metric,before,after,sep="__")

	#calculate the difference
	print(paste("Subtracting ",after," from ",before," for ",metric),sep="")
	delta<-after.rast-before.rast
	names(delta)<-dffname

	return(delta)
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

