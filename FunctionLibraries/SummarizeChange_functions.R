#SummarizeChange.R

library("ggplot2")
library("foreign")
library("viridis")
#library("sf")
library('terra')


#########################################################
##########################################################
#################### FUNCTIONS ######################

#function to read in raster1 and raster2 rasters
#this function expects a single pair of rasters to calculate a difference between
#if 'conv' is NA, don't multiply by any factors
#raster1name and raster2name are just strings to go in print statements, etc
#raster1.filename and raster2.filename need to be actual filenames
#do specify the file path as 'location', e.g. use "getwd()" to pull your current
#location, or specify a different location
#TODO*** add a default argument for location to just be getwd()
#returns a single raster
read.checkcrs.convert.and.diff.rasters<-function(location,raster1name,raster1.filename,raster2name,raster2.filename,metr,conv){

	#read in the rasters
	print(paste("Reading in ",location,raster1.filename,sep=""))
	raster1.met<-rast(paste(location,raster1.filename,sep=""))
	print(paste("Reading in ",location,raster2.filename,sep=""))
	raster2.met<-rast(paste(location,raster2.filename,sep=""))

	#check that the Coordinate Reference System is the same
	#or set it to the raster1 metric (the first argument)
	#(note that this may not handle datum transformations)
	raster2.met<-check.crs.match(raster1.met,raster2.met)

	#use a conversion factor for each raster if necessary
	if(!is.na(conv)){
		raster1.met.conv<-multiply.conversion.factor(raster1name,raster1.met,metr,conv)
		raster2.met.conv<-multiply.conversion.factor(raster2name,raster2.met,metr,conv)
		delta.met<-diff.rasters(raster1name, raster1.met.conv,raster2name,raster2.met.conv, metr)
	} else {
		print("No conversion factor applied")
		delta.met<-diff.rasters(raster1name, raster1.met,raster2name,raster2.met, metr)
	}
	return(delta.met)
}

#Generate CECS filenames to read in rasters
generate.CECS.filename<-function(metrname,yearname,datavintage){
	#generate the CECS filenames for this metric, year, and version/vintage
	metr.nm<-paste("CECS_CAWide_",metrname,"_",yearname,"_V",datavintage,".tif",sep="")
	print(paste("Preparing to read in ",metr.nm,sep=""))
	return(metr.nm)
}

#Generate SIG filenames to read in rasters
generate.SIG.filename<-function(additional.wd.folders, scenario.fldr, scenario.nm,year.nm, metric, data.typ, dte.time){
  #generate the CECS filenames for this metric, year, and version/vintage
  metr.nm<-paste(additional.wd.folders, "treated_",scenario.nm,"_","year","_", year.nm, "_",metric, "_", data.typ, "_", dte.time,".tif",sep="")
  print(paste("Preparing to read in: ",metr.nm,sep=""))
  return(metr.nm)
}

parse.SIG.filenames<-function(filenames){
  filenames.df<-data.frame(scenario=character(),
                           year=character(),
                           metric=character(),
                           filename=character(),stringsAsFactors=FALSE)
  for(i in 1:nrow(filenames)){
    filenames.df[i,]<-c(
      paste("S_",sub(".*scenario_(\\d{1}).*", "\\1", filenames[i,],perl=TRUE),sep=""),
      sub(".*year_(\\d{4}).*", "\\1", filenames[i,],perl=TRUE),
      sub(".*year_\\d{4}_([^_]+)_.*", "\\1", filenames[i,],perl=TRUE),
      filenames[i,])
  }
  return(filenames.df)
}

#use the conversion factor (multiplicative) -- some CECS and other
#metrics are in different units than are typically reported
#or are multiplied by a factor of 100 or 1000 in order to be
#stored as integers.  Look up the documentation to check what 
#the conversion should be
#expects a single rastrer
#if you specify NA for the conversion factor, then the function just
#returns the original raster
multiply.conversion.factor<-function(which.rast, met.rast, metname,conv.fact){
	if(!is.na(conv.fact)){
		print(paste("Converting raster, multiplying (",which.rast,") ",metname, " by",conv.fact),sep="")
		conv.rast<-met.rast*conv.fact
	} else {
		print("No conversion factor specified.")
		conv.rast<-met.rast
	}
	return(conv.rast)
}

#function to diff two rasters - for scenario modeling, 'raster1' is a base
#case and 'raster2' is one of the scenarios; or 'raster1' is an earlier time 
#step and 'raster2' is a later time step.  For outcome reporting, 'raster1' is
#before treatments, and 'raster2' is after treatments.
#'raster1' is the name, and 'raster1.met' is the actual raster
diff.rasters<-function(raster1,raster1.rast,raster2,raster2.rast,metric){

	#this function expects a single pair of rasters to calculate a difference between
	#set the layer name 
	#commenting out the more complex layer name and keeping them the same for each calculation
	#dffname<-paste(metric,raster1,raster2,sep="__")

	#*Changing the name to a more complex name makes joining dataframes from a list
	#*not work. So stick with the simpler "diff" name. 
  dffname<-"diff"

	#calculate the difference
	print(paste("Subtracting ",raster2," from ",raster1," for ",metric),sep="")
	delta<-raster2.rast-raster1.rast
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
plot.results<-function(dt.dff,ttlestrng,xlbl,metnm,af.yr,bf.yr,sum.area,sumIDnm,lnd.clss,dttme,reg){
	dt.dff.shp<-dt.dff
	dt.dff<-as.data.frame(dt.dff)
	
	ttlestrng<-paste(reg,"\n",metnm, " ",af.yr,"-",bf.yr," for ",sum.area," \nwithin each ",sumIDnm,
		"\n",lnd.clss," (mean ",round(mean(dt.dff[,"diff"],na.rm=TRUE),2), ")", sep="")		
	
	print(ttlestrng)
	plt<-ggplot(dt.dff,aes(x=dt.dff[,"diff"]))+
		geom_histogram()+
		geom_vline(xintercept = 0)+
		geom_vline(xintercept = mean(dt.dff[,"diff"],na.rm=TRUE), color = "blue", linewidth=1.5)+
		ggtitle(ttlestrng)+
		theme(text=element_text(size=10))+
		xlab(xlbl)
	plt
	hstnm<-paste(reg,"diff",sumIDnm,lnd.clss,dttme,sum.area,".png",sep="_")
	ggsave(hstnm, units="in", width=4,height=2)

	#----- Make a map version of the whole area zonal calcs ------#
	if(sum.area=="SummaryUnit" && lnd.clss=="AllEcosystems"){
		png(paste(reg,"diff",sumIDnm,lnd.clss,dttme,"map.png",sep="_"),width=5.5,height=6, units="in",res=150)
		plot(dt.dff.shp,"diff",map.pal("viridis",10),main=ttlestrng)
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

#TODO*** once you confirm the other functions work, remove this function
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
	crop_poly_proj<-check.crs.match(rstrs$raster1,cr.poly)
	print(paste(crop.poly," read in and processed.",sep=""))


	#---------------- Polygons to summarize over ------------#

	summary_poly<-vect(sumPly)
	#returns layers with both projected to first argument's CRS
	summary_poly_proj<-check.crs.match(rstrs$raster1,summary_poly)
	summary_ID_name<-sumPlyNm
	#crop to CA or regional boundary
	summary_poly_proj<-crop(summary_poly_proj,crop_poly_proj)
	#explicitly calculate the area of the HUC8
	summary_poly_proj$huc_area<-expanse(summary_poly_proj,unit="ha")
	print(paste(sumPly," read in and processed. Summary field: ",sumPlyNm,sep=""))

	return(list(boundary=crop_poly_proj,sumPoly=summary_poly_proj))

}

##########EDITED VERSION TO MAKE MORE GENERAL###############
#When trying to process a vector for a single raster (i.e. that has not been differenced
#and only includes one raster layer), the read.in.and.process.vectors function would
#have an error because it was looking for a specific layer with the "rsters" call. 
#This edited function does the same thing that read.in.and.process.vectors does
#but calls for a single raster rather than the stack of three that results from 
#the differencing function. You could use this version on a differenced raster
#but the 'rstr' call would require "differenced.raster.result$raster1" instead of 
#just "differenced.raster.result" which would contain "$raster1", "$raster2", and "$diff."

read.in.and.process.vectors.single.raster<-function(crop.poly,rstr,sumPly,sumPlyNm){
  
  #Project, crop, calculate areas, and subset by date
  
  #-------------- State boundary for clipping ------------#
  #               (or region, as appropriate)             #
  
  cr.poly<-vect(crop.poly)
  #returns layers with both projected to first argument's CRS
  crop_poly_proj<-check.crs.match(rstr,cr.poly)
  print(paste(crop.poly," read in and processed.",sep=""))
  
  
  #---------------- Polygons to summarize over ------------#
  
  summary_poly<-vect(sumPly)
  #returns layers with both projected to first argument's CRS
  summary_poly_proj<-check.crs.match(rstr,summary_poly)
  summary_ID_name<-sumPlyNm
  #crop to CA or regional boundary
  summary_poly_proj<-crop(summary_poly_proj,crop_poly_proj)
  #explicitly calculate the area of the HUC8
  summary_poly_proj$huc_area_ha<-expanse(summary_poly_proj,unit="ha")
  print(paste(sumPly," read in and processed. Summary field: ",sumPlyNm,sep=""))
  
  return(list(boundary=crop_poly_proj,sumPoly=summary_poly_proj))
  
}

#TODO*** once you confirm the other functions work, remove this function
#This function actually does the zonal calculations for raster pixels that fall within
#the entire spatial summary area (e.g. HUC)
zonal.calculations<-function(rsters,prepVec){

	 #----------- Zonal calcs for entire summary areas ------------#
	 #this takes a long time
	 summaryzonal.time<- system.time(zonal.stats.summarypoly<-zonal(rsters$diff,prepVec$sumPoly,fun="mean",as.polygons=TRUE,na.rm=TRUE) )
	print("Zonal stats calculated for whole summary unit (raw averages)")
	 print(summaryzonal.time/60)

		return(list(zonalAll=zonal.stats.summarypoly))
}


#############EDITED VERSION OF ZONAL CALCULATIONS FUNCTION######################
#This version does zonal calculations for raster pixels that fall within the specified
#summary unit. However, rather than requiring the result from differencing rasters 
#(which has 3 rasters - $raster1, $raster2, $diff), this only requires a single raster. 
#You could still use this version for the differenced result, but would have to 
#specify 'differenced.result$diff' in the 'rstr' call rather than 'differenced.result'. 
#outputs a single SpatVector with zonal results


zonal.calculations.single.raster<-function(rster,prepVec){
  
  #----------- Zonal calcs for entire summary areas ------------#
  #this takes a long time
  ###***TODO MV add in the print statement the name of the vector, which means adding an argument 
  ### specifying what the huc level is
  ### names(rster) should be the same for every loop so that the list can be joined after processing
  ### I think there needs to be a separate identifier for the printing statement. 
  summaryzonal.time<- system.time(zonal.stats.summarypoly<-zonal(rster,prepVec$sumPoly,fun="mean",as.polygons=TRUE,na.rm=TRUE) )
  print(paste("Zonal stats calculated for ",names(rster), sep=""))
  print(summaryzonal.time/60)
  
  return(zonal.stats.summarypoly)
}


#***TODO edit this one to take one raster of the veg classification, and a list of rasters to be masked
#this function assumes we want a 'current' vegetation classification
#for a given set of years - I'm choosing the 'before year' as the 
#one to base the classification on
#this is using SIG's crosswalk between CWHR type to a higher level
#aggregation of forestland, shrubland, and grassland
create.subset.masks<-function(rast1, rast.diff, b.yr,vintage,location){
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
	eco<-check.crs.match(rasts$raster1,eco)

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

