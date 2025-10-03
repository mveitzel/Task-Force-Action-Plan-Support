#SummarizeChange.R

library("ggplot2")
library("foreign")
library("viridis")
#library("sf")
library('terra')

###################  GLOSSARY ##########################

#raster.name refers to a human readable phrase for a raster layer
#raster.file refers to the filename of the raster
#raster.rast refers to the raster object itself in memory

#"name" means human-readable phrase to describe an element of 
# the analysis, e.g. for printing out
#"code" means short abbreviation of an element of analysis, 
# e.g. for a filename or needs to match a column name (often 
# a 2-3 letter code)
#"shape" means an actual file name for a shapefile/vector dataset
#"vect" means the vector object itself in memory

#METRIC -- this will be its own data frame and analysis
#metric.code is a short code, usually should be the code that
#is in the file
#metric.name is what will be displayed in figures. 

#BOUNDARY -- this may get looped through so might have more than one element
#but does assume a single shapefile at a time
#This is the filename for whatever your cropped boundary is (meaning the cookie
#cutter shape)
# boundary.shape is the filename, boundary.code is the 2-3 character code, and 
# boundary.name is the human-readable name for the boundary we're cropping to

#ZONAL SUMMARY UNIT -- the unit we will do zonal averages or other summaries for
#Filename for whatever your summary unit is
#e.g. zonal.summary.shape is the filename, zonal.summary.name is human readable,
# and zonal.summary.code is a short 2-3 character code for filenames and column headers

#PATCH -- this refers to areas that are not extensive/don't cover the whole area
#most commonly this is outcome reporting referring to fire footprints or treatment areas

#########################################################
##########################################################
#################### FUNCTIONS ######################

# MV TODO: I think this function can be deprecated because we do this in
# a number of different smaller functions now

# #function to read in raster1 and raster2 rasters
# #this function expects a single pair of rasters to calculate a difference between
# #if 'conv' is NA, don't multiply by any factors
# #raster1name and raster2name are just strings to go in print statements, etc
# #raster1.filename and raster2.filename need to be actual filenames
# #do specify the file path as 'location', e.g. use "getwd()" to pull your current
# #location, or specify a different location
# #TODO*** add a default argument for location to just be getwd()
# #returns a single raster
# read.checkcrs.convert.and.diff.rasters<-function(location,raster1name,raster1.filename,raster2name,raster2.filename,metr,conv){

# 	#read in the rasters
# 	print(paste("Reading in ",location,raster1.filename,sep=""))
# 	raster1.met<-rast(paste(location,raster1.filename,sep=""))
# 	print(paste("Reading in ",location,raster2.filename,sep=""))
# 	raster2.met<-rast(paste(location,raster2.filename,sep=""))

# 	#check that the Coordinate Reference System is the same
# 	#or set it to the raster1 metric (the first argument)
# 	#(note that this may not handle datum transformations)
# 	raster2.met<-check.crs.match(raster1.met,raster2.met)

# 	#use a conversion factor for each raster if necessary
# 	if(!is.na(conv)){
# 		raster1.met.conv<-multiply.conversion.factor(raster1name,raster1.met,metr,conv)
# 		raster2.met.conv<-multiply.conversion.factor(raster2name,raster2.met,metr,conv)
# 		delta.met<-diff.rasters(raster1name, raster1.met.conv,raster2name,raster2.met.conv, metr)
# 	} else {
# 		print("No conversion factor applied")
# 		delta.met<-diff.rasters(raster1name, raster1.met,raster2name,raster2.met, metr)
# 	}
# 	return(delta.met)
# }

#Generate CECS filenames to read in rasters
generate.CECS.filename<-function(metrname,yearname,datavintage){
	#generate the CECS filenames for this metric, year, and version/vintage
	metr.nm<-paste("CECS_Data/CECS_CAWide_",metrname,"_",yearname,"_V",datavintage,".tif",sep="")
	print(paste("Preparing to read in ",metr.nm,sep=""))
	return(metr.nm)
}

# #***LM TODO: do we need this function?
# #Generate SIG filenames to read in rasters
# generate.SIG.filename<-function(additional.wd.folders, scenario.fldr, scenario.nm,year.nm, metric, data.typ, dte.time){
#   #generate the SIG filenames for this metric, year, and scenario numbers
#   metr.nm<-paste(additional.wd.folders, "treated_",scenario.nm,"_","year","_", year.nm, "_",metric, "_", data.typ, "_", dte.time,".tif",sep="")
#   print(paste("Preparing to read in: ",metr.nm,sep=""))
#   return(metr.nm)
# }

#Parse SIG filenames to read in FVS rasters
parse.SIG.fvs.filenames<-function(filenames){
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

#Parse SIG filenames to read in FIRE RESULTS rasters
parse.SIG.fireresults.filenames<-function(filenames){
  filenames.df<-data.frame(scenario=character(),
                           year=character(),
                           metric=character(),
                           filename=character(),stringsAsFactors=FALSE)
  for(i in 1:nrow(filenames)){
    filenames.df[i,]<-c(
      paste("S_",sub(".*scenario_(\\d{1}).*", "\\1", filenames[i,],perl=TRUE),sep=""),
      sub(".*(\\d{4}).*", "\\1", filenames[i,],perl=TRUE),
      sub(".*\\d{4}_([^_]+)_.*", "\\1", filenames[i,],perl=TRUE),
      filenames[i,])
  }
  return(filenames.df)
}

#just a little function that combines the file system string
#with the filename (raster.file is the filename, raster.name is
#the human readable name for the print statement) and calls rast
read.in.raster<-function(loc,raster.file,raster.name){
		print(paste("Reading in ", raster.name," at ",loc,raster.file,sep=""))
  	raster1.rast<-rast(paste(loc,raster.file,sep=""))
}

#use the conversion factor (multiplicative) -- some CECS and other
#metrics are in different units than are typically reported
#or are multiplied by a factor of 100 or 1000 in order to be
#stored as integers.  Look up the documentation to check what 
#the conversion should be
#expects a single raster, 'rast.name' is the user-readable raster (or metric) name, rast.rast 
#is the actual raster object
#if you specify NA for the conversion factor, then the function just
#returns the original raster
multiply.conversion.factor<-function(rast.name, rast.rast,conv.fact){
	if(!is.na(conv.fact)){
		print(paste("Converting raster, multiplying (",rast.name,") ", " by",conv.fact),sep="")
		conv.rast<-rast.rast*conv.fact
	} else {
		print("No conversion factor specified.")
		conv.rast<-rast.rast
	}
	return(conv.rast)
}

#LM TODO: is this a good example for scenarios? let's adjust the comment text appropriately
#function to diff two rasters - for scenario modeling, 'raster1' is a base
#case and 'raster2' is one of the scenarios; or 'raster1' is an earlier time 
#step and 'raster2' is a later time step.  For outcome reporting, 'raster1' is
#before treatments, and 'raster2' is after treatments.
#'raster1.name' is the human readable name, and 'raster1.rast' is the actual raster object
#'metric' needs to match the 'diffname' metric name, and is used in the print statement
diff.rasters<-function(raster1.name,raster1.rast,raster2.name,raster2.rast,metric){

	#this function expects a single pair of rasters to calculate a difference between
	#set the layer name 
	dffname<-paste(metric,raster1.name,raster2.name,sep="_")

	#calculate the difference
	print(paste("Subtracting ",raster1.name," from ",raster2.name," for ",metric," (",raster2.name," minus ",raster1.name,")"),sep="")
	delta<-raster2.rast-raster1.rast
	names(delta)<-dffname

	return(delta)
}


#returns second layer (dat.comp) projected to dat.ref (first argument)'s Coordinate Reference System
#if CRS matches, just passes through original second layer (dat.comp)
check.crs.match<-function(dat.ref,dat.comp,resample.method){
	#check crs/projection is the same for both
	crs.match<-identical(crs(dat.ref),crs(dat.comp))
	if(crs.match){
		print(paste("Coordinate Reference System match? ",crs.match,sep=""))
	} else {
		print("Set second layer's CRS to first layer's CRS")
		if(class(dat.comp)=="SpatRaster"){
				dat.comp<-project(dat.comp,dat.ref, method=resample.method,threads=TRUE)
			}else if(class(dat.comp)=="SpatVector"){
				dat.comp<-project(dat.comp,dat.ref)
			}
		crs.match2<-identical(crs(dat.ref),crs(dat.comp))
		print(paste("Coordinate Reference System match? ",crs.match2,sep=""))
	}
	return(dat.comp)
}

#these may get deprecated if we go with boxplots in general
#but leaving function here just in case we need it
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

#TODO*** I think I made this one too complicated so might not be helpful
#This function assumes you'll use a vector or raster
#to subset or mask out parts of your input raster
#two steps to make sure the extent of the raster is the 
#extent of the subset mask, and then do the mask
#to assign NAs to the parts outside the vector
#or to the NAs in the raster outside the area of interest
#the function also checks CRS compatibility and crops the mask to the boundary
#if mask and mask.name are NA, then this just crops and masks the raster to the boundary
#(used for the global summary)
subset.raster<-function(input.rast,name.rast,mask,mask.name,boundary.vect,boundary.name){
	print("Starting subset.raster")
	if(!is.na(mask)){
		#this assumes the mask is a raster, because "near" doesn't apply to vectors
		mask.proj<-check.crs.match(boundary.vect,mask,"near")
		mask<-crop(mask.proj,boundary.vect)
		print(paste("CRS checked for ",mask.name," and cropped to ",boundary.name,sep =""))
		}else{
			print(paste("No mask specified; using ",boundary.name,sep=""))
			mask<-boundary.vect
			mask.name<-boundary.name
		}
	cropped.rast<-crop(input.rast, mask)
	masked.rast<-mask(cropped.rast,mask)
	print(paste("Subsetted/masked ",name.rast," to include only ",mask.name, sep=""))
	return(masked.rast)
}
	

read.and.prepare.boundary.vector<-function(bdry.shape,bdry.name,ref.rast){
	print("Starting read.and.prepare.boundary.vector")
  boundary.vect<-vect(bdry.shape)
  #returns layers with both projected to first argument's CRS
  boundary.vect.proj<-check.crs.match(ref.rast,boundary.vect)
  print(paste(bdry.name," read in and processed.",sep=""))
 	return(boundary.vect.proj)
}

#this function just reads in a vector file and checks CRS
read.vector.and.check.crs<-function(bdr.vect,vect.shape,vect.name){
  print("Starting read.vector.and.check.crs")
	new.vect<-vect(vect.shape)
  print(paste(vect.name," read in and processed.",sep=""))
  #returns layers with the second projected to first argument's CRS
  vect.proj<-check.crs.match(bdr.vect,new.vect,"near")
 	return(vect.proj)
}

#This function both crops by the boundary vector and also recalculates areas
#because you will always want to have the correct area after doing a vector operation
crop.vector.by.boundary.and.recalc.area<-function(bdr.vect, bdr.name,vect.vect,vect.name){
 #crop using boundary
  print("Starting crop.vector.by.boundary.and.recalc.area")
	cropped.vect<-crop(vect.vect,bdr.vect)
  print(paste(vect.name," cropped to ",bdr.name,sep=""))
  #explicitly calculate the area of the cropped vector
  cropped.vect$post_crop_area_ha<-expanse(cropped.vect,unit="ha")
  print(paste(vect.name," Areas recalculated",sep=""))
  return(cropped.vect)
}

#if you don't specify a reference layer, it won't do the projection
read.and.check.crs.patch.vector<-function(ptch.shape,ptch.name,ptch.layer,ref.layer=NULL){
	print("Starting read.and.check.crs.patch.vector")
	ptch.vect<-vect(ptch.shape,layer=ptch.layer)
	print(paste(ptch.name, " read in, layer: ", ptch.layer,sep=""))
 	#returns layers with second projected to first argument's CRS
 	if(!is.null(ref.layer)){
		ptch.vect.proj<-check.crs.match(ref.layer,ptch.vect,"near")
		} else {
			print("Reference layer not specified; no reprojecting implemented")
			ptch.vect.proj<-ptch.vect
		}
	return(ptch.vect.proj)
}

#If you are summarizing vectors at the scale of a boundary poly, inter.vect/name and bound.vect/name will be repeated
intersect.and.aggregate.vectors<-function(inter.vect,inter.name,pch.vect,pch.name,ag.name,ag.code,bound.vect,bound.name){
  print("Starting intersect.and.aggregate.vectors")
	#first intersect the patch variable with a boundary polygon (e.g. region) or a zonal summary polygon (e.g. HUC)
	intersected.patch.vect<-intersect(inter.vect,pch.vect)
	print(paste(inter.name," intersected with ", pch.name, sep=""))
	#Then aggregate based on the salient ID of the intersecting layer, e.g. 'huc12' or 'Region'
	ag.pch.vect<-aggregate(intersected.patch.vect,by=ag.code,dissolve=TRUE)
	print(paste(pch.name," aggregated based on ",ag.name," (column name ",ag.code,")",sep=""))
	#Then crop (though intersect already basically does that), and recalculate the area of resulting polygons
	crop.agg.patch.vect<-crop.vector.by.boundary.and.recalc.area(bound.vect,bound.name,ag.pch.vect,pch.name)
	return(crop.agg.patch.vect)
}


#This version does zonal calculations for raster pixels that fall within the specified
#summary unit.  this only requires a single raster and outputs a single SpatVector with zonal results
#calculating 'mean' is hard-coded in
zonal.calculations<-function(rster.rast,zonal.sum.name,zonal.sum.vect){
  print("Starting zonal.calculations")
  summaryzonal.time<- system.time(zonal.calcs.vect<-zonal(rster.rast,zonal.sum.vect,fun="mean",as.polygons=TRUE,na.rm=TRUE) )
  print(paste("Zonal stats calculated for ",names(rster.rast), " using ", zonal.sum.name, sep=""))
  print(paste("Time to calculate zonal: ",round(summaryzonal.time[[1]]/60)," minute(s)", sep=""))
 
  return(zonal.calcs.vect)
}

#calculating 'mean' is hard-coded in
global.calculations<-function(rst.rast,rst.name,bound.vect,bound.name,dffnm){
	print("Starting global.calculations")
	sub.rast<-subset.raster(rst.rast,rst.name,NA,NA,bound.vect,bound.name)
	summaryglobal.time<-system.time(global.avg<-as.numeric(global(sub.rast,"mean",na.rm=TRUE)))
	print(paste("global stats calculated for ",names(rst.rast), " using ", bound.name, sep=""))
  print(paste("Time to calculate global: ",round(summaryglobal.time[[1]]/60)," minute(s)", sep=""))
	global.sum.vect<-bound.vect
	global.sum.vect[,dffnm]<-global.avg
	return(global.sum.vect)
}


#for the function that does the raster summary, have a method=global and method=zonal with an if statement
#rast.name is a metric name
summarize.pixels.in.area.of.interest<-function(rast.rast,rast.name,vect.vect,vect.name,method,dffnm){
	if(method=="zonal"){
			print(method)
			#call zonal summary
			result<-zonal.calculations(rast.rast,vect.name,vect.vect)
		}else if(method=="global"){
			print(method)
			#call global summary, vect.vect/name is expected to be the boundary name
			result<-global.calculations(rast.rast,rast.name,vect.vect,vect.name, dffnm)
		}
	return(result)
}


#this function filters patches for e.g. treatment types and date ranges
#TODO*** in a future iteration make this function take the 'is it a fire or a treatment' argument
# and choose appropriate column headers for it
filter.treatments<-function(treat.prp.vect,pol.targ,start.yr,end.yr="present"){
  print("Starting filter.treatments")
  #choose which list of activity types to filter treatments by.  Use NA if just subsetting by year
  if(!is.na(pol.targ)){
	  if(pol.targ!="Suppression Support"){
	     trt.subs.vect<-subset(treat.prp.vect,treat.prp.vect$ACTIVITY_DESCRIPTION %in% activity.list[[pol.targ]]$activities)
	     print(paste("Subsetted for policy goal: ",pol.targ,sep=""))
	     #note that for fuel breaks, need to use "primary objective" instead of activity description
	    }else if(pol.targ=="Suppression Support"){ 
	     trt.subs.vect<-subset(treat.prp.vect,treat.prp.vect$PRIMARY_OBJECTIVE %in% activity.list[[pol.targ]]$activities)
	     print(paste("Subsetted for policy goal: ",pol.targ,sep=""))
	   	}
	   }else{
	      print("No policy target indicated; subsetting only for year.")
	      trt.subs.vect<-treat.prp.vect
    }

  if(end.yr!="present"){
  		#most exclusive criteria - starts after the initial date and ends before the final date
  		trtmnts<-trt.subs.vect[format(as.Date(trt.subs.vect$ACTIVITY_START),"%Y-%m-%d")>start.yr &
           format(as.Date(trt.subs.vect$ACTIVITY_END),"%Y-%m-%d")<end.yr ,]
  		#most inclusive criteria - ends after the initial date, and starts before the final date (maybe should be 'or')
#      trtmnts<-trt.subs.vect[format(as.Date(trt.subs.vect$ACTIVITY_START),"%Y-%m-%d")<end.yr &
#           format(as.Date(trt.subs.vect$ACTIVITY_END),"%Y-%m-%d")>start.yr ,]
      print(paste("Subsetted between ",start.yr," and ",end.yr,sep=""))
    }else{
    	#most exclusive criteria - starts after the initial date
      trtmnts<-trt.subs.vect[format(as.Date(trt.subs.vect$ACTIVITY_START),"%Y-%m-%d")>start.yr,]
  		#most inclusive criteria - ends after the initial date
 #     trtmnts<-trt.subs.vect[format(as.Date(trt.subs.vect$ACTIVITY_END),"%Y-%m-%d")>start.yr,]
      print(paste("Treatments subsetted starting at ",start.yr," through present",sep=""))
    }
  return(trtmnts)
}


filter.fires<-function(fire.vect,start.yr,end.yr){
  	print("Starting filter.fires")
  	#most exclusive criteria - fire starts after the initial date and ends before the final date
    fire.filt.vect<-fire.vect[format(as.Date(fire.vect$ALARM_DATE),"%Y-%m-%d")>start.yr &
    		format(as.Date(fire.vect$CONT_DATE),"%Y-%m-%d")<end.yr ,]
  	#most inclusive criteria - fire ends after the initial date, and starts before the final date (maybe should be 'or')
#    fire.filt.vect<-fire.vect[format(as.Date(fire.vect$ALARM_DATE),"%Y-%m-%d")<end.yr &
#       format(as.Date(fire.vect$CONT_DATE),"%Y-%m-%d")>start.yr ,]
    print(paste("Fires subsetted between ",start.yr," and ",end.yr,sep=""))
  return(fire.filt.vect)
}

#to get a total here, instead of treatments for trt.rast, put in the mask raster name
raster.math.proportion.calc<-function(pri.rast,pri.name,trt.rast,pol.name,bdry.nm){
	print(pri.name)
  total.area<-pri.rast*trt.rast
  priority.rast<-total.area
  priority.rast[priority.rast==0]<-NA
  print("Calculating global sum of priority area within total area")
  #convert from 30-m pixels to acres 
  priority<-as.numeric(global(priority.rast,"notNA"))*30*30*0.000247105
  print("Calculating global sum of total area")
  #convert from 30-m pixels to acres 
  total<-as.numeric(global(total.area,"notNA"))*30*30*0.000247105
  print(paste("Raster math calc complete for ",pri.name," for policy objective ",pol.name," within ",bdry.nm,sep=""))
  return(c(priority,total,priority/total))
}

    crosstab.calc<-function(pri.rast,pri.name,trt.rast,pol.name,bdry.nm){
      names(pri.rast)<-pri.name
      print(names(pri.rast))
      crosstab.time<- system.time(crosstab.result<-crosstab(c(pri.rast,trt.rast)) )
      print(paste("Crosstab calc complete for ",pri.name," for policy objective ",pol.name," within ",bdry.nm,sep=""))
      print(paste("Time to calculate crosstab: ",round(crosstab.time[[1]]/60)," minute(s)", sep=""))
      result<-as.data.frame(crosstab.result)
      #convert from 30-m pixels to acres 
      result$area<-result$Freq*30*30*0.000247105
#      print(result)
      prop.pri<-result$area[result[,pri.name]==1]/sum(result$area)
      #print(c(result$area[result[,pri.name]==1],sum(result$area),prop.pri))
      return(c(result$area[result[,pri.name]==1],sum(result$area),prop.pri))
    }



#function to do this, takes policy target and metric name and boundary name to construct vector file name,
#name of the raster to rasterize by & check projection for, name of the mask (including none), 
#name of the priority layershrub.whp.rast
#doesn't return anything but writes to the file.
#if there's no mask, use 'none' for the mask name and leave the last argument blank.
rasterize.mask.calculate.proportions<-function(ct,pr.rast,m.name,pol.t,b.name,b.shape,ref.rast,msk.name="none",msk.rast=NULL){
    print("Starting rasterize.mask.calculate.proportions")
    #read in the appropriate pre-filtered, pre-aggregated vector file
    agg.treat.vect<-vect(paste(loc.data,"IntermediateFiles/AggregatedVectors/Treatments_",
      b.name,"_",pol.t,"_",start.year,"-present.shp",sep=""))
    print(paste(loc.data,"IntermediateFiles/AggregatedVectors/Treatments_",
      b.name,"_",pol.t,"_",start.year,"-present.shp Read in",sep=""))
    #check CRS match with appropriate projection
    treat.proj.vect<-check.crs.match(ref.rast,agg.treat.vect)
    #rasterize treatment layer first
    print("Rasterizing treatments")
    rasterize.time<-system.time(treat.rast<-rasterize(treat.proj.vect,ref.rast)) 
    print(paste("Time to rasterize treatments: ",round(rasterize.time[[1]]/60)," minute(s)", sep=""))

    if(msk.name!="none"){
      print("Stratifying using mask")
      #then stratify as necessary - subset treatments for appripriate mask
      treat.strat.rast<-treat.rast*msk.rast #using the appropriate projection version
      print("Completed stratification using mask")
    } else {
      treat.strat.rast<-treat.rast
      print("No mask specified")
    }

    print("Starting raster math calculation of priority proportions in treatments")
    rast.math.time<-system.time(rastmath.result<-raster.math.proportion.calc(pr.rast, m.name,treat.strat.rast,pol.t , b.name))
    print(paste("Time to do raster math for treatments: ",round(rast.math.time[[1]]/60)," minute(s)", sep=""))
    print(rastmath.result)
    targeted.effort.results[ct,]<-c(b.name,pol.t,m.name,msk.name,"Treatments",rastmath.result)
    ct<-ct+1
    
    print(paste("Starting raster math calculation of priority proportions in ",msk.name,sep=""))
    #clip the mask for the region (treatments have already had this done in preprocessing)
    bound<-vect(b.shape)
    bound.proj<-check.crs.match(ref.rast,bound)
    msk.rast.clip<-mask(msk.rast,bound.proj)
    rast.math.time<-system.time(rastmath.total.result<-raster.math.proportion.calc(pr.rast, m.name,msk.rast.clip,pol.t , b.name))
    print(paste("Time to do raster math for ",msk.name,": ",round(rast.math.time[[1]]/60)," minute(s)", sep=""))
    print(rastmath.total.result)
    targeted.effort.results[ct,]<-c(b.name,pol.t,m.name,msk.name,"TotalArea",rastmath.total.result)
    write.table(targeted.effort.results,paste("TargetedEffortResults_",datestamp,".csv",sep=""),append=TRUE,sep=",")
    print(paste("Wrote treatments, ",msk.name,", & priority proportions results to file",sep=""))
    ct<-ct+1
 		return(ct)
  }

################ END FUNCTIONS #######################
##########################################################
##########################################################

