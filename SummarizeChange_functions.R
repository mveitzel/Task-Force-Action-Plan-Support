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

#PATCH -- this refers to areas that are not extensive/don't cover the whole area
#most commonly this is outcome reporting referring to fire footprints or treatment areas

#########################################################
##########################################################
#################### FUNCTIONS ######################

	#Generate CECS filenames to read in rasters
	generate.CECS.filename<-function(metrname,yearname,datavintage){
		#generate the CECS filenames for this metric, year, and version/vintage
		metr.nm<-paste("CECS_Data/CECS_CAWide_",metrname,"_",yearname,"_V",datavintage,".tif",sep="")
		print(paste("Preparing to read in ",metr.nm,sep=""))
		return(metr.nm)
	}


	#just a little function that combines the file system string
	#with the filename (raster.file is the filename, raster.name is
	#the human readable name for the print statement) and calls rast
	read.in.raster<-function(loc,raster.file,raster.name){
			print(paste("Reading in ", raster.name," at ",loc,raster.file,sep=""))
	  	raster1.rast<-rast(paste(loc,raster.file,sep=""))
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


	read.and.prepare.boundary.vector<-function(bdry.shape,bdry.name,ref.rast){
		print("Starting read.and.prepare.boundary.vector")
	  boundary.vect<-vect(bdry.shape)
	  #returns layers with both projected to first argument's CRS
	  boundary.vect.proj<-check.crs.match(ref.rast,boundary.vect)
	  print(paste(bdry.name," read in and processed.",sep=""))
	 	return(boundary.vect.proj)
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


	#this function filters patches for e.g. treatment types and date ranges
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

	    #leaving the commented out parts if someone wants to use different criteria 
	    # for treatments and fires with durations overlapping the start and end year
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

