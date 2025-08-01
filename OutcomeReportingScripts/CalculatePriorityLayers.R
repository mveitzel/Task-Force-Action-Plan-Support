#CalculatePriorityLayers.R

  # #---------- WHP read in and process-------------------------*
  #2024 Wildfire Hazard Potential from CALFIRE
  whp.rast <- rast(paste(loc.data,"PriorityLayers/whp_classified_20240906.tif",sep=""))
  whp.proj.rast<-check.crs.match(whp.rast,whp.rast) #for this layer only, use itself as reference
  #High fire risk is hazard classes 4 (high) and 5 (very high), as in SIG scenario modeling
  whp.priority.rast<-whp.rast
  whp.priority.rast[whp.rast %in% c(4,5)]<-1
  whp.priority.rast[whp.priority.rast!=1]<-0
  writeRaster(whp.priority.rast,paste(loc.data,"PriorityLayers/FinalPriorityLayers/WHPpriority_CALFIREproj.tif",sep=""))


  # #---------- Drought Vulnerability read in and process---------*

  # CECS drought vulnerability, in Oct 2020 before treatments started in January
  dv.rast<-rast(paste(loc.data,"CECS_Data/CECS_CAWide_Vulner_TreeDieoff_SPI-2_2020_V250614.tif",sep=""))
  #check CRS
  dv.proj.rast<-check.crs.match(reference.rast,dv.rast) #of course it's hilarious that this actually is the reference rast
  ##          Set any thresholds, add any buffers, etc
  # the layer was vetted using the 2012-2014 drought, trees that did die then had a drought
  # vulnerability higher than 10,000
  dv.priority.rast<-dv.proj.rast
  dv.priority.rast[dv.proj.rast> 10000]<-1
  dv.priority.rast[dv.priority.rast!=1]<-0
  writeRaster(dv.priority.rast,paste(loc.data,"PriorityLayers/FinalPriorityLayers/DroughtVulnerabilityPriority_CECSproj.tif",sep=""))

  # #---------- Flame Length read in and process---------*

  # CECS flame length from FLAMMAP, in Oct 2020 before treatments started in January
  fl.rast<-rast(paste(loc.data,"CECS_Data/CECS_CAWide_Fire_FlamMap_FL_2020_V250324.tif",sep=""))
  #check CRS
  fl.proj.rast<-check.crs.match(reference.rast,fl.rast)
  ##          Set any thresholds, add any buffers, etc
  # Everyone seems to agree that flame length above 8 feet is likely to be a high-severity or hard to control fire
  #convert from meters to feet and undo the storage multiplicative factor:
  #"Units are FL 0.01 m" -> *3.28084/100
  fl.proj.ft.rast<-fl.proj.rast*0.0328084
  fl.priority.rast<-fl.proj.ft.rast
  fl.priority.rast[fl.proj.ft.rast > 8 ]<-1
  fl.priority.rast[fl.priority.rast!=1]<-0
  writeRaster(fl.priority.rast,paste(loc.data,"PriorityLayers/FinalPriorityLayers/FlameLengthPriority_CECSproj.tif",sep=""))

  # #---------------- Critical Habitat -------------------------#

  #from CDFW
  #"summed up as a 1-5 ranking, in which hexagons receiving a 5 represent the 20% of areas with the highest values within a given ecoregion."
  cr.vect<-vect(paste(loc.data,"PriorityLayers/ACE_SpeciesBiodiversity.shp",sep=""))
  #select 4 and 5, 40% highest values, which is preliminary SB 63 reporting statement in the text
  cr.proj.vect<-check.crs.match(reference.rast,cr.vect)
  cr.rast<-rasterize(cr.proj.vect,reference.rast,field="SpBioRnkEc")
  cr.pri.rast<-cr.rast
  cr.pri.rast[cr.rast %in% c(4,5)]<-1
  cr.pri.rast[cr.pri.rast!=1]<-0
  writeRaster(cr.pri.rast,paste(loc.data,"PriorityLayers/FinalPriorityLayers/CriticalHabitatPriority_CECSproj.tif",sep=""))

  # #---------------- Hydropower -------------------------#
  #layer from Han Guo and Roger Bales, the watersheds that feed power stations above a certain production level
  hy.vect<-vect(paste(loc.data,"PriorityLayers/shapefile_watershed_all_1.shp",sep=""))
  hy.proj.vect<-check.crs.match(reference.rast,hy.vect)
  hy.pri.rast<-rasterize(hy.proj.vect,reference.rast)
  hy.pri.rast[is.na(hy.pri.rast)]<-0
  writeRaster(hy.pri.rast,paste(loc.data,"PriorityLayers/FinalPriorityLayers/HydropowerPriority_CECSproj.tif",sep=""))

  # #---------------- Debris Flow Risk -------------------------#
  #from California Geological Survey in Department of Conservation
  #Combined probability of: P(F) probability of fire of 0.05 (because
  #the annual fire probability is max ~ 10%)
  # times
  # P(R>T50) probability of having a storm with rainfall exceeding the 
  #triggering threshold set equivalent to a 50% chance of debris flow occurrence
  # = 0.05*0.5 = 0.025
  de.vect<-vect(paste(loc.data,"PriorityLayers/ca_prefire_pfdf_basins.shp",sep=""))
  de.proj.vect<-check.crs.match(reference.rast,de.vect)
  de.proj.rast<-rasterize(de.proj.vect,reference.rast,field="pfprgt")
  de.pri.rast<-de.proj.rast
  de.pri.rast[de.proj.rast>(0.025)]<-1
  de.pri.rast[de.pri.rast!=1]<-0
  writeRaster(de.pri.rast,paste(loc.data,"PriorityLayers/FinalPriorityLayers/DebrisFlowPriority_CECSproj.tif",sep=""))

  # #---------------- High-Risk Shrubs -------------------------#

  #buffer with 1.5 miles, in line with 'wui influence' from CALFIRE logic
  #only in socal but reached out to Emma to see if it's been extended

  sh.rast<-rast(paste(loc.data,"PriorityLayers/SoCal_prepregen2020_202312_T2_v5.tif",sep=""))
  sh.proj.rast<-check.crs.match(reference.rast,sh.rast)
  sh.proj.vect<-as.polygons(sh.proj.rast)
  sh.vuln.vect<-sh.proj.vect[sh.proj.vect$SoCal_prepregen2020_202312_T2_v5==1,]
  #1 mile = 1609.34 meters
  sh.buff.vect<-buffer(sh.vuln.vect,width=1.5*1609.34)
  #intersect with road buffer

  sh.pri.vect<-intersect(road.buff.vect,sh.buff.vect)
  #then re-rasterize and add back in '0' for those not included
  sh.pri.rast<-rasterize(sh.pri.vect,reference.rast)
  sh.pri.rast[is.na(sh.pri.rast)]<-0
  writeRaster(sh.pri.rast,paste(loc.data,"PriorityLayers/FinalPriorityLayers/AtRiskShrubsPriority_CECSproj.tif",sep=""))

