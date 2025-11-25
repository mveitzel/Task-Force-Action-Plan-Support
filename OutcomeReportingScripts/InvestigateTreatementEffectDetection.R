#InvestigateTreatementEffectDetection.R


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

reference.rast<-rast(paste(loc.data,"CECS_Data/CECS_CAWide_Vulner_TreeDieoff_SPI-2_2020_V250614.tif",sep=""))

# CECS layers all have the same CRS and extent
cecs.rast<-rast(paste(loc.data,"CECS_Data/CECS_CAWide_Vulner_TreeDieoff_SPI-2_2020_V250614.tif",sep=""))
# whp is the same projection as state boundaries/TF regions, WHR/veg classifications
whp.rast <- rast(paste(loc.data,"PriorityLayers/whp_classified_20240906.tif",sep="")) 

library("exactextractr")
library("sf")

############### GLOBAL PARAMETERS ###################

#date stamp of this set of results - appended to all outputs to avoid overwriting older versions
datetime<-"2025Nov23_TestTrEffects"

#ending year of water year

start.year<-"2020"
end.year<-"2024"

#water year
start.y<-paste(start.year,"-09-30",sep="")
end.y<-paste(end.year,"-10-01",sep="")


metrics<-c( "FlameLengthWUI",
			"FlameLengthLandscape",
			"FlameLengthUtilities",
			"FlameLengthRoads",
			"DroughtVulnerability", 
#			"Shrub-GrassRatio")
			"GrassProportion",
			"BeneficialFireLandscape")

#(need to run everything up through here in order to make masked before and after rasters)
######################

#use 'metrics' to be the straight filenames of the thresholded layers

metrics<-c(
	"Forest_FlameLengthLandscape", 
	"Forest_FlameLengthLandscape",
	"Forest_Nofire_FlameLengthLandscape",
	"Forest_Nofire_FlameLengthLandscape",
	"Forest_Nofire_Disturbance_FlameLengthLandscape",
	"Forest_Nofire_Disturbance_FlameLengthLandscape")

policy.target<-c("WildlandFireRisk",
				  "Canopy",
				  "WildlandFireRisk",
				  "Canopy",
				  "WildlandFireRisk",
				  "Canopy")

#to calculate what the area is we're talking about
forest.cecs.rast<-rast(paste(loc.data,"WUIVegetationClassifications/WHR13_RECLASS_FOREST_CECS.tif",sep=""))
forest.cecs.rast[is.na(forest.cecs.rast)]<-0

nofire.cecs.rast<-rast(paste(loc.data,"WUIVegetationClassifications/NonFireFootprints_2020-2024_CECSproj.tif",sep=""))
disturbed.cecs.rast<-rast(paste(loc.data,"WUIVegetationClassifications/ForestDisturbances_2021-2024_CECSproj.tif",sep=""))
nofire.cecs.rast<-nofire.cecs.rast*forest.cecs.rast
nofire.dist.cecs.rast<-nofire.cecs.rast*disturbed.cecs.rast
nofire.cecs.rast[is.na(nofire.cecs.rast)]<-0
nofire.dist.cecs.rast[is.na(nofire.dist.cecs.rast)]<-0

spat.rast<-list(forest.cecs.rast,
				forest.cecs.rast,
				nofire.cecs.rast,
				nofire.cecs.rast,
				nofire.dist.cecs.rast,
				nofire.dist.cecs.rast
				)


boundary.shape<-c(paste(loc.scripts,"ReferenceFiles/CA_State.shp",sep=""))
boundary.name<-c("CA")
nice.boundary.name<-c("All of California")

k=1

	print(paste("Start ", boundary.name[k]))

	boundary.sf<-st_read(boundary.shape[k])
	prepped.boundary.sf<-st_transform(boundary.sf, st_crs(cecs.rast))

	agg.fire.sf<-st_read(paste(loc.data,"IntermediateFiles/AggregatedVectors/Fires_",
				boundary.name[k],"_",start.year,"_",end.year,".shp",sep=""))
	agg.fire.proj.sf<-st_transform(agg.fire.sf, st_crs(cecs.rast))

	#############################################
	###  BEGIN LOOP THROUGH METRICS   ###########
	#############################################

	for(i in 1:length(metrics)){
		#choose the correct metric
		metric.name<-metrics[i]
		print(metric.name)

		#read in the appropriate raster(s) (already masked for appropriate geographical subsets)
		print(paste("Reading: ",loc.data,"IntermediateFiles/ThresholdedEfficacyLayers/Thresholded_",metric.name,"_",start.year,".tif",sep=""))
		bef.thr.rast<-rast(paste(loc.data,"IntermediateFiles/ThresholdedEfficacyLayers/Thresholded_",metric.name,"_",start.year,".tif",sep=""))
		print(paste("Reading: ",loc.data,"IntermediateFiles/ThresholdedEfficacyLayers/Thresholded_",metric.name,"_",end.year,".tif",sep=""))
		aft.thr.rast<-rast(paste(loc.data,"IntermediateFiles/ThresholdedEfficacyLayers/Thresholded_",metric.name,"_",end.year,".tif",sep=""))


		#read in vector files for summarizing/clipping
			print(paste("Reading: ",loc.data,"IntermediateFiles/AggregatedVectors/Treatments_",
						boundary.name[k],"_",policy.target[i],"_",start.year,"_",end.year,".shp",sep=""))
			agg.treat.sf<-st_read(paste(loc.data,"IntermediateFiles/AggregatedVectors/Treatments_",
						boundary.name[k],"_",policy.target[i],"_",start.year,"_",end.year,".shp",sep=""))
			agg.treat.proj.sf<-st_transform(agg.treat.sf, st_crs(cecs.rast))

			diffname<-paste(metric.name,start.year,end.year,sep="_")

			print("------------------------------------------------------------")
			print(paste("Starting region-level proportion calcs for ",metric.name," ",policy.target[i]," in ",boundary.name[k],sep=""))
			all.global.results<-rbind(
				cbind(
				method="Global",
				metric=metric.name,
				boundary=boundary.name[k],
				TreatType=policy.target[i],
				area_ac=st_area(prepped.boundary.sf)*0.000247105*exact_extract(spat.rast[[i]],prepped.boundary.sf,fun="mean"),
				subset="WholeArea",
				before=exact_extract(bef.thr.rast,prepped.boundary.sf,fun="mean"),
				after=exact_extract(aft.thr.rast,prepped.boundary.sf,fun="mean")
				),
				cbind(
				method="Global",
				metric=metric.name,
				boundary=boundary.name[k],
				TreatType=policy.target[i],
				area_ac=st_area(agg.treat.proj.sf)*0.000247105*exact_extract(spat.rast[[i]],agg.treat.proj.sf,fun="mean"),
				subset="Treatments",
				before=exact_extract(bef.thr.rast,agg.treat.proj.sf,fun="mean"),
				after=exact_extract(aft.thr.rast,agg.treat.proj.sf,fun="mean")
				),
				cbind(
				method="Global",
				metric=metric.name,
				boundary=boundary.name[k],
				TreatType=policy.target[i],
				area_ac=st_area(agg.fire.proj.sf)*0.000247105*exact_extract(spat.rast[[i]],agg.fire.proj.sf,fun="mean"),
				subset="Fires",
				before=exact_extract(bef.thr.rast,agg.fire.proj.sf,fun="mean"),
				after=exact_extract(aft.thr.rast,agg.fire.proj.sf,fun="mean")
				)
			)

			all.global.results.df<-as.data.frame(all.global.results)
			all.global.results.df$percdiff<-(as.numeric(as.character(all.global.results.df$after))-
										 as.numeric(as.character(all.global.results.df$before)))/
										 as.numeric(as.character(all.global.results.df$before))

			print(paste("Writing to GlobalThresholdCalcOutput_",diffname,"_",boundary.name[k],"_",policy.target[i],"_",datetime,".csv",sep=""))
			write.table(all.global.results.df,paste("EfficacyResults/GlobalThresholdCalcOutput_",diffname,"_",boundary.name[k],"_",policy.target[i],"_",datetime,".csv",sep=""),
				sep = ",",quote = FALSE, col.names = TRUE, row.names = FALSE,na="NA") 
	}



efficacy.list<-read.csv("EfficacyResults/EfficacyOutputsNov23_TestTrEffects.csv",header=FALSE)

efficacy.results<-list()

for(i in 1:nrow(efficacy.list))
	efficacy.results[[i]]<-read.csv(paste("EfficacyResults/",efficacy.list[i,],sep=""))

efficacy.df<-do.call(rbind,efficacy.results)
efficacy.df$method<-factor(efficacy.df$method)
efficacy.df$metric<-factor(efficacy.df$metric)
efficacy.df$boundary<-factor(efficacy.df$boundary)
efficacy.df$subset<-factor(efficacy.df$subset)
efficacy.df$absdiff<-efficacy.df$after-efficacy.df$before

write.csv(efficacy.df,"EfficacyResults/AllEfficacyOutputsTestTrEffects.csv")

global.result<-efficacy.df[efficacy.df$subset=="Treatments",]

nice.metric.name<-c(
					"Wildland Fire Risk\nAll Forest in CA",
					"Canopy Treatments Only\nAll Forest in CA",
					"Wildland Fire Risk\n CA Forest with no Fire",
					"Canopy Treatments Only\nCA Forest with no Fire",
					"Wildland Fire Risk\nCA Forest no fire only disturbed",
					"Canopy Treatments Only\nCA Forest no fire only disturbed"
					)

		#this is your main result for the efficacy modeling
#		plot.title<-paste("Change in Proportion of\n",nice.metric.name[i], sep="")
		bar.plt<-ggplot(data=global.result, aes(x=metric,fill=metric,y=absdiff)) +
		  geom_bar(stat="identity")+
#		  theme(legend.position="none")+
		  facet_grid(.~TreatType)+
	      labs(title = "Flame length change",x = element_blank(), y = "Difference in Proportion 2020-2024")#+
#		  scale_fill_manual(values=c("#E9E5C3","#9C8F57","#9F2214"))#+
		  #geom_text(aes(label=before.rnd), vjust=-0.5, color="black", size=3.5)
		pltnm.b<-paste("EfficacyResults/AbsDiff_bar_TestTrEffect.png",sep="")
	  	ggsave(pltnm.b, units="in", width=8,height=3)


		#this is your main result for the efficacy modeling
#		plot.title<-paste("Change in Proportion of\n",nice.metric.name[i], sep="")
		bar.plt<-ggplot(data=global.result, aes(x=metric,fill=metric,y=percdiff)) +
		  geom_bar(stat="identity")+
#		  theme(legend.position="none")+
		  facet_grid(.~TreatType)+
	      labs(title = "Flame length change",x = element_blank(), y = "Relative Percent difference 2020-2024")#+
#		  scale_fill_manual(values=c("#E9E5C3","#9C8F57","#9F2214"))#+
		  #geom_text(aes(label=before.rnd), vjust=-0.5, color="black", size=3.5)
		pltnm.b<-paste("EfficacyResults/PerDiff_bar_TestTrEffect.png",sep="")
	  	ggsave(pltnm.b, units="in", width=8,height=3)

