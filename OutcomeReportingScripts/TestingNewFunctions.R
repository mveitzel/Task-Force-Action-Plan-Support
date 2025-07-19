#TestingNewFunctions.R

loc.scripts<-"D:/GitRepos/BattlesLabRepos/Task-Force-Action-Plan-Support/"
loc.data<-"D:/GIS_Large_Files/CECS_Data/"
loc.output<-"D:/DropboxFiles/Dropbox/Professional/UCB_Battles/ActionPlanSupport/"


source(paste(loc.scripts,"FunctionLibraries/SummarizeChange_functions.R",sep=""))

setwd()


#setwd("/home/mves/Dropbox/Professional/UCB_Battles/TreatmentEffectiveness")

boundary.shape<-c(paste(loc.scripts,"/VectorFiles/Region_Sierra.shp",sep=""))
boundary.name<-c("Sierra")
reference.rast<-rast(paste(loc.data,"CECS_CAWide_Vulner_TreeDieoff_SPI-2_2020_V250418.tif",sep=""))


prepped.boundary.vect<-read.and.prepare.boundary.vector(boundary.shape,boundary.name,reference.rast)


