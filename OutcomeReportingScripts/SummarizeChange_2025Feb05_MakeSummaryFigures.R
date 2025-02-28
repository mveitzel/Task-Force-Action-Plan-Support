


library("ggplot2")
library("foreign")
library("viridis")
#library("sf")
library('terra')

### This script creates the barplots used in the October 10th 2024 Task Force meeting

############## CREATE SUMMARY FIGURES ####################


#----------------------------------
# plotting all of the metrics as means

setwd("/home/mves/Dropbox/Professional/UCB_Battles/TreatmentEffectiveness/Results2024Sep16")

bio<-read.csv("ZonalMeansOutput_Biodiv_WildlifeWeightedAll__2020__2023_2024Sep16-HUC10only.csv")
carb<-read.csv("ZonalMeansOutput_ClimateForce_LiveAboveC__2020__2023_2024Sep16-HUC10only.csv")
fire<-read.csv("ZonalMeansOutput_Fire_FlamMap_FL__2020__2023_2024Sep16-HUC10only.csv")
smoke<-read.csv("ZonalMeansOutput_Fire_FOFEM_PM25__2020__2023_2024Sep16-HUC10only.csv")
water<-read.csv("ZonalMeansOutput_WaterFlux_Runoff_SPI0__2020__2023_2024Sep16-HUC10only.csv")

#changing the wording of the variables, and the ordering, for each of the metrics.  
#Eventually write a function to do this.

bio$crop.area[bio$crop.area=="AllCA"]<-"Statewide"
bio$crop.area[bio$crop.area=="Sierra"]<-"Sierra Nevada Region"
bio$crop.area<-factor(bio$crop.area,c("Statewide","Sierra Nevada Region"))
bio$summary.unit<-factor(bio$summary.unit)
bio$land.class[bio$land.class=="AllEcosystems"]<-"All Area"
bio$land.class[bio$land.class=="Urban-WUI"]<-"WUI"
bio$land.class[bio$land.class=="Wildland"]<-"Non-WUI"
bio$land.class<-factor(bio$land.class,c("All Area","WUI","Non-WUI"))
bio$area.type<-factor(bio$area.type,c("SummaryUnit","Treatments","Fires"))
#bio$hucID<-factor(bio$hucID)
summary(bio)


fire$crop.area[fire$crop.area=="AllCA"]<-"Statewide"
fire$crop.area[fire$crop.area=="Sierra"]<-"Sierra Nevada Region"
fire$crop.area<-factor(fire$crop.area,c("Statewide","Sierra Nevada Region"))
fire$summary.unit<-factor(fire$summary.unit)
fire$land.class[fire$land.class=="AllEcosystems"]<-"All Area"
fire$land.class[fire$land.class=="Urban-WUI"]<-"WUI"
fire$land.class[fire$land.class=="Wildland"]<-"Non-WUI"
fire$land.class<-factor(fire$land.class,c("All Area","WUI","Non-WUI"))
fire$area.type<-factor(fire$area.type,c("SummaryUnit","Treatments","Fires"))
#fire$hucID<-factor(fire$hucID)
summary(fire)

water$crop.area[water$crop.area=="AllCA"]<-"Statewide"
water$crop.area[water$crop.area=="Sierra"]<-"Sierra Nevada Region"
water$crop.area<-factor(water$crop.area,c("Statewide","Sierra Nevada Region"))
water$summary.unit<-factor(water$summary.unit)
water$land.class[water$land.class=="AllEcosystems"]<-"All Area"
water$land.class[water$land.class=="Urban-WUI"]<-"WUI"
water$land.class[water$land.class=="Wildland"]<-"Non-WUI"
water$land.class<-factor(water$land.class,c("All Area","WUI","Non-WUI"))
water$area.type<-factor(water$area.type,c("SummaryUnit","Treatments","Fires"))
#water$hucID<-factor(water$hucID)
summary(water)

smoke$crop.area[smoke$crop.area=="AllCA"]<-"Statewide"
smoke$crop.area[smoke$crop.area=="Sierra"]<-"Sierra Nevada Region"
smoke$crop.area<-factor(smoke$crop.area,c("Statewide","Sierra Nevada Region"))
smoke$summary.unit<-factor(smoke$summary.unit)
smoke$land.class[smoke$land.class=="AllEcosystems"]<-"All Area"
smoke$land.class[smoke$land.class=="Urban-WUI"]<-"WUI"
smoke$land.class[smoke$land.class=="Wildland"]<-"Non-WUI"
smoke$land.class<-factor(smoke$land.class,c("All Area","WUI","Non-WUI"))
smoke$area.type<-factor(smoke$area.type,c("SummaryUnit","Treatments","Fires"))
#smoke$hucID<-factor(smoke$hucID)
summary(smoke)

carb$crop.area[carb$crop.area=="AllCA"]<-"Statewide"
carb$crop.area[carb$crop.area=="Sierra"]<-"Sierra Nevada Region"
carb$crop.area<-factor(carb$crop.area,c("Statewide","Sierra Nevada Region"))
carb$summary.unit<-factor(carb$summary.unit)
carb$land.class[carb$land.class=="AllEcosystems"]<-"All Area"
carb$land.class[carb$land.class=="Urban-WUI"]<-"WUI"
carb$land.class[carb$land.class=="Wildland"]<-"Non-WUI"
carb$land.class<-factor(carb$land.class,c("All Area","WUI","Non-WUI"))
carb$area.type<-factor(carb$area.type,c("SummaryUnit","Treatments","Fires"))
#carb$hucID<-factor(carb$hucID)
summary(carb)


#make the figure with all of them together

allmetrics<-rbind(carb,water,fire)

allmetrics$metric[allmetrics$metric=="Runoff"]<-"Water Availability"
allmetrics$metric<-factor(allmetrics$metric)

plt<-ggplot(subset(allmetrics,
					(summary.unit=="huc10" &
					 crop.area=="Sierra Nevada Region" & 
					 land.class=="All Area")),
		aes(
			x=area.type,
			y=mean,
			fill=area.type))+
		geom_bar(stat="identity",position=position_dodge())+
		geom_hline(yintercept=0,linewidth=0.2)+
		facet_grid(.~metric,scales="free")+
		labs(x = NULL)+
		labs(y = "Suitability Index")+
		scale_fill_manual(
			name="",
			labels=c("Entire HUC10","Treatments Within HUC10s","Fires Within HUC10s"),
			values=inferno(4,direction=-1)[2:4])+
		theme(legend.position="none")
plt
ggsave("Metrics_Means3Yrs.png", units="in", width=8,height=4,dpi=300)


#now just potential habitat availability
plt.b<-ggplot(subset(bio,
					(summary.unit=="huc10" &
					 crop.area=="Sierra Nevada Region" & 
					 land.class=="All Area")),
		aes(
			x=area.type,
			y=mean,
			fill=area.type))+
		geom_bar(stat="identity",position=position_dodge())+
		geom_hline(yintercept=0,linewidth=0.2)+
		ggtitle("Habitat Suitability")+
		scale_x_discrete(labels = NULL, breaks = NULL)+
		labs(x = NULL)+
		labs(y = "Suitability Index")+
		scale_fill_manual(
			name="",
			labels=c("Entire HUC10","Treatments Within HUC10s","Fires Within HUC10s"),
			values=c("#9C8F57","#855914","#9F2214"))+
		theme(legend.position="none")
plt.b
ggsave("Habitat_Means3Yrs.png", units="in", width=4,height=4,dpi=300)


#now just flame length
plt.f<-ggplot(subset(fire,(summary.unit=="huc10" &
					 crop.area=="Sierra Nevada Region" & 
					 land.class=="All Area")),
		aes(
			x=area.type,
			y=mean,
			fill=area.type))+
		geom_bar(stat="identity",position=position_dodge())+
		geom_hline(yintercept=0,linewidth=0.2)+
		ggtitle("Change in Estimated\nFlame Length")+
		scale_x_discrete(labels = NULL, breaks = NULL)+
		labs(x = NULL)+
		labs(y = "ft")+
		scale_fill_manual(
			name="",
			labels=c("Entire HUC10","Treatments Within HUC10s","Fires Within HUC10s"),
			values=c("#9C8F57","#855914","#9F2214"))+
		theme(legend.position="none")
plt.f
ggsave("FlameLength_Means3Yrs.png", units="in", width=4,height=4,dpi=300)

#now just water availability
plt.w<-ggplot(subset(water,(summary.unit=="huc10" &
					 crop.area=="Sierra Nevada Region" & 
					 land.class=="All Area")),
		aes(
			x=area.type,
			y=mean,
			fill=area.type))+
		geom_bar(stat="identity",position=position_dodge())+
		geom_hline(yintercept=0,linewidth=0.2)+
		ggtitle("Change in Estimated\nWater Availability")+
		scale_x_discrete(labels = NULL, breaks = NULL)+
		labs(x = NULL)+
		labs(y = "mm/yr")+
		scale_fill_manual(
			name="",
			labels=c("Entire HUC10","Treatments Within HUC10s","Fires Within HUC10s"),
			values=c("#9C8F57","#855914","#9F2214"))+
		theme(legend.position="none")
plt.w
ggsave("AET_Means3Yrs.png", units="in", width=4,height=4,dpi=300)

#now just potential smoke production
plt.s<-ggplot(subset(smoke,(summary.unit=="huc10" &
					 crop.area=="Sierra Nevada Region" & 
					 land.class=="All Area")),
		aes(
			x=area.type,
			y=mean,
			fill=area.type))+
		geom_bar(stat="identity",position=position_dodge())+
		geom_hline(yintercept=0,linewidth=0.2)+
		ggtitle("Smoke Emissions")+
		scale_x_discrete(labels = NULL, breaks = NULL)+
		labs(x = NULL)+
		labs(y = "PM 2.5 (lbs/acre)")+
		scale_fill_manual(
			name="",
			labels=c("Entire HUC10","Treatments Within HUC10s","Fires Within HUC10s"),
			values=c("#9C8F57","#855914","#9F2214"))+
		theme(legend.position="none")
plt.s
ggsave("Smoke_Means3Yrs.png", units="in", width=4,height=4,dpi=300)

#now just carbon
plt.c<-ggplot(subset(carb,(summary.unit=="huc10" &
					 crop.area=="Sierra Nevada Region" & 
					 land.class=="All Area")),
		aes(
			x=area.type,
			y=mean,
			fill=area.type))+
		geom_bar(stat="identity",position=position_dodge())+
		geom_hline(yintercept=0,linewidth=0.2)+
		ggtitle("Change in Estimated\nLive Aboveground Carbon")+
		scale_x_discrete(labels = NULL, breaks = NULL)+
		labs(x = NULL)+
		labs(y = "Mg/ha")+
		scale_fill_manual(
			name="",
			labels=c("Entire HUC10","Treatments Within HUC10s","Fires Within HUC10s"),
			values=c("#9C8F57","#855914","#9F2214"))+
		theme(legend.position="none")
plt.c
ggsave("Carbon_Means3Yrs.png", units="in", width=4,height=4,dpi=300)



############## END CREATE SUMMARY FIGURES ################
##########################################################
##########################################################



