#ActivityList.R


##########################################################
############       ACTIVITY LIST         #################
##########################################################

#note "Suppression Support" is in "PRIMARY OBJECTIVE" not "ACTIVITY_DESCRIPTION"
#also note that "Wildland Fire Risk", "WUI Fire Risk", and "Fire Risk in Utility Corridors" are the same list
	activity.list<-list(
	"WildlandFireRisk"=list(metric="Acres of treatment in high fire risk areas",activities=
	  c("BIOMASS_REMOVAL", "BROADCAST_BURN", "CHIPPING", "DOZER_LINE", "HANDLINE", "LOP_AND_SCAT", "MASTICATION", "PILE_BURN", "PRUNING", "SLASH_DISPOSAL", "THIN_MAN", "THIN_MECH", "PRESCRB_HERBIVORY", "MOWING")),
	"Suppression Support"=list(metric="Miles of fuel breaks",activities=
	  c("FUEL BREAK")),
	"WUIFireRisk"=list(metric="Acres of treatment near high risk communities",activities=
	  c("BIOMASS_REMOVAL", "BROADCAST_BURN", "CHIPPING", "DOZER_LINE", "HANDLINE", "LOP_AND_SCAT", "MASTICATION", "PILE_BURN", "PRUNING", "SLASH_DISPOSAL", "THIN_MAN", "THIN_MECH", "PRESCRB_HERBIVORY", "MOWING")),
	"FireRiskinUtilityCorridors"=list(metric="Acres treated near utility corridors",activities=
	  c("BIOMASS_REMOVAL", "BROADCAST_BURN", "CHIPPING", "DOZER_LINE", "HANDLINE", "LOP_AND_SCAT", "MASTICATION", "PILE_BURN", "PRUNING", "SLASH_DISPOSAL", "THIN_MAN", "THIN_MECH", "PRESCRB_HERBIVORY", "MOWING")),
	"ForestHealth"=list(metric="Acres treated of drought-vulnerable forest",activities=
	  c("BROADCAST_BURN", "PILE_BURN", "THIN_MAN", "THIN_MECH", "PRESCRB_HERBIVORY", "MOWING", "TREE_PLNTING", "SITE_PREP", "SEEDBED_PREP", "INV_PLANT_REMOVAL", "HERBICIDE_APP", "HABITAT_REVEG", "BIOMASS_REMOVAL", "VARIABLE_RETEN_HARVEST", "TREE_FELL", "MASTICATION", "LOP_AND_SCAT", "PRUNING", "ECO_HAB_RESTORATION", "SLASH_DISPOSAL", "EROSION_CONTROL")),
	"Restoringhealthyfire"=list(metric="Acres of broadcast burn",activities=
	  c("BROADCAST_BURN")),
	"ShrublandHealth"=list(metric="Acres/miles of treatments near high risk roadways",activities=
	  c("BROADCAST_BURN", "MASTICATION", "MOWING", "HERBICIDE_APP", "INV_PLANT_REMOVAL", "LOP_AND_SCAT", "PILE_BURN", "ECO_HAB_RESTORATION", "HABITAT_REVEG", "SEEDBED_PREP", "PRESCRB_HERBIVORY", "TREE_FELL", "THIN_MAN","DISCING")),
#trying to remove CHAIN_CRUSH to see if it helps with geometry problems
#	"ShrublandHealth"=list(metric="Acres/miles of treatments near high risk roadways",activities=
#	  c("BROADCAST_BURN", "MASTICATION", "CHAIN_CRUSH", "MOWING", "HERBICIDE_APP", "INV_PLANT_REMOVAL", "LOP_AND_SCAT", "PILE_BURN", "ECO_HAB_RESTORATION", "HABITAT_REVEG", "SEEDBED_PREP", "PRESCRB_HERBIVORY", "TREE_FELL", "THIN_MAN","DISCING")),
	"RangelandHealth"=list(metric="Acres of prescribed grazing/herbivory",activities=
	  c("BROADCAST_BURN", "PILE_BURN", "PRESCRB_HERBIVORY", "MOWING", "HERICIDE_APP", "INV_PLANT_REMOVAL", "SEEDBED_PREP", "SITE_PREP", "TREE_FELL", "MASTICATION", "HABITAT_REVEG", "ECO_HAB_RESTORATION", "LOP_AND_SCAT")),
	"Social-Economic Health"=list(metric="Acres of timber projects",activities=
	  c("THIN_MAN", "THIN_MECH", "VARIABLE_RETEN_HARVEST", "TREE_FELL", "BIOMASS_REMOVAL", "PRUNING", "SLASH_DISPOSAL", "CHIPPING", "MASTICATION", "YARDING", "CLEARCUT", "PILING", "CHIPPING")),
	"CarbonStorage"=list(metric="Acres treated near high carbon areas?",activities=
	  c("THIN_MECH", "THIN_MAN", "VARIABLE_RETEN_HARVEST", "TREE_FELL", "MASTICATION", "BIOMASS_REMOVAL", "CLEARCUT", "BROADCAST_BURN")),
	"Habitat"=list(metric="Acres treated near sensitive habitat?",activities=
	  c("THIN_MECH", "THIN_MAN", "VARIABLE_RETEN_HARVEST", "MASTICATION", "PILE_BURN", "BROADCAST_BURN", "TREE_FELL", "HERBICIDE_APP", "INV_PLANT_REMOVAL", "PRUNING", "LOP_AND_SCAT")),
	"Water"=list(metric="Acres treated with high potential debris flow",activities=
	  c("EROSION_CNTRL", "BROADCAST_BURN", "PILE_BURN", "THIN_MECH", "THIN_MAN", "TREE_FELL", "MASTICATION", "BIOMASS_REMOVAL", "WETLAND_RESTOR")),
	"Air"=list(metric="Acres treated in watersheds feeding into hydropower & reservoirs",activities=
	  c("EROSION_CNTRL", "BROADCAST_BURN", "PILE_BURN", "THIN_MECH", "THIN_MAN", "TREE_FELL", "MASTICATION", "BIOMASS_REMOVAL", "WETLAND_RESTOR", "ECO_HAB_RESTORATION"))
	  )

	"Canopy"=list(metric="Acres treated that could affect canopy",activities=
	  c("CLEARCUT","THIN_MAN","THIN_MECH","TREE_FELL","VARIABLE_RETEN_HARVEST")
	  )
	"NonCanopy"=list(metric="Acres treated that are likely not to affect canopy",activities=
	  c("CHIPPING","DISCING","DOZER_LINE","ECO_HAB_RESTORATION","EROSION_CONTROL",
	  	"HABITAT_REVEG","HANDLINE","HERBICIDE_APP","INV_PLANT_REMOVAL","LOP_AND_SCAT",
	  	"MASTICATION","MOWING","NOT_DEFINED","PEST_CNTRL","PILE_BURN","PILING",
	  	"PRESCRB_HERBIVORY","PRUNING","ROAD_CLEAR","SEEDBED_PREP","SITE_PREP",
	  	"SLASH_DISPOSAL","TBD","TREE_PLNTING","WETLAND_RESTOR","YARDING","BROADCAST_BURN")
	  )
	#chain crushing should be in nonCanopy but we don't know whether that geometry is safe
	#biomass removal is not included because we're not sure whether it would affect canopy or not


