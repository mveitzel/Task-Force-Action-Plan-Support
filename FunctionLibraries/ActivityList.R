#ActivityList.R


##########################################################
############       ACTIVITY LIST         #################
##########################################################

#note "Suppression Support" is in "PRIMARY OBJECTIVE" not "ACTIVITY_DESCRIPTION"
	activity.list<-list(
	"Wildland Fire Risk"=list(metric="Acres of treatment in high fire risk areas",activities=
	  c("BIOMASS_REMOVAL", "BROADCAST_BURN", "CHIPPING", "DOZER_LINE", "HANDLINE", "LOP_AND_SCATTER", "MASTICATION", "PILE_BURN", "PRUNING", "SLASH_DISPOSAL", "THIN_MAN", "THIN_MECH", "PRESCRIB_HERBIVORY", "MOWING")),
	"Suppression Support"=list(metric="Miles of fuel breaks",activities=
	  c("FUEL BREAK")),
	"WUI Fire Risk"=list(metric="Acres of treatment near high risk communities",activities=
	  c("BIOMASS_REMOVAL", "BROADCAST_BURN", "CHIPPING", "DOZER_LINE", "HANDLINE", "LOP_AND_SCATTER", "MASTICATION", "PILE_BURN", "PRUNING", "SLASH_DISPOSAL", "THIN_MAN", "THIN_MECH", "PRESCRIB_HERBIVORY", "MOWING")),
	"Fire Risk in Utility Corridors"=list(metric="Acres treated near utility corridors",activities=
	  c("BIOMASS_REMOVAL", "BROADCAST_BURN", "CHIPPING", "DOZER_LINE", "HANDLINE", "LOP_AND_SCATTER", "MASTICATION", "PILE_BURN", "PRUNING", "SLASH_DISPOSAL", "THIN_MAN", "THIN_MECH", "PRESCRIB_HERBIVORY", "MOWING")),
	"Forest Health"=list(metric="Acres treated of drought-vulnerable forest",activities=
	  c("BROADCAST_BURN", "PILE_BURN", "THIN_MAN", "THIN_MECH", "PRESCRIB_HERBIVORY", "MOWING", "TREE_PLNTING", "SITE_PREP", "SEEDBED_PREP", "INV_PLANT_REMOVAL", "HERBICIDE_APP", "HABITAT_REVEG", "BIOMASS_REMOVAL", "VARIABLE_RETEN_HARVEST", "TREE_FELL", "MASTICATION", "LOP_AND_SCAT", "PRUNING", "ECO_HAB_RESTORATION", "SLASH_DISPOSAL", "EROSION_CONTROL")),
	"Restoring healthy fire"=list(metric="Acres of broadcast burn",activities=
	  c("BROADCAST_BURN")),
	"Shrubland Health"=list(metric="Acres/miles of treatments near high risk roadways",activities=
	  c("BROADCAST_BURN", "MASTICATION", "CHAIN_CRUSH", "MOWING", "HERBICIDE_APP", "INV_PLANT_REMOVAL", "LOP_AND_SCAT", "PILE_BURN", "ECO_HAB_RESTORATION", "HABITAT_REVEG", "SEEDBED_PREP", "PRESCRIB_HERBIVORY", "TREE_FELL", "THIN_MAN","DISCING")),
	"Rangeland Health"=list(metric="Acres of prescribed grazing/herbivory",activities=
	  c("BROADCAST_BURN", "PILE_BURN", "PRESCRIBED_HERBIVORY", "MOWING", "HERICIDE_APP", "INV_PLANT_REMOVAL", "SEEDBED_PREP", "SITE_PREP", "TREE_FELL", "MASTICATION", "HABITAT_REVEG", "ECO_HABITAT", "RESTORATION", "LOP_AND_SCAT")),
	"Social/Economic Health"=list(metric="Acres of timber projects",activities=
	  c("THIN_MAN", "THIN_MECH", "VARIABLE_RETEN_HARVEST", "TREE_FELL", "BIOMASS_REMOVAL", "PRUNING", "SLASH_DISPOSAL", "CHIPPING", "MASTICATION", "YARDING", "CLEARCUT", "PILING", "CHIPPING")),
	"Carbon Storage"=list(metric="Acres treated near high carbon areas?",activities=
	  c("THIN_MECH", "THIN_MAN", "VARIABLE_RETEN_HARVEST", "TREE_FELL", "MASTICATION", "BIOMASS_REMOVAL", "CLEARCUT", "BROADCAST_BURN")),
	"Habitat"=list(metric="Acres treated near sensitive habitat?",activities=
	  c("THIN_MECH", "THIN_MAN", "VARIABLE_RETEN_HARVEST", "MASTICATION", "PILE_BURN", "BROADCAST_BURN", "TREE_FELL", "HERBICIDE_APP", "INV_PLANT_REMOVAL", "PRUNING", "LOP_AND_SCAT")),
	"Water"=list(metric="Acres treated with high potential debris flow",activities=
	  c("EROSION_CNTRL", "BROADCAST_BURN", "PILE_BURN", "THIN_MECH", "THIN_MAN", "TREE_FELL", "MASTICATION", "BIOMASS_REMOVAL", "WETLAND_RESTOR")),
	"Air"=list(metric="Acres treated in watersheds feeding into hydropower & reservoirs",activities=
	  c("EROSION_CNTRL", "BROADCAST_BURN", "PILE_BURN", "THIN_MECH", "THIN_MAN", "TREE_FELL", "MASTICATION", "BIOMASS_REMOVAL", "WETLAND_RESTOR", "ECO_HAB_RESTORATION"))
	  )

