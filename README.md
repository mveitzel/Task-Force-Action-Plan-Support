This repo provides the code for the science advisory panel staff scientist outcome reporting pilot analysis. (See [coming soon: link to outcome reporting pilot report] for the results of this analysis, and [link to EDI repository] for the data needed to run the analysis)

There are two types of analysis done here:
(see [coming soon: link to framework] for definitions of these two)

1) Targeted Effort (TargetedEffort.R)
2) Efficacy, using Thresholded metrics (EfficacyOutcomeReportingThresholded.R)

These main scripts refer to (R 'source') four supporting scripts:
1) create spatial masks for wildland-urban interface, vegetation types, and other spatial stratifications (CalculateWUI_Veg_Masks.R)
2) create the thresholded versions of the metrics for the efficacy calculations (CalculateEfficacyThresholdedLayers.R)
3) create the 'priority layers' for the targeted effort calculations (CalculatePriorityLayers.R)
4) create the aggregated (for both targeted effort and efficacy) treatment and (for efficacy) fire footprints (CalculateAggregatedPatches.R)

The four supporting scripts do many raster and vector operations that take the majority of computation time, so unless the masks change, the underlying efficacy metrics change, the priority layers change, or the treatment and fire data change, you don't need to rerun them every time you do the analysis.  Rerun any of these when there is an update to their respective data.

The code should largely work 'out of the box', with the exception that the user should adjust the file path as appropriate when reading in vector and raster layers to wherever they are keeping the layers locally on their computer.

There are also references to two different coordinate reference systems used by different raster layers; in many cases during analysis we kept two different versions of various data layers so we didn't unnecessarily reproject rasters (both for computation cost, and to not introduce errors due to multiple resampling steps for the rasters).

We use ggplot2 for data visualizations, terra for some raster operations, and sf to handle vector and mixed operations.
