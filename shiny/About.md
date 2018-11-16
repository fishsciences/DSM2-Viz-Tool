#### Background

The <a href="http://baydeltaoffice.water.ca.gov/modeling/deltamodeling/models/dsm2/dsm2.cfm" target="_blank">DSM2</a> HYDRO model (hereafter, DSM2) is a one-dimensional hydrodynamic model simulating stage, flow, and velocity in the Delta's network of channels. For a given set of boundary conditions (tides, river inflows, exports, and barriers), DSM2 provides stage, flow, and velocity estimates at 15-minute intervals for hundreds of channels in the Delta. DSM2 has been extensively tested by the California Department of Water Resources and is used for planning and operation of the State Water Project and Central Valley Project diversion pumping as well as for management and satisfaction of stringent water quality standards.  

#### Objective

The visualization approach used in this app is the outcome of our own efforts to effectively visualize DSM2 output. Our initial efforts to visualize DSM2 output involved calculating channel-level summary statistics, creating static, color-scaled maps of the summary statistics, and arranging the color-scaled maps in panels according to boundary conditions (see example below). 

<br>
<img src="PanelMapExample.jpg" height="500" width="824"/>
<br><br>

Mapping DSM2 output allows for identification of broad spatial patterns, but requires summarizing the 15-min values to a single value for mapping. Because distributions of 15-min values in the Delta are frequently bimodal (tidal influence) and asymmetric, a single value (e.g., median, mean) may not effectively represent conditions in the channel. Arranging maps in panels with different boundary conditions allows for comparing patterns across conditions, but changes in colors across map panels may be difficult to detect.

We suggest that Delta maps should be linked to density plots that show the distribution of 15-min (or 1-hr) DSM2 output for a selected channel and that the maps should be based on comparative metrics to address the challenges of representing distributions with a single value and making comparisons between maps, respectively. In this app, we provide two comparative metrics: the absolute difference in summary statistics and the proportion overlap in density distributions (more below). 

#### Approach 

Summary statistics and kernel density estimates are calculated for the 15-min (or 1-hr) velocity and flow values over the specified time period. The summary statistics include 6 standard statistics (e.g., minimum, median, mean) and a metric that we call 'reversal', which is the proportion of values in the 61-day period that are negative (with directionality as defined in DSM2). The kernel density estimates represent a smoothing of the empirical distribution of 15-min values.

Summary statistics are compared between two scenarios by calculating the absolute difference between the two values. We chose to calculate absolute difference rather than relative difference because of the difficulty in calculating relative difference when values can be positive, negative, or zero. Because flow varies substantially across the Delta, the absolute difference in summary statistics for flow can be misleading (except for reversal, which is naturally bound from 0 to 1). 

The proportion overlap of two kernel density estimates is calculated with the following steps: 1) calculate the total area under the curve (AUC<sub>t</sub>) as the sum of the AUC for each density estimate, 2) calculate the AUC of the overlapping portions (AUC<sub>o</sub>) of the two density distributions being compared, and 3) calculate the overlapping proportion of the density distributions as AUC<sub>o</sub>/AUC<sub>t</sub>. From this proportion, a value of one indicates complete overlap and a value of zero indicates no overlap.

The map color is scaled by the absolute difference or proportion overlap value for each channel. An absolute difference at or near zero or a proportion overlap value at or near one are reliable indicators that a channel is not affected by the comparison selected. However, a value greater than zero (difference) or less than one (overlap) requires investigation of the density plots because there are numerous shapes of distributions that can produce similar difference or overlap values. In this way, though, the map can be used to delineate the spatial extent of the selected effect and density plots can be examined for channels within that spatial extent to visualize the changes in velocity (or flow) distributions.

#### Bug Reports and Feature Requests

This app has only be tested on a small number of DSM2 output files. Please don't hesitate to contact us (see below) with bug reports and feature requests or file an issue on the <a href="https://github.com/fishsciences/DSM2-Viz-Tool/issues" target="_blank">GitHub repository</a>.

<br>
<center>Contact [Brad Cavallo](bcavallo@fishsciences.net) or [Travis Hinkelman](travis.hinkelman@fishsciences.net) with questions.</center>

<center><a href = "http://www.fishsciences.net" target="_blank"><img src="cfs-logo_web-centered.jpg" width = "216" height = "147"/></a></center>

