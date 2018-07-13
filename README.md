# DSM2 Hydro Visualization Tool

An interactive app for visualizing [DSM2](http://baydeltaoffice.water.ca.gov/modeling/deltamodeling/models/dsm2/dsm2.cfm) Hydro output (H5 files) based on the [Shiny](https://shiny.rstudio.com/) web framework for [R](https://www.r-project.org/).

### Run app on the web

The app is accessible on the [web](https://fishsciences.shinyapps.io/dsm2-hydro-visualization/) with a modern browser and fast internet connection. However, if visualizing large files or many files, file upload times may be unacceptably long. In those situations, it may be preferable to run the app locally.

### Run app locally

(1) Install [R](https://cran.rstudio.com/) (>=3.5.0).

(2) Install the 'devtools' package by running the following line in the R console.
```
install.packages("devtools", repos = "https://cran.rstudio.com/")
```

(3) Run the launch script by running the following in the R console.
```
devtools::source_gist(id = "4351fe4185507fb2e4fc99c189a0f885", filename = "DSM2VizLaunchScript.R")
```
The launch script checks for the required packages, installs any missing packages, and then launches the app in your default browser. The first time running the launch script may take several minutes for installation of all the required R packages. If using [RStudio](https://www.rstudio.com/), the app will first launch in the RStudio Viewer pane. Click on 'Open in Browser' in the top left corner of the Viewer pane to launch your default browswer.


