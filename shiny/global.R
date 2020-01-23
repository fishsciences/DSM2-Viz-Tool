# BiocManager::install("rhdf5")  # reminder of how to install Bioconductor packages

library(shiny)
library(shinyWidgets)
library(leaflet)
library(rhdf5)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate) 
library(shinyjs)
library(scales)   # rescale()
library(shinyFiles) 
library(DT)
library(shinybusy)

# on windows system uses Cairo for anti-aliasing of plots
# is not a problem when run on windows system through R but was a problem with Electron app
if (.Platform[["OS.type"]] == "windows") {
  library(Cairo)
  options(shiny.usecairo = T)
}

source("helper.R")

shp = rgdal::readOGR(dsn = "./shapefiles", layer = "FlowlinesLatLong")
cll = read.csv("data/ChannelLatLong.csv")   # used for placing map markers for selected channels
channels = sort(cll$channel_nu)  # used for selectInput widget

siw = 150 # selectInput width

# lookup vectors for linking descriptive names to simple names for plotting purposes
x.labs = c(
  "Velocity (ft/s)" = "velocity",
  "Flow (cfs)" = "flow",
  "Stage (ft)" = "stage"
)

summ.stats = c(
  "Min" = "min",
  "1st quartile" = "first.quart",
  "Median" = "median",
  "Mean" = "mean",
  "3rd quartile" = "third.quart",
  "Max" = "max",
  "Reversal" = "prop.neg"
)

# don't need to rescale prop.neg; need unnamed vector for mutate_at (i.e., can't use summ.stats)
rescale.cols = c("min", "first.quart", "median", "mean", "third.quart", "max")

# custom ggplot2 theme
theme.mod = theme(
  plot.title = element_text(size = 16),
  legend.text = element_text(size = 12),
  legend.title = element_text(size = 14),
  strip.text.x = element_text(size = 12),
  strip.text.y = element_text(size = 12),
  axis.title = element_text(size = 14),
  axis.text = element_text(size = 12)
)

# # test performance of loading whole file compared to repeated reading of smaller subsets
# library(microbenchmark)
# fn = "/Users/travishinkelman/sjr1500_omr2500.h5"
# h5read_loop <- function(n, x){
#   for (i in 1:n){
#     h5read(fn, "/hydro/data/channel stage", index = list(NULL, NULL, x))
#   }
# }
# microbenchmark(
#   h5read_loop(2, 1:720),
#   h5read_loop(50, 1:28),
#   h5read_loop(100, 1:14)
# )
# # it is much faster to read in bigger chunks


