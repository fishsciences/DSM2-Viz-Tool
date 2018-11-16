options(shiny.maxRequestSize = 1000*1024^2) # increase upload size to 1000 MB (1 GB)

## code to get bioconductor repo info
# bioc <- local({
#   env <- new.env()
#   on.exit(rm(env))
#   evalq(source("http://bioconductor.org/biocLite.R", local = TRUE), env)
#   biocinstallRepos()
# })
# bioc

# # set repos such that rsconnect can find bioconductor repos
# r <- getOption("repos")
# r["BioCsoft"] <- "https://bioconductor.org/packages/3.7/bioc"
# r["BioCann"] <- "https://bioconductor.org/packages/3.7/data/annotation"
# r["BioCexp"] <- "https://bioconductor.org/packages/3.7/data/experiment"
# r["BioCworkflows"] <- "https://bioconductor.org/packages/3.7/workflows"
# options(repos = r)

# source("https://bioconductor.org/biocLite.R")
# biocLite("rhdf5")

library(shiny)
library(shinyWidgets)
library(shinycssloaders)
library(leaflet)
library(rhdf5)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(shinyjs)
library(scales)

source("helper.R")

shp = rgdal::readOGR(dsn = "./shapefiles", layer="FlowlinesLatLong")
# # Commented code below needs to be re-run if input shapefile changes
# shp@data$id = rownames(shp@data)
# all.points = ggplot2::fortify(shp, channel_nu = "id")
# all.df = plyr::join(all.points, shp@data, by = "id") %>%
#   mutate(chan.ord = paste(channel_nu, order, sep = "."))
# # find approximate midpoint of channel
# mid = all.df %>%
#   group_by(channel_nu) %>%
#   summarise(mid = round(median(order, na.rm = TRUE), 0)) %>%
#   mutate(chan.ord = paste(channel_nu, mid, sep = "."))
# # nad = new all.df
# nad = all.df %>%
#   filter(chan.ord %in% mid$chan.ord) %>%
#   select(long, lat, channel_nu)
# write.csv(nad, "data/ChannelLatLong.csv", row.names = FALSE)

cll = read.csv("data/ChannelLatLong.csv")   # used for placing map markers for selected channels
channels = sort(cll$channel_nu)  # used for selectInput widget

siw = 150 # selectInput width

# lookup vectors for linking descriptive names to simple names for plotting purposes
x.labs = c("Velocity (ft/s)" = "velocity", "Flow (cfs)" = "flow", "Stage (ft)" = "stage")

summ.stats = c("Min" = "min",
              "1st quartile" = "first.quart",
              "Median" = "median",
              "Mean" = "mean",
              "3rd quartile" = "third.quart",
              "Max" = "max",
              "Reversal" = "prop.neg")

# don't need to rescale prop.neg; need unnamed vector for mutate_at
rescale.cols = c("min", "first.quart", "median", "mean", "third.quart", "max")

# custom ggplot2 theme
theme.mod = theme(plot.title = element_text(size = 16),
                  legend.text = element_text(size = 12),
                  legend.title = element_text(size = 12),
                  strip.text.x = element_text(size = 12),
                  strip.text.y = element_text(size = 12),
                  axis.title = element_text(size = 14),
                  axis.text = element_text(size = 12))






