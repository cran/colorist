## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(warning = FALSE, 
                      message = FALSE,
                      collapse = TRUE,
                      comment = "#>",
                      out.width = "\\textwidth", 
                      fig.height = 4, 
                      fig.width = 7, 
                      fig.align = "center")
BUILD_VIGNETTES <- isTRUE(as.logical(Sys.getenv("BUILD_VIGNETTES")))

## ----fiespa-data--------------------------------------------------------------
library(sf)
library(ggplot2)
library(colorist)

# load data 
data("fiespa_occ")
fiespa_occ

## ----fiespa-metrics-----------------------------------------------------------
# pull information from the stack
m1 <- metrics_pull(fiespa_occ)
m1

## ----fiespa-palette-----------------------------------------------------------
# generate a color palette
p1 <- palette_timecycle(fiespa_occ)
head(p1)

## ----fiespa-mapmult, dpi = 150, eval = BUILD_VIGNETTES------------------------
#  # map each of the layers
#  map_multiples(m1, p1, ncol = 4, labels = names(fiespa_occ))

## ----fiespa-mapsing-----------------------------------------------------------
# map one layer
map_single(m1, p1, layer = 6)

## ----fiespa-distill-----------------------------------------------------------
# distill distribution information across layers
m1_distill <- metrics_distill(fiespa_occ)

# visualize distilled information on a single map
map_single(m1_distill, p1)

## ----fielsp-legend------------------------------------------------------------
# generate a legend
legend_timecycle(p1, origin_label = "Jan 1")

## ----fisher-data--------------------------------------------------------------
# loda data
data("fisher_ud")
fisher_ud

## ----fisher-map---------------------------------------------------------------
# pull information from the stack
m2 <- metrics_pull(fisher_ud)

# generate a color palette
p2 <- palette_timeline(fisher_ud)

# map each of the layers
map_multiples(m2, p2)

## ----fisher-lambda_i----------------------------------------------------------
# map each of the layers and adjust visual weights
map_multiples(m2, p2, lambda_i = -5)

## ----fisher-distill-----------------------------------------------------------
# distill distribution information across layers
m2_distill <- metrics_distill(fisher_ud)

# visualize distilled information on a single map
map_single(m2_distill, p2, lambda_i = -5)

## ----fisher-legend------------------------------------------------------------
# generate a legend
legend_timeline(p2, time_labels = c("April 7", "April 15"))

## ----elephant-pull------------------------------------------------------------
# load data
data("elephant_ud")

# pull information from the stack
m3 <- metrics_pull(elephant_ud)

# assign a color palette
p3 <- palette_set(elephant_ud)

# generate maps for each individual
map_multiples(m3, p3, ncol = 2, lambda_i = -5, labels = names(elephant_ud))

## ----elephant-distill---------------------------------------------------------
# distill distribution information across individuals
m3_distill <- metrics_distill(elephant_ud)

# visualize distilled information on a single map
map_single(m3_distill, p3, lambda_i = -5)
# generate a legend
legend_set(p3, group_labels = names(elephant_ud))

## ----elephant-sfdl, eval = FALSE----------------------------------------------
#  # download data to a temp directory
#  url <- "https://github.com/mstrimas/colorist/raw/master/data-raw/"
#  f <- file.path(tempdir(), "etosha-features.gpkg")
#  download.file(paste0(url, basename(f)), f)

## ----elephant-sfpath, echo = FALSE--------------------------------------------
f <- "../data-raw/etosha-features.gpkg"

## ----elepaphant-sf, eval = BUILD_VIGNETTES------------------------------------
#  pans <- read_sf(f, layer = "pans") %>%
#    st_transform(crs = st_crs(elephant_ud))
#  
#  waterholes <- read_sf(f, layer = "waterholes") %>%
#    st_transform(crs = st_crs(elephant_ud))
#  
#  park <- read_sf(f, layer = "etosha") %>%
#    st_transform(crs = st_crs(elephant_ud))
#  
#  roads <- read_sf(f, layer = "roads") %>%
#    st_transform(crs = st_crs(elephant_ud))

## ----elephant-pretty, dpi = 150, fig.width = 6, fig.height = 3.5, eval = BUILD_VIGNETTES----
#  # visualize both distributions on a single map and add environmental data
#  elephant_map <- map_single(m3_distill, p3, lambda_i = -5) +
#    geom_sf(data = pans, alpha = 0.2, size = 0.15, color = "gray40") +
#    geom_sf(data = roads, size = 0.1, color = "gray60") +
#    geom_sf(data = waterholes, size = 0.25) +
#    geom_sf(data = park, size = 3, fill = NA, color = alpha("gray60", 0.2)) +
#    geom_sf(data = park, size = 0.2, fill = NA, color = "gray20", linetype = 6) +
#    ggtitle("Two Elephants in Etosha National Park")
#  
#  # show the map
#  elephant_map

## ----elephant-save, eval = FALSE----------------------------------------------
#  # save the map
#  ggsave(plot = elephant_map,
#         filename = "afrele_map_singles.png",
#         width = 6,
#         height = 3.5,
#         dpi = 600)

