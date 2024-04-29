## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(warning = FALSE, 
                      message = FALSE,
                      collapse = TRUE,
                      comment = "#>",
                      out.width = "\\textwidth", 
                      fig.height = 4, 
                      fig.width = 7, 
                      fig.align = "center",
                      dpi = 300)
# only build vignettes locally and not for R CMD check
knitr::opts_chunk$set(eval = nzchar(Sys.getenv("BUILD_VIGNETTES")))

## ----libraries----------------------------------------------------------------
#  library(colorist)
#  library(ggplot2)
#  library(raster)
#  library(rnaturalearth)
#  library(sf)
#  library(tigris)

## ----fiespa-------------------------------------------------------------------
#  # calculate metrics, choose a palette, make a map
#  m_fiespa <- metrics_distill(fiespa_occ)
#  p_fiespa <- palette_timecycle(fiespa_occ)
#  map_fiespa <- map_single(m_fiespa, p_fiespa)

## ----ne-downloads, results = "hide"-------------------------------------------
#  # download, transform, and crop spatial data
#  countries <- ne_download(category = "cultural", type = "countries",
#                           returnclass = "sf", scale = 110) %>%
#    st_transform(crs = st_crs(fiespa_occ)) %>%
#    st_crop(st_bbox(fiespa_occ))
#  
#  rivers <- ne_download(category = "physical", type = "rivers_lake_centerlines",
#                        returnclass = "sf", scale = 110) %>%
#    st_transform(crs = st_crs(fiespa_occ)) %>%
#    st_crop(st_bbox(fiespa_occ))
#  
#  lakes <- ne_download(category = "physical", type = "lakes", returnclass = "sf",
#                       scale = 110) %>%
#    st_transform(crs = st_crs(fiespa_occ)) %>%
#    st_crop(st_bbox(fiespa_occ))
#  
#  # create polygon describing ocean
#  ocean <- st_as_sfc(st_bbox(fiespa_occ)) %>%
#    st_difference(st_union(countries))

## ----fiespa-ne----------------------------------------------------------------
#  # add supplementary spatial data to map
#  map_fiespa_ne <- map_fiespa +
#    geom_sf(data = ocean, fill = "gray95", color = "black", size = 0.25) +
#    geom_sf(data = rivers, size = 0.25, color = "gray35") +
#    geom_sf(data = lakes, fill = "gray95", size = 0.25, color = "gray35") +
#    geom_sf(data = st_as_sfc(st_bbox(fiespa_occ)), fill = NA, color = "black",
#            size = 0.25)
#  
#  # show the map
#  print(map_fiespa_ne)

## ----fisher-multiples---------------------------------------------------------
#  # calculate metrics, choose a palette, make a series of maps
#  m_fisher <- metrics_pull(fisher_ud)
#  p_fisher <- palette_timeline(9, start_hue = -40)
#  map_fisher <- map_multiples(m_fisher, p_fisher, lambda_i = -5,
#                              labels = paste("April", 7:15))

## ----tigris-download, results="hide"------------------------------------------
#  # download, transform, and crop spatial data
#  streams <- linear_water("NY", "Rensselaer") %>%
#    st_transform(crs = st_crs(fisher_ud)) %>%
#    st_crop(st_bbox(fisher_ud))
#  
#  ponds <- area_water("NY", "Rensselaer") %>%
#    st_transform(crs = st_crs(fisher_ud)) %>%
#    st_crop(st_bbox(fisher_ud))
#  
#  roads <- roads("NY", "Rensselaer") %>%
#    st_transform(crs = st_crs(fisher_ud)) %>%
#    st_crop(st_bbox(fisher_ud))

## ----fisher-multiples-tigris, fig.height=6------------------------------------
#  # add supplementary spatial data to the series of maps
#  map_fisher +
#    geom_sf(data = streams, linetype = 6, color = "lightblue4", size = 0.25) +
#    geom_sf(data = ponds, linetype = 6, color = "lightblue4", fill = "lightblue",
#            size = 0.25) +
#    geom_sf(data = roads, size = 0.25, color = alpha("black", 0.5)) +
#    geom_sf(data = st_as_sfc(st_bbox(fisher_ud)), fill = NA, color = "gray",
#            size = 0.25)

## ----fiespa-legend------------------------------------------------------------
#  # create a legend using default settings
#  legend_timecycle(p_fiespa)

## ----fiespa-legend-custom-----------------------------------------------------
#  # change labels on legend
#  l_fiespa <- legend_timecycle(p_fiespa,
#                               origin_label = "Jan 1",
#                               # specificity labels
#                               label_s = c("Low\nseasonality",
#                                           "Moderate\nseasonality",
#                                           "High\nseasonality"),
#                               # intensity label
#                               label_i = "Peak occurrence",
#                               # layers label
#                               label_l = "Month of peak occurrence")
#  
#  # show legend
#  print(l_fiespa)

## ----fiespa-map, fig.height = 7, fig.width = 7--------------------------------
#  # position legend below map
#  map_fiespa_ne +
#    coord_sf(ylim = c(-3153281, 1405830)) +
#    annotation_custom(ggplotGrob(l_fiespa),
#                      xmin = -1482551, xmax = 1850606,
#                      ymin = -3153281, ymax = -1453281)

## ----fisher-single------------------------------------------------------------
#  # calculate distribution metrics
#  m_fisher_distill <- metrics_distill(fisher_ud)
#  
#  # download building footprints
#  f_buildings <- file.path(tempdir(), "buildings.rds")
#  download.file(paste0("https://github.com/mstrimas/colorist/raw/master/",
#                       "data-raw/buildings.rds"),
#                f_buildings)
#  buildings <- readRDS(f_buildings)
#  unlink(f_buildings)
#  
#  # make a map
#  map_fisher_distill <- map_single(m_fisher_distill, p_fisher,
#                                   lambda_i = -5, lambda_s = 10) +
#    geom_sf(data = streams, linetype = 6, color = "lightblue4", size = 0.25) +
#    geom_sf(data = ponds, linetype = 6, color = "lightblue4", fill = "lightblue",
#            size = 0.25) +
#    geom_sf(data = roads, size = 0.25, color = alpha("black", 0.5)) +
#    geom_sf(data = buildings, size = 0.25, color = alpha("black", 0.5)) +
#    geom_sf(data = st_as_sfc(st_bbox(fisher_ud)), fill = NA, color = "black",
#            size = 0.25)
#  
#  # show the map
#  print(map_fisher_distill)

## ----fisher-legend------------------------------------------------------------
#  # create a legend using default settings
#  legend_timeline(p_fisher)

## ----fisher-legend-custom-----------------------------------------------------
#  # change labels of legend
#  l_fisher <- legend_timeline(p_fisher,
#                              time_labels = c("Apr 7", "Apr 15"),
#                              # intensity label
#                              label_i = "Peak use",
#                              # layer label
#                              label_l = "Night of peak use",
#                              # specificity labels
#                              label_s = c("Consistent use", "Occasional use", "Ephemeral use"))
#  
#  # show legend
#  print(l_fisher)

## ----map-fisher, fig.width=8, fig.height=8------------------------------------
#  # position legend to the left of the map
#  map_fisher_distill +
#    coord_sf(xlim = c(-4300, 2150)) +
#    annotation_custom(ggplotGrob(l_fisher),
#                      xmin = -4400, xmax = -2500,
#                      ymin = 5100266, ymax = 5104666)

## ----elephants----------------------------------------------------------------
#  # calculate metrics, choose a palette, make a series of maps
#  m_elephants <- metrics_distill(elephant_ud)
#  p_elephants <- palette_set(elephant_ud)
#  map_elephants <- map_single(m_elephants, p_elephants, lambda_i = -5)

## ----elephants-legend---------------------------------------------------------
#  # create a legend using default settings
#  legend_set(p_elephants)

## ----elephant-legend-custom---------------------------------------------------
#  # change labels of legend
#  l_elephants <- legend_set(p_elephants,
#                            group_labels = names(elephant_ud),
#                            # intensity label
#                            label_i = "Intensity of use",
#                            # specificity label
#                            label_s = "Share of use",
#                            # specificty axis
#                            axis_s = c("equal", "sole"))
#  
#  # show legend
#  print(l_elephants)

## ----elephants-map, fig.width=8, fig.height=6---------------------------------
#  # position legend within map
#  map_elephants +
#    annotation_custom(ggplotGrob(l_elephants),
#                      xmin = -3000, xmax = 37000,
#                      ymin = -2347500, ymax = -2305000)

## ----fiespa-rgban-------------------------------------------------------------
#  # ask for map as a raster stack
#  s_fiesp <- map_single(m_fiespa, p_fiespa, return_type = "stack")
#  
#  # show stack information
#  s_fiesp

## ----fiespa-rgban-raster------------------------------------------------------
#  # plot layers using raster plot function
#  plot(s_fiesp)

## ----fiespa-rgban-map---------------------------------------------------------
#  # plot r, g, b, and alpha layers to reconstruct colorist map
#  plotRGB(s_fiesp, 1, 2, 3, alpha = as.vector(s_fiesp[["alpha"]]))

## ----elephants-rgban-qgis, eval = FALSE---------------------------------------
#  # ask for map as a RasterStack object
#  s_elephants <- map_single(m_elephants, p_elephants, lambda_i = -5,
#                            return_type = "stack")
#  
#  # save RasterStack to R working directory
#  writeRaster(s_elephants, "elephants_rgba.grd")

