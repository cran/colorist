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
#  library(RColorBrewer)

## ----hues-fiespa-default------------------------------------------------------
#  # pull metrics, generate default palette, map layers
#  m1 <- metrics_pull(fiespa_occ)
#  p1 <- palette_timecycle(12)
#  map_multiples(m1, p1, labels = names(fiespa_occ), ncol = 4)

## ----echo = FALSE-------------------------------------------------------------
#  # pull metrics, generate default palette, map layers
#  m1 <- metrics_pull(fiespa_occ)
#  p1 <- palette_timecycle(12)

## ----hues-fiespa-custom-------------------------------------------------------
#  # change palette start position on color wheel
#  p1_custom <- palette_timecycle(12, start_hue = 60)
#  
#  # map layers
#  map_multiples(m1, p1_custom, labels = names(fiespa_occ), ncol = 4)

## ----hues-elephant-default----------------------------------------------------
#  # pull metrics, generate default palette, map layers
#  m2 <- metrics_pull(elephant_ud)
#  p2 <- palette_set(2)
#  map_multiples(m2, p2, labels = c("'Purple People-eater'", "'Jolly Green Giant'"), ncol = 2)

## ----hues-elephant-custom-----------------------------------------------------
#  # use custom_hues argument to make specific hue choices
#  p2_custom <- palette_set(2, custom_hues = c(280, 120))
#  
#  # map layers
#  map_multiples(m2, p2_custom, labels = c("'Purple People-eater'", "'Jolly Green Giant'"), ncol = 2)

## ----opacity-intensity-plot, echo = FALSE, fig.cap = "**Cell opacity as a function of intensity values and `lambda_i`.**"----
#  # describe modulus function
#  modulus <- function(y, lambda) {
#    if (lambda != 0) {
#      y_t <- sign(y) * ((abs(y) + 1) ^ lambda - 1) / lambda
#    } else {
#      y_t = sign(y) * log(abs(y) + 1)
#    }
#    return(y_t)
#  }
#  
#  # create data for plotting
#  d <- data.frame(y = seq(0, 1, .01), lambda = rep(seq(-12, 12, 3), each = 101))
#  for (i in 1:nrow(d)) {
#    d$y_t[i] <- modulus(d$y[i], d$lambda[i] + 1) / modulus(1, d$lambda[i] + 1)
#  }
#  
#  # plot data describing effects of adjustments to lambda
#  ggplot(d, aes(y, y_t, group = lambda, color = factor(lambda))) +
#    geom_path() +
#    scale_color_brewer(type = "div", palette = "BrBG", direction = 1,
#                       name = "lambda_i") +
#    xlab("intensity") +
#    ylab("opacity (apparent intensity)") +
#    theme(panel.background = element_blank(),
#          panel.border = element_rect(color = "black", fill = NA),
#          panel.grid = element_blank(),
#          aspect.ratio = 1,
#          legend.key = element_blank())

## ----opacity-elephant-default-------------------------------------------------
#  # map one layer
#  map_single(m2, p2_custom, layer = 2)

## ----opacity-elephant-custom--------------------------------------------------
#  # map one layer with adjustment to lambda_i
#  map_single(m2, p2_custom, layer = 2, lambda_i = -12)

## ----opacity-fisher-default---------------------------------------------------
#  # pull metrics, generate default palette, map layers
#  m3 <- metrics_pull(fisher_ud)
#  p3 <- palette_timeline(fisher_ud)
#  map_multiples(m3, p3, labels = names(fisher_ud))

## ----opacity-fisher-custom----------------------------------------------------
#  # map layers with adjustment to lambda_i
#  map_multiples(m3, p3, labels = names(fisher_ud), lambda_i = 12)

## ----chroma-specificity-plot, echo = FALSE, fig.cap = "**Cell chroma as a function of specificity values and `lambda_s`.**"----
#  # get colors from palette and edit
#  cols <- brewer.pal(9, "RdGy")
#  cols[5] <- "#F5F5F5"
#  names(cols) <- seq(-12, 12, 3)
#  
#  # plot data describing effects of adjustments to lambda_s
#  ggplot(d, aes(100 * y, 100 * y_t, group = lambda, color = factor(lambda))) +
#    geom_path() +
#    scale_color_manual(values = cols, name = "lambda_s") +
#    xlab("specificity") +
#    ylab("chroma (apparent specificity)") +
#    theme(panel.background = element_blank(),
#    panel.border = element_rect(color = "black", fill = NA),
#    panel.grid = element_blank(),
#    aspect.ratio = 1,
#    legend.key = element_blank())

## ----chroma-fisher-default----------------------------------------------------
#  # distill metrics, visualize metrics in a single map, create legend
#  m3_distill <- metrics_distill(fisher_ud)
#  map_single(m3_distill, p3, lambda_i = -5)
#  legend_timeline(p3, time_labels = c("April 7", "April 15"))

## ----chroma-fisher-custom-----------------------------------------------------
#  # visualize metrics in a single map with adjustment to lambda_s
#  map_single(m3_distill, p3, lambda_i = -5, lambda_s = 12)

## ----chroma-fiespa-default----------------------------------------------------
#  # distill metrics, visualize metrics in a single map, create legend
#  m1_distill <- metrics_distill(fiespa_occ)
#  map_single(m1_distill, p1)
#  legend_timecycle(p1, origin_label = "Jan 1")

## ----chroma-fiespa-custom-----------------------------------------------------
#  # visualize metrics in a single map with adjustment to lambda_s
#  map_single(m1_distill, p1, lambda_s = -12)

