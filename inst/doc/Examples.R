## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  warning = FALSE,
  message = FALSE,
  fig.width = 10,
  fig.height = 8
)

## ----setup, echo=FALSE--------------------------------------------------------
library(sugarglider)
library(knitr)
library(ggplot2)
library(sf)
library(tidyverse)
library(grid)
library(viridis)
library(gridExtra)
library(ozmaps)
library(ggthemes)
library(kableExtra)
library(usmap)
library(ggiraph)


## -----------------------------------------------------------------------------
head(aus_temp) |>
  kable() |> kable_styling()

## -----------------------------------------------------------------------------
aus_temp |>
  ggplot(aes(
    x_major = long, 
    y_major = lat, 
    x_minor = month, 
    y_minor = tmin, 
    yend_minor = tmax)) +
  geom_sf(data = abs_ste, fill = "antiquewhite",
          inherit.aes = FALSE, color = "white") +
  coord_sf(xlim = c(110,155)) +
  # Add glyph box to each glyph
  add_glyph_boxes( width = 3, height = 2) +
  # Add points for weather station 
  geom_point(aes(x = long, y = lat,
                 color = "Weather Station")) +
  # Customize the size of each glyph box using the width and height parameters.
  geom_glyph_segment(
    width = 3, height = 2,
    aes(color = "Temperature")) +
  # Theme and aesthetic 
  scale_color_manual(
    values = c("Weather Station" = "firebrick",
               "Temperature" = "black")) +
  labs(color = "Data",
       title = "Daily Temperature Variations Across Australian Weather Stations")  +
  theme_glyph()


## ----fig.height = 6-----------------------------------------------------------
# Global rescale
p1 <- aus_temp |>
  ggplot(aes(
    x_major = long, 
    y_major = lat, 
    x_minor = month, 
    y_minor = tmin, 
    yend_minor = tmax)) +
  geom_sf(data = abs_ste, fill = "antiquewhite",
          inherit.aes = FALSE, color = "white") +
  coord_sf(xlim = c(110,155)) +
  # Add glyph box to each glyph
  add_glyph_boxes(width = 3, height = 2) +
  # Add reference lines to each glyph
  add_ref_lines(width = 3, height = 2) +
  # Glyph segment plot with global rescale
  geom_glyph_segment(global_rescale = TRUE,
                     width = 3, height = 2) +
  labs(title = "Global Rescale") +
  theme_glyph()
  
# Local Rescale
p2 <- aus_temp |>
  ggplot(aes(
    x_major = long, 
    y_major = lat, 
    x_minor = month, 
    y_minor = tmin, 
    yend_minor = tmax)) +
  geom_sf(data = abs_ste, fill = "antiquewhite",
          inherit.aes = FALSE, color = "white") +
  coord_sf(xlim = c(110,155)) +
  # Add glyph box to each glyph
  add_glyph_boxes(width = 3, height = 2) +
  # Add reference lines to each glyph
  add_ref_lines(width = 3, height = 2) +
  # Glyph segment plot with local rescale
  geom_glyph_segment(global_rescale = FALSE,
                     width = 3, height = 2) +
  labs(title = "Local Rescale") +
  theme_glyph()

grid.arrange(p1, p2, ncol = 2) 

## -----------------------------------------------------------------------------
prcp <- aus_temp |>
   group_by(id) |>
   mutate(prcp = mean(prcp, na.rm = TRUE)) |>
   ggplot(aes(x_major = long, y_major = lat,
              x_minor = month, ymin_minor = tmin,
              ymax_minor = tmax, 
              fill = prcp, color = prcp)) +
  geom_sf(data = abs_ste, fill = "antiquewhite",
          inherit.aes = FALSE, color = "white") +
  # Add glyph box to each glyph
   add_glyph_boxes() +
  # Add ref line to each glyph
   add_ref_lines() +
  # Add glyph ribbon plots
   geom_glyph_ribbon() +
   coord_sf(xlim = c(112,155)) +
  # Theme and aesthetic 
  theme_glyph() +
  scale_fill_gradientn(colors = c("#ADD8E6", "#2b5e82", "dodgerblue4")) +
  scale_color_gradientn(colors = c( "#ADD8E6", "#2b5e82", "dodgerblue4")) +
  labs(fill = "Percepitation", color = "Percepitation",
       title = "Precipitation and Temperature Ranges Across Australia") 

prcp

## -----------------------------------------------------------------------------

fact <- historical_temp |> 
  filter(id %in% c("ASN00026021", "ASN00085291", "ASN00084143")) |>
   ggplot(aes(color = factor(year), fill = factor(year),
              group = interaction(year,id),
              x_major = long, y_major = lat,
              x_minor = month, ymin_minor = tmin, 
              ymax_minor = tmax)) +
  geom_sf(data = abs_ste |> filter(NAME == "Victoria"),
           fill = "antiquewhite", color = "white",
          inherit.aes = FALSE)  +
  # Customized the dimension of each glyph with `width` and `height` parameters
   add_glyph_boxes(width = rel(2),
                   height = rel(1.5)) +
   add_ref_lines(width = rel(2),
                 height = rel(1.5)) +
   geom_glyph_ribbon(alpha = 0.5,
                     width = rel(2),
                     height = rel(1.5)) +
  labs(x = "Longitude", y = "Latitude",
       color = "year", fill = "year",
       title = "Temperature Trends in Selected Victorian Weather Stations") +
  # Theme and aesthetic
  theme_glyph() +
  theme(legend.position.inside = c(.4,0)) +
  scale_colour_wsj("colors6") +
  scale_fill_wsj("colors6") 

fact


## ----fig.height=6-------------------------------------------------------------
set.seed(28493)
legend <- aus_temp |>
   ggplot(aes(x_major = long, y_major = lat,
              x_minor = month, ymin_minor = tmin,
              ymax_minor = tmax)) +
  geom_sf(data = abs_ste, fill = "antiquewhite",
          inherit.aes = FALSE, color = "white") +
  add_glyph_boxes(color = "#227B94") +
  add_ref_lines(color = "#227B94") +
  add_glyph_legend(color = "#227B94", fill = "#227B94") +
  # Add a ribbon legend
  geom_glyph_ribbon(color = "#227B94", fill = "#227B94") +
  theme_glyph()  + 
  labs(title = "Temperature Ranges Across Australia with Glyph Legend") 

legend


