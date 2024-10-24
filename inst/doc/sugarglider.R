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

## ----fig.height=6-------------------------------------------------------------
vic_temp <- aus_temp |>
  filter(id %in% c("ASN00026021", "ASN00085291", "ASN00084143"))

# Define a color palette
color_palette <- c("deepskyblue4", "coral3")

p1 <- vic_temp |>
   ggplot(aes(x_major = long,
              y_major = lat,
              x_minor = month,
              ymin_minor = tmin,
              ymax_minor = tmax)) +
  geom_sf(data = abs_ste |> filter(NAME == "Victoria"),
          fill = "antiquewhite", color = "white", inherit.aes = FALSE)  +
  # Customize the size of each glyph box using the width and height parameters.
  add_glyph_boxes(color = color_palette[1]) +
  add_ref_lines(color = color_palette[1]) +
  geom_glyph_ribbon(color = color_palette[1], fill = color_palette[1]) +
  # Theme and aesthetic
  theme_glyph() +
  labs(title = "geom_glyph_ribbon()") +
  theme(plot.title = element_text(hjust = 0.5),
        title = element_text(color = color_palette[1],
                             family  = "mono")) 

p2 <- vic_temp |>
   ggplot(aes(x_major = long,
              y_major = lat,
              x_minor = month,
              y_minor = tmin,
              yend_minor = tmax)) +
  geom_sf(data = abs_ste |> filter(NAME == "Victoria"),
         fill = "antiquewhite", color = "white", inherit.aes = FALSE)  +
  # Customize the size of each glyph box using the width and height parameters.
  add_glyph_boxes(color = color_palette[2]) +
  add_ref_lines(color = color_palette[2]) +
  geom_glyph_segment(color = color_palette[2]) +
  # Theme and aesthetic
  theme_glyph() +
  labs(title = "geom_glyph_segment()") +
  theme(plot.title = element_text(hjust = 0.5),
        title = element_text(color = color_palette[2]))

grid.arrange(p1, p2, ncol = 2) 

## ----eval=FALSE---------------------------------------------------------------
#  
#  # Generate a list of unique train stations
#  df_station <- train$station_name |> unique()
#  
#  # Generate PNG of all the ribbon glyph
#  purrr::map(1:length(df_station), function(i) {
#    dt <- train |> filter(station_name == df_station[i])
#    p <- dt |>
#    ggplot(aes(x_major = long, y_major = lat,
#                     x_minor = month_year, ymin_minor = min_monthly,
#                     ymax_minor = max_monthly)) +
#      add_glyph_boxes(color = "#FFAD60",
#                      fill = "#FFEEAD", alpha = 0.5,
#                      linewidth = 1, width = 3, height  =2) +
#      add_ref_lines(color = "#FFAD60", alpha = 1,
#                    linewidth = 1, width = 3, height  =2) +
#      geom_glyph_ribbon(color = "#A66E38", fill = "#A66E38",
#                        width = 3, height  =2) +
#      theme_void()
#  
#    file_path <- paste0("figures/glyph_", df_station[i], ".png")
#    ggsave(file_path, plot = p, width = 3, height = 2, units = "in", dpi = 300,
#           bg = "transparent")
#    return(file_path)
#  
#    }) -> train_png
#  

## ----eval=FALSE---------------------------------------------------------------
#  # Create a leaflet map
#  leaflet_map <- leaflet() |>
#    addProviderTiles("CartoDB.Positron") |>
#    addScaleBar(position = "bottomleft")

## ----eval=FALSE---------------------------------------------------------------
#  # Loop through the PNG files and add them to the map
#  for (i in seq_along(train_png)) {
#    icon <- makeIcon(iconUrl = train_png[i], iconWidth = 100, iconHeight = 60)
#  
#    dt <- train |> filter(station_name == df_station[i])
#    leaflet_map <- leaflet_map |>
#      addMarkers(lng = dt$long[1], lat = dt$lat[1], icon = icon,
#                 label = dt$station_name, options = markerOptions(opacity = 0.1))
#  }
#  
#  leaflet_map

## ----eval=FALSE---------------------------------------------------------------
#  vic_nsw <- aus_temp |>
#    filter(id %in% c("ASN00026021", "ASN00085291", "ASN00084143",
#                     "ASN00055325", "ASN00049000"))
#  # Specify tooltip for ggiraph
#  vic_nsw <- vic_nsw |>
#    mutate(tooltip = paste("Station ID: ", id,
#                           "\nmonth: ", month,
#                           "\nmin-temp: ", round(tmin,2),
#                           "\nmax-temp: ", round(tmax,2)))

## ----eval=FALSE---------------------------------------------------------------
#  temp <- vic_nsw |>
#    ggplot(aes(x_major = long, y_major = lat,
#               x_minor = month, y_minor = tmin,
#               yend_minor = tmax,
#               tooltip = tooltip)) +
#    geom_sf(data = abs_ste |> filter(NAME %in% c("New South Wales", "Victoria")),
#            color = "white",
#            fill = "antiquewhite", inherit.aes = FALSE) +
#    add_glyph_boxes(color = "#CD5C08") +
#    add_ref_lines(color = "#CD5C08") +
#    geom_glyph_segment(color = "#CD5C08") +
#    coord_sf(xlim = c(140,153)) +
#    theme_glyph()
#  # Interactive plot using ggiraph
#  girafe(ggobj = temp)

