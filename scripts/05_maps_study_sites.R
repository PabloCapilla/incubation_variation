##
##
##### Script aim: #####
#'
#' 
##
##

##
##
##### libraries #####
##

pacman::p_load(openxlsx, gtsummary, gt, ggokabeito,
               lubridate, dplyr, tidyr,
               lme4, performance, rptR,
               stringr,
               cowplot,
               ggdist,
               ggsn,
               ggmap,
               ggokabeito,   
               gghalves,     
               ggbeeswarm,
               ggspatial,
               ggplot2, extrafont)
loadfonts()

#####

##
##
##### site coor #####
##
##
coor <- read.csv('./data/site_coordenates.csv')
coor


# map of UK in Europe
# Define the bounding box for your map
bbox_large <- c(
  left = -15.00,
  bottom = 25.072,
  right = 28.66,
  top = 60.00
)



# Get a basemap. Adjust zoom as needed (3-5 for continental, higher for more detail)
ggmap::register_stadiamaps(key = '575f493a-55fd-4d74-8943-b9bee4a61306')
basemap_large <- get_stadiamap(bbox_large, zoom = 3, maptype = "stamen_terrain_background")
ggmap(basemap_large) 

# Plot the map with the migration path
offset = 1
map_large <- ggmap(basemap_large) +
  geom_rect(aes(xmin = bbox[1]-offset, xmax = bbox[3]+offset, ymin = bbox[2]-offset, ymax = bbox[4]+offset),
            fill = NA,
            color = 'darkblue',
            linewidth = 0.75) +
  theme_minimal() +
  labs(
    x = "Longitude",
    y = "Latitude") +
  theme(legend.position = "none", 
        axis.text = element_text(size = 25),
        axis.title = element_text(size = 30)) +
  annotation_north_arrow(
    location = "topright",  # Position of the arrow
    which_north = "true",   # "true" for geographic north, "grid" for grid north
    style = north_arrow_fancy_orienteering() # Choose the arrow style
  ) 
map_large

# Define the bounding box for your map
bbox <- c(
  left = -4.75,
  bottom = 55.80,
  right = -4.15,
  top = 56.20
)



# Get a basemap. Adjust zoom as needed (3-5 for continental, higher for more detail)
ggmap::register_stadiamaps(key = '575f493a-55fd-4d74-8943-b9bee4a61306')
basemap <- get_stadiamap(bbox, zoom = 10, maptype = "stamen_terrain")
ggmap(basemap) 

# Plot the map with the migration path
map_zoom <- ggmap(basemap) +
  geom_point(
    data = coor,
    aes(x = lon, y = lat, fill = habitat),
    shape = 21,
    color = 'black',
    size = 4
  ) +
  scale_fill_okabe_ito() +
  scale_color_okabe_ito() +
  theme_minimal() +
  labs(
    x = "Longitude",
    y = "Latitude",
    color = "Habitat"
  ) +
  theme(legend.position = "none", 
        axis.text = element_text(size = 25),
        axis.title = element_text(size = 30), 
        strip.text = element_text(size = 20)) +
  annotation_north_arrow(
    location = "topright",  # Position of the arrow
    which_north = "true",   # "true" for geographic north, "grid" for grid north
    style = north_arrow_fancy_orienteering() # Choose the arrow style
  ) 
  

## plot
#
##
##### Panel Figure 1 #####
##
##
map <- ggdraw() +
  draw_plot(map_large, x = 0.00, y = 0.00, width = 0.48, height = 1.00) +
  draw_plot(map_zoom, x = 0.50, y = 0.00, width = 0.48, height = 1.00) +
  draw_plot_label(label = c("A", "B"), 
                  size = 25,
                  x = c(0.00, 0.50), 
                  y = c(1.00, 1.00)) 
map

ggsave(filename = "./plots/Map_study_sites.png", 
       plot = map, 
       units = "mm",
       device = "png", 
       width = 250,
       height = 175)


ggsave(filename = "./plots/Map_study_sites.jpg", 
       plot = map, 
       units = "mm",
       device = "jpg", 
       width = 250,
       height = 175)
