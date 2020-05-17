library(raster)
library(mltools)

###
# Plot SAPIA style abundance map
###

# 1. Data processing 

###########################################################################

# Initialise a raster of uniform cells over South Africa 
rast <- raster(xmn= -180, 
               ymn= -90, 
               xmx = 180, 
               ymx = 90, 
               resolution = 0.5,
               crs = '+proj=utm +zone=33 +ellps=WGS84 +datum=WGS84 +units=m +no_defs ')

# Set the resolution of the raster 
x_res <- 0.25
y_res <- 0.25 
res(rast) <- c(x_res, y_res)  
res(rast)

# Get xy-cords into a matrix 
xy <- raw_data %>%
  dplyr::select(longitude, latitude) %>%
  as.matrix()

# Count the number of GPS points in each raster cell
r0 <- rasterize(xy, 
                rast, 
                fun = "count")

# Convert matrix to data frame
datf <- as.data.frame(r0, 
                      xy=TRUE)


############################################################################

# 2. Plot presence/absence map

# Remove raster cells with no records 
# Must do this so that it doesn't plot points in every cell 
datf <- datf %>%
  dplyr::filter(layer > 0)

# Plot map
ggplot(data = world) +
  geom_point(data = datf, aes(x = x,
                               y = y,
                               fill = layer)) +
  scale_fill_viridis_c(na.value = "white") +
  geom_sf(fill = "NA") +
  labs(x = "Longitude", 
       y = "Latitude") + 
  coord_sf(xlim = c(15.5, 33.5), 
           ylim = c(-35, -21.75), 
           expand = FALSE) +
  annotation_scale(location = "br", # 
                   style = "ticks", 
                   width_hint = 0.150) +
  annotation_north_arrow(location = "br", 
                         which_north = "true", 
                         pad_x = unit(0.175, "in"), 
                         pad_y = unit(0.3, "in"),
                         style = north_arrow_fancy_orienteering)

#######################################################
# - Plot abundance map
########################################################

# Remove raster cells with no records 
# Must do this so that it doesn't plot points in every cell 

# Convert matrix to data frame
datf <- as.data.frame(r0, 
                      xy=TRUE)

# Replace NA with 0
datf <- datf %>%
  tidyr::drop_na(layer)

# Remove raster cells with no records 
# Must do this so that it doesn't plot points in every cell 
#datf <- datf %>%
#dplyr::filter(layer > 0)
#datf 

# Cut into five categories
# Cat1 = 0 records (filtered out = white cells)
# Cat2 = 1 record
# Cat3 = 2-5 records
# Cat4 = 6-25 records
# Cat5 = 25+
datf$layer <- 
  cut(datf$layer, breaks = c(0, 1, 2, 5, 25), 
      labels = c("1", "2-5", "6-25", "25+"))

# Plot abundance map
ggplot(data = world) +
  geom_sf(fill = "white") +
  geom_tile(data = datf, aes(x = x,
                             y = y,
                             fill = layer)) +
  scale_fill_manual(values = c("gray75", 
                               "gray50", 
                               "gray25", 
                               "black"),
                    drop = FALSE,
                    labels = c("1", "2-5", "6-25", "25+")) +
  labs(x = "Longitude", 
       y = "Latitude",
       fill = "No. of records") + 
  coord_sf(xlim = c(15.5, 33.5), 
           ylim = c(-35, -21.75), 
           expand = FALSE) +
  annotation_scale(location = "br", # 
                   style = "ticks", 
                   width_hint = 0.150) +
  annotation_north_arrow(location = "br", 
                         which_north = "true", 
                         pad_x = unit(0.175, "in"), 
                         pad_y = unit(0.3, "in"),
                         style = north_arrow_fancy_orienteering) +
  theme(legend.position = c(0.135, 0.775),
        legend.title = element_text(size = 12.5),
        legend.text = element_text(size = 12.5)) +
  annotate("text", 
           x = 19.2, 
           y = -22.5, 
           fontface = "italic",
           size = 5,
           label = "Sporobolus pyramidalis")

ggsave("./figures/sapia_legend_example.png",
       width = 8,
       height = 6)
