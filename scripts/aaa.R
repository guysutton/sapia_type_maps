# create a raster
r <- raster(ncols=36, nrows=18)
n <- 1000

# create some points
set.seed(123)
x <- runif(n) * 360 - 180
y <- runif(n) * 180 - 90
xy <- cbind(x, y)
class(xy)

# count the number of points in each raster cell
r0 <- rasterize(xy, r, fun = "count")
r0

df <- as.data.frame(r0, xy=TRUE)
df

plot(r0); points(xy, pch = 16, cex=0.5)



###
###
###

# Create a raster of cells over South Africa 
rast <- raster(resolution = 0.5,
               crs = '+proj=utm +zone=33 +ellps=WGS84 +datum=WGS84 +units=m +no_defs ')

# Get xy-cords into a matrix 
xy <- raw_data %>%
  dplyr::select(longitude, latitude) %>%
  as.matrix()
head(xy)
class(xy)

# count the number of points in each raster cell
r0 <- rasterize(xy, rast, fun = "count")
r0

# Set the resolution (in metres) of the raster 
x_res <- 10000  # 10km resolution in x
y_res <- 10000  # 10km resolution in y
res(rast) <- c(x_res, y_res)  
res(rast)

# convert to data.frame and plot with ggplot
datf <- as.data.frame(rast, xy=TRUE)
head(datf)

# Plot map
ggplot(data = world) +
  geom_sf(fill = "NA") +
  geom_raster(data = datf, aes(x = )) +
  #geom_point(data = df, aes(x = longitude, 
  #                          y = latitude)) + 
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


# Plot abundance map
ggplot(data = world) +
  geom_tile(data = datf, aes(x = x,
                             y = y,
                             fill = layer)) +
  scale_fill_viridis_c(na.value = "white") +
  geom_sf(fill = "NA") +
  #geom_point(data = df, aes(x = longitude, 
  #                          y = latitude)) + 
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