


# Basic function
scatter_fun = function(x, y) {
  ggplot(dat, aes(x = .data[[x]], y = .data[[y]]) ) +
    geom_point() +
    geom_smooth(method = "loess", se = FALSE, color = "grey74") +
    theme_bw() +
    labs(x = "Longitude",
         y = "Latitude")
}

# Basic map function
map_fun = function(x, y) {
ggplot(data = world, aes(x = .data[[x]], y = .data[[y]]) ) +
  geom_sf() +
  xlab("Longitude") + ylab("Latitude")
}




