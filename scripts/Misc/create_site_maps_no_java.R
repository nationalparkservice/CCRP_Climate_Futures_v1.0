
library(ggmap)

###### Map of MACA cell in context of the park  #############################

# Park

park <- st_transform(park, 4326) # in order to use auto zoom feature, must be in lat/long

box = sf::st_bbox(park) # Get bbox before turning into sp object
Sp_park= as(park, "Spatial")

myMap <- get_stamenmap(bbox = c(left = Sp_park@bbox[1],
                                bottom = Sp_park@bbox[2],
                                right = Sp_park@bbox[3],
                                top = Sp_park@bbox[4]),
                       maptype = "terrain",
                       crop = FALSE, zoom = calc_zoom(lat = c(box[2],box[4]),lon=c(box[1],box[3])))
ggmap(myMap)

# MACA grid


maca.shp <- st_read('./data/general/spatial-data/Climate_grid/MACA_grid.shp') 
maca_grid_sf <- st_as_sf(maca_shp)
maca_grid_sf <- st_transform(maca_grid_sf, 4326)
maca_crop <- st_crop(maca_grid_sf, box)

# MACA grid cell 
maca.sf <- st_as_sf(maca.poly)
maca.sf <- st_transform(maca.sf, 4326)

ggmap(myMap, aes(x=x, y=y)) +
  geom_sf(data = park, inherit.aes = FALSE, aes(color = "Park"), fill = NA,lwd=1) + 
  geom_sf(data = maca_crop, inherit.aes = FALSE, aes(color="MACA grid"), fill = NA, lwd=0.25) +
  geom_sf(data = maca.sf, inherit.aes = FALSE,fill = NA,lwd=0.5, aes(colour="Selected cell")) +
  scale_color_manual(values = c("Park" = "black", 
                                "MACA grid" = alpha("black", 0.25), 
                                "Selected cell" = "orange")) + 
  theme_classic()


  






