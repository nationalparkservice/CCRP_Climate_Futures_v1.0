
library(ggmap)
library(ggspatial)

###### Map of MACA cell in context of the park  #############################

# Park

park <- st_transform(park, 4326) # in order to use auto zoom feature, must be in lat/long

box = sf::st_bbox(park) # Get bbox before turning into sp object
Sp_park= as(park, "Spatial")

myMap <- get_terrainmap(bbox = c(left = Sp_park@bbox[1],
                                bottom = Sp_park@bbox[2],
                                right = Sp_park@bbox[3],
                                top = Sp_park@bbox[4]),
                       maptype = "terrain",
                       crop = FALSE, zoom = calc_zoom(lat = c(box[2],box[4]),lon=c(box[1],box[3])))
ggmap(myMap)

# MACA grid

#maca_shp <- rasterToPolygons(maca) # Create new MACA shapefile that overlaps MACA raster - will add to spatial data on SHarepoint

maca_grid_shp <- st_read('./data/general/spatial-data/Climate_grid/MACA_grid.shp') 
maca_grid_shp <- st_transform(maca_grid_shp, 4326)
maca_grid_crop <- st_crop(maca_grid_shp, box)

# MACA grid cell 
maca.sf <- st_as_sf(maca.poly)
maca.sf <- st_transform(maca.sf, 4326)

ggmap(myMap, aes(x=x, y=y)) +
  geom_sf(data = park, inherit.aes = FALSE, aes(color = "Park"), fill = NA,lwd=1) + 
  geom_sf(data = maca_grid_crop, inherit.aes = FALSE, aes(color="MACA grid"), fill = NA, lwd=0.25) +
  geom_sf(data = maca.sf, inherit.aes = FALSE,fill = NA,lwd= 1, aes(colour="Selected CMIP5 cell")) +
  scale_color_manual(values = c("Park" = "black", 
                                "MACA grid" = alpha("black", 0.25), 
                                "Selected CMIP5 cell" = "orange")) + 
  annotation_scale() + annotation_north_arrow(which_north = "True", location = "tr") +
  theme_classic() + 
  theme(axis.line = element_blank(), 
        axis.title = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank(),
        legend.title = element_blank(),
        legend.margin = margin(0,0,0,0),
        legend.position = "bottom"
        )


#st_write(maca_grid_sf, './data/general/spatial-data/Climate_grid/maca_grid.shp') 






