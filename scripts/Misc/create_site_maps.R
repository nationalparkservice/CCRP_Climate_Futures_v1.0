


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
  geom_sf(data = maca.sf, inherit.aes = FALSE,fill = NA,lwd= 1.5, aes(colour="Selected CMIP5 cell")) +
  scale_color_manual(values = c("Park" = "black", 
                                "MACA grid" = alpha("black", 0.25), 
                                "Selected CMIP5 cell" = "orange")) + 
  annotation_scale() + 
  annotation_north_arrow(which_north = "True", 
                         location = "tr", 
                         height = unit(1, "cm"), 
                         width = unit(1, "cm"), 
                         pad_x = unit(0.25, "in"), 
                         pad_y = unit(0.33, "in")) +
  theme_classic() + 
  theme(axis.line = element_blank(), 
        axis.title = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank(),
        legend.title = element_blank(),
        legend.margin = margin(0,0,0,0),
        legend.position = "bottom"
        )


ggsave(filename = paste0(SiteID, "-map-MACA-zoomed-out", ".png"), device = "png", path = './figures/maps')

############    MACA cell zoomed-in   #############################################################################

adjacent_cells <- adjacent(maca, cells = cell, directions = 8) # Find cells around centroid. 8 = "Queen's case"
adjacent_cells <- rasterFromCells(maca, adjacent_cells) # Create new raster from cells
adjacent_poly <- rasterToPolygons(adjacent_cells) # Convert raster to polygon so cell outlines are visible

adjacent_poly <- spTransform(adjacent_poly, CRSobj = "+init=epsg:4326")

adjacent_poly_sf <- st_as_sf(adjacent_poly)
box = sf::st_bbox(adjacent_poly) 

# Get bounding box and map

myMap2 <- get_stamenmap(bbox = c(left = adjacent_poly@bbox[1],
                                  bottom = adjacent_poly@bbox[2],
                                  right = adjacent_poly@bbox[3],
                                  top = adjacent_poly@bbox[4]),
                         maptype = "terrain",
                         crop = FALSE, zoom = calc_zoom(lat = c(box[2],box[4]),lon=c(box[1],box[3])))

ggmap(myMap2, aes(x=x, y=y)) + 
  geom_sf(data = adjacent_poly_sf, inherit.aes = FALSE, aes(color = "MACA grid"), fill = NA, lwd = 1) + 
  geom_sf(data = maca.sf, inherit.aes = FALSE,fill = NA,lwd= 1.5, aes(colour="Selected CMIP5 cell")) +
  scale_color_manual(values = c("MACA grid" = alpha("black", 0.5),
                                "Selected CMIP5 cell" = "orange")) + 
  annotation_scale() + 
  annotation_north_arrow(which_north = "True", 
                         location = "tr", 
                         height = unit(1, "cm"), 
                         width = unit(1, "cm"), 
                         pad_x = unit(0.25, "in"), 
                         pad_y = unit(0.33, "in")) +
  theme_classic() + 
  theme(axis.line = element_blank(), 
        axis.title = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank(),
        legend.title = element_blank(),
        legend.margin = margin(0,0,0,0),
        legend.position = "bottom"
  )
  
                                  
ggsave(filename = paste0(SiteID, "-map-MACA-zoomed-in", ".png"), device = "png", path = './figures/maps')                         

rm(myMap,myMap2,Sp_park,park,maca.sf,maca.poly,maca_grid_shp,maca_grid_crop,maca_cell,maca,adjacent_poly_sf,
   adjacent_poly,adjacent_cells)

