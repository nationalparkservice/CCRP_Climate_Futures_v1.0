########################################
####  GENERATE SITE MAP   ##############
########################################

# This script generates a series of maps of the park, highlighting the single MACA cell selected for analysis. 
# Multiple maps are created to accommodate differences in park sizes. Project lead can choose the map scaled most appropriately.

# NOTE: It does not seem to work to overlay the park layer onto the osm map and add transparency. You need to overlay the osm onto the park layer
# and make the osm layer transparent. 

tmap_mode('plot')

# Maps with 25 MACA cells
#"Directions" refers to the number of directions in which cells should be connected. 
# 16 = knight and one-cell queen moves. See adjacent function for more info. 

adjacent_cells <- adjacent(maca, cells = cell, directions = 16) # Find cells around centroid. 
adjacent_cells <- rasterFromCells(maca, adjacent_cells) # Create new raster from cells
adjacent_poly <- rasterToPolygons(adjacent_cells) # Convert raster to polygon so cell outlines are visible

adjacent_poly <- spTransform(adjacent_poly, CRSobj = "+init=epsg:4326")

map_types = c("bing", "osm", "stamen-terrain", "esri-topo", "apple-iphoto") # map types can be found here: https://www.rdocumentation.org/packages/OpenStreetMap/versions/0.3.4/topics/openmap

# Loop that creates series of maps with 25 MACA cells

for(i in 1:length(map_types)){
  osm <- tmaptools::read_osm(bb(adjacent_poly), type = map_types[i])
  map <- tm_shape(park) + tm_borders(col = "black", lwd = 3) + 
    tm_fill(col = "darkgreen") + 
    tm_shape(osm, is.master = TRUE) + tm_rgb(alpha = 0.8) +
    tm_shape(adjacent_poly) + tm_borders(lwd = 3, col = "orange") + 
    tm_shape(maca.poly, unit = "mi") + tm_borders(col = "#0000ff", lwd = 4) + 
    tm_add_legend(type = "line", label = "MACA grid", col = "orange", lwd = 3) + 
    tm_add_legend(type = "line", label = "Selected CMIP5 cell", col = "#0000ff", lwd = 4) +
    tm_add_legend(type = "fill", label = "National Park", col = "darkgreen") +
    tm_compass(type = "4star", position = c("right", "top"), size = 3, bg.color = "white") + tm_scale_bar(text.size = 0.75, breaks = c(0,1,2,3), bg.color = "white") +
    tm_layout(legend.position = c("left", "bottom"), legend.stack = "vertical", legend.frame = TRUE, legend.bg.color = "white", legend.text.size = 1)
  
  tmap_save(map, filename = paste0(maps, "/", SiteID, "-MACA25-", map_types[i], ".png"))
  
}

# Loop that creates maps with 9 MACA cells 

adjacent_cells <- adjacent(maca, cells = cell, directions = 8) # Find cells around centroid. 8 = "Queen's case"
adjacent_cells <- rasterFromCells(maca, adjacent_cells) # Create new raster from cells
adjacent_poly <- rasterToPolygons(adjacent_cells) # Convert raster to polygon so cell outlines are visible

adjacent_poly <- spTransform(adjacent_poly, CRSobj = "+init=epsg:4326")

map_types = c("bing", "osm", "stamen-terrain", "esri-topo", "apple-iphoto")

for(i in 1:length(map_types)){
  osm <- tmaptools::read_osm(bb(adjacent_poly), type = map_types[i])
  map <- tm_shape(park, name = "National Park") + 
    tm_borders() +
    tm_fill(col = "darkgreen") + 
    tm_shape(osm, is.master = TRUE) + tm_rgb(alpha = 0.5) +
    tm_shape(adjacent_poly) + tm_borders(lwd = 3, col = "orange") + 
    tm_shape(maca.poly, unit = "mi") + tm_borders(col = "#0000ff", lwd = 4) + 
    tm_add_legend(type = "line", label = "MACA grid", col = "orange", lwd = 3) + 
    tm_add_legend(type = "line", label = "Selected CMIP5 cell", col = "#0000ff", lwd = 4) +
    tm_add_legend(type = "fill", label = "National Park", col = "darkgreen", alpha = 0.5) +
    tm_compass(type = "4star", position = c("right", "top"), size = 3, bg.color = "white") + tm_scale_bar(text.size = 0.75, breaks = c(0,1,2,3), bg.color = "white") +
    tm_layout(legend.position = c("left", "bottom"), legend.stack = "vertical", legend.frame = TRUE, legend.bg.color = "white", legend.text.size = 1)
  
  tmap_save(map, filename = paste0(maps, "/", SiteID, "-MACA9-", map_types[i], ".png"))
  
}