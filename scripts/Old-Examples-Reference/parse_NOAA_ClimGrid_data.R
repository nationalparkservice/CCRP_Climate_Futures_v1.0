# How untar multiple files for NOAA data

# Downloaded .tar files from NOAA ftp and unzipped to D:/nClimGrid on external hard drive.
# Next week: Add to RCF so will parse NOAA data automatically



# Initials
data.dir <- paste(tar.dir,"data",sep="/") #location for text files

# spatial data
##############################################################################
# initials
site = "CARE"

# read in parks shapefile
nps_boundary <- st_read('./data/general/spatial-data/nps_boundary/nps_boundary.shp')
park <- filter(nps_boundary, UNIT_CODE == site) # subset to WRST only
# park <- st_transform(park, st_crs(epsg))
Sp_park <- as_Spatial(park)

CRS  = crs(park) # get CRS
bbox<-data.frame(Sp_park@bbox) # get bounding box

# create stack for each var by pattern - "prcp.alaska" [AK not for whole period]
pnt.list<-list.files(path=data.dir, pattern=".prcp.conus") #list all files by var

# read in, create df, subset by max lat/lon
df1 <- read.table(paste(data.dir,pnt.list[1],sep="/"))

colnames(df1)=c("Lat","Lon","PrecipC")

dfx<-subset(df1, Lat >= bbox["y","min"] & Lat <= bbox["y","max"] &
              Lon >=bbox["x","min"] & Lon<=bbox["x","max"]) #doesn't work with small parks, need to fix

#rasterize
dfx<-as.matrix(dfx)
e<-extent(dfx[,2:1])
r <- raster(e, ncol=5, nrow=3)
x <- rasterize(dfx[, 2:1], r, dfx[,3])
plot(x)

projection(x) = CRS("+init=epsg:4326")

# Plot park over raster
Sp_park<-spTransform(Sp_park,CRSobj = "+init=epsg:5070") #reproj sp obj; #  North America Albers Equal Area Conic

# reproj raster
X<-projectRaster(x,crs=crs(Sp_park))

plot(X)
plot(Sp_park,add=TRUE)

# extract by polygon


ggplot() + geom_raster(data = dfx, aes(x=Lon, y = Lat, fill=PrecipC)) 




