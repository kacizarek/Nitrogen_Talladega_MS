#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Title: Watershed Delineation
#Coder: Kaci Zarek
#Date Created: 3/3/2023
#Purpose: Create a TAL amp
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
remove(list=ls())

#load packages
library(tidyverse)
library(raster)
library(sf)
#install.packages("elevatr")
library(elevatr)
#install.packages("whitebox")
#whitebox::install_whitebox()
library(whitebox)
#install.packages("stars")
library(stars)
#install.packages("mapview")
library(mapview)
library(hillshader)
library(tmap)

#Create temp dir
temp_dir <- "C:\\Users\\Kaci Zarek\\Documents\\MS_manuscript\\TAL_Data\\Nitrogen_Talladega_MS\\temp"

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Gather data ------------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Import Data
pnts <-read_csv("01_tidydata/TAL_sitemap_points.csv") %>% 
  st_as_sf(
    coords = c("long", "lat"), 
    crs = '+proj=longlat +datum=WGS84 +no_defs')

#2.1 -----
#separate into sensor and LTM points
tal_supersensor <- pnts %>% 
  filter(sensortype == "scan")

tal_ltms <- pnts %>% 
 filter(sensortype == "LTM") 

tal_extra <- pnts %>% 
  filter(sensortype == "extra") 

STIC <- pnts %>% 
  filter(sensortype == "STIC") 

#2.2 ----
#Define watershed outlet and sites for map
outlet <- st_as_sf(tal_supersensor, coords = c("long", "lat"), 
  crs = '+proj=longlat +datum=WGS84 +no_defs')

LTMs <- st_as_sf(tal_ltms, coords = c("long", "lat"), 
                  crs = '+proj=longlat +datum=WGS84 +no_defs')

dnfexp <- st_as_sf(tal_extra, coords = c("long", "lat"), 
                 crs = '+proj=longlat +datum=WGS84 +no_defs')

STICs <- st_as_sf(STIC, coords = c("long", "lat"), 
                 crs = '+proj=longlat +datum=WGS84 +no_defs')

#2.3 ----
#Define area of interest based on coordinates
pnt1<-c(33.754, -85.583)
pnt2<-c(33.79, -85.61)
area_of_interest<-tibble(
  lat = c(pnt1[1], pnt1[1], pnt2[1], pnt2[1]),
  lon = c(pnt1[2], pnt2[2], pnt1[2], pnt2[2])) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = '+proj=longlat +datum=WGS84 +no_defs')
##Load in DEM if already made
#DEM <- raster(paste0(data_dir, "\\TAL_croppedDEM.tif"))

#Download DEM
dem <- get_elev_raster(area_of_interest, z=14)

#Export data to temp directory
writeRaster(dem,paste0(temp_dir, '\\dem.tif'), overwrite=T)
st_write(outlet, paste0(temp_dir, "\\outlet.shp"), append=F)
mapview(dem)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create fdr and face ----------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Smooth the dem
wbt_fast_almost_gaussian_filter(
  input = "dem.tif",
  output = "dem_smooth.tif", 
  sigma = 1.8, 
  wd = temp_dir
)

#breach depressions 
wbt_breach_depressions(
  dem = "dem_smooth.tif",
  output = "dem_breach.tif", 
  wd= temp_dir
)

#flow direction
wbt_d8_pointer(
  dem = "dem_breach.tif",
  output = "fdr.tif",
  wd = temp_dir)

#flow accumulation
wbt_d8_flow_accumulation(
  input = 'dem_breach.tif',
  output = 'fac.tif',
  pntr = F,
  wd = temp_dir
)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Delineate Watershed-----------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Snap pour point
wbt_snap_pour_points(
  pour_pts = "outlet.shp",
  flow_accum = "fac.tif",
  snap_dist = 0.001,
  output = "snap.shp",
  wd = temp_dir
)

#Check snap pour point with mapview (you may need to change the search dist)
fac<-raster(paste0(temp_dir, "//fac.tif"))
snap<-st_read(paste0(temp_dir,"//snap.shp"))
mapview(fac) + mapview(snap) + mapview(outlet)

#Create watershed
wbt_watershed(
  d8_pntr  = "fdr.tif",
  pour_pts = "snap.shp",
  output   = "watershed.tif",
  wd       = temp_dir
)

#read into R so we can use polygon area to get WS area
watershed <- raster(paste0(temp_dir, "//watershed.tif"))

#Convert raster to vector
watershed <- watershed %>% st_as_stars() %>% st_as_sf(., merge = TRUE)

#Plot for funzies
mapview(dem) + mapview(watershed) + mapview(outlet) + mapview(dnfexp)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create flownet ---------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Create flow net
wbt_extract_streams(
  flow_accum = "fac.tif",
  output = "flow_net.tif",
  threshold = 650, 
  wd = temp_dir)

#convert to vector
wbt_raster_streams_to_vector(
  streams = "flow_net.tif", 
  d8_pntr = "fdr.tif", 
  output = "streams.shp", 
  wd = temp_dir)

#Read into R environement
flow_net<-st_read(
  paste0(temp_dir, "\\streams.shp"), 
  crs = st_crs('+proj=longlat +datum=WGS84 +no_defs'))

mapview(flow_net) + mapview(watershed)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Prep to make a pretty map ----------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Define extent
#extent_ws <- extent(-85.61,-85.583, 33.754, 33.79) ##Doesn't plot onto tmap right
extent_ws <- extent(-85.61, -85.594, 33.756 , 33.773) ##Makes perfect tmap!

#Crop DEM
dem_crop <- crop(dem, extent_ws)
dem_mask <- mask(dem_crop, watershed)

#Crop flownet (streams)
flow_net <- flow_net[watershed,]

mapview(dem_mask) + mapview(watershed) + mapview(flow_net)

#determine area of watershed (92ha)
st_area(watershed)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Make a pretty map ------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Make map a topo map
#3.0----
  #First, create topo features
slope = terrain(dem_mask, opt = "slope")
aspect = terrain(dem_mask, opt = 'aspect')
hill = hillShade(slope, aspect, angle = 40, 
                 direction = 270, normalize = TRUE)
hill_spdf <- as(hill, "SpatialPixelsDataFrame")
hill_spdf <- as.data.frame(hill_spdf)
colnames(hill_spdf) <- c("value", "x", "y")

#3.1----
#Add plot onto topo map
#Further N2 excess map making time!
p1 <- ggplot() + 
  geom_sf(data = watershed, col = "black", lwd = 0.8) +
  geom_raster(data = hill_spdf, 
              aes(x=x, y=y, fill = value)) +
  geom_sf(data = flow_net, col = "darkblue", lwd = 0.8) + 
  geom_sf(data = tal_ltms, aes(color = sensortype), size = 4, fill = "purple1",
          color = "black",
          shape = 21) +
  geom_sf(data = tal_supersensor, aes(color = sensortype), size = 4, fill = "springgreen3",
          color = "black", shape = 23) +
  scale_fill_gradient(low = "grey25", high = "grey100", name = "Elevation") +
  guides(fill=guide_legend(title="Sampling Sites")) +
  theme_classic() +
  theme(text = element_text(size = 10),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.line = element_blank(), 
        axis.ticks = element_blank(),
        legend.position = "none") 
  #theme(text = element_text(size = 10),
  #axis.text = element_text(size = 6)) 

#Add compass to map
p1 <- p1 + 
  ggspatial::annotation_north_arrow(
    location = "bl", which_north = "true",
    height = unit(0.4, "in"), width = unit(0.4, "in"),
    pad_x = unit(0.8, "cm"),
    pad_y = unit(1.4, "cm"),
    style = ggspatial::north_arrow_orienteering(
      fill = c("black", "white"),
      line_col = "grey20")) 
p1 + ggspatial::annotation_scale(
  location = "bl",
  plot_unit = "km",
  pad_x = unit(0.6, "cm"),
  pad_y = unit(0.8, "cm"),
)

#3.2----
tmap <- tm_shape(dem_mask) +
  tm_raster(palette = "-Greys", n = 8, alpha = 0.75, legend.reverse = TRUE, 
            legend.show = T, title = "Elevation [m]") +
  tm_shape(watershed) +
  tm_borders(col = "black", lwd = 4) +
  tm_shape(flow_net) +
  tm_lines(col = "darkblue", lwd = 3) +
  ##add sampling sites 
  tm_shape(tal_ltms) +
  tm_dots(size = 1.75, shape = 21, col = "purple1", border.col = "black", border.lwd = 3) +
  ##add high frequency sensor station site
  tm_shape(outlet) +
  tm_dots(size = 1.75, shape = 24, col = "springgreen1", border.col = "black", border.lwd = 3) +
  ##add experimental sampling sites for follow up study in 2023
  tm_shape(dnfexp) +
  tm_dots(size = 1.75, shape = 24, col = "purple1", border.col = "black", border.lwd = 3) +
  ##add STIC sensors to amp
  tm_shape(STICs) +
  tm_dots(size = 1.5, shape = 21, col = "white", border.col = "black", border.lwd = 3) +
  tm_layout(
    legend.title.fontface = "bold",
    legend.title.size = 1.4,
    legend.frame = FALSE,
    legend.position = c(0.54, 0.6),
    legend.bg.color = "white",
    legend.bg.alpha = 0.54,
    legend.text.size = 1,
    legend.width = 1.5, legend.height = 0.5) +
  tm_add_legend(type = "symbol", size = 1, shape = c(24,21,21,24),
                col = c("purple1", "purple1","white","springgreen3"),
                border.col = c("black", "black", "black","black"),
                labels = c("Denitrification potentials & sampling site","Sampling sites",
                           "STIC sensors", "High-frequency sensor station & sampling site")) + 
  tm_compass(text.size = 1.5,
             position = c(0.02, 0.1)) +
  tm_scale_bar(text.size = 1.1,
               position = c("left", "bottom"))
print(tmap)
#Save final site map
tmap_save(tm = tmap, filename = "03_plots/TAL_sitemap.png")









