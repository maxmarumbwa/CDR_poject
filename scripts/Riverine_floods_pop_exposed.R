#import required libraries
library(here)
library(sf)
library(raster)
library(exactextractr)
library(rgdal)   # Replace with sf rgdal stoping 2023

#library(tidyverse)
#library(rasterVis)
#library(lubridate)
#library(dplyr)

#load input raster and shapefile
# note St_read best but take time
in_raster<- raster(here('data','raw', 'raster', 'Pillar1','Pillar1_all_files','clip_manual','flood_hazard_50_yrp_reclass_All_Countries_Clip.tif'))
pop_raster <- raster(here('data','raw', 'raster', 'Pillar1','Pillar1_all_files','clip_manual','Pop_gpw_v4_population_count_adjusted_to_2015_unwpp_country_totals_rev11_2020_30_sec_Clip_All_Countries.tif'))
shp_adm2 <- readOGR(here('data','raw', 'shp', 'All_pilot_countries','all_pilot_adm2_wfp.shp'))
shp_adm3 <- readOGR(here('data','raw', 'shp', 'All_pilot_countries','all_pilot_adm3_wfp.shp'))
#shp_adm2 <- st_read(here('data','raw', 'shp', 'All_pilot_countries','all_pilot_adm2_wfp.shp'))
#shp_adm3 <- st_read(here('data','raw', 'shp', 'All_pilot_countries','all_pilot_adm3_wfp.shp'))
### Subset individual countries
Cambodia_adm2_shp <- shp_adm2[shp_adm2$ADM2_NAME %in% c( "Cambodia), ]
#plot(Cambodia_adm2_shp)
plot(shp_adm2)
plot(pop_raster)
plot(in_raster)

### Resample the in_raster to match the population grid
# use nearest neigbour 'ngb'method because bilinear changes the original information to the new resolution
in_raster <- resample(in_raster,pop_raster)

########## Reclassify the raster image ###########
## -------
#-----

## population exposed & save the raster file
pop_exposed_raster <- in_raster*pop_raster
writeRaster(pop_exposed_raster,here('data','processed', 'raster', 'pop_exposed', 'pop_exposed_riverine_floods.tif'),options=c('TFW=YES'))

# Generate the zonal stats and save to csv
pop_exposed_csv_adm2 <- extract(pop_exposed_raster, shp_adm2, fun=sum, na.rm=TRUE, df=TRUE)
pop_exposed_csv_adm3 <- extract(pop_exposed_raster, shp_adm3, fun=sum, na.rm=TRUE, df=TRUE)
### Save to csv
write.csv(pop_exposed_csv_adm2, file = here('data','processed', 'csv', 'pop_exposed', 'pop_exposed_riverine_floods.csv'))



- Add map of the hazard and pop
input map -hzard - composite index from xls (composite)

####### TO DO phase 2 ######
- Add map of the hazard and pop
- Absolute and relative child population (National value available) 
- Create composite index (gemtric avg)
- Normalisation based on the data st






Riverine flood
Data colaborative
suggested to use a recent data than the on old one shared = use WRI- Aquduct see meeting chat 10 Feb 2023.
Global inform all return periods used. In model use 1 return period but - Mantain the same return period.


From programming- historical people affected by hazard and min and number of people that can be affacted 












m



























############# VERSION 1 ########################
import required libraries
library(here)
library(sf)
library(raster)
library(exactextractr)
library(rgdal)   # Replace with sf rgdal stoping 2023

#library(tidyverse)
#library(rasterVis)
#library(lubridate)
#library(dplyr)

#load input raster and shapefile
# note St_read best but take time
in_raster<- raster(here('data','raw', 'raster', 'Pillar1','Flood','flood_hazard_50_yrp_reclass.tif'))
pop_raster <- raster(here('data','raw', 'raster', 'Population','gpw_v4_population_count_adjusted_to_2015_unwpp_country_totals_rev11_2020_30_sec.tif'))
shp_adm2 <- readOGR(here('data','raw', 'shp', 'All_pilot_countries','all_pilot_adm2_wfp.shp'))
shp_adm3 <- readOGR(here('data','raw', 'shp', 'All_pilot_countries','all_pilot_adm3_wfp.shp'))
#shp_adm2 <- st_read(here('data','raw', 'shp', 'All_pilot_countries','all_pilot_adm2_wfp.shp'))
#shp_adm3 <- st_read(here('data','raw', 'shp', 'All_pilot_countries','all_pilot_adm3_wfp.shp'))
### Subset individual countries
Cambodia_adm2_shp <- shp_adm2[shp_adm2$ADM2_NAME %in% c( "Cambodia"), ]
#plot(Cambodia_adm2_shp)
plot(shp_adm2)
plot(pop_grid)
########## Clip to the Study area###########

## Reclassify the raster image
###


## population exposed
pop_exposed <- in_raster*pop_raster
plot(pop_exposed)



















########## Clip to the Study area###########
inras_crop<-crop(in_raster,shp_adm2)
in_raster_clip<-mask(inras_crop,shp_adm2)
inpop_crop<-crop(pop_raster,shp_adm2)
pop_raster_clip<-mask(inpop_crop,shp_adm2)

plot(in_raster_clip)
plot(pop_raster_clip)
## Reclassify the raster image
###

terra::ext(in_raster_clip) <- terra::ext(pop_raster_clip)


## population exposed
pop_exposed <- in_raster_clip*pop_raster_clip
plot(pop_exposed)













library(raster)

#with raster
t <- r <- raster(ncol=100,nrow=100)
r[] <- sample(0:5,10000,replace = T)
t[] <- sample(5:15,10000,replace = T)
s <- t*r

library(terra)
#with terra
rr <- rast(r)
tt <- rast(t)
ss <- rr*tt
