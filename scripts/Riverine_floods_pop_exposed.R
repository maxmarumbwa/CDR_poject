
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
# note St_read best but takes time
in_raster<- raster(here('data','raw', 'raster', 'Pillar1','Flood', 'Aqueduct Floods Hazard Maps','inunriver_historical_000000000WATCH_1980_rp00050.tif'))
#in_raster<- raster(here('data','raw', 'raster', 'Pillar1','Pillar1_all_files','clip_manual','flood_hazard_50_yrp_reclass_All_Countries_Clip.tif'))
pop_raster <- raster(here('data','raw', 'raster', 'Population','gpw_v4_population_count_adjusted_to_2015_unwpp_country_totals_rev11_2020_30_sec.tif'))
##shp_adm3 <- readOGR(here('data','raw', 'shp', 'All_pilot_countries','all_pilot_adm3_wfp.shp'))
shp_adm2 <- st_read(here('data','raw', 'shp', 'All_pilot_countries','all_pilot_adm2_wfp.shp'))
#shp_adm2 <- readOGR(here('data','raw', 'shp', 'All_pilot_countries','all_pilot_adm2_wfp.shp'))
#shp_adm3 <- st_read(here('data','raw', 'shp', 'All_pilot_countries','all_pilot_adm3_wfp.shp'))
pop_raster(pop_raster)

# Convert the dataframe to shapefile
shp_adm2.df <- as.data.frame(shp_adm2)
head(shp_adm2.df,1)


### Subset individual countries
kenya <- shp_adm2[shp_adm2$adm0_name %in% c( "Indo"), ]
#kenya <- shp_adm2[shp_adm2$adm0_name %in% c( "kenya", "Somalia"), ]
Somalia = shp_adm2[shp_adm2$adm0_name == "Somalia",]
Antigua_Barbuda <- shp_adm2[shp_adm2$adm0_name %in% c( "Antigua and Barbuda"), ]
Cambodia = shp_adm2[shp_adm2$adm0_name == "Cambodia",]
Dominica = shp_adm2[shp_adm2$adm0_name == "Dominica",]
plot(kenya['adm2_id'])
plot(Antigua_Barbuda['adm2_id'])

# Method 2
# Remove the "-" to plot the items defined
#filter_vars= c("kenya", "Somalia")
#kenya <- shp_adm2[-which(shp_adm2$adm0_name %in% filter_vars),]


########## Clip the population and indicator for each pilot country ###########
# To do - add map layouts for the in raster and pop  . Do loop for all countries to shorten the code
# kenya
in_ras_kenya<-crop(in_raster,kenya)
in_ras_kenya_clip<-mask(in_ras_kenya,kenya)
in_pop_kenya<-crop(pop_raster,kenya)
in_pop_kenya_clip<-mask(in_pop_kenya,kenya)
plot(in_ras_kenya_clip)
plot(kenya, add=TRUE, lwd=0.5,border="green")
title(main = "Population exposed to riverine floods", sub = "source: Aqueduct Floods Hazard Maps")
plot(in_ras_kenya_clip)

#Creating a boolean map - Select pixels greater than 0
in_ras_kenya_clip[in_ras_kenya_clip > 0] <- 1
plot(in_ras_kenya_clip)

# Calculate the population exposed - Risk extent * population grid
### Resample the in_raster to match the population grid
# use nearest neigbour 'ngb'method because bilinear changes the original information to the new resolution
in_ras_kenya_clip <- resample(in_ras_kenya_clip,in_pop_kenya_clip)
pop_exposed_kenya =in_ras_kenya_clip*in_pop_kenya_clip

# Save the output geotiff files
writeRaster(in_ras_kenya_clip,here('data','processed', 'raster', 'pop_exposed', 'riverine_floods_kenya.tif'),overwrite=TRUE)
writeRaster(in_pop_kenya_clip,here('data','processed', 'raster', 'pop_exposed', 'pop_kenya.tif'),overwrite=TRUE)
writeRaster(pop_exposed_kenya,here('data','processed', 'raster', 'pop_exposed', 'pop_exposed_riverine_floods_50years_kenya.tif'),overwrite=TRUE)


### Plot the hazard maps
rbrick<- stack(in_ras_kenya_clip,pop_exposed_kenya, in_pop_kenya_clip)
plot(rbrick)

####################----------Defining legend labels-------##################
label_names_clean = c('pop1', 'pop2')
library(rasterVis)
my.at <- seq(0, 1, by = 1)
myColorkey <- list(at=my.at, ## where the colors change
                   labels=list(
                     labels=c('','no drought','drought'), ## labels
                     at=my.at ## where to print labels
                   ),names.attr=label_names_clean)

# define the colours corresponsin to the 
cols<-c('pink', 'palegreen')
#levelplot(r, at=my.at,colorkey=myColorkey, col.regions=cols, names.attr=label_names_clean)
#levelplot(vci_brick, at=my.at,colorkey=myColorkey, col.regions=cols, names.attr=label_names_clean)


# Working the adding of boundaries not working
levelplot(vci_mask,
          at=my.at,
          main="Differential vegetative drought impacts\n based on reclassified VCI",
          colorkey=myColorkey,
          col.regions=cols,
          names.attr=label_names_clean)









# Cambodia
in_ras_kenya<-crop(in_raster,kenya)
in_ras_kenya_clip<-mask(in_ras_kenya,kenya)
in_pop_kenya<-crop(pop_raster,kenya)
in_pop_kenya_clip<-mask(in_pop_kenya,kenya)
plot(in_pop_kenya_clip)
plot(in_ras_kenya_clip)
a=in_pop_kenya_clip*in_ras_kenya_clip
# Save the output geotiff files
writeRaster(in_ras_kenya_clip,here('data','processed', 'raster', 'pop_exposed', 'pop_exposed_riverine_floods.tif'),options=c('TFW=YES'))
writeRaster(in_pop_kenya_clip,here('data','processed', 'raster', 'pop_exposed', 'pop_kenya.tif'),options=c('TFW=YES'))


# Somalia
in_ras_somalia<-crop(in_raster,Somalia)
in_ras_somalia_clip<-mask(in_ras_somalia,Somalia)
in_pop_somalia<-crop(pop_raster,Somalia)
in_pop_somalia_clip<-mask(in_pop_kenya,Somalia)
plot(in_pop_somalia_clip)
plot(in_ras_somalia_clip)
plot(Somalia, add=TRUE, lwd=2)
# Save the output geotiff files
writeRaster(in_ras_kenya_clip,here('data','processed', 'raster', 'pop_exposed', 'pop_exposed_riverine_floods_somalia.tif'))
writeRaster(in_pop_kenya_clip,here('data','processed', 'raster', 'pop_exposed', 'pop_somalia.tif'))

a=in_ras_somalia_clip*in_pop_somalia_clip
plot(a)
plot(Somalia, add=TRUE, lwd=2)
# Antigua_Barbuda
in_ras_Antigua_Barbuda<-crop(in_raster,Antigua_Barbuda)
in_ras_Antigua_Barbuda_clip<-mask(in_ras_Antigua_Barbuda,Antigua_Barbuda)
in_pop_Antigua_Barbuda<-crop(pop_raster,Antigua_Barbuda)
in_pop_Antigua_Barbuda_clip<-mask(in_pop_Antigua_Barbuda,Antigua_Barbuda)
plot(in_pop_Antigua_Barbuda_clip)
plot(in_ras_Antigua_Barbuda_clip)
# Save the output geotiff files
writeRaster(in_ras_kenya_clip,here('data','processed', 'raster', 'pop_exposed', 'pop_exposed_riverine_floods_Antigua_Barbuda.tif'),options=c('TFW=YES'))
writeRaster(in_pop_kenya_clip,here('data','processed', 'raster', 'pop_exposed', 'pop_Antigua_Barbuda.tif'),options=c('TFW=YES'))


# kenya
in_ras_kenya<-crop(in_raster,kenya)
in_ras_kenya_clip<-mask(in_ras_kenya,kenya)
in_pop_kenya<-crop(pop_raster,kenya)
in_pop_kenya_clip<-mask(in_pop_kenya,kenya)
plot(in_pop_kenya_clip)
plot(in_ras_kenya_clip)
# Save the output geotiff files
writeRaster(in_ras_kenya_clip,here('data','processed', 'raster', 'pop_exposed', 'pop_exposed_riverine_floods.tif'),options=c('TFW=YES'))
writeRaster(in_pop_kenya_clip,here('data','processed', 'raster', 'pop_exposed', 'pop_kenya.tif'),options=c('TFW=YES'))




















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



















































########### ALL countries ##########################

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
Cambodia_adm2_shp <- shp_adm2[shp_adm2$ADM2_NAME %in% c( "Cambodia",) ]
plot(Cambodia_adm2_shp)
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
