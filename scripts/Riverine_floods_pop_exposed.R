
#import required libraries
library(here)
library(sf)
library(raster)
library(exactextractr) # zonal stats
library(rgdal)   # Replace with sf rgdal stoping 2023

#load input raster and shapefile
# note St_read best but takes time
in_raster<- raster(here('data','raw', 'raster', 'Pillar1','Flood', 'Aqueduct Floods Hazard Maps','inunriver_historical_000000000WATCH_1980_rp00050.tif'))
#in_raster<- raster(here('data','raw', 'raster', 'Pillar1','Pillar1_all_files','clip_manual','flood_hazard_50_yrp_reclass_All_Countries_Clip.tif'))
pop_raster <- raster(here('data','raw', 'raster', 'Population','gpw_v4_population_count_adjusted_to_2015_unwpp_country_totals_rev11_2020_30_sec.tif'))
##shp_adm3 <- readOGR(here('data','raw', 'shp', 'All_pilot_countries','all_pilot_adm3_wfp.shp'))
shp_adm2 <- st_read(here('data','raw', 'shp', 'All_pilot_countries','all_pilot_adm2_wfp.shp'))
shp_adm2 <- readOGR(here('data','raw', 'shp', 'All_pilot_countries','all_pilot_adm2_wfp.shp'))
#shp_adm3 <- st_read(here('data','raw', 'shp', 'All_pilot_countries','all_pilot_adm3_wfp.shp'))
plot(pop_raster)

# Convert the dataframe to shapefile
shp_adm2.df <- as.data.frame(shp_adm2)
head(shp_adm2.df,1)


### Subset individual countries
poly_filter <- c("Kenya")
kenya <- shp_adm2[shp_adm2$adm0_name %in% poly_filter, ]
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
# use col arg to define the background colour
plot(kenya, add=TRUE,col =NA, lwd=0.1,border="black")
title(main = "Population exposed to riverine floods", sub = "source: Aqueduct Floods Hazard Maps")
plot(in_ras_kenya_clip)

#Creating a boolean map - Select pixels greater than 0
in_ras_kenya_clip[in_ras_kenya_clip > 0] <- 1
plot(in_ras_kenya_clip)
plot(kenya, add=TRUE,col =NA, lwd=0.1,border="black")

# Calculate the population exposed - Risk extent * population grid
### Resample the in_raster to match the population grid
# use nearest neigbour 'ngb'method because bilinear changes the original information to the new resolution
in_ras_kenya_clip <- resample(in_ras_kenya_clip,in_pop_kenya_clip)
pop_exposed_kenya =in_ras_kenya_clip*in_pop_kenya_clip
plot(pop_exposed_kenya)

# Save the output geotiff files
#writeRaster(in_ras_kenya_clip,here('data','processed', 'raster', 'pop_exposed', 'riverine_floods_kenya.tif'),overwrite=TRUE)
#writeRaster(in_pop_kenya_clip,here('data','processed', 'raster', 'pop_exposed', 'pop_kenya.tif'),overwrite=TRUE)
#writeRaster(pop_exposed_kenya,here('data','processed', 'raster', 'pop_exposed', 'pop_exposed_riverine_floods_50years_kenya.tif'),overwrite=TRUE)


#################### Calculation of population exposed per polygon ############################
plot(pop_exposed_kenya)

# create Raster stack for total pop and risk pop. Useful for calculating pop proportions
pop_stack <- stack(pop_exposed_kenya, in_pop_kenya_clip)
names(pop_stack) <- c("population_exposed", "total_population")


start <- Sys.time() # time the computation
#extract raster cell total pop within each polygon area
for (i in 1:nlayers(pop_stack)){
  zon_stats <- extract(pop_stack, kenya, fun=sum, na.rm=TRUE, df=TRUE)
}
# Calc time to execute
end <- Sys.time()
difftime(end,start)

#write to a data frame
zonal_stats_df <- data.frame(zon_stats)
# Add shapefile field from the polygon's name
zonal_stats_df$adm2_name=kenya$adm2_name
zonal_stats_df$adm2_id=kenya$adm2_id
zonal_stats_df$adm1_name=kenya$adm1_name
zonal_stats_df$adm1_id=kenya$adm1_id

#write to a CSV file
write.csv(zonal_stats_df, file = here('test', 'zonal_stats.csv'))

          
 s         
          

 
