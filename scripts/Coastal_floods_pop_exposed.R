#----------------------------------------------------------------------------------------
# File        : Riverine_floods_pop_exposed
# Author      : Farai Marumbwa
# Email       : fmarumbwa@unicef.org
# Organisation:Risk Analysis and Preparedness Section- Office of Emergency Programmes , UNICEF 
# Purpose     : Calculation of population exposed to Natural hazards
# Notes       : Variables to change: Admin level (shp, out_shp, Out_folder, in_raster)
# Threshold   : inraster > 0
#----------------------------------------------------------------------------------------

#import required libraries
library(here)
library(sf)
library(raster)
library(exactextractr) # zonal stats
library(rgdal)   # Replace with sf rgdal stoping 2023

#load input raster and shapefile
# note St_read best but takes time
in_raster<- raster(here('data','raw', 'raster', 'Pillar1','Flood', 'Aqueduct Floods Hazard Maps','inuncoast_historical_nosub_hist_rp0050_0.tif'))
#in_raster<- raster(here('data','raw', 'raster', 'Pillar1','Pillar1_all_files','clip_manual','flood_hazard_50_yrp_reclass_All_Countries_Clip.tif'))
pop_raster <- raster(here('data','raw', 'raster', 'Population','gpw_v4_population_count_adjusted_to_2015_unwpp_country_totals_rev11_2020_30_sec.tif'))
##shp_adm3 <- readOGR(here('data','raw', 'shp', 'All_pilot_countries','all_pilot_adm3_wfp.shp'))
#shp_adm2 <- st_read(here('data','raw', 'shp', 'All_pilot_countries','all_pilot_adm2_wfp.shp'))
shp_adm2 <- readOGR(here('data','raw', 'shp', 'All_pilot_countries','all_pilot_adm2_wfp.shp'))
#shp_adm3 <- st_read(here('data','raw', 'shp', 'All_pilot_countries','all_pilot_adm3_wfp.shp'))
plot(pop_raster)

# Convert the dataframe to shapefile
shp_adm2.df <- as.data.frame(shp_adm2)
head(shp_adm2.df,1)


### Subset individual countries
poly_filter <- c("Kenya")
phase1_pilot_filter <- c("Kenya","Antigua and Barbuda", "Dominica","Cambodia","Somalia")
phase1_pilot_country <- shp_adm2[shp_adm2$adm0_name %in% phase1_pilot_filter, ]
kenya <- shp_adm2[shp_adm2$adm0_name %in% poly_filter, ]
#kenya <- shp_adm2[shp_adm2$adm0_name %in% c( "kenya", "Somalia"), ]
Somalia = shp_adm2[shp_adm2$adm0_name == "Somalia",]
Antigua_Barbuda <- shp_adm2[shp_adm2$adm0_name %in% c( "Antigua and Barbuda"), ]
Cambodia = shp_adm2[shp_adm2$adm0_name == "Cambodia",]
Dominica = shp_adm2[shp_adm2$adm0_name == "Dominica",]
plot(kenya['adm2_id'])
plot(phase1_pilot_country['adm2_id'])

# Method 2
# Remove the "-" to plot the items defined
#filter_vars= c("kenya", "Somalia")
#kenya <- shp_adm2[-which(shp_adm2$adm0_name %in% filter_vars),]


########## Clip the population and indicator for each pilot country ###########
# To do - add map layouts for the in raster and pop  . Do loop for all countries to shorten the code

######### Processing for all pilot countries ########## 
######### Processing for all pilot countries ########## 
in_ras_phase1_pilot_country<-crop(in_raster,phase1_pilot_country)
in_ras_phase1_pilot_country_clip<-mask(in_ras_phase1_pilot_country,phase1_pilot_country)
in_pop_phase1_pilot_country<-crop(pop_raster,phase1_pilot_country)
in_pop_phase1_pilot_country_clip<-mask(in_pop_phase1_pilot_country,phase1_pilot_country)
plot(in_ras_phase1_pilot_country_clip)
# use col arg to define the background colour
plot(phase1_pilot_country, add=TRUE,col =NA, lwd=0.1,border="black")
title(main = "Population exposed to riverine floods", sub = "source: Aqueduct Floods Hazard Maps")
plot(in_ras_phase1_pilot_country_clip)

#Creating a boolean map - Select pixels greater than 0 to define flood extent
in_ras_phase1_pilot_country_clip[in_ras_phase1_pilot_country_clip > 0] <- 1
plot(in_ras_phase1_pilot_country_clip)
plot(phase1_pilot_country, add=TRUE,col =NA, lwd=0.1,border="black")

# Calculate the population exposed - Risk extent * population grid
### Resample the in_raster to match the population grid
# use nearest neigbour 'ngb'method because bilinear changes the original information to the new resolution
in_ras_phase1_pilot_country_clip <- resample(in_ras_phase1_pilot_country_clip,in_pop_phase1_pilot_country_clip)
pop_exposed_phase1_pilot_country <- in_ras_phase1_pilot_country_clip*in_pop_phase1_pilot_country_clip
plot(pop_exposed_phase1_pilot_country)

# Save the output geotiff files
#writeRaster(pop_exposed_phase1_pilot_country,here('data','processed', 'raster', 'pop_exposed', 'pop_exposed_riverine_floods_50years_phase1_pilot_country.tif'),overwrite=TRUE)


#################### Calculation of population exposed per polygon ############################

# create Raster stack for total pop and risk pop. Useful for calculating pop proportions
pop_stack <- stack(pop_exposed_phase1_pilot_country, in_pop_phase1_pilot_country_clip)
names(pop_stack) <- c("population_exposed", "total_population")


start <- Sys.time() # time the computation
#extract raster cell total pop within each polygon area
for (i in 1:nlayers(pop_stack)){
  zon_stats <- extract(pop_stack, phase1_pilot_country, fun=sum, na.rm=TRUE, df=TRUE)
}
# Calc time to execute
end <- Sys.time()
difftime(end,start)

#write to a data frame
zonal_stats_df <- data.frame(zon_stats)
# Add shapefile field from the polygon's name
zonal_stats_df$adm2_name=phase1_pilot_country$adm2_name
zonal_stats_df$adm2_id=phase1_pilot_country$adm2_id
zonal_stats_df$adm1_name=phase1_pilot_country$adm1_name
zonal_stats_df$adm1_id=phase1_pilot_country$adm1_id
zonal_stats_df$country=phase1_pilot_country$adm0_name

############ write to a CSV file (all countries)
write.csv(zonal_stats_df, file = here('data','processed','csv','pop_exposed', 'pop_exposed_riverine_flood_50year_All.csv'))

# Split the data frame into individual countries and save as csv
# Create a folder 'old' to make the script save to the correct folder
csv_var <- c('pop_exposed_coastal_flood_50year_adm2_')
df_dat <- split(zonal_stats_df, zonal_stats_df$country)
lapply(df_dat, function(x) write.csv(x, paste(here('data','processed','csv','pop_exposed_adm2'), csv_var, x$country[1], '.csv'), row.names = TRUE))


