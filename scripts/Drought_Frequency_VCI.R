#----------------------------------------------------------------------------------------
# File        : Riverine_floods_pop_exposed
# Author      : Farai Marumbwa
# Email       : fmarumbwa@unicef.org
# Organisation:Risk Analysis and Preparedness Section- Office of Emergency Programmes, UNICEF 
# Purpose     : Calculation of drought frequency
# Notes       : Variables to change: VCI threshlod VCI < 35 - moderate drought, VCI<20- severe drought)
#----------------------------------------------------------------------------------------


library(raster)
library(here)
library(sf)

# Update Year 2011 fro the google earth engine from 10000 to 1000)
in_shp <- st_read(here('data','raw', 'shp', 'All_pilot_countries','all_pilot_adm0_wfp.shp'))
plot(in_shp[1])
poly_filter <- c("Somalia")
shp_adm0 <- in_shp[in_shp$adm0_name %in% poly_filter, ]
plot(shp_adm0[1])

# Load VCI Images and create raster stack
inpath<- here('data','raw', 'raster', 'VCI_drought', 'VCI_CRD_pilot_countries_1km','/')
vci_list <- list.files(inpath,full.names = FALSE,pattern = "*.tif$")
vci_stack <- stack(paste0(path = inpath, vci_list))
plot(vci_stack[[1]])

# Clip the raster stack and reclassify image to quantify drought extent
# Replace values <=  to 200 
# (Use a very large number above max VCI vals)
season_vci <-  stack(paste0(path = inpath, vci_list))
season_vci_crop<-crop(vci_stack,shp_adm0)
season_vci_clip<-mask(season_vci_crop,shp_adm0)
plot(season_vci_clip)

#reclassify VCI
season_vci_clip[season_vci_clip >=35] <- 200
plot(season_vci_clip)

# Replace other values to 0
season_vci_clip[season_vci_clip < 200] <- 1
plot(season_vci_clip)
# Replace 200 to 0
season_vci_clip[season_vci_clip == 200] <- 0
plot(season_vci_clip)

# Calculate the percentage of severe wet
vci_stack_perc <- mean(season_vci_clip) * 100
plot(vci_stack_perc, main = "Frequency of drought (%) (VCI <= 35)")
summary(vci_stack_perc)

# Save the results
writeRaster(vci_stack_perc,here('data','processed', 'raster', 'pillar1', 'Drought_Frequency_VCI_less_35', 'Somalia_Drought_Frequency_VCI_less_35.tif'),overwrite=TRUE)




############ OLD ######################
######### Crop the VCI stack ########## 
vci_stack_crop<-crop(vci_stack,shp_adm0)
plot(vci_stack_crop[[1]])
vci_stack_clip<-mask(vci_stack_crop,shp_adm0)
plot(vci_stack_clip[[1]])
plot(shp_adm0, add=TRUE,col =NA, lwd=0.1,border="red")

