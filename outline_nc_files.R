
##Aggregate SILO monthly and annual and build map layers
##================================
rm(list=ls())

##Libraries and functions=================================================================
# install.packages("raster")
# install.packages("ncdf4")
# install.packages("RNetCDF")
# install.packages("data.table")
# install.packages("reshape2")
# install.packages("doBy")
# install.packages("maptools")
# install.packages("maps")
# install.packages("rasterVis")
# install.packages("lattice")
# install.packages("mapdata")
# install.packages("RColorBrewer")
# install.packages("lubridate")
# install.packages("spatial.tools")
# install.packages("mapdata")
# install.packages("RSenaps")
# install.packages("settings")
# install.packages("httr")
# install.packages("maps")


library(sp)
#library(rgdal)
library(raster)

library(ncdf4)
library(RNetCDF)
library(RColorBrewer)
library(data.table)
library(reshape2)
library(doBy)
library(maptools)
library(maps)
library(lattice)
library(latticeExtra)
library(rasterVis)
library(mapdata)

library(lubridate)
#library(spatial.tools)
library(mapdata)
require(RSenaps) #error message for my R version
library(settings)
library(httr)
library(sf)
#library(rgeos)
library(tidyverse)

#===========================
# Compute temp variables ------------------------------------------------------




############################################################################################################################
################### Start here ############################################################################################
#setwd("W:/Pastures/Gridded_seasonal_break") #jackie
setwd("W:/Economic impact of weeds round 2/Climate/AEZ")#jackie

GRDC_bound_mallee <- sf::st_read("SA_Vic_Mallee.shp")
GRDC_bound_mallee_sf <- as(GRDC_bound_mallee, "Spatial") #convert to a sp object
plot(GRDC_bound_mallee_sf)



#as a function per year for rainfall in GS
#function one

function_rainfall_GS <- function(year_input) {
  
  # daily_rain <- raster::brick(
  #   paste("//af-osm-05-cdc.it.csiro.au/OSM_CBR_AF_CDP_work/silo/daily_rain/",
  #         year_input, ".daily_rain.nc", sep = ""),varname = "daily_rain")
  
  daily_rain <- raster::brick(
    paste("I:/work/silo/daily_rain/",
          year_input, ".daily_rain.nc", sep = ""),varname = "daily_rain")
  
  #crop to a fix area
  daily_rain_crop <- raster::crop(daily_rain, GRDC_bound_mallee_sf)
  
  #only use a few days classic GS
  daily_rain_crop_subset_day <- subset(daily_rain_crop, 91:304) #pull out the 1st April- Oct Classic GS (nb leap yrs 92 -305)
 
  
  #Add the moving window
  GS_rainfall2000_MovMean7 <- calc(daily_rain_crop_subset_day, function(x) movingFun(x, 7, mean, "to"))
  
  #add in the evaporation stuff here similar to above
  
  #then run the test here
  
}




### list of years ####

jax_list <- as.character(c(2002:2005)) #xx years of data as string I want 1966 to 2025


#make loop ooh seems to be running that created a raster of rainfall for each year
for (i in jax_list) {
  assign(paste0("GS_rainfall", i), function_rainfall_GS(i))
}
plot(GS_rainfall2002) #I can see the moving average has done something beacuse the first 6 layers are missing
tail(GS_rainfall2002) #still 122 layer but we have some missing cells not sure what this is about??



GSRainfall_moving_window_02_05 <- stack(GS_rainfall2002,
                                            GS_rainfall2003, 
                                            GS_rainfall2004,
                                            GS_rainfall2005
                                            )


GSRainfall_moving_window_02_05
head(GSRainfall_moving_window_02_05,2) #this makes sense I have 4 layers of 214 stacked together4*214=856
GSRainfall_moving_window_02_05
#means_jan_rainfall <- calc(STACK1, fun = mean, na.rm = T)
#try this
#x <- calc(jan_rainfall2000, function(x) movingFun(x, 3, mean))
#y <- st1 - x

####################################################################################################
### as a function per year for annual rainfall
#year_input = 2000

function_rainfall_annual <- function(year_input) {
  
  # daily_rain <- raster::brick(
  #   paste("//af-osm-05-cdc.it.csiro.au/OSM_CBR_AF_CDP_work/silo/daily_rain/",
  #         year_input, ".daily_rain.nc", sep = ""),varname = "daily_rain")
  
  daily_rain <- raster::brick(
    paste("I:/work/silo/daily_rain/",
          year_input, ".daily_rain.nc", sep = ""),varname = "daily_rain")
  
  #crop to a fix area
  daily_rain_crop <- raster::crop(daily_rain, GRDC_bound_mallee_sf)
  
  #only use a few days annual
  daily_rain <- subset(daily_rain_crop, 1:335) #annual rainfall (nb leap yrs 1 -366)
  
  
  #Add the moving window
  annual_rainfall2000_MovMean7 <- calc(daily_rain, function(x) movingFun(x, 7, mean, "to"))
  
  #add in the evaporation stuff here similar to above
  
  #then run the test here
  
}

### list of years ####

jax_list <- as.character(c(2002:2005)) #xx years of data as string I want 1966 to 2025


#make loop ooh seems to be running that created a raster of rainfall for each year
for (i in jax_list) {
  assign(paste0("rainfall_annual", i), function_rainfall_annual(i))
}
plot(rainfall_annual2002) #I can see the moving average has done something beacuse the first 6 layers are missing
tail(rainfall_annual2002) #still 122 layer but we have some missing cells not sure what this is about??



rainfall_annual_moving_window_02_05 <- stack(rainfall_annual2002,
                                            rainfall_annual2003, 
                                            rainfall_annual2004,
                                            rainfall_annual2005
)


rainfall_annual_moving_window_02_05
head(rainfall_annual_moving_window_02_05,2) #this makes sense I have 4 layers of 214 stacked together4*214=856
rainfall_annual_moving_window_02_05


#######################################################################################################

### Annula Rainfall and GS rainfall
function_rainfall_type <- function(year_input) {
  ############################################
  ##1. Rainfall annual
  
  
  ############################################
  ##2. Rainfall GS
  
  
  ############################################
  ##3. Prop of annual rainfall that is summer (we want to see a trend in more summer rain)
  summer_rainmoving_window <- rainfall_annual_moving_window - GS_rainfall2000_MovMean7
  proportion_summer_rain <- summer_rain / rainfall_annual_moving_window
  
  return(proportion_summer_rain)
}


for (i in jax_list) {
  assign(paste0("prop_summer_rain", i), function_rainfall_type(i))
}

plot(Rain_evap2000)
head(Rain_evap2000)
Rain_evap2000

#########################################################################################################################
####                           create a plot of how rainfall and evap  has changed over time
#########################################################################################################################
#UP to here 


##1. define the boundary with and use a single layer raster 
GRDC_bound_wimm_raster <- Rain_evap2000$layer.7
plot(GRDC_bound_wimm_raster)
GRDC_bound_wimm_raster

##2. extract points from the raster as a point shapefile
GRDC_bound_wimm_pts2 <- rasterToPoints(GRDC_bound_wimm_raster)
names(GRDC_bound_wimm_pts2) <- c("longitude", "latitude", "value")
GRDC_bound_wimm_pts_df <- as.data.frame(GRDC_bound_wimm_pts2)
GRDC_bound_wimm_pts_df <- select(GRDC_bound_wimm_pts_df, x, y)
#check to see its worked (note that it extends further than the boundary)
write.csv(GRDC_bound_wimm_pts_df, "test_extract.csv") #it has! This is just a file with coordinates that define our study area
str(GRDC_bound_wimm_pts_df)
##3. is this in the correct format?

# GRDC_bound_wimm_pts_df_point <- st_as_sf(x = GRDC_bound_wimm_pts_df, 
#                                          coords = c("x", "y"),
#                                          crs = "+proj=longlat +datum=WGS84")

GRDC_bound_wimm_pts_df_point <- SpatialPointsDataFrame(GRDC_bound_wimm_pts_df[,c("x", "y")], GRDC_bound_wimm_pts_df)
plot(GRDC_bound_wimm_pts_df_point)
str(GRDC_bound_wimm_pts_df_point)


##3, now I can use this as a cookie cutter for rasters GRDC_bound_wimm_pts_df_point

Rain_evap2000_extract <- raster::extract(Rain_evap2000, GRDC_bound_wimm_pts_df_point, method="simple")
str(Rain_evap2000_extract)
class(Rain_evap2000_extract)
head(Rain_evap2000_extract)

Rain_evap2000_extract_wide <- data.frame(GRDC_bound_wimm_pts_df_point$x, GRDC_bound_wimm_pts_df_point$y, Rain_evap2000_extract)
head(Rain_evap2000_extract_wide)


##### assign names for all the layers this will days not years

names(Rain_evap2000_extract_wide) <- c("POINT_X", "POINT_Y", 
                                       "61", "62", "63", "64", "65", "66","67","68","69","70",
                                       "71", "72", "73", "74", "75", "76","77","78","79","80",
                                       "81", "82", "83", "84", "85", "86","87","88","89","90",
                                       "91", "92", "93", "94", "95", "96","97","98","99","100",
                                       "101", "102", "103", "104", "105", "106","107","108","109","110",
                                       "111", "112", "113", "114", "115", "116","117","118","119","120",
                                       "121", "122", "123", "124", "125", "126","127","128","129","130",
                                       "131", "132", "133", "134", "135", "136","137","138","139","140",
                                       "141", "142", "143", "144", "145", "146","147","148","149","150",
                                       "151", "152", "153", "154", "155", "156","157","158","159","160",
                                       "161", "162", "163", "164", "165", "166","167","168","169","170",
                                       "171", "172", "173", "174", "175", "176","177","178","179","180",
                                       "181", "182")
#Remove the clm that have no data                                       
Rain_evap2000_extract_wide <- select(Rain_evap2000_extract_wide, -"61", -"62", -"63", -"64", -"65", -"66" )
Rain_evap2000_extract_wide
Rain_evap2000_extract_narrow <- gather(Rain_evap2000_extract_wide, key = "day", value = "Rainfall_evap", `68`:`182` )
head(Rain_evap2000_extract_narrow, 11)
Rain_evap2000_extract_narrow
#after this I can plot the rainfall-evap for the area ie each point
#do this as a check can be done for evap and then our new variable

str(Rain_evap2000_extract_narrow)

Rain_evap2000_extract_narrow$day_numb <- as.double(Rain_evap2000_extract_narrow$day)

ggplot(Rain_evap2000_extract_narrow, aes(day_numb, Rainfall_evap))+
  geom_point()+
  geom_smooth(method = "lm", se=FALSE, color="black", aes(group=1))+ #straight line regression
  #geom_smooth(color="black", aes(group=1))+ #smooth line
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, hjust=1),
        plot.caption = element_text(hjust = 0))+
  labs(x = "Day of year",
       y = "Sum 7 days rainfall - sum 7 days evaporation",
       title = "Seasonal break check for 1 year",
caption = "This the sum 7 days rainfall - sum 7 days evaopration for each pixel in study area")





Rain_evap2000_extract_wide
#strip point x and y from df
Rain_evap2000_extract_wide_x_y <- select(Rain_evap2000_extract_wide, "POINT_X",  "POINT_Y")
Rain_evap2000_extract_wide_values <- select(Rain_evap2000_extract_wide,"67":"182")
# #replace the values with 0 or 1; 0 = less than 0 and 1 is greater than 0
# 
Rain_evap2000_extract_wide_values <- Rain_evap2000_extract_wide_values %>% mutate_all(funs(ifelse(.<=0, 0, .))) #if its less than 0 give it value 0
Rain_evap2000_extract_wide_values <- Rain_evap2000_extract_wide_values %>% mutate_all(funs(ifelse(.>0, 1, .))) #if its greater than 0 give it value 1


first_occurance = names(Rain_evap2000_extract_wide_values)[apply(Rain_evap2000_extract_wide_values,1,match,x=1)]

Rain_evap2000_occurance <- cbind(Rain_evap2000_extract_wide_x_y, first_occurance)
Rain_evap2000_occurance


ggplot(Rain_evap2000_occurance, aes(POINT_X, POINT_Y, colour = first_occurance))+
  geom_point()



######################################################################################################################################
#### set it up as a function
######################################################################################################################################
#before I can run as function I need to set up a study area

##1. define the boundary with and use a single layer raster 
GRDC_bound_wimm_raster <- Rain_evap2000$layer.7
##2. extract points from the raster as a point shapefile
GRDC_bound_wimm_pts2 <- rasterToPoints(GRDC_bound_wimm_raster)
names(GRDC_bound_wimm_pts2) <- c("longitude", "latitude", "value")
GRDC_bound_wimm_pts_df <- as.data.frame(GRDC_bound_wimm_pts2)
GRDC_bound_wimm_pts_df <- select(GRDC_bound_wimm_pts_df, x, y)

##FUNCTION for occurance
function_occurance <- function(rain_evap) {

##2., now I can use this as a cookie cutter for rasters GRDC_bound_wimm_pts_df_point

Rain_evap_extract <- raster::extract(rain_evap, GRDC_bound_wimm_pts_df_point, method="simple")

Rain_evap_extract_wide <- data.frame(GRDC_bound_wimm_pts_df_point$x, GRDC_bound_wimm_pts_df_point$y, Rain_evap_extract)

##### assign names for all the layers this will days
names(Rain_evap_extract_wide) <- c("POINT_X", "POINT_Y", 
                                       "61", "62", "63", "64", "65", "66","67","68","69","70",
                                       "71", "72", "73", "74", "75", "76","77","78","79","80",
                                       "81", "82", "83", "84", "85", "86","87","88","89","90",
                                       "91", "92", "93", "94", "95", "96","97","98","99","100",
                                       "101", "102", "103", "104", "105", "106","107","108","109","110",
                                       "111", "112", "113", "114", "115", "116","117","118","119","120",
                                       "121", "122", "123", "124", "125", "126","127","128","129","130",
                                       "131", "132", "133", "134", "135", "136","137","138","139","140",
                                       "141", "142", "143", "144", "145", "146","147","148","149","150",
                                       "151", "152", "153", "154", "155", "156","157","158","159","160",
                                       "161", "162", "163", "164", "165", "166","167","168","169","170",
                                       "171", "172", "173", "174", "175", "176","177","178","179","180",
                                       "181", "182")
#Remove the clm that have no data                          
Rain_evap_extract_wide <- select(Rain_evap_extract_wide, -"61", -"62", -"63", -"64", -"65", -"66" )

#strip point x and y from df
Rain_evap_extract_wide_x_y <- select(Rain_evap_extract_wide, "POINT_X",  "POINT_Y")
Rain_evap_extract_wide_values <- select(Rain_evap_extract_wide,"67":"182")
# #replace the values with 0 or 1; 0 = less than 0 and 1 is greater than 0
# 
Rain_evap_extract_wide_values <- Rain_evap_extract_wide_values %>% mutate_all(funs(ifelse(.<=0, 0, .))) #if its less than 0 give it value 0
Rain_evap_extract_wide_values <- Rain_evap_extract_wide_values %>% mutate_all(funs(ifelse(.>0, 1, .))) #if its greater than 0 give it value 1

first_occurance = names(Rain_evap_extract_wide_values)[apply(Rain_evap_extract_wide_values,1,match,x=1)]

Rain_evap_occurance <- cbind(Rain_evap_extract_wide_x_y, first_occurance)
Rain_evap_occurance
}

raster_list <- c(Rain_evap2000, Rain_evap2001, Rain_evap2002)

for (i in raster_list) {
  assign(paste0("Rain_evap_occurance", i), function_occurance(i))
}



#### Will I get error with leap year?




















