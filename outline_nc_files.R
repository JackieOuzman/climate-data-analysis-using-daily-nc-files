
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
#library(doBy)
#library(maptools)
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

# Extract CRS from one SILO nc file and reproject shapefile to match
silo_crs <- raster::crs(raster::brick("N:/work/Climate_analysis_nc_file_jackie/silo_rain_monthly/1959.monthly_rain.nc", varname = "monthly_rain"))
GRDC_bound_mallee_sf <- sp::spTransform(GRDC_bound_mallee_sf, silo_crs)

# Confirm
cat("Raster CRS:\n"); print(silo_crs)
cat("Shapefile CRS after reprojection:\n"); print(raster::crs(GRDC_bound_mallee_sf))
plot(GRDC_bound_mallee_sf, main = "Check shapefile looks correct")



# Quick check of monthly_rain variable and layers
test_brick <- raster::brick("N:/work/Climate_analysis_nc_file_jackie/silo_rain_monthly/1959.monthly_rain.nc", varname = "monthly_rain")
cat("Number of layers:", raster::nlayers(test_brick), "\n")
print(test_brick) # monthly_rain


### list of years ####
jax_list <- as.character(c(1959:2025)) #xx years of data as string


#######################################################################################################
function_rainfall_type <- function(year_input) {
  
  year_input <- as.integer(year_input)
  
  # Monthly indices 
  gs_start <- 4   # April
  gs_end   <- 10  # October
  
  ############################################
  ## 1. Load and crop
  
  monthly_rain <- raster::brick(
    paste0("N:/work/Climate_analysis_nc_file_jackie/silo_rain_monthly/", year_input, ".monthly_rain.nc"),
    varname = "monthly_rain" # 
  )
  
  monthly_rain_crop <- raster::crop(monthly_rain, GRDC_bound_mallee_sf)
  monthly_rain_crop <- raster::mask(monthly_rain_crop, GRDC_bound_mallee_sf)
  
  ## 2. Annual total rainfall (all 12 months)
  
  annual_total <- raster::calc(raster::subset(monthly_rain_crop, 1:12), sum, na.rm = TRUE)
  
  ############################################
  ## 3. Growing season (GS) total rainfall: April - October (months 4-10)
  
  gs_total <- raster::calc(raster::subset(monthly_rain_crop, gs_start:gs_end), sum, na.rm = TRUE)
  
  ############################################
  ## 4. Non-GS (summer) total and proportion
  
  summer_total <- annual_total - gs_total
  
  proportion_summer_rain <- raster::overlay(
    summer_total, annual_total,
    fun = function(s, a) ifelse(a > 0, s / a, NA)
  )
  proportion_summer_rain <- raster::mask(proportion_summer_rain, GRDC_bound_mallee_sf)
  
  ############################################
  ## 5. Write to disk                            
  
  out_path <- paste0("N:/work/Climate_analysis_nc_file_jackie/GRDC_bound_mallee/prop_summer_rain_", year_input, ".tif")
  raster::writeRaster(proportion_summer_rain, out_path, format = "GTiff", overwrite = TRUE)
  
}
  
  
  



for (i in jax_list) {
  if (!file.exists(paste0("N:/work/Climate_analysis_nc_file_jackie/GRDC_bound_mallee/prop_summer_rain_", i, ".tif"))) {
    function_rainfall_type(i)
    gc()
    cat("Done:", i, "\n")
  } else {
    cat("Skipping (already exists):", i, "\n")
  }
}


# Then read back as a stack for analysis
# pre_files  <- paste0("N:/work/Climate_analysis_nc_file_jackie/GRDC_bound_mallee/prop_summer_rain_", 1959:1994, ".tif")
# post_files <- paste0("N:/work/Climate_analysis_nc_file_jackie/GRDC_bound_mallee/prop_summer_rain_", 1995:2025, ".tif")

# Read from disk - 
pre_files  <- paste0("N:/work/Climate_analysis_nc_file_jackie/GRDC_bound_mallee/prop_summer_rain_", 1959:1994, ".tif")
post_files <- paste0("N:/work/Climate_analysis_nc_file_jackie/GRDC_bound_mallee/prop_summer_rain_", 1995:2025, ".tif")

# Stack directly from file paths
pre_stack  <- raster::stack(pre_files)
post_stack <- raster::stack(post_files)

# Mean proportion for each period
pre_mean  <- raster::calc(pre_stack,  mean, na.rm = TRUE)
post_mean <- raster::calc(post_stack, mean, na.rm = TRUE)

# Difference (positive = more summer rain post 1995)
difference <- post_mean - pre_mean



# Plot all three
par(mfrow = c(1, 3))

raster::plot(pre_mean,  main = "Mean summer rain proportion\npre-1995 (1959-1994)")
sp::plot(GRDC_bound_mallee_sf, add = TRUE, border = "black", col = NA)

raster::plot(post_mean, main = "Mean summer rain proportion\npost-1995 (1995-2018)")
sp::plot(GRDC_bound_mallee_sf, add = TRUE, border = "black", col = NA)

raster::plot(difference, main = "Difference\n(post minus pre 1995)")
sp::plot(GRDC_bound_mallee_sf, add = TRUE, border = "black", col = NA)



## summing the proportion of summer rain for the 2 time point pre and post

# Extract mean proportion for whole region for each year
years_all <- 1959:2018

region_means <- sapply(years_all, function(yr) {
  r <- raster::raster(paste0("N:/work/Climate_analysis_nc_file_jackie/GRDC_bound_mallee/prop_summer_rain_", yr, ".tif"))
  raster::cellStats(r, mean, na.rm = TRUE)
})

# Build a dataframe
df <- data.frame(
  year   = years_all,
  mean_prop = region_means,
  period = ifelse(years_all < 1995, "Pre-1995 (1959-1994)", "Post-1995 (1995-2025)")
)


# Time series plot with vertical line at 1995
ggplot(df, aes(x = year, y = mean_prop, colour = period)) +
  geom_line() +
  geom_point() +
  geom_vline(xintercept = 1994.5, linetype = "dashed", colour = "grey40") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    x      = "Year",
    y      = "Mean proportion of annual rainfall in non-GS (summer)",
    colour = "Period",
    title  = "SA-Vic Mallee: summer rainfall proportion over time"
  ) +
  theme_classic()

# Summary bar chart
df_summary <- df %>%
  dplyr::group_by(period) %>%
  dplyr::summarise(
    mean = mean(mean_prop),
    se   = sd(mean_prop) / sqrt(n())
  ) %>%
  dplyr::mutate(period = factor(period, levels = c("Pre-1995 (1959-1994)", "Post-1995 (1995-2025)")))

ggplot(df_summary, aes(x = "Mallee", y = mean, fill = period)) +
  geom_col(position = "dodge") +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                position = position_dodge(0.9), width = 0.2) +
  scale_fill_manual(values = c("Pre-1995 (1959-1994)" = "grey70", 
                               "Post-1995 (1995-2025)" = "grey30")) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    x     = "Region",
    y     = "Mean proportion of annual rainfall in non-GS (summer)",
    fill  = "Period",
    title = "SA-Vic Mallee: summer rainfall proportion pre vs post 1995"
  ) +
  theme_classic()









plot(prop_summer_rain2002)
head(prop_summer_rain2002)
prop_summer_rain2002
str(GRDC_bound_mallee_sf) # this is the raster
GRDC_bound_mallee

## some checks
# Look at one year's output
test <- prop_summer_rain2002  # swap in a year from your list
print(test)                   # basic info: extent, resolution, CRS, value range
raster::cellStats(test, summary)


raster::plot(test, main = "Proportion of annual rainfall in non-GS (summer) 2000")
sp::plot(GRDC_bound_mallee_sf, add = TRUE, border = "black", col = NA)

# Load and crop/mask raw data for 2002
raw <- raster::brick("I:/work/silo/daily_rain/2002.daily_rain.nc", varname = "daily_rain")
raw_crop <- raster::crop(raw, GRDC_bound_mallee_sf)
raw_crop <- raster::mask(raw_crop, GRDC_bound_mallee_sf)

# Find first valid cell and its coordinates
good_idx   <- which(raster::values(prop_summer_rain2002) > 0)[1]
good_point <- raster::xyFromCell(prop_summer_rain2002, good_idx)

# Extract all 365 daily values for that point
pixel_values <- raster::extract(raw_crop, good_point)  # 1 row x 365 cols

# Sum to match what the function does
pixel_annual <- sum(pixel_values[1, 1:365], na.rm = TRUE)   # all days
pixel_gs     <- sum(pixel_values[1, 91:304], na.rm = TRUE)  # 1 Apr - 31 Oct
pixel_summer <- pixel_annual - pixel_gs
pixel_prop   <- pixel_summer / pixel_annual

cat("Manual proportion:", pixel_prop, "\n")
cat("Raster cell value:", raster::extract(prop_summer_rain2002, good_point), "\n")
cat("Difference:", abs(pixel_prop - raster::extract(prop_summer_rain2002, good_point)), "\n")






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




















