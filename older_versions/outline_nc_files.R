
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

################### Start here ############################################################################################
setwd("W:/Economic impact of weeds round 2/Climate/AEZ")#jackie

# Extract CRS from one SILO nc file
silo_crs <- raster::crs(raster::brick("N:/work/Climate_analysis_nc_file_jackie/silo_rain_monthly/1959.monthly_rain.nc", varname = "monthly_rain"))

# Quick check of monthly_rain variable and layers
test_brick <- raster::brick("N:/work/Climate_analysis_nc_file_jackie/silo_rain_monthly/1959.monthly_rain.nc", varname = "monthly_rain")
cat("Number of layers:", raster::nlayers(test_brick), "\n")
print(test_brick)

### List of shapefiles ###
shp_list <- c(
  "NSW_Central", "NSW_NE_Qld_SE", "NSW_NW_Qld_SW", "NSW_Vic_Slopes",
  "Qld_Atherton", "Qld_Burdekin", "Qld_Central",
  "SA_Midnorth_Lower_Yorke_Eyre", "SA_Vic_Bordertown_Wimmera", "SA_Vic_Mallee",
  "Tas_Grain", "Vic_High_Rainfall",
  "WA_Central", "WA_Eastern", "WA_Mallee", "WA_Northern", "WA_Ord", "WA_Sandplain"
)

### List of years ####

jax_list <- as.character(c(1959:2025))




#######################################################################################################
function_rainfall_type <- function(year_input, region_sf, region_name) {
  
  year_input <- as.integer(year_input)
  
  # Monthly indices (no leap year logic needed)
  gs_start <- 4   # April
  gs_end   <- 10  # October
  
  ############################################
  ## 1. Load and crop
  
  monthly_rain <- raster::brick(
    paste0("N:/work/Climate_analysis_nc_file_jackie/silo_rain_monthly/", year_input, ".monthly_rain.nc"),
    varname = "monthly_rain"
  )
  
  monthly_rain_crop <- raster::crop(monthly_rain, region_sf)
  monthly_rain_crop <- raster::mask(monthly_rain_crop, region_sf)
  
  ############################################
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
  proportion_summer_rain <- raster::mask(proportion_summer_rain, region_sf)
  
  ############################################
  ## 5. Write to disk
  
  out_path <- paste0("N:/work/Climate_analysis_nc_file_jackie/", region_name, "/prop_summer_rain_", year_input, ".tif")
  raster::writeRaster(proportion_summer_rain, out_path, format = "GTiff", overwrite = TRUE)
  
}

#######################################################################################################
### Outer loop over regions ###

for (region_name in shp_list) {
  
  cat("\n--- Processing region:", region_name, "---\n")
  
  # Load and reproject shapefile
  region_bound <- sf::st_read(paste0(region_name, ".shp"))
  region_sf    <- sp::spTransform(as(region_bound, "Spatial"), silo_crs)
  plot(region_sf, main = region_name)
  
  # Create output folder if it doesn't exist
  out_folder <- paste0("N:/work/Climate_analysis_nc_file_jackie/", region_name)
  if (!dir.exists(out_folder)) {
    dir.create(out_folder, recursive = TRUE)
    cat("Created folder:", out_folder, "\n")
  }
  
  # Inner loop over years
  for (i in jax_list) {
    out_file <- paste0(out_folder, "/prop_summer_rain_", i, ".tif")
    if (!file.exists(out_file)) {
      function_rainfall_type(i, region_sf, region_name)
      gc()
      cat("Done:", region_name, i, "\n")
    } else {
      cat("Skipping (already exists):", region_name, i, "\n")
    }
  }
}
 
  
  
  
  
  


##### up to here


# Then read back as a stack for analysis
# pre_files  <- paste0("N:/work/Climate_analysis_nc_file_jackie/GRDC_bound_mallee/prop_summer_rain_", 1959:1994, ".tif")
# post_files <- paste0("N:/work/Climate_analysis_nc_file_jackie/GRDC_bound_mallee/prop_summer_rain_", 1995:2025, ".tif")
# 
# # Read from disk - 
# pre_files  <- paste0("N:/work/Climate_analysis_nc_file_jackie/GRDC_bound_mallee/prop_summer_rain_", 1959:1994, ".tif")
# post_files <- paste0("N:/work/Climate_analysis_nc_file_jackie/GRDC_bound_mallee/prop_summer_rain_", 1995:2025, ".tif")
# 
# # Stack directly from file paths
# pre_stack  <- raster::stack(pre_files)
# post_stack <- raster::stack(post_files)
# 
# # Mean proportion for each period
# pre_mean  <- raster::calc(pre_stack,  mean, na.rm = TRUE)
# post_mean <- raster::calc(post_stack, mean, na.rm = TRUE)
# 
# # Difference (positive = more summer rain post 1995)
# difference <- post_mean - pre_mean






## summing the proportion of summer rain for the 2 time point pre and post

#######################################################################################################
### Extract mean proportion for each region and year ###

years_all <- 1959:2018

# Loop over all regions and build one combined dataframe
df_all <- do.call(rbind, lapply(shp_list, function(region_name) {
  
  region_means <- sapply(years_all, function(yr) {
    tif_path <- paste0("N:/work/Climate_analysis_nc_file_jackie/", region_name, "/prop_summer_rain_", yr, ".tif")
    if (file.exists(tif_path)) {
      r <- raster::raster(tif_path)
      raster::cellStats(r, mean, na.rm = TRUE)
    } else {
      NA
    }
  })
  
  data.frame(
    year        = years_all,
    mean_prop   = region_means,
    region      = region_name,
    period      = ifelse(years_all < 1995, "Pre-1995 (1959-1994)", "Post-1995 (1995-2025)")
  )
}))

### Summary by region and period ###
df_summary <- df_all %>%
  dplyr::group_by(region, period) %>%
  dplyr::summarise(
    mean = mean(mean_prop, na.rm = TRUE),
    se   = sd(mean_prop, na.rm = TRUE) / sqrt(sum(!is.na(mean_prop))),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    period = factor(period, levels = c("Pre-1995 (1959-1994)", "Post-1995 (1995-2025)")),
    region = factor(region, levels = shp_list)
  )

### Plot ###
ggplot(df_summary, aes(x = region, y = mean, fill = period)) +
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
    title = "All regions: summer rainfall proportion pre vs post 1995"
  ) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

##### need to group some aez and remove some all together

#################################################################################
## DO above agian but now with a different subset of AEZ and with regions options
### Define grouped region list (drop Qld_Atherton, Qld_Burdekin, WA_Ord) ###

region_groups <- data.frame(
  region = c(
    # Northern
    "Qld_Central", "NSW_NE_Qld_SE", "NSW_NW_Qld_SW", "NSW_Vic_Slopes", "NSW_Central",
    # Southern
    "SA_Midnorth_Lower_Yorke_Eyre", "SA_Vic_Mallee", "SA_Vic_Bordertown_Wimmera", "Tas_Grain", "Vic_High_Rainfall",
    # Western
    "WA_Central", "WA_Eastern", "WA_Northern", "WA_Sandplain", "WA_Mallee"
  ),
  zone = c(
    rep("Northern", 5),
    rep("Southern", 5),
    rep("Western", 5)
  )
)

# Nice x-axis labels (shorter than the shapefile names)
region_labels <- c(
  "Qld_Central"                  = "Qld Central",
  "NSW_NE_Qld_SE"                = "NSW NE/Qld SE",
  "NSW_NW_Qld_SW"                = "NSW NW/Qld SW",
  "NSW_Vic_Slopes"               = "NSW Vic Slopes",
  "NSW_Central"                  = "NSW Central",
  "SA_Midnorth_Lower_Yorke_Eyre" = "SA Midnorth-LYE",
  "SA_Vic_Mallee"                = "SA Vic Mallee",
  "SA_Vic_Bordertown_Wimmera"    = "SA Vic Bordertown-Wimmera",
  "Tas_Grain"                    = "Tas Grain",
  "Vic_High_Rainfall"            = "Vic High Rainfall",
  "WA_Central"                   = "WA Central",
  "WA_Eastern"                   = "WA Eastern",
  "WA_Northern"                  = "WA Northern",
  "WA_Sandplain"                 = "WA Sandplain",
  "WA_Mallee"                    = "WA Mallee"
)

#######################################################################################################
### Extract mean proportion - filtered to grouped regions only ###

years_all <- 1959:2018

df_all <- do.call(rbind, lapply(region_groups$region, function(region_name) {
  
  region_means <- sapply(years_all, function(yr) {
    tif_path <- paste0("N:/work/Climate_analysis_nc_file_jackie/", region_name, "/prop_summer_rain_", yr, ".tif")
    if (file.exists(tif_path)) {
      r <- raster::raster(tif_path)
      raster::cellStats(r, mean, na.rm = TRUE)
    } else {
      NA
    }
  })
  
  data.frame(
    year      = years_all,
    mean_prop = region_means,
    region    = region_name,
    period    = ifelse(years_all < 1995, "Pre-1995 (1959-1994)", "Post-1995 (1995-2025)")
  )
}))

# Join zone grouping
df_all <- df_all %>%
  dplyr::left_join(region_groups, by = "region")

### Summary by region, zone and period ###
df_summary <- df_all %>%
  dplyr::group_by(zone, region, period) %>%
  dplyr::summarise(
    mean = mean(mean_prop, na.rm = TRUE),
    se   = sd(mean_prop, na.rm = TRUE) / sqrt(sum(!is.na(mean_prop))),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    period = factor(period, levels = c("Pre-1995 (1959-1994)", "Post-1995 (1995-2025)")),
    region = factor(region, levels = region_groups$region),
    zone   = factor(zone,   levels = c("Northern", "Southern", "Western"))
  )

### Zone fill colours (paired light/dark per zone) ###
zone_period_colours <- c(
  "Northern_Pre"  = "#a8d1f0",  # light blue
  "Northern_Post" = "#1a6fad",  # dark blue
  "Southern_Post" = "#2d8a4e",  # dark green
  "Southern_Pre"  = "#a8d9b8",  # light green
  "Western_Pre"   = "#f5c18a",  # light orange
  "Western_Post"  = "#c45e0a"   # dark orange
)

df_summary <- df_summary %>%
  dplyr::mutate(
    fill_group = factor(
      paste0(zone, "_", ifelse(period == "Pre-1995 (1959-1994)", "Pre", "Post")),
      levels = c(
        "Northern_Pre", "Northern_Post",
        "Southern_Pre", "Southern_Post",
        "Western_Pre",  "Western_Post"
      )
    )
  )
### Plot ###
ggplot(df_summary, aes(x = region, y = mean, fill = fill_group)) +
  geom_col(position = "dodge") +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                position = position_dodge(0.9), width = 0.2) +
  # Zone divider lines
  geom_vline(xintercept = c(5.5, 10.5), linetype = "dashed", colour = "grey50", linewidth = 0.6) +
  # Zone labels across the top
  annotate("text", x = 3,    y = Inf, label = "Northern", vjust = 1.5, fontface = "bold", size = 3.5) +
  annotate("text", x = 8,    y = Inf, label = "Southern", vjust = 1.5, fontface = "bold", size = 3.5) +
  annotate("text", x = 12.5, y = Inf, label = "Western",  vjust = 1.5, fontface = "bold", size = 3.5) +
  scale_fill_manual(
    values = zone_period_colours,
    labels = c(
      "Northern_Pre"  = "Northern Pre-1995",
      "Northern_Post" = "Northern Post-1995",
      "Southern_Pre"  = "Southern Pre-1995",
      "Southern_Post" = "Southern Post-1995",
      "Western_Pre"   = "Western Pre-1995",
      "Western_Post"  = "Western Post-1995"
    ),
    name = "Zone / Period"
  ) +
  scale_x_discrete(labels = region_labels) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    x     = "Region",
    y     = "Mean proportion of annual rainfall in non-GS (summer)",
    title = "Summer rainfall proportion pre vs post 1995 by zone"
  ) +
  theme_classic() +
  theme(
    axis.text.x     = element_text(angle = 45, hjust = 1, size = 8),
    legend.position = "right",
    plot.title      = element_text(face = "bold")
  )

ggsave(
  filename = "N:/work/Climate_analysis_nc_file_jackie/summer_rain_proportion_pre_post_1995.png",
  width  = 14,
  height = 7,
  dpi    = 300,
  units  = "in"
)

#################################################################################
## different options 
### Summarise to zone level ###
df_zone_summary <- df_all %>%
  dplyr::group_by(zone, period, year) %>%
  dplyr::summarise(mean_prop = mean(mean_prop, na.rm = TRUE), .groups = "drop") %>%
  dplyr::group_by(zone, period) %>%
  dplyr::summarise(
    mean = mean(mean_prop, na.rm = TRUE),
    se   = sd(mean_prop, na.rm = TRUE) / sqrt(sum(!is.na(mean_prop))),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    period = factor(period, levels = c("Pre-1995 (1959-1994)", "Post-1995 (1995-2025)")),
    zone   = factor(zone,   levels = c("Northern", "Southern", "Western")),
    fill_group = factor(
      paste0(zone, "_", ifelse(period == "Pre-1995 (1959-1994)", "Pre", "Post")),
      levels = c("Northern_Pre", "Northern_Post",
                 "Southern_Pre", "Southern_Post",
                 "Western_Pre",  "Western_Post")
    )
  )

### Plot ###



ggplot(df_zone_summary, aes(x = zone, y = mean, fill = period, group = period)) +
  geom_col(position = "dodge") +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                position = position_dodge(0.9), width = 0.2) +
  scale_fill_manual(
    values = c("Pre-1995 (1959-1994)" = "grey70",
               "Post-1995 (1995-2025)" = "grey30"),
    name = "Period"
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    x     = "",
    y     = "",
    title = "Mean proportion of annual rainfall that falls in summer."
  ) +
  theme_classic() +
  theme(
    axis.text.x     = element_text(size = 11),
    legend.position = "bottom",
    plot.title      = element_text(face = "bold")
  ) 
 

ggsave(
  filename = "N:/work/Climate_analysis_nc_file_jackie/summer_rain_proportion_regions.png",
  width  = 8,
  height = 6,
  dpi    = 300,
  units  = "in"
)



