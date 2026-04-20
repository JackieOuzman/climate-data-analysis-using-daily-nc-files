# split all of the AEZ into shapefiles
library(sf)
library(dplyr)


setwd("W:/Economic impact of weeds round 2/Climate/AEZ")#jackie

AEZ_Boundary_region <- sf::st_read("AEZ_Boundary_region.shp")

names(AEZ_Boundary_region)

AEZ_Boundary_region %>%
  group_by(AEZ) %>%
  group_split() %>%
  lapply(function(x) {
    # replace spaces and special characters with underscores
    clean_name <- gsub("[^A-Za-z0-9]", "_", unique(x$AEZ))
    st_write(x, 
             paste0("W:/Economic impact of weeds round 2/Climate/AEZ/", clean_name, ".shp"),
             delete_dsn = TRUE)
  })


list.files("W:/Economic impact of weeds round 2/Climate/AEZ/", pattern = "\\.shp$")
SA_Vic_Mallee  <- sf::st_read("SA_Vic_Mallee.shp" )
plot(SA_Vic_Mallee)
