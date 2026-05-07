year_input <- 2018

TESTdaily_rain <- raster::brick(
  paste0("I:/work/silo/daily_rain/", year_input, ".daily_rain.nc"),
  varname = "daily_rain"
)



# Check file sizes for 2018 onwards
files <- paste0("I:/work/silo/daily_rain/", 2018:2025, ".daily_rain.nc")
data.frame(
  year = 2018:2025,
  size_MB = round(file.size(files) / 1024^2, 1),
  exists = file.exists(files)
)


# Check 2018 (works) vs 2019 (fails)
nc2018 <- ncdf4::nc_open("I:/work/silo/daily_rain/2018.daily_rain.nc")
print(nc2018)
ncdf4::nc_close(nc2018)

nc2019 <- ncdf4::nc_open("I:/work/silo/daily_rain/2019.daily_rain.nc")
print(nc2019)
ncdf4::nc_close(nc2019)


library(RNetCDF)
nc2019_test <- RNetCDF::open.nc("I:/work/silo/daily_rain/2019.daily_rain.nc")
# Check if R can at least see the file permissions
file.access("I:/work/silo/daily_rain/2019.daily_rain.nc", mode = 4)  # 0 = readable, -1 = not readable
file.access("I:/work/silo/daily_rain/2018.daily_rain.nc", mode = 4)  # compare with working file



for (yr in 2019:2025) {
  file.copy(
    from = paste0("I:/work/silo/daily_rain/", yr, ".daily_rain.nc"),
    to   = paste0("N:/work/Climate_analysis_nc_file_jackie/", yr, ".daily_rain.nc")
  )
}


nc2019 <- ncdf4::nc_open("N:/work/Climate_analysis_nc_file_jackie/2019.daily_rain.nc")
print(nc2019)
ncdf4::nc_close(nc2019)