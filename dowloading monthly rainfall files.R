# ============================================================
# Download SILO monthly rainfall NetCDF files (1959-2025)
# ============================================================

# Set your destination folder (change this to wherever you want the files)
dest_dir <- "N:/work/Climate_analysis_nc_file_jackie/silo_rain_monthly"   # this was run on VM, my computer it is D drive

# Create the folder if it doesn't exist
if (!dir.exists(dest_dir)) dir.create(dest_dir, recursive = TRUE)

# Base URL for the SILO open data bucket
base_url <- "https://s3-ap-southeast-2.amazonaws.com/silo-open-data/Official/annual/monthly_rain"

# Years to download
years <- 1959:2025

# ============================================================
# Download loop
# ============================================================
for (year in years) {
  
  filename <- paste0(year, ".monthly_rain.nc")
  url      <- paste0(base_url, "/", filename)
  destfile <- file.path(dest_dir, filename)
  
  # Skip if already downloaded
  if (file.exists(destfile)) {
    message("Already exists, skipping: ", filename)
    next
  }
  
  message("Downloading: ", filename, " ...")
  
  tryCatch({
    download.file(
      url      = url,
      destfile = destfile,
      mode     = "wb",        # binary mode — essential for .nc files
      quiet    = FALSE
    )
    message("  OK: ", filename)
  }, error = function(e) {
    message("  FAILED: ", filename, " — ", conditionMessage(e))
  })
  
  # Small pause to be polite to the server
  Sys.sleep(0.5)
}

message("Done! Files saved to: ", dest_dir)