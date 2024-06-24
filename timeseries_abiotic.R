#Create time series with following variables: 
#sapflow, soil volumetric water content, PAR, VPD, air temperature
#From Jan-May 2024 (Timescale can be modified as needed)

library(readr)

site <- "TMP"
variables <- c("sapflow_2.5cm", "sapflow_5cm", 
               "soil_vwc_5cm", "soil_vwc_10cm", "soil_vwc_15cm", "soil_vwc_30cm", 
               "wx_tempavg15", "wx_par_den24", "wx_vpd15")

# Construct a "regular expression" to find the files we want: in this case,
# CSV files starting with the site code above
pat <- paste0("^", site, ".*csv$")

# Get the names of the files we need. Note this assumes that your
# working directory is the main directory of the L1 data
files <- list.files("./TMP_2024/", pattern = pat, recursive = TRUE, full.names = TRUE)

#Function to read files and only include variables in the above vector
f <- function(f) {
  message("Reading ", basename(f))
  x <- read_csv(f, col_types = "ccTccccdccii")
  x[x$research_name %in% variables,]
}

#Apply function and bind files
dat <- lapply(files, f)
dat <- do.call("rbind", dat)

#New dataframe
var_full <- dat

#saveRDS(var_full, file = "sapflow_abiotic_complete.rds")

<<<<<<< Updated upstream
=======

>>>>>>> Stashed changes
