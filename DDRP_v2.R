#!/usr/bin/Rscript
#.libPaths("/usr/lib64/R/library/")
# Last modified on 1/3/20: fixed bug that was deleting output raster files
# One 12/16/19: renamed file names and uploaded to GitHub
# On 11/26/19: fixed bug w/ loading 30 yr climate normals
# resolve for next DDRP version

# DDRP cohorots v. 1
options(echo = FALSE)

# Load the required packages
pkgs <- c("doParallel", "plyr", "dplyr", "foreach", "ggplot2", "ggthemes", 
          "lubridate", "mapdata", "mgsub", "optparse", "parallel",
          "purrr", "RColorBrewer", "rgdal", "raster", "readr", "sp", "stringr", 
          "tidyr", "tictoc", "tools")
ld_pkgs <- invisible(suppressMessages(suppressWarnings(lapply(pkgs, library, 
              lib.loc = "/usr/lib64/R/library/", character.only = TRUE))))

# Load collection of functions for this model
source("/usr/local/dds/DDRP_B1/DDRP_v2_funcs.R")

# Bring in states feature for summary maps (PNG files)
# Requires these libraries: "mapdata" and "maptools"
cat("\n\nDownloading US states feature\n")
states <- map_data("state")
# coordinates(states) <- ~long + lat
# proj4string(states) <- CRS("+proj=longlat +datum=NAD83")
# states <- spTransform(states, CRS(proj4string(r)))
# states <- data.frame(states)

# Start timing the model run
tic("Total run time")

################################################################################
########                  Header Documentation Section             #############
#  Cohorts DDRP: Degree-Day, establishment Risk, and Pest event mapping system #
########  By Brittany Barker, Len Coop, Gericke Cook, Dan Upper, and ###########
########  Tyson Wepprich for APHIS PPQ and IPM needs ###########################
################################################################################

# DDRP cohorts vs. 1 is an update from the previous version of DDRP 
# (v24proDDRP_B1.R) that includes:
# 1. The ability to accomodate multiple population cohorts, in which a user-
#    defined number of cohorts emerges from the overwintering stage at different
#    times, such that a population in any given area could contain a mix of
#    indidivuals in different life stages and generations
# 2. Parallel processing, in which several processes are run in parallel in 
#    order to speed up the model run (e.g., temperature raster cropping, 
#    daily loop runs for multiple CONUS tiles and cohorts, production of raster 
#    and summary map outputs)
# 3. A new summary map type - Lifestage by Generation, which shows the relative
#    population size of a specific life stage (currently only adults) for each 
#    distinct generation (e.g., Gen1 adults, Gen 2 adults...)
# 4. Other more minor improvements/features are included as well, such as: 
#       a) the ability for summary maps to show a specific state or region of 
#       interest. The previous version of DDRP can only show CONUS.
#       b) the ability to generate summary maps for odd generations only. This 
#       may be useful if there is a lot of spatial overlap between generations, 
#       which would make it difficult to see where the boundaries of Gen1 and
#       Gen 3 are, for example. 
#       c) the use of a command line parser that includes parameter abbrevs.
#       d) the ability to include or exclude leap day in the model run
#       
#     
# (1). PARAM HANDLING -----
#### * Process command line args ####
#### # Uses "optparse" package, a command line parser similar to Python's
option_list <- list(
  make_option(c("--spp"), action = "store", type = "character", 
              default = NA, help = "species parameter file name"),
  make_option(c("--forecast_data"), action = "store", type = "character", 
              default = NA, help = "weather data for forecast"),
  make_option(c("--start_year"),  action = "store", type = "character", 
              default = NA, help = "start year"),
  make_option(c("--start_doy"), action = "store", type = "integer", 
              default = NA, help = "start day of year"),
  make_option(c("--end_doy"),  action = "store", type = "integer",
              default = NA, help = "end day of year"),
  make_option(c("--keep_leap"), action = "store", type = "integer", 
              default = NA, help = "should leap day be kept? 0=no, 1=yes"),
  make_option(c("--region_param"), type = "character", action = "store", 
              default = NA, help = "study region"),
  make_option(c("--exclusions_stressunits"), type = "integer", action = "store",
              default = NA, help = "0 = off, 1 = on"),
  make_option(c("--pems"), type = "integer", action = "store", 
              default = NA, help = "0 = off, 1 = on"),
  make_option(c("--mapA"), type = "integer", action = "store", 
              default = NA, help = "0 = off, 1 = on"),
  make_option(c("--mapE"), type = "integer", action = "store", 
              default = NA, help = "0 = off, 1 = on"),
  make_option(c("--mapL"), type = "integer", action = "store", 
              default = NA, help = "0 = off, 1 = on"),
  make_option(c("--mapP"), type = "integer", action = "store", 
              default = NA, help = "0 = off, 1 = on"),
  make_option(c("--out_dir"), type = "character", action = "store", 
              default = NA, help = "name of out directory"),
  make_option(c("--out_option"), type = "integer", action = "store", 
              default = NA, help = "sampling frequency: 1 = 30 days; 
              2 = 14 days; 3 = 10 days; 4 = 7 days; 5 = 2 days; 6 = 1 day"),
  make_option(c("--ncohort"), type = "integer", action = "store", 
              default = NA, help = "number of cohorts"),
  make_option(c("--odd_gen_map"), type = "numeric", action = "store", 
              default = NA, help = "0 = off, 1 = on")
)

# Read in commands 
# If command line isn't used, then opts list elements will be NA
opts <- parse_args(OptionParser(option_list = option_list))
if (!is.na(opts[1])) {
  spp <- opts$spp
  forecast_data <- opts$forecast_data
  start_year <- opts$start_year
  start_doy <- opts$start_doy
  end_doy <- opts$end_doy
  keep_leap <- opts$keep_leap
  region_param <- opts$region_param
  exclusions_stressunits <- opts$exclusions_stressunits
  pems <- opts$pems
  mapA <- opts$mapA
  mapE <- opts$mapE
  mapL <- opts$mapL
  mapP <- opts$mapP
  out_dir <- opts$out_dir
  out_option <- opts$out_option
  ncohort <- opts$ncohort
  odd_gen_map <- opts$odd_gen_map
} else {
  #### * Default values for params, if not provided in command line ####
  spp           <- "SUNP" # Default species to use
  forecast_data <- "PRISM" # Forecast data to use (PRISM or NMME)
  start_year    <- "1990_daily_30yr" # Year to use
  start_doy     <- 1 # Start day of year          
  end_doy       <- 365 # End day of year - need 365 if voltinism map 
  keep_leap     <- 1 # Should leap year be kept?
  region_param  <- "CONUS" # Default REGION to use
  exclusions_stressunits    <- 1 # Turn on/off climate stress unit exclusions
  pems          <- 1 # Turn on/off pest event maps
  mapA          <- 1 # Make maps for adult stage
  mapE          <- 1 # Make maps for egg stage
  mapL          <- 1 # Make maps for larval stage
  mapP          <- 1 # Make maps for pupal stage
  out_dir       <- "SUNP_test" # Output dir
  out_option    <- 1 # Output option category
  ncohort       <- 7 # Number of cohorts to approximate end of OW stage
  odd_gen_map   <- 0 # Create summary plots for odd gens only (gen1, gen3, ..)
}

# (2). DIRECTORY INIT ------

#### * Param inputs - species params; thresholds, weather, etc. ####
params_dir <- "/usr/local/dds/DDRP_B1/spp_params/"

#### * Weather inputs and outputs - climate data w/subdirs 4-digit year ####
# If outdir has 2 consec. numbers, assume webuser; otherwise just use base dir
#if (grepl("16", start_year, perl = TRUE)) {
#  base_dir <- "/mnt/ssd1/PRISM/"
#} else {
  base_dir <- "/data/PRISM/"
#}
prism_dir <- paste0(base_dir, start_year)

cat("\nBASE DIR: ", base_dir, "\n")
cat("\nWORKING DIR: ", prism_dir, "\n")

#### * Output directory, log file, and error message file ####
# MUST remove .tif files or script will crash during processing because it will 
# try to analyze previously processed results. 

#output_dir <- paste0("/home/httpd/html/CAPS/",spp, "_cohorts")
output_dir <- paste0("/usr/local/dds/DDRP_B1/DDRP_results/", out_dir)

# Remove all files if output_dir exists, or else create output_dir
if (file.exists(output_dir)) {
  unlink(paste0(output_dir, "/*"), recursive = TRUE, force = TRUE)
  cat("\n", str_wrap(paste0("EXISTING OUTPUT DIR: ", output_dir, 
                     "; removing all files\n"), width = 80), sep = "") 
} else {
  dir.create(output_dir)
  cat("NEW OUTPUT DIR:", output_dir, "\n")
}

# Push out a rlogging file with all main messages in model
# Put all log, message, and metadata files in a separate folder
setwd(output_dir)
dir.create("Logs_metadata")
Model_rlogging <- sprintf("%s%s", "./", "/Logs_metadata/Model_rlogging.txt")

# Make header for logging file
cat(paste0(rep("#", 36), collapse = ""), "\n", 
    "### Log file for DDRP cohorts v1 ###\n", 
    paste0(rep("#", 36), collapse = ""), "\n\n", sep = "", 
    file = Model_rlogging)

# Record PRISM and output dir
cat("BASE DIR: ", base_dir, "\n", file = Model_rlogging, append = TRUE)
cat("WORKING DIR: ", prism_dir, "\n", file = Model_rlogging, append = TRUE)
cat(str_wrap(paste0("EXISTING OUTPUT DIR: ", output_dir, 
    "; removing all files"), width = 80), "\n\n", sep = "", 
    file = Model_rlogging, append = TRUE)

# Push out a message file with all R error messages
#msg <- file(paste0("/home/httpd/html/CAPS/",out_dir, "/rmessages.txt"), 
#open="wt")
msg <- file(paste0("/usr/local/dds/DDRP_B1/DDRP_results/", out_dir, 
                   "/Logs_metadata/rmessages.txt"), open = "wt")
sink(msg, type = "message")

# (3). PARAMETER AND SETTINGS SETUP ----- 
cat("PARAMETER AND SETTINGS SETUP: getting parameters to use in model\n", 
    file = Model_rlogging, append = TRUE)
cat("\n\nPARAMETER AND SETTINGS SETUP: getting parameters to use in model\n")

# Read from source param files in ./spp_params/SPP.params
param_file <- sprintf("%s%s", spp, ".params")
spp <- gsub(".params", "", param_file) # Get species abbr.
species_params <- sprintf("%s%s", params_dir, param_file) # Location of file

if (file.exists(species_params)) {
  cat("Species params: ",species_params, "\n", file = Model_rlogging, 
      append = TRUE)
  source(species_params) # Read in species parameters
  cat("Reading params for species: ", spp, " Fullname: ", fullname, "\n", 
      file = Model_rlogging, append = TRUE)
  cat("\nReading params for species: ", spp, " Fullname: ", fullname, "\n")
} else {
  cat("Param file: ", species_params, "...not found; exiting program\n", 
      file = Model_rlogging, append = TRUE)
  cat("\nParam file: ", species_params, "...not found; exiting program\n")
  q()  # No reason to keep going without any params
}

# Change year to numeric if it's a specific year
# If using climate normals, there may be letters in folder name
if (!grepl("[A-z]", start_year)) {
  start_year <- as.numeric(start_year)
} else {
  start_year <- "daily30yr" # currently where 30yr normal are - may need to chng
}

# Set up start and stop day of year depending on whether it's a leap year or
# not (need to modify the last day of the year if the year is a leap year 
# and user wants to include it)
# This does not apply to 30 yr climate data, which would not have a numeric
# start year
if (is.numeric(start_year)) {
  
  if (start_year %% 4 == 0 & keep_leap == 1) {
    cat(str_wrap(paste0(start_year, " is a leap year and leap day (2/29) will be 
                 included in the model"), width = 80), "\n", 
        file = Model_rlogging, append = TRUE)
    
    # Need to add an extra day onto year if all 365 days are being included
    if (end_doy == 365) {
      end_doy <- 366
    }
    
  } else if (start_year %% 4 == 0 & keep_leap == 0) {
     cat(str_wrap(paste0(start_year, " is a leap year but leap day (2/29) will 
                        not be included in the model"), width = 80), "\n", 
        file = Model_rlogging, append = TRUE)
  } else if (start_year %% 4 != 0) {
    cat(start_year, "is not a leap year - ignoring 'keep_leap' parameter\n", 
        file = Model_rlogging, append = TRUE)
  }
}

# Create a list of days to use for daily loop
sublist <- start_doy:end_doy

#### * Format threshold and DD params for Daily Loop ####

# Need to match length and order of stgorder, which is species specific
# Remove "F" from stgorder param - this is for an old version of DDRP
stgorder <- stgorder[-length(stgorder)]

# Upper and lower thresholds
# OW stage will have the same threshold as actual stage (e.g., OWadult = adult)
# Need to match LDT or UDT value to stage, which first requires changing 
# stage abbr to the stage name ("E" = "egg")
stage_ldt_list <- list()
stage_udt_list <- list()
j <- 1

for (i in 1:length(stgorder)) {
  stg_nam <- stgorder[i]
  stg_nam <- mgsub(string = stg_nam, # Requires "mgsub" package
                   pattern = c("OE", "OL", "OP", "OA", "E", "L", "P", "A"), 
                   replacement = c("egg", "larvae", "pupae", "adult", "egg", 
                                   "larvae", "pupae", "adult"))
  stage_ldt_val <- get(paste0(stg_nam, "LDT")) # returns LDT value for stage
  stage_ldt_list[[j]] <- stage_ldt_val
  stage_udt_val <- get(paste0(stg_nam, "UDT")) # returns UDT value for stage
  stage_udt_list[[j]] <- stage_udt_val
  j <- j + 1
}

# DD parameters - OW stage has it's own DD, so change stage abbr. to stage 
# name or "OW" plus stage name
stage_dd_list <- list()
j <- 1

for (i in 1:length(stgorder)) {
  stg_nam <- stgorder[i]
  stg_nam <- mgsub(string = stg_nam, 
                   pattern = c("OE", "OL", "OP", "OA", "E", "L", "P", "A"), 
                   replacement = c("OWegg", "OWlarvae", "OWpupae", "OWadult", 
                                   "egg", "larvae", "pup", "adult"))
  stage_dd_val <- get(paste0(stg_nam, "DD")) # returns DD value for stage
  stage_dd_list[[j]] <- stage_dd_val
  j <- j + 1
}

# Put the values in the list into a numeric vector
stage_ldt <- as.numeric(do.call(cbind, stage_ldt_list))
stage_udt <- as.numeric(do.call(cbind, stage_udt_list))
stage_dd <- as.numeric(do.call(cbind, stage_dd_list))

# Cohort response parameters
# SD is equal the square root of the variance (sigma-squared) parameter
# May consider changing length.out to something else in future versions...
# Also consider making the percent (in cohort_distro) an input parameter
xdist <- seq(xdist1, xdist2, length.out = 1000)
ydist <- dnorm(xdist, mean = distro_mean, sd = sqrt(distro_var))

inputdist <- data.frame(x = xdist, y = ydist) %>% 
  arrange(x) %>% 
  mutate(CDF = cumsum(y/sum(y)))
cohort_distro <- CohortDistrib(dist = inputdist, numstage = ncohort, perc = .99)
relpopsize <- cohort_distro$weights

# Parameters of required degree-days
# Replace OW gen with emergence distro values
if (ncohort == 1) {
  ddpar <- matrix(stage_dd, nrow = 1, byrow = TRUE)
  stage_dd <- ddpar
} else {
  ddpar <- cbind(cohort_distro$means, 
                 matrix(rep(stage_dd[-1], nrow(cohort_distro)), 
                        nrow = nrow(cohort_distro), byrow = TRUE))
  stage_dd <- ddpar
}

# (4). METADATA OUTPUT FILE -----

# Push out a metadata file with all inputs used in model
cat("\nMETADATA: creating metadata file for all inputs used in model\n", 
    file = Model_rlogging, append = TRUE)
cat("\nMETADATA: creating metadata file for all inputs used in model\n")

# Create the metadata file
setwd(output_dir)
metadata <- sprintf("%s%s", "./", "/Logs_metadata/metadata.txt")
cat("### Metadata for DDRP cohorts v1 ###\n", file = metadata)

# Document species information
cat("\n### Model Species Parameters ###\n Species Abbrev:", spp, 
    "\n Full Name:", fullname, 
    "\n Pest of:", pestof,
    "\n Overwintering Stage:", owstage, file = metadata, append = TRUE)

# Document developmental threshold temperatures
cat("\n \n Developmental threshold temperatures",
    "\n Egg Lower Devel Threshold:", eggLDT, 
    "\n Egg Upper Devel Threshold:", eggUDT, 
    "\n Larvae Lower Devel Threshold:", larvaeLDT, 
    "\n Larvae Upper Devel Threshold:", larvaeUDT, 
    "\n Pupae Lower Devel Threshold:", pupaeLDT,
    "\n Pupae Upper Devel Threshold:", pupaeUDT, 
    "\n Adult Lower Devel Threshold:", adultLDT, 
    "\n Adult Upper Devel Threshold:", adultUDT, file =  metadata, append = T)

# Document stage durations
cat("\n\n Stage durations in degree-days (DDs)",
    "\n Egg DDs:", eggDD, 
    "\n Larvae DDs", larvaeDD, 
    "\n Pupae DDs:", pupDD, 
    "\n Adult DDs:", adultDD, "\n ", 
    file = metadata, append = TRUE)

# Document climate stress exclusion parameter values, if applicable
if (exclusions_stressunits) {
  cat("\n Climate stress parameters",
      "\n Lower Chill Threshold:", chillstress_threshold, 
      "\n Upper Heat Threshold:", heatstress_threshold,
      "\n Max Chill Units (lower bound):", chillstress_units_max1, 
      "\n Max Chill Units (upper bound):", chillstress_units_max2,
      "\n Max Heat Stress Units (lower bound):", heatstress_units_max1,
      "\n Max Heat Stress Units (upper bound):", heatstress_units_max2, 
      file = metadata, append = TRUE)
}

# Document Pest Event Map parameter values, if applicable
if (pems) {
  cat("\n \n Pest Event Map parameters",
  "\n Number of generations to make Pest Event Maps (PEMs): ", PEMnumgens,
  "\n Egg Event DDs and Label: ", eggEventDD, " (", eggEventLabel,")", 
  "\n Larvae Event DDs and Label: ", larvaeEventDD, " (", larvaeEventLabel, ")",
  "\n Pupae Event DDs and Label: ", pupaeEventDD, " (", pupaeEventLabel, ")",
  "\n Adult Event DDs and Label: ", adultEventDD, " (", adultEventLabel, ")",
  sep = "", file = metadata, append = TRUE)
}

cat("\n\n### Model Input Parameters ###\n Start Year:", start_year, 
    "\n Weather data for forecasts:", forecast_data, 
    "\n Start day-of-year:", start_doy,
    "\n End day-of-year:", end_doy, 
    "\n Region:", region_param, 
    "\n Climate stress exclusion maps:", exclusions_stressunits, 
    "\n Pest Event Maps:", pems,    
    "\n Adult Event Maps:", mapA, 
    "\n Egg Event Maps:", mapE, 
    "\n Larvae Event Maps:", mapL, 
    "\n Pupae Event Maps:", mapP,
    "\n Output_Dir:", out_dir, 
    "\n Output option:", out_option, 
    "\n No. of cohorts:", ncohort, 
    "\n Mean of end of OW stage (DDs):", distro_mean, 
    "\n Low bound of end of OW stage (DDs), xdist:", xdist1, 
    "\n High bound of end of OW stage (DDs), ydist:", xdist2, 
    "\n Variance in end of OW stage (DDs):", distro_var,
    "\n Shape of distribution of end of OW stage (DDs):", distro_shape, 
    "\n Plot odd gens only:", odd_gen_map, 
    file = metadata, append = TRUE)

# Make a table of stage DDs for each cohort and print to metadata
stage_dd.print <- as.data.frame(stage_dd)
stage_dd.print[1] <- round(stage_dd.print[1], 0)
colnames(stage_dd.print) <- stgorder
stage_dd.print <- cbind("cohort" = as.integer(rownames(stage_dd.print)), 
                        data.frame(stage_dd.print, row.names = NULL))

cat("\n\n###Durations (in degree-days) of stages in each of", 
    ncohort, "cohorts ###\n", file = metadata, append = TRUE) 
suppressWarnings(write.table(stage_dd.print, file = metadata, 
                             row.names = FALSE, 
                             col.names = colnames(stage_dd.print), 
                             append = TRUE))

cat("\nDurations (degree-days) of stages in each of", ncohort, "cohorts:\n\n") 
print(stage_dd.print, row.names = FALSE)

cat("\nDone writing metafile\n\n", forecast_data, " DATA PROCESSING\n", sep = "",
    file = Model_rlogging, append = TRUE)
cat("\nDone writing metafile\n\n", forecast_data, " DATA PROCESSING\n",
    sep = "")

# (5). WEATHER DATA LOADING AND PROCESSING -----

# Weather inputs and outputs - PRISM climate data w/subdirs 4-digit year
# New feature - choose whether to use PRISM or NMME for weather forecasts 
# (forecast_data = PRISM, or forecast_data = NMME)
tminfiles <- list.files(path = prism_dir, 
                        pattern = glob2rx(paste0("*PRISM_tmin_*", 
                                                 start_year, "*.bil$*")), 
                        all.files = FALSE, full.names = TRUE, recursive = TRUE)
tminfiles <- ExtractBestPRISM(tminfiles, forecast_data, 
                              keep_leap)[start_doy:end_doy]

tmaxfiles <- list.files(path = prism_dir, 
                        pattern = glob2rx(paste0("*PRISM_tmax_*",
                                                 start_year, "*.bil$*")), 
                        all.files = FALSE, full.names = TRUE, recursive = TRUE)
tmaxfiles <- ExtractBestPRISM(tmaxfiles, forecast_data, 
                              keep_leap) [start_doy:end_doy]

## Extract date from temperature files using regex pattern matching
dats <- regmatches(tminfiles, regexpr(pattern = "[0-9]{8}", text = tminfiles))

# Specify sampling frequency (how many days until output maps are generated?)
# This feature may be removed in production version
if (out_option == 1) {
  sample_freq <- 30 # Monthly maps
} else if (out_option == 2) {
  sample_freq <- 14  # Biweekly maps
} else if (out_option == 3) {
  sample_freq <- 10 # Dekad maps
} else if (out_option == 4) {
  sample_freq <- 7  # Weekly maps
} else if (out_option == 5) {
  sample_freq <- 2  # Even day maps
} else if (out_option == 6) {
  sample_freq <- 1 # Daily maps
  dats2 <- dats
} else if (out_option %in% !c(1, 2, 3, 4, 5, 6)) {
  cat("Error: out_option =", out_option, "is unacceptable; exiting program\n", 
      file = Model_rlogging, append = TRUE)
  cat("Error: out_option =", out_option, "is unacceptable; exiting program\n")
  q()  # No reason to keep going if sampling freq is not correctly specified
}

# Make vector of dates to use when processing results - last day is added too
# Using "unique" will only keep date if it doesn't already occur in vector
# This happens if the end day of year is a multiple of the sampling frequency 
# (e.g. 1 to 300, w/ a 30 day sampling frequency)
dats2 <- unique(c(dats[seq(0, length(dats), sample_freq)], last(dats)))

# Create vector of days in the sublist that will be sampled (rasters are saved 
# for those days). Then tack on last day for sampling; using "unique" will only 
# keep day if it doesn't already occur in vector
sample_pts <- sublist[seq(0, length(sublist), sample_freq)]
sample_pts <- unique(c(sample_pts, last(sublist)))

# Log file and terminal messages
cat("Finished loading ", forecast_data, " files for ", 
    length(start_doy:end_doy), " days\nCreating template file for ", 
    region_param, "\n", sep = "", file = Model_rlogging, append = TRUE)
cat("\nFinished loading ", forecast_data, " files for ", 
    length(start_doy:end_doy), " days\n\nCreating template file for ", 
    region_param, "\n", sep = "")

### * Create blank template from a temp file
# This template is used for cropping the temperature (tmin, tmax) rasters
REGION <- Assign_extent(region_param) # Bounding box
template <- crop(raster(tminfiles[1]), REGION) # Template for cropping
template[!is.na(template)] <- 0
dataType(template) <- "INT2U"
crs <- crs("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0, 0, 0")
crs(template) <- crs

#### * If CONUS or EAST, split template into tiles (and run in parallel)
# Benefit of tiles is lost for smaller regions, so these are not split
# SpaDES.tools requires the 'sf' package, which requires a newer version of GDAL
# .inorder must be set to TRUE so that output files are in correct order!

# Register DoParallel
# The "RegCluster" function determines an appropriate # of cores depending on 
# the "region_param" and "ncohort" parameters, so the server doesn't become
# overloaded
RegCluster(ncohort)

if (region_param %in% c("CONUS", "EAST")) {
  # Split template (2 pieces per side)
  tile_list <- SplitRas(template, ppside = 2, save = FALSE, plot = FALSE) 
  tile_n <- 1:length(tile_list) # How many tiles?
  cat("Splitting template into", length(tile_list), "tiles\n", 
      file = Model_rlogging, append = TRUE)
  
  # Name the 4 tiles (tile1, tile2, tile3, tile4)
  template <- mapply(function(n, t) {
    names(n) <- paste0("tile", t)
    #values(n) <- 0
    return(n)
  }, n = tile_list, t = tile_n )
  
  # Crop temp files by each template tile
  cat("Cropping tmax and tmin tiles for", region_param, "\n", 
      file = Model_rlogging, append = TRUE)
  cat("\nCropping tmax and tmin tiles for", region_param, "\n")
  
  tmax_list <- foreach(tile = template, .packages = "raster", 
                       .inorder = FALSE) %:% 
    foreach(tmax = tmaxfiles, .packages = "raster", .inorder = TRUE) %dopar% { 
      m <- as.matrix(crop(raster(tmax), tile))
    }
  
  tmin_list <- foreach(tile = template, .packages = "raster", 
                       .inorder = FALSE) %:% 
    foreach(tmin = tminfiles, .packages = "raster", .inorder = TRUE) %dopar% { 
      m <- as.matrix(crop(raster(tmin), tile))
    }
  
# If region is not CONUS or EAST, simply crop temp files by the single template
} else {
  cat("Cropping tmax and tmin tiles for", region_param, "\n", 
      file = Model_rlogging, append = TRUE)
  cat("\nCropping tmax and tmin tiles for", region_param, "\n")
  
  tmax_list <- foreach(t = tmaxfiles, .packages = "raster", 
                       .inorder = TRUE) %dopar% {
    m <- as.matrix(crop(raster(t), template))
  }
  
  tmin_list <- foreach(t = tminfiles, .packages = "raster", 
                       .inorder = TRUE) %dopar% {
    m <- as.matrix(crop(raster(t), template))
  }
  
  stopCluster(cl)
}

cat("Done processing ", forecast_data, " data\n\nDAILY LOOP\n", sep = "",
    file = Model_rlogging, append = TRUE)
cat("\nDone processing ", forecast_data, " data\n\nDAILY LOOP\n", sep = "")

## (6). RUN THE DAILY LOOP -----
# First set up number of cores to use
# IMPORTANT: using too many cores will result in low memory, killing the daily 
# loop part-way through. For "mclapply" functions, set mc.cores manually.

# For mclapply and mcmapply: Can not use >1 core on Windows, so detect OS
if (grepl("Windows", Sys.info()[1])) {
  mc.cores <- 1
} else {
  mc.cores <- 4 # use 4 here, because there are 4 tiles being run in parallel
}

# Split cohorts into smaller chunks for CONUS and EAST to avoid overloading 
# memory when running in parallel. Three cohorts puts load up to ~13.
if (region_param %in% c("CONUS", "EAST")) {
  cohort_chunks <- split(1:ncohort, ceiling(1:length(1:ncohort)/2)) 
} else {
  cohort_chunks <- split(1:ncohort, ceiling(1:length(1:ncohort)/5))
}

tic("Daily loop run time") # Start timing the daily loop run-time
# cat("DAILY LOOP: daily loop log files show loop progress and 
# output file info\n", file = Model_rlogging, append = TRUE)
cat("Sampling every", sample_freq, "days between", first(dats), "and", 
    last(dats), "\n", file = Model_rlogging, append = TRUE) 
cat("\nSampling every", sample_freq, "days between", first(dats), "and", 
    last(dats), "\n") 

RegCluster(ncohort)

# Run it! If there is an error the program will stop
tryCatch(
  {
    # If the region is CONUS or EAST, then both cohorts and tiles will be run in
    # parallel. To avoid overloading the server, mc.cores = 4 (for the 4 tiles,
    # which keeps load < 12. The run-time is approx 1.5-2.5 minutes.
    if (region_param %in% c("CONUS", "EAST")) {
      # Total number of nodes is mc.cores * 2 because use mclapply twice in loop
      for (c in cohort_chunks) {
        cat("Running daily loop for cohorts", as.character(c), "\n", 
            file = Model_rlogging, append = TRUE)
        cat("\nRunning daily loop for cohorts", as.character(c), "\n")
        cohort_vec <- unname(unlist(c)) # change to an unnamed vector
        # For some reason foreach here just doesn't work well for CONUS 
        # (slow, way too much load) - not sure why
        mclapply(cohort_vec, function(cohort) {
          mclapply(1:length(template), function(tile_num) {
            tile <- template[[tile_num]]
            DailyLoop(cohort, tile_num, tile) 
          }, mc.cores = mc.cores)
        }, mc.cores = mc.cores)  
      }
    } else {
      # If the region is not CONUS or EAST, then we don't need to run function 
      # for multiple tiles.
      for (c in cohort_chunks) {
        cat("Running daily loop for cohorts", as.character(c), "\n", 
            file = Model_rlogging, append = TRUE)
        cat("\nRunning daily loop for cohorts", as.character(c), "\n")
        cohort_vec <- unname(unlist(c)) # change to an unnamed vector
        foreach(cohort = cohort_vec, .packages = pkgs, 
                .inorder = FALSE) %dopar% {
          DailyLoop(cohort, NA, template)
        }
        
      }
    }
  },
  error = function(e) {
    cat("Error in Daily Loop - stopped run - check rmessages file\n", 
        file = Model_rlogging, append = TRUE) 
    cat("\nError in Daily Loop - stopped run - check rmessages file\n")
})

stopCluster(cl)

# Document daily loop execution time
loop_exectime <- toc(quiet = TRUE)
loop_exectime <- (loop_exectime$toc - loop_exectime$tic) / 60 

cat("Daily loop done (run time = ", round(loop_exectime, digits = 2), " min)",
    "\n\nFINAL ANALYSES AND MAP PRODUCTION\n", sep = "",
    file = Model_rlogging, append = TRUE) 
cat("\nDaily loop done (run time = ", round(loop_exectime, digits = 2), " min)",
    "\n\nFINAL ANALYSES AND MAP PRODUCTION\n", sep = "")

## (7). PROCESS DAILY LOOP RESULTS -----
setwd(output_dir)
tic("Data processing run time") # Start timing for data processing

# Create a directory ("Misc_output") to put secondary outfiles
dir.create("Misc_output")

#### * If CONUS or EAST, merge and delete tiles ####
# If CONUS or EAST, merge the tiles
if (region_param %in% c("CONUS", "EAST")) {
  cat("\nMerging tiles for", region_param, "\n\n", 
      file = Model_rlogging, append = TRUE)
  cat("\nMerging tiles for", region_param, "\n")
  # Get list of brick files for each tile, the type of file, 
  # its cohort number, and then make a list of the file types
  # File types are split up to avoid overloading sytem when running in parallel
  brick_files <- list.files(pattern = glob2rx("*cohort*tile*tif$"), 
                            recursive = FALSE)
  type_list <- unique(str_split_fixed(brick_files, 
                                      pattern = "_cohort", 4)[,1]) 
  type_list_split <- split(type_list, ceiling(1:length(type_list)/3)) 
  cohorts <- unique(str_split_fixed(brick_files, pattern = "cohort", 4)[,2])
  cohorts <- unique(substring(cohorts, 1, 1)) # Vector of cohorts (1, 2, 3, ...)
  
  # For each file type, merge the tiles for all cohorts
  # File type and cohorts are both run in parallel to increase speed
  # If system is overloaded then consider: 
  # 1) splitting up the cohorts list (as in the "type_list_split"); or 
  # 2) decreasing the number of splits in "type_list_split")
  RegCluster(ncohort)
  
  mrg_by_type <- foreach(type = type_list_split, .packages = pkgs, 
                         .inorder = FALSE) %dopar% {
    type_vec <- unname(unlist(type)) # Change to an unnamed vector
    for (t in type_vec) {
      # If type is exclusion, stressunits, or ddtotal files, 
      # then just do cohort 1; no other cohorts present
      if (grepl("Stress_Excl|Stress_Units|DDtotal", t)) {
        CombineMaps(brick_files, t, "1")
        cat("Merged", t, "tiles for cohort 1\n", 
            file = Model_rlogging, append = TRUE)
      # If another file type, then merge tiles for all cohorts
      } else {
        MrgTiles <- foreach(c = cohorts, .packages = pkgs, 
                            .inorder = TRUE) %dopar% {
          CombineMaps(brick_files, t, c)
          cat("Merged", t, "tiles for cohort", c, "\n", 
              file = Model_rlogging, append = TRUE)
        }
      } 
    }
  }
  stopCluster(cl)
  cat("\nDone merging tiles\n", file = Model_rlogging, append = TRUE)
  cat("\nDone merging tiles\n")
}

# If CONUS or EAST, remove tile files, but first check that merged files 
# exist for each type (e.g., Lifestage, NumGen, ...)
if (region_param %in% c("CONUS", "EAST")) {
  cat("\nDeleting tiles for", region_param, "\n\n", 
      file = Model_rlogging, append = TRUE)
  cat("\nDeleting tiles for", region_param, "\n")
  for (t in type_list) {
    # Should only be a single merged file for these types
    # Remove the "_all" from the name of the file - this isn't needed
    if (grepl("Stress_Excl|Stress_Units|DDtotal", t)) {
      fl <- list.files(pattern = glob2rx(paste0("*", t, "_*all*tif$")))
      if (length(fl == 1)) {
        unlink(list.files(pattern = glob2rx(paste0("*", t, "_*tile*tif$"))))
        file.rename(from = fl, to = sub(pattern = "_cohort1_all", 
                                        replacement = "", fl))
      }
     
      # For other types, the number of merged files should equal the number 
      # of cohorts. The exception is if OW stage DD distro parameters for 
      # cohorts results in some cohorts not making DD cutoffs for PEMs - 
      # a warning will be recorded if this is the case
    } else {
      fls <- list.files(pattern = glob2rx(paste0("*", t, "_*all*tif$")))
      if (length(fls) == ncohort) {
        unlink(list.files(pattern = glob2rx(paste0("*", t, "_*tile*tif$"))))
        cat("Deleted tile files for", t, "\n", 
            file = Model_rlogging, append = TRUE)
        # Generate a warning message if some cohorts are missing
      } else {
        unlink(list.files(pattern = glob2rx(paste0("*", t, "_*tile*tif$"))))
        cat("\nWarning: only", length(fls), 
            "cohort files found for", t, "- check params\n", 
            file = Model_rlogging, append = TRUE)
        cat("\nWarning: only", length(fls), 
            "cohort files found for", t, "- check params\n")
      }
    }
  }
  cat("\nDone deleting tile files\n", file = Model_rlogging, append = TRUE)
  cat("\nDone deleting tile files\n")
}

#### * Some ggplot2 settings ####
# Some ggplot2 settings are specified here, but other settings are specified
# in the functions file

# Map production in ggplot requires specifying plot.height and plot.width
# These need to be dynamic because regions have different aspect ratios, 
# which results warped looking maps
# Calculate bounding box (xmin, xmax, ymin, ymax) of REGION 
coord <- coord_quickmap(xlim = c(REGION@xmin, REGION@xmax), 
                        ylim = c(REGION@ymin, REGION@ymax), expand = FALSE)
asp <- coord$aspect(list(x.range = c(REGION@xmin, REGION@xmax), 
                         y.range = c(REGION@ymin, REGION@ymax))) # aspect ratio

# Adjust base_size for ggplot2 (font size) according to aspect ratio
if (asp >= 1.7) {
  base_size <- 10.5
  legend_units <- 1.4
} else if (asp >= 1.5 & asp < 1.7) {
  base_size <- 9.5
  legend_units <- 1.3
} else if (asp >= 1.2 & asp < 1.5) {
  base_size <- 8.5 
  legend_units <- 1.2
} else if (asp >= 1 & asp < 1.2) {
  base_size <- 8
  legend_units <- 1
} else if (asp < 1 & asp >= 0.6) {
  base_size <- 7.5
  legend_units <- asp
} else if (asp < 0.6 & asp >= 0.45) {
  base_size <- 5.5
  legend_units <- asp
} else if (asp < 0.45) {
  base_size <- 5
  legend_units <- asp
}

# Theme to use for plots
mytheme <- theme(legend.text = element_text(size = rel(1)), 
  legend.title = element_text(size = rel(1.2), face = "bold"),
  legend.position = "right", 
  legend.justification = "left",
  legend.margin = margin(t = 0, r = 0.10, b = 0, l = 0.10, unit = "cm"),
  legend.key.width = unit(legend_units, "line"), 
  legend.key.height = unit(legend_units, "line"),
  plot.title = element_text(size = rel(1.55), face = "bold", hjust = 0.5, 
                            vjust = -3, lineheight = 1, 
                            margin = margin(t = 0, r = 0, b = 2, l = 0)), 
  plot.subtitle = element_text(size = rel(1.25), hjust = 0.5, vjust = -3, 
                               lineheight = 1, 
                               margin = margin(t = 5, r = 0, b = 15, l = 0)),
  plot.margin = margin(t = 0.05, r = 0.25, b = 0.05, l = 0.25, unit = "cm"),
  panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
  panel.background = element_blank(), panel.border = element_blank(),
  axis.title.x = element_blank(), 
  axis.title.y = element_blank(), 
  axis.ticks = element_blank(),
  axis.text.x = element_blank(), 
  axis.text.y = element_blank())

### * DDtotal and climate stress ####

if (exclusions_stressunits) {
  cat("\n", str_wrap("### SUMMARY MAPS: DDTOTAL, CLIMATE STRESS EXCL., AND 
                     CLIMATE STRESS UNITS ###", width = 80), sep = "", 
      file = Model_rlogging, append = TRUE)
  cat("\n", str_wrap("SUMMARY MAPS: DDTOTAL, CLIMATE STRESS EXCL., AND CLIMATE 
               STRESS UNITS", width = 80), "\n", sep = "")
} else {
  cat("\n### SUMMARY MAPS: DDTOTAL ###", file = Model_rlogging, append = TRUE)
  cat("\nSUMMARY MAPS: DDTOTAL\n")
}

# Split up the dates into chunks - this avoids overloading the server w/ 
# running too many dates in parallel
dats_list <- split(dats2, ceiling(1:length(dats2)/4))
last_date <- last(dats2)

# For each date in a date chunk, plot and save summary maps for:
# degree-day accumulation, chill stress unit accumulation, chill stress 
# exclusion, heat stress unit accumulation, heat stress exclusion, and all 
# stress exclusion
RegCluster(ncohort)

#for (dat in dats_list) {
stress_results <- foreach(dat = dats_list, .packages = pkgs, 
                          .inorder = TRUE) %dopar% {
  dat_vec <- unname(unlist(dat)) # change to an unnamed vector
    for (d in dat_vec) {
      # get position (layer) of date in raster brick
      lyr <- which(dats2 == d)
      # make the plots
      DDtotal_brick <- brick("DDtotal.tif")
      PlotMap(DDtotal_brick[[lyr]],d, "Degree day (DD) accumulation", 
              "Degree Days", "Misc_output/DDtotal")
      
      if (exclusions_stressunits) {
        # Bring in climate stress bricks for each cohort
        # Chill stress unit accumulation
        chillunitsCUM_brick <- brick("Chill_Stress_Units.tif")
        PlotMap_stress(chillunitsCUM_brick[[lyr]], d, chillstress_units_max1,
                       chillstress_units_max2, "Chill stress units", 
                       "Chill Stress Units", "Misc_output/Chill_Stress_Units")
        # Chill stress exlusions (-1 = moderate; -2 = severe)
        chillEXCL_brick <- brick("Chill_Stress_Excl.tif")
        PlotMap(chillEXCL_brick[[lyr]], d,  "Chill stress exclusion", 
                "Exclusion status", "Misc_output/Chill_Stress_Excl")
        # Heat unit accumulation
        heatunitsCUM_brick <- brick("Heat_Stress_Units.tif")
        PlotMap_stress(heatunitsCUM_brick[[lyr]], d, heatstress_units_max1,
                       heatstress_units_max2, "Heat stress units", 
                       "Heat Stress Units", "Misc_output/Heat_Stress_Units")
        # Heat stress exclusions (-1 = moderate; -2 = severe)
        heatEXCL_brick <- brick("Heat_Stress_Excl.tif")
        PlotMap(heatEXCL_brick[[lyr]], d,  "Heat stress exclusion", 
                "Exclusion status", "Misc_output/Heat_Stress_Excl")
        # All stress exclusions (chill stress + heat stress exclusions)
        AllEXCL_brick <- brick("All_Stress_Excl.tif")
        PlotMap(AllEXCL_brick[[lyr]], d,  "All stress exclusion", 
                "Exclusion status", "All_Stress_Excl")
      }
    }
}

stopCluster(cl)

# Log file messages

# If no PEMS and no climate stress exclusions, then moving on to Lifestage 
# analyses 
if (!pems & !exclusions_stressunits) {
  cat("\n\nDone with DDtotal maps\n", file = Model_rlogging, append = TRUE)  
  cat("\n", str_wrap("### SUMMARY MAPS AND WEIGHTED RASTER OUTPUT: LIFESTAGE 
                     ###", width = 80), "\n\nStages: ", 
      paste(stgorder, collapse = ", "), 
      sep = "", file = Model_rlogging, append = TRUE)
  cat("\nSUMMARY MAPS AND WEIGHTED RASTER OUTPUT: LIFESTAGE ###","\n\nStages: ",
      paste(stgorder, collapse = ", "), "\n")
  # If no PEMS but climate stress exclusions, then moving on to Lifestage 
  # analyses that also include climate stress exclusions
} else if (!pems & exclusions_stressunits) {
  cat("\n\n", str_wrap("Done with DDtotal, climate stress exclusions, and 
                       climate stress unit maps", width = 80), "\n", sep = "",
      file = Model_rlogging, append = TRUE)  
  cat("\n", str_wrap("### SUMMARY MAPS AND WEIGHTED RASTER OUTPUT:
      LIFESTAGE W/ CLIM. STRESS EXCL. ###", width = 80), "\n\nStages: ", 
      paste(stgorder, collapse = ", "), sep = "",
      file = Model_rlogging, append = TRUE)
  cat("\n", str_wrap("### SUMMARY MAPS AND WEIGHTED RASTER OUTPUT:
      LIFESTAGE W/ CLIM. STRESS EXCL. ###", width = 80), "\nStages: ", 
      paste(stgorder, collapse = ", "), sep = "")
  # If PEMS but no climate stress exclusions, then conducting PEM analyses
} else if (pems & !exclusions_stressunits) {
  cat("\n\nDone with DDtotal summary maps\n\n", 
      "### SUMMARY MAPS AND RASTER OUTPUT: PEST EVENT MAPS ###", sep = "",
      file = Model_rlogging, append = TRUE)
  cat("\nDone with DDtotal summary maps\n\n", 
      "SUMMARY MAPS AND RASTER OUTPUT: PEST EVENT MAPS\n", sep = "")
  # If PEMS and climate stress exclusions, then conducting PEM analyses that 
  # also include climate stress exclusions
} else if (pems & exclusions_stressunits) {
  cat("\n\n", str_wrap("Done with DDtotal, climate stress exclusions, and 
                       climate stress unit maps", width = 80), "\n\n", 
              str_wrap("### SUMMARY MAPS AND RASTER OUTPUT: PEST EVENT MAPS W/
                       CLIMATE STRESS EXCL. ###", width = 80), sep = "",
      file = Model_rlogging, append = TRUE)
  cat("\n", str_wrap("Done with DDtotal, climate stress exclusions, and climate 
               stress unit maps", width = 80), "\n\n", 
      str_wrap("SUMMARY MAPS AND RASTER OUTPUT: PEST EVENT MAPS W/
                CLIMATE STRESS EXCL.\n", width = 80), sep = "")
}

#### * Pest Event Maps ####

# Process and plot the Pest Event Maps (PEMs) 
# Currently, the earliest date across cohorts and average data across cohorts 
# is calculated for the last day (last day is last element of "sublist" vector)

if (pems) {
  # Get all PEM files, split them by type (e.g., PEMe1, PEMe2) and stage 
  # (e.g., "egg", "adult")
  pem_files <-  list.files(pattern = glob2rx("*PEM*.tif$")) # all PEM files
  pem_types <- unique(substr(pem_files, start = 1, stop = 5)) # split by type
  
  # Create a data frame with PEM labels - the labels will be joined to the 
  # appropriate PEM file below
  pem_event_labels <- cbind(data.frame("pem_types" = pem_types), 
    data.frame("gen" = substr(pem_types, start = 5, stop = 5)), 
    data.frame("stg" = substr(pem_types, start = 4, stop = 4)))
  pem_event_labels <- pem_event_labels %>%
    mutate(genLabel = ifelse(gen == 0, "date of OW gen.", 
                      ifelse(gen == 1, "date of 1st gen.", 
                      ifelse(gen == 2, "date of 2nd gen.", 
                      ifelse(gen == 3, "date of 3rd gen.", 
                      ifelse(gen == 4, "date of 4th gen.", NA)))))) %>% 
    mutate(eventLabel = ifelse(stg == "e", eggEventLabel, 
                        ifelse(stg == "l", larvaeEventLabel, 
                        ifelse(stg == "p", pupaeEventLabel, 
                        ifelse(stg == "a", adultEventLabel, NA))))) %>%
    mutate(finalLabel = paste(genLabel, eventLabel, sep = " ")) %>% 
                      dplyr::select(pem_types, finalLabel)
  
  # Which PEM is for the OW stage?
  OW_pem <- paste0("PEM", tolower(substr(owstage, start = 2, stop = 2)), "0")
  
  # Analyze the PEMs for each cohort in parallel, and export resulting rasters 
  # and summary maps
  RegCluster(ncohort)
  
  pem_results <- foreach(type = pem_types, .packages = pkgs, 
                        .inorder = FALSE) %dopar% {
  #for (type in pem_types) {
    #print(type)
    # Find files by type (e.g., "PEMe1" for each cohort) 
    files_by_type <- pem_files[grep(pattern = type, x = pem_files, 
                                    fixed = TRUE)] 
    # Change 0 values to NA so they are not averaged 
    pem_stk <- stack(files_by_type)
    pem_stk[pem_stk == 0] <- NA
    
    # Remove PEM raster brick files if all PEM values are 0 because 
    # there's no point in plotting an all zero result
    if (sum(matrix(pem_stk), na.rm = TRUE) == 0) {
      unlink(list.files(pattern = glob2rx(paste0("*", type, "_*tif$"))))
    # Remove PEM raster brick file if there is only a single unique value
    # This may happen for the last day of sampling period? (check on this)
    } else if (length(unique(values(pem_stk))) < 3) {
        vals <- unique(getValues(pem_stk))
        vals <- vals[!is.na(vals)]
        if (length(vals) == 1) {
          unlink(list.files(pattern = glob2rx(paste0("*", type, "_*tif$"))))
        }
      # If PEM raster brick has data, then process and plot it
    } else {
    # Create event label to be used for making summary maps
      eventLabel_df <- dplyr::filter(pem_event_labels, pem_types == type) %>% 
        dplyr::select(finalLabel) %>%
        mutate(., finalLabel = ifelse(type == OW_pem, paste("date of OW gen.", 
                                              OWEventLabel), finalLabel))          
        eventLabel <- paste(eventLabel_df$finalLabel)
      # Remove layers in the PEM stack if they are all NA, and print warning 
      # If this happens, should maybe change cohort emergence params
      zero_sum <- cellStats(pem_stk, sum)
      pem_na <- pem_stk[[which(zero_sum == 0)]]
      pem_stk <- pem_stk[[which(zero_sum > 0)]]
      if (nlayers(pem_stk) < ncohort) {
        cat(str_wrap("WARNING: removed", names(pem_na), 
            "layer because all values were NA - check emergence parameters \n",
            width = 80), file = Model_rlogging, append = TRUE)
        cat("\nWARNING: removed", names(pem_na), "layer because all values 
            were NA - check emergence parameters \n\n")
      }
          
      # Calc. avg. date of pest event across cohorts
      # Then save raster brick, and create and save summary maps
      avg_PEM <- calc(pem_stk, fun = function(x, na.rm= TRUE) { 
        mean(x, na.rm = TRUE) }) # average in day of event among cohorts
      names(avg_PEM) <- "Avg" # name layer for use below
      SaveRaster2(avg_PEM, paste("Avg", type, last_date, sep = "_"), 
                  "INT2U", paste("- Avg.", eventLabel))
      PlotMap(avg_PEM, last_date, paste("Avg.", eventLabel, sep = " "), 
              paste("Avg.", eventLabel, sep = " "), 
              paste("Avg", type, sep = "_"))
          
      # Calc. the earliest date of pest event across cohorts 
      # Then save raster brick, and create and save summary maps
      min_PEM <- calc(pem_stk, fun = function(x, na.rm= TRUE) { min(x) })
      names(min_PEM) <- "Earliest" # name layer for use below
      SaveRaster2(min_PEM, paste("Earliest", type, last_date, sep = "_"), 
                  "INT2U", paste("- Earliest", eventLabel))
      PlotMap(min_PEM, last_date, paste("Earliest", eventLabel, sep = " "), 
              paste("Earliest", eventLabel, sep = " "), 
              paste("Earliest", type, sep = "_"))
          
      # If climate stress exclusions are specified, then take PEM results from 
      # above and substitute values where the species is under severe stress 
      # only with -1 (Excl1), and areas where the species is under both
      # moderate and severe stress with -1 and -2, respectively (Excl2)
      if (exclusions_stressunits) {
        PEM_list <- list(avg_PEM, min_PEM)
        for (PEM in PEM_list) {
          nam <- names(PEM)
          
          # Do calculations
          PEM_excl1 <- Rast_Subs_Excl1(PEM)[[1]] # Sev. stress only (Excl1) 
          PEM_excl2 <- Rast_Subs_Excl2(PEM)[[1]] # Sev. and mod. stress (Excl2) 
              
          # Save raster brick results; create and save summary maps
          SaveRaster2(PEM_excl1, paste0(nam, "_", type, "Excl1_", last_date), 
                      "INT2S", paste("-", nam, eventLabel))
          SaveRaster2(PEM_excl2, paste0(nam, "_", type, "Excl2_", last_date), 
                      "INT2S", paste("-", nam, eventLabel))
          PlotMap(PEM_excl1, last_date, paste0(nam, " ", eventLabel, 
                  " w/ climate stress exclusion"), paste0(nam, " ", eventLabel),
                  paste0(nam, "_", type, "Excl1"))
          PlotMap(PEM_excl2, last_date, paste0(nam, " ", eventLabel, 
                  " w/ climate stress exclusion"), paste0(nam, " ", eventLabel), 
                  paste0(nam, "_", type, "Excl2"))
          }
        }
      }
  }
  stopCluster(cl)
}

# Remove PEM cohort files - no longer needed
unlink(list.files(pattern = glob2rx(paste0("*PEM*cohort*"))))

# Log file messages
if (pems & exclusions_stressunits) {
  cat("\n\nDone with Pest Event Maps\n\n", str_wrap("### WEIGHTED RASTER OUTPUT 
      AND SUMMARY MAPS: LIFESTAGE W/ CLIM. STRESS EXCL. ###", width = 80), 
      "\n\nStages: ", paste(stgorder, collapse = ", "), sep = "",
      file = Model_rlogging, append = TRUE)  
  cat("\n\nDone with Pest Event Maps\n\n", str_wrap("WEIGHTED RASTER OUTPUT AND 
      SUMMARY MAPS: LIFESTAGE W/ CLIM. STRESS EXCL.", width = 80), "\nStages: ",
      paste(stgorder, collapse = ", "), "\n", sep = "")
} else if (pems & !exclusions_stressunits) {
  cat("\n\nDone with Pest Event Maps\n\n", str_wrap("### WEIGHTED RASTER OUTPUT 
      AND SUMMARY MAPS: LIFESTAGE ###", width = 80), "\n\nStages: ", 
      paste(stgorder, collapse = ", "), sep = "",
      file = Model_rlogging, append = TRUE) 
  cat("\nDone with Pest Event Maps\n\n", 
      "WEIGHTED RASTER OUTPUT AND SUMMARY MAPS: LIFESTAGE", "\nStages: ",
      paste(stgorder, collapse = ", "), "\n", sep = "")
}

# Make file lists for weighting the rasters by relative population size
Lfstg_fls <- list.files(pattern = glob2rx("*Lifestage_*.tif$"))
if (exclusions_stressunits) {
  Lfstg_Excl1_fls <- list.files(pattern = glob2rx("*LifestageExcl1*.tif$"))
  Lfstg_Excl2_fls <- list.files(pattern = glob2rx("*LifestageExcl2*.tif$"))
}

#### * Lifestage raster processing and plots ####
# Output weighted rasters and summary maps for ALL stages - this splits outs 
# the OW stage separately. Next, output weighted rasters and summary maps for 
# stages that combine OW stage w/ actual stage (e.g. OW adult = adult).

# Match OW stage to actual stage (e.g., OA = adult)
if (owstage == "OL") {
  stage_list <- c("OL","E","L","P","A")
} else if (owstage == "OP") {
  stage_list <- c("OP","E","L","P","A")
} else if (owstage == "OA") {
  stage_list <- c("OA","E","L","P","A")
} else if (owstage == "OE") {
  stage_list <- c("OE","L","P","A","E")
}

# What is the non-OW form of the OW stage? (e.g., OWadult = Adult)
stg_nonOW <- substring(owstage, 2) 

# Weight the lifestage rasters to calc. the relative size of the population 
# in any given lifestage). The stages are in parallel for increased speed.
RegCluster(3)

Wtd_Lfstg_others <-  foreach(stg = stage_list, .packages = pkgs, 
                                 .inorder = TRUE) %dopar% {
#for (stg in stage_list) {
  # Get stage number of stage, and then rename to a more descriptive name
  stg_num <- match(stg, stgorder)
  stg_nam <- mgsub(string = stg, pattern = 
                     c("OE", "OL", "OP", "OA", "E", "L", "P", "A"), 
                   replacement = c("OWegg", "OWlarvae", "OWpupae", "OWadult",
                                   "Egg", "Larvae", "Pupae", "Adult"))
  # Comment
  if (stg != stg_nonOW) {
    ### Weight the rasters ###
    # "mapply" (or parallel version = mcmapply) requires two input lists of the 
    # same length, and outputs a list of bricks for each cohort with nlayer = # 
    # of sampling pts. For example, if there are 7 cohorts sampled each month of 
    # yr (12 mon + end of year), then rast_wtd will be a list of 7 cohorts each 
    # w/ 13 layers (13 * 7 = 91 layers). "mcmapply" can be used for increased 
    # speed, but it really overloads memory. 
    
    # For each life stage, extract data from raster brick, and multiple it by 
    # the rel. pop size of each cohort. The result is a list of raster bricks 
    # that provide the relative pop. size of each cohort for each date.
    Lfstg_wtd_list <- mapply(function(x, y) {
      calc((brick(x) == stg_num), 
           fun = function(z) {
             round(100 * z) * y
          })
    }, x = Lfstg_fls, y = relpopsize)
    
    # The results from previous step need to be summed across all cohort. This 
    # produces a single raster brick in which each layer represents the relative 
    # population size of each cohort for each date.
    # "Reduce" accomplishes this task.
    Lfstg_wtd_brk <- Reduce("+", Lfstg_wtd_list) 
    SaveRaster2(Lfstg_wtd_brk, paste0("Misc_output/", stg_nam), "INT2U", 
                paste("-", stg_nam, "relative pop. size for all", 
                      nlayers(Lfstg_wtd_brk), "dates")) 
    
    # Create and save summary maps
    Lfstg_plots <- mclapply(1:nlayers(Lfstg_wtd_brk), function(lyr) {
        lyr_name <- paste0(dats2[[lyr]])
        PlotMap(Lfstg_wtd_brk[[lyr]], lyr_name, 
                paste0(stg_nam, " relative pop. size"), 
                "Relative pop. size", paste0("Misc_output/", stg_nam))
    }, mc.cores = 3)
    
    # If climate stress exclusions are specified, then take weighted lifestage 
    # results from above and substitute values where the species is under severe 
    # stress only with -1 (Excl1), and areas where the species is both moderate 
    # and severe stress with -1 and -2, respectively (Excl2)
    if (exclusions_stressunits) {
      Lfstg_wtd_excl1 <- Rast_Subs_Excl1(Lfstg_wtd_brk)  
      Lfstg_wtd_excl2 <- Rast_Subs_Excl2(Lfstg_wtd_brk)
      
      # Make a raster brick of results and save the brick
      Lfstg_wtd_excl1_brk <- do.call(brick, Lfstg_wtd_excl1)
      Lfstg_wtd_excl2_brk <- do.call(brick, Lfstg_wtd_excl2)
      SaveRaster2(Lfstg_wtd_excl1_brk, 
                  paste0("Misc_output/", stg_nam, "_Excl1"), 
                  "INT2S", paste("-", stg_nam, "relative pop. size for all", 
                  nlayers(Lfstg_wtd_excl1_brk), "dates")) 
      SaveRaster2(Lfstg_wtd_excl2_brk, 
                  paste0("Misc_output/", stg_nam, "_Excl2"), 
                  "INT2S", paste("-", stg_nam, "relative pop. size for all", 
                  nlayers(Lfstg_wtd_excl2_brk), "dates")) 
      
      # create and save summary maps of results
      Lfstg_Excl1_plots <- mclapply(1:nlayers(Lfstg_wtd_excl1_brk), 
        function(lyr) {
          lyr_name <- paste0(dats2[[lyr]])
          PlotMap(Lfstg_wtd_excl1_brk[[lyr]], lyr_name, paste(stg_nam, 
                 "relative pop. size w/ climate stress exclusion", sep = " "), 
                 "Relative pop. size", 
                 paste0("Misc_output/", stg_nam, "_Excl1"))
      }, mc.cores = 3)
      
      Lfstg_Excl2_plots <- mclapply(1:nlayers(Lfstg_wtd_excl2_brk), 
        function(lyr) {
          lyr_name <- paste0(dats2[[lyr]])
          PlotMap(Lfstg_wtd_excl2_brk[[lyr]], lyr_name, paste(stg_nam, 
                  "relative pop. size w/ climate stress exclusion", sep = " "), 
                  "Relative pop. size", 
                  paste0("Misc_output/", stg_nam, "_Excl2"))
      }, mc.cores = 3)
    }
  
  } 
  
  # If the stage is the non-OW form of the OWstage (i.e., Adult = OWadult,
  # Larvae = OWlarvae), then the output from each of these should be merged.
  # It doesn't make sense to have an Adult raster/plot that does not also 
  # include the OW form of that stage. Thus, the OW results (e.g., "OWadult) are
  # split out the non-OW form (e.g. "Adult") above, but here they are being 
  # combined. 
  # Match overwintering stage to actual stage (e.g. OA = A, OE = E, etc.)
  
  else if (stg == stg_nonOW) {
    stg_nonOW <- substring(owstage, 2) # The non-OW stage? (OWadult = adult)
    stg_nonOW_nam <- mgsub(string = stg_nonOW, pattern = c("E", "L", "P", "A"), 
                           replacement = c("Egg", "Larvae", "Pupae", "Adult"))
    
    # Function to recode results from the OW stage (stg_num = 1) to the same 
    # value as the non-OW stage, and then calculate the rel. pop. size of the
    # combined stages (e.g., "Adult" size + "OWadult" size)
    Wtd_Lfstg_incOW <- mapply(function(x, y) {
      stg_num <- match(stg_nonOW, stgorder) # Get stage no. of non-OW stage
      brk <- brick(x)
      brk[brk == 1] <- stg_num # Replace OW val with nonOW val (OWadult = adult)
      calc(brk == stg_num, fun = function(z) { round(100 * z) * y })
    }, x = Lfstg_fls, y = relpopsize)
    
    # Sums values of all cohorts (raster brick layers) together; then save 
    # raster brick, and create and save summary plots
    Lfstg_incOW_wtd <- Reduce("+", Wtd_Lfstg_incOW) 
    SaveRaster2(Lfstg_incOW_wtd, paste0("Misc_output/", stg_nonOW_nam), 
                "INT2U", paste("-", stg_nonOW_nam, "relative pop. size for", 
                               nlayers(Lfstg_incOW_wtd), "dates"))
    
    Lfstg_incOW_plots <- mclapply(1:nlayers(Lfstg_incOW_wtd), 
      function(lyr) {
        lyr_name <- paste0(dats2[[lyr]])
        PlotMap(Lfstg_incOW_wtd[[lyr]], lyr_name, 
                paste(stg_nonOW_nam, "relative pop. size"), 
                "Relative pop. size", paste0("Misc_output/", stg_nonOW_nam))
    }, mc.cores = 2)
    
    # If climate stress exclusions are specified, then take substitute values 
    # where the species is under severe stress with -1 (Excl1), and areas where 
    # the species is under both moderate and severe stress with -1 and -2, 
    # respectively (Excl2)
    if (exclusions_stressunits) {
      Lfstg_incOW_wtd_excl1 <- Rast_Subs_Excl1(Lfstg_incOW_wtd)
      Lfstg_incOW_wtd_excl2 <- Rast_Subs_Excl2(Lfstg_incOW_wtd)
      Lfstg_incOW_wtd_excl1_brk <- do.call(brick, Lfstg_incOW_wtd_excl1)
      Lfstg_incOW_wtd_excl2_brk <- do.call(brick, Lfstg_incOW_wtd_excl2)
      SaveRaster2(Lfstg_incOW_wtd_excl1_brk, 
        paste0("Misc_output/", stg_nonOW_nam, "_Excl1"), "INT2S", 
        str_wrap(paste("-", stg_nonOW_nam, " relative pop. size w/ sev. climate 
                       stress exclusion for", 
                       nlayers(Lfstg_incOW_wtd_excl1_brk), "dates"), 
                 width = 80))
      SaveRaster2(Lfstg_incOW_wtd_excl2_brk, 
        paste0("Misc_output/", stg_nonOW_nam, "_Excl2"), "INT2S",
        str_wrap(paste("-", stg_nonOW_nam, "relative pop. size w/ sev. and mod. 
                       climate stress exclusion for", 
                       nlayers(Lfstg_incOW_wtd_excl2_brk), "dates"), 
                 width = 80))
      
      # Create and save summary maps of results
      Lfstg_incOW_Excl1_plots <- mclapply(1:nlayers(Lfstg_incOW_wtd_excl1_brk), 
        function(lyr) {
          lyr_name <- paste0(dats2[[lyr]])
          PlotMap(Lfstg_incOW_wtd_excl1_brk[[lyr]], 
                  lyr_name, paste(stg_nonOW_nam, 
                  "relative pop. size w/ climate stress exclusion", sep = " "), 
                  "Relative pop. size", 
                  paste0("Misc_output/", stg_nonOW_nam, "_Excl1"))
      }, mc.cores = 3)
      
      Lfstg_incOW_Excl2_plots <- mclapply(1:nlayers(Lfstg_incOW_wtd_excl2_brk), 
        function(lyr) {
          lyr_name <- paste0(dats2[[lyr]])
          PlotMap(Lfstg_incOW_wtd_excl2_brk[[lyr]], lyr_name, 
                  paste("All", tolower(stg_nonOW_nam), 
                  "relative pop. size w/ climate stress exclusion", sep = " "), 
                  "Relative pop. size", paste0("Misc_output/", stg_nonOW_nam, 
                                               "_Excl2"))
      }, mc.cores = 3)
      
    }
  }
}

stopCluster(cl)

# Remove processed Lifestage bricks to free up memory, and delete 
# Lifestage cohort rasters now that they have been processed
rm(list = ls(pattern = "Lfstg_wtd|Lfstg_incOW_wtd"))
unlink(list.files(pattern = glob2rx(paste0("*Lifestage*cohort*"))))

#### * NumGen raster processing ####
NumGen_fls <- list.files(pattern = glob2rx("*NumGen_*.tif$"))

if (exclusions_stressunits) {
  NumGenExcl1_fls <- list.files(pattern = glob2rx("*NumGenExcl1*.tif$"))
  NumGenExcl2_fls <- list.files(pattern = glob2rx("*NumGenExcl2*.tif$"))
}

# Calculate the highest generation to occur across all NumGen bricks, 
# so can weight data from each
maxgens <- max(unlist(lapply(NumGen_fls, function(x) { maxValue(brick(x)) })))

# Log file messages
if (exclusions_stressunits) {
  cat("\n\n", str_wrap("Done with weighted raster outputs and summary maps for 
                       Lifestage, Lifestage_Excl1, and Lifestage_Excl2", 
                       width = 80), "\n\n", 
      str_wrap(paste("### WEIGHTED RASTER OUTPUT: NUMGEN WITH CLIMATE STRESS 
                     EXCL. FOR", maxgens, "GENS ###"), width = 80), sep = "", 
      file = Model_rlogging, append = TRUE) 
  cat("\n", str_wrap("Done with weighted raster outputs and summary maps for 
                       Lifestage, Lifestage_Excl1, and Lifestage_Excl2", 
                       width = 80), "\n", sep = "")
  cat("\nWEIGHTED RASTER OUTPUT: NUMGEN WITH CLIMATE STRESS EXCL. FOR ", 
       maxgens, " GENS\n", sep = "")
} else {
  cat("\n\n", str_wrap("Done with weighted raster outputs and summary maps for 
                       Lifestage", width = 80), "\n\n", 
      "### WEIGHTED RASTER OUTPUT: NUMGEN FOR ", maxgens, 
      " GENERATIONS ###", sep = "", file = Model_rlogging, append = TRUE) 
  cat("\n", str_wrap("Done with weighted raster outputs and summary maps for 
                       Lifestage", width = 80), "\n\n", 
      "WEIGHTED RASTER OUTPUT: NUMGEN FOR ", maxgens, 
      " GENERATIONS\n", sep = "")
}

# Weight the bricks for NumGen, exporting each generation as a single brick file

RegCluster(ncohort)

NumGen_wtd_byGen <- foreach(gen = as.list(0:maxgens), .packages = pkgs, 
                            .inorder = TRUE) %dopar% {
#for (gen in as.list(1:maxgens)) {
  
  # Weight the raster bricks - i.e. calc. the relative size of pop. in each gen
  NumGen_wtd <- mapply(function(x, y) { 
    calc((brick(x) == gen), fun = function(z) { 
      round(100 * z) * y 
    })
  }, x = NumGen_fls, y = relpopsize)
  
  # Sum values across all cohorts (raster brick layers)
  NumGen_wtd <- Reduce("+", NumGen_wtd)
  SaveRaster2(NumGen_wtd, paste("NumGen", gen, sep = "_"), "INT2U", 
              paste("- Gen.", gen, "for all", nlayers(NumGen_wtd), "dates"))
  
  # If exclusions_stressunits, import the All_Stress_Excl brick to replace 
  # weighted NumGen raster values in climatically unsuitable areas with 
  # -2 (Excl2) or -1 (Excl1)
  if (exclusions_stressunits) {
    NumGen_wtd_excl1 <- Rast_Subs_Excl1(NumGen_wtd)  
    NumGen_wtd_excl2 <- Rast_Subs_Excl2(NumGen_wtd)
    # Make a brick of results and save it
    NumGen_wtd_excl1_brk <- do.call(brick, NumGen_wtd_excl1)
    SaveRaster2(NumGen_wtd_excl1_brk, 
                paste("NumGenExcl1", gen, sep = "_"), "INT2S",
                str_wrap(paste("- Gen.", gen,
                "with severe climate stress excl. for all", 
                               nlayers(NumGen_wtd), "dates"), width = 80))
    NumGen_wtd_excl2_brk <- do.call(brick, NumGen_wtd_excl2)
    SaveRaster2(NumGen_wtd_excl2_brk, 
                paste("NumGenExcl2", gen, sep = "_"), "INT2S",
                str_wrap(paste("- Gen.", gen,
                               "with severe and moderate climate stress 
                               excl. for all", nlayers(NumGen_wtd), 
                               "dates"), width = 80))
  }
}

stopCluster(cl)

### * Create summary maps of NumGen results, weighted across cohorts

# Log file messages
if (exclusions_stressunits) {
  cat("\n\n", str_wrap("Done with weighted raster outputs for 
                       NumGen, NumGen_Excl1, and NumGen_Excl2", width = 80),
      sep = "", file = Model_rlogging, append = TRUE) 
  cat("\n\n### SUMMARY MAP OUTPUT: NUMGEN WITH CLIMATE STRESS EXCLUSIONS ###", 
      file = Model_rlogging, append = TRUE)
  cat("\n", str_wrap("Done with weighted raster outputs for NumGen, 
      NumGen_Excl1, and NumGen_Excl2", width = 80))
  cat("\n\nSUMMARY MAP OUTPUT: NUMGEN WITH CLIMATE STRESS EXCLUSIONS\n")
} else {
  cat("\n\nDone with weighted raster outputs for NumGen", 
      file = Model_rlogging, append = TRUE) 
  cat("\n\n### SUMMARY MAP OUTPUT: NUMGEN ###", 
      file = Model_rlogging, append = TRUE)
  cat("\nDone with weighted raster outputs for NumGen\n")
  cat("\nSUMMARY MAP OUTPUT: NUMGEN\n", sep = "")
}

if (odd_gen_map == 1) {
  cat("\n\nPlotting odd generations only", file = Model_rlogging, 
      append = TRUE) 
}

# Remove NumGen cohort files
unlink(list.files(pattern = glob2rx(paste0("*NumGen*cohort*"))))

# Create list of raster brick files for NumGen, NumGenExcl1, and 
# NumGenExcl2 (if specified)
NumGen_wtd_fls <- list.files(pattern = glob2rx("*NumGen*.tif$"))
NumGen_wtd_fls_nams <- NumGen_wtd_fls %>% gsub(".tif", "",.)
NumGen_wtd_fls <- as.list(NumGen_wtd_fls)
names(NumGen_wtd_fls) <- NumGen_wtd_fls_nams

# Name each layer of each raster brick by it's type 
# (NumGen, NumGenExcl1, or NumGenExcl2) and sampling date
NumGen_wtd_mrgd_brk <- brick() # blank brick to put named raster into

for (NumGen_wtd_fl in NumGen_wtd_fls) {
  NumGen_wtd_brk <- lapply(NumGen_wtd_fl, function(x) { 
    brk <- brick(x)
    # Replace period w/ underscore in brick name so all text sep. by a "_"
    names(brk) <- sub("\\.", "_", names(brk)) 
    type <- unique(str_split_fixed(names(brk), pattern = "_", 3)[,1])
    gen_no <- unique(str_split_fixed(names(brk), pattern = "_", 3)[,2])
    names(brk) <- paste(type, gen_no, dats2, sep = "_")
    return(brk)
  })
  NumGen_wtd_mrgd_brk <- addLayer(NumGen_wtd_mrgd_brk, NumGen_wtd_brk)
}

# Split the NumGen layers out by type (NumGen, NumGenExcl1, or NumGenExcl2)
NumGen_wtd_brk <- raster::subset(NumGen_wtd_mrgd_brk, 
                                 grep("NumGen_", names(NumGen_wtd_mrgd_brk)))
if (exclusions_stressunits) {
  NumGenExcl1_wtd_brk <- raster::subset(NumGen_wtd_mrgd_brk, 
                          grep("NumGenExcl1_", names(NumGen_wtd_mrgd_brk)))
  NumGenExcl2_wtd_brk <- raster::subset(NumGen_wtd_mrgd_brk, 
                          grep("NumGenExcl2_", names(NumGen_wtd_mrgd_brk)))
}

rm(NumGen_wtd_mrgd_brk)

# List of NumGen bricks to plot
NumGen_wtd_patrn <- glob2rx("*NumGen*wtd*brk*")
NumGen_wtd_plot_list <- mget(ls(pattern = glob2rx("*NumGen*wtd*brk*")))

# For each sampling date, the relative size of the popualtion in each 
# generation is calculated; these results are saved as raster bricks and 
# visualized in summary maps
RegCluster(ncohort)

# Make the plots
#for (brk in NumGen_wtd_plot_list) {
  NumGen_sum_maps <- foreach(brk = NumGen_wtd_plot_list, .packages = pkgs, 
                             .inorder = TRUE) %:%
    foreach(d = dats_list, .packages = pkgs, .inorder = TRUE) %dopar% {
 # for (d in dats_list) {  
    nam <- unique(str_split_fixed(names(brk), pattern = "_", 2)[,1])
    print(nam)
    print(d)
    dat_vec <- unname(unlist(d)) # change to an unnamed vector
    
    # Loop through date vector, creating a raster stack for each date
    for (dat in dat_vec) {
      stack_sub <- raster::subset(brk, grep(dat, names(brk)))
      # Convert each layer of raster stack to a datframe; add generation no.
      NumGen_lyrs_toPlot <- list()
      j <- 1
      for (lyr in 1:nlayers(stack_sub)) {
        df <- ConvDF(stack_sub[[lyr]])
        df$gen_num <- as.numeric(str_split_fixed(names(stack_sub[[lyr]]), 
                                      pattern = "_", 3)[,2])
        
        # Replace "Gen1" with "GenOW" if value is 0
        df$gen <- paste(as.character(df$gen_num),"gens.")
        # Don't include data if all values are >= 0 - don't want them to be 
        # in legend key
        if (any(df$value > 0)) {
          NumGen_lyrs_toPlot[[j]] <- df
        }
        j <- j + 1
      }
      
      # Format data if pops of NO gens are present (no value > 0) AND there are 
      # climate stress exclusions - i.e. the spp is excluded from entire area. 
      # Then take the last data frame in the list; the generation column will 
      # be replaced with simply "excl. severe" and/or "excl. moderate" in plot.
      # Don't want to show generation in legend (b/c all would be excluded); 
      # just show "excl. severe" and/or "excl. moderate"
      stack_sub_uniqueVals <- unique(
        c(as.matrix(stack_sub)))[!is.na(unique(c(as.matrix(stack_sub))))]
      
      if (all(stack_sub_uniqueVals < 0)) {
        mrgd2 <- df
        mrgd2 <- mutate(mrgd2, gen = ifelse(value == -2, "excl.-severe", 
                  ifelse(value == -1, "excl.-moderate", value)))
      }
      
      # Merge data frames in the list - if list is empty b/c all data frames 
      # had value = 0, then just use most recent data frame
      if (any(stack_sub_uniqueVals > -1) & length(NumGen_lyrs_toPlot) > 0) {
        mrgd <- do.call(rbind, NumGen_lyrs_toPlot)
      } else {
        mrgd <- df
      }
      
      # Create a layer for OW generation, using dataframe from latest 
      # generation, if present in data
      if (any(stack_sub_uniqueVals > -1) & any(mrgd$gen == "0 gens.")) {
        OW <- data.frame(mrgd %>% dplyr::filter(gen == "0 gens."))
      }
      
      # From merged data frame, remove duplicate cells, showing the most 
      # recent generation in cases of overlap
      if (any(stack_sub_uniqueVals > -1) & any(mrgd$value > 0)) {
        noZero <- data.frame(mrgd %>% group_by(x, y) %>% arrange(gen))
        noZero <- dplyr::filter(noZero, !value == 0)
        if (any(mrgd$gen == "0 gens.")) { 
          mrgd2 <- rbind(OW,noZero) 
        } else {
          mrgd2 <- noZero
        }
        
        # Format data if pops of any gen are present (value > 0) AND there are 
        # climate stress exclusions, but first check that there are any values 
        # in data >= 0 (if all values < 0, data are handled differently above)
        } else if (any(stack_sub_uniqueVals > 0) & any(mrgd$value < 0)) {
          if (any(mrgd$value < 0)) {
            excl_vals <- data.frame(mrgd %>% dplyr::filter(value != 0))
            #excl_vals$gen <- "GenOW"
            if (any(df$gen == "0 gens.")) {
              mrgd2 <- rbind(excl_vals,OW) 
            } else {
              mrgd2 <- excl_vals
            }
          } else {
            mrgd2 <- OW
          }
        }
      
      # Change Gen0 to GenOW for plotting purposes
      # mrgd2 <- mutate(mrgd2, gen = ifelse(gen == "Gen0", "GenOW", gen))
      
      # If specified, create summary maps for odd generations only - beginning 
      # for 1st gen (i.e., 1, 3, 5, ..)
      if (odd_gen_map == 1) { # Should odd gens be plotted instead of all gens?
        # Extract gen. number
        mrgd2$gen_val <- as.numeric(gsub("([0-9]+).*$", "\\1", mrgd2$gen))
        mrgd2$gen_val[is.na(mrgd2$gen_val)] <- 0
        mrgd2 <- mrgd2 %>% filter(gen_val %% 2 != 0) %>%
          dplyr::select(-gen_val)
      }
      
      # Plot results as long as there are data in "mrgd2" - this data frame
      # will be empty only if "odd_gen_map == 1," and there are no data for 
      # Gen1, Gen3, ... etc. (e.g., if there are only data for GenOW, which 
      # has been removed)
      if (nrow(mrgd2) > 0) {
        # Create and save summary maps
        if (nam == "NumGenExcl1") {
          PlotMap(mrgd2, dat, 
                  "Number of generations w/ climate stress exclusions",
                  "No. of\ngenerations", "NumGen_Excl1")
        } else if (nam == "NumGenExcl2") {
          PlotMap(mrgd2, dat, 
                  "Number of generations w/ climate stress exclusions",
                  "No. of\ngenerations", "NumGen_Excl2")
        } else if (nam == "NumGen") {
          PlotMap(mrgd2, dat, "Number of generations", 
                  "No. of\ngenerations", "NumGen")
        }
      } else {
        cat("\n\nWARNING: NumGen results for", nam, "on", dat, 
            "\nnot plotted - no odd generation data for this date", 
            file = Model_rlogging, append = TRUE)
      }
    }
  }
#}

stopCluster(cl)

# Log file messages
if (exclusions_stressunits) {
  cat("\n\n", str_wrap("Done with summary maps for NumGen, NumGen_Excl1, and 
                       NumGen_Excl2", width = 80), sep = "", 
      file = Model_rlogging, append = TRUE) 
  cat("\n\n", str_wrap("### ANALYSIS: LIFESTAGE W/ NO. OF GENS. AND CLIMATE 
                       STRESS EXCLUSIONS ###\n", 
                       width = 80), sep = "", 
      file = Model_rlogging, append = TRUE)
  cat("\nDone with summary maps for NumGen, NumGen_Excl1, and NumGen_Excl2\n\n", 
      str_wrap("ANALYSIS: LIFESTAGE W/ NO. OF GENS. AND CLIMATE STRESS 
               EXCLUSIONS\n\n", width = 80), sep = "")
} else {
  cat("\n\nDone with summary maps for NumGen", 
      file = Model_rlogging, append = TRUE) 
  cat("\n\n### ANALYSIS: LIFESTAGE WITH NUMBER OF GENS. ###\n", sep = "", 
      file = Model_rlogging, append = TRUE)
  cat("\nDone with summary maps for NumGen\n\n", str_wrap("ANALYSIS: LIFESTAGE 
      WITH NUMBER OF GENS.\n\n"), sep = "")
}

#### * Lifestage by generation analysis ####

# NumGen layers may overlap at edges due to cohorts
# Want to show later generations in case of overlap, or maps will show adults 
# that are assigned to the wrong generation. First make a vector of the various 
# generation combinations to deal w/ this overlap issue.
# TO DO: find a more efficient way to overaly generation combos w/o several 
# lines of code here
gens <- as.numeric(unique(substr(names(NumGen_wtd_brk), start = 8, stop = 8)))
gens_noOW <- gens[gens != 0] # remove OW generation

# If more than 2 generations, split even and odd gens, make combos, and then 
# paste together
if (length(gens_noOW) > 2) {
  # Take every other gen no., starting with element 1 (odd), and again with 
  # element 2 (even)
  gens_even <- gens_noOW[gens_noOW %% 2 == 0] # if divisible by 2, is even 
  gens_even <- paste("NumGen", gens_even, sep = "_")
  gens_odd <- gens_noOW[gens_noOW %% 2 != 0] # if not divisible by 2, is odd
  gens_odd <- paste("NumGen", gens_odd, sep = "_")
  gens_odd2 <- gens_odd[2:length(gens_odd)]
  # First combo - consecutive numbers starting at NumGen1 
  # (e.g. NumGen_2|NumGen_1, NumGen_4|NumGen_3...)
  # If uneven no. of gens, need to trim odd and even to same length
  if (length(gens_odd) > length(gens_even)) {
    gens_combo1 <- paste(gens_even, gens_odd[1:length(gens_even)], sep = "|")
  } else {
    gens_combo1 <- paste(gens_even, gens_odd, sep = "|")
  }
  # Second combo - consecutive numbers starting at NumGen2 
  # (e.g., NumGen_3|NumGen_2, NumGen_5|NumGen_4)
  gens_combo2 <- paste(gens_odd2, gens_even[1:length(gens_odd2)], sep = "|")
  # Combine the two combo vectors and sort, then add on OW gen
  gens_combo_all <- sort(c(gens_combo1, gens_combo2)) 
  gens_combo_all <- append("NumGen_1|NumGen_0", gens_combo_all)
  # If only two generations, then just make this combo
  } else if (length(gens_noOW) == 2) {
    gens_combo_all <- c("NumGen_1|NumGen_0", "NumGen_2|NumGen_1")
  # If only one generation, then make this combo
  } else if (length(gens_noOW) == 1) {
    gens_combo_all <- c("NumGen_1|NumGen_0")
}

# For each combination, overlay the combos for each date and replace older 
# generation values (e.g., for combo "NumGen_2|NumGen_1" - NumGen_2 will replace 
# NumGen1 where there is overlap)
cat("\n", str_wrap("Replacing older generation vals with newer gen. 
                   vals in areas of overlap", width = 80), "\n", sep = "",
    file = Model_rlogging, append = TRUE)

corrected_brick_list <- list()

if (length(gens) > 1) {
  RegCluster(4)
  corrected_NumGen <- foreach(d = 1:length(dats2), .packages = pkgs, 
                              .inorder = FALSE) %dopar% {
    #for (d in 1:length(dats2)) { 
    for (i in 1:length(gens_combo_all)) {
      sub1 <- raster::subset(NumGen_wtd_brk, grep(dats2[d], 
                            names(NumGen_wtd_brk))) # subet raster stack by date
      # Replace all non-zero values w/ 1, and 0 values with NA
      sub1[sub1 > 0] <- 1 
      sub1[sub1 == 0] <- NA
      # Get each gen in the combo
      genA <- str_split_fixed(gens_combo_all[i], pattern = "\\|", 2)[,1]
      genB <- str_split_fixed(gens_combo_all[i], pattern = "\\|", 2)[,2]
      # Search for each of those gens in the stack and combine them
      # The order matters here, genA must be first layer
      sub1A <- raster::subset(sub1, grep(paste0(genA, "_"), names(sub1)))
      sub1B <- raster::subset(sub1, grep(paste0(genB, "_"), names(sub1)))
      sub2 <- stack(sub1A, sub1B)
      # Identify areas where the two gens overlap by summing them
      # Areas of overlap = 2
      gen_ovlp <- stackApply(sub2, indices = c(1), fun = sum) 
      # Crop the layer of the later generation if it overlaps w/ earlier one 
      # (but if values are all NA, just keep it)
      if (!is.na(any(values(sub2[[2]] > 0)))) {
        gen_ovlp_NA <- overlay(sub2[[1]], sub2[[2]], gen_ovlp, 
          fun = function(x, y,z) {
            x[z == 2] <- NA
            return(x) # Returns cropped layer of later generation
          })
        names(gen_ovlp_NA) <- names(sub2[[1]])
      } else {
         gen_ovlp_NA <- sub2[[1]]
      }
      # Need to add the very first layer back in (should be NumGen0)
      if (i == length(gens_combo_all)) {
        gen0 <- sub1[[1]]
        gen_ovlp_NA <- stack(gen0, gen_ovlp_NA)
      }
      # "Corrected" combo is put back into a list
      corrected_brick_list[i] <- gen_ovlp_NA 
      }
      corrected_brick <- do.call(stack, corrected_brick_list) 
      }
    stopCluster(cl)
}
      
# Combine all rasters in the list and order them by order them by generation 
# and date
if (length(gens) > 1) {
  NumGen_wtd_stk2 <- do.call(stack, corrected_NumGen)
} else {
  NumGen_wtd_stk2 <- NumGen_wtd_stk
}
      
ord <- c(sort(names(NumGen_wtd_stk2))) # List of ordered layer names
mtch <- as.integer()
for (j in 1:length(ord)) {
  # Match layer number to ordered names so can rerrange stack
  mtch[j] <- match(ord[j], names(NumGen_wtd_stk2)) 
  mtch <- unname(mtch)
}
      
# Rearrange the stack
NumGen_wtd_stk3 <- stack()
for (m in mtch) {
    NumGen_wtd_stk3 <- addLayer(NumGen_wtd_stk3, NumGen_wtd_stk2[[m]])
}

# Now use resulting NumGen stack layers to mask out Lifestage (adult) layers
if (exclusions_stressunits) {
  cat("\n", str_wrap("Assigning a generation number to pixels in the Adult, 
                   Adult_Excl1, and Adult_Excl2 raster bricks", width = 80), 
      sep = "", file = Model_rlogging, append = TRUE)
} else {
  cat("\n", str_wrap("Assigning a generation number to pixels in the Adult 
                     raster brick", width = 80), sep = "", 
      file = Model_rlogging, append = TRUE)
}

# Get the weighted adult ("Adult") raster brick. Note that if the species 
# overwinters as that brick will have "Adult" raster brick will be the rel.
# pop. size of both "OWadult" and "Adult"
Adults_wtd <- brick("Misc_output/Adult.tif")
if (exclusions_stressunits) {
  AdultsExcl1_wtd <- brick("Misc_output/Adult_Excl1.tif")
  AdultsExcl2_wtd <- brick("Misc_output/Adult_Excl2.tif")
}

# Subet NumGen raster stack by generation; recode vals <34(?), and mask out 
# adult weighted raster w/ by gen
Adults_wtd_sublist <- list()
AdultsExcl1_wtd_sublist <- list()
AdultsExcl2_wtd_sublist <- list()

if (odd_gen_map == 1) {
  gens2 <- gens[seq(1, length(gens), 2)]
}

j <- 1
#foreach(gen = gens, .packages = pkgs, .inorder = FALSE) %dopar% {
for (gen in gens) {
  #print(j)
  NumGen_wtd_sub <- raster::subset(NumGen_wtd_stk3, 
                      grep(paste0("NumGen_", gen), names(NumGen_wtd_stk3)))
  NumGen_msk <- NumGen_wtd_sub
  # For Adults (no clim. stress exclusions)
  # Mask out areas in adults raster that do not belong to the gen. of interest
  Adults_wtd_sub <- overlay(Adults_wtd, NumGen_msk, fun = function(x, y) {
    x[is.na(y[])] <- NA
    return(x)
  })
  names(Adults_wtd_sub) <- paste("NumGen", gen, dats2, sep = "_")
  Adults_wtd_sublist[[j]] <- Adults_wtd_sub
  # Do the same for AdultsExcl1 and AdultsExcl2 if relevant
  if (exclusions_stressunits) {
    #AdultsExcl1
    AdultsExcl1_wtd_sub <- overlay(AdultsExcl1_wtd, NumGen_msk, 
      fun = function(x, y) {
        x[is.na(y[])] <- NA
        return(x)
      })
    names(AdultsExcl1_wtd_sub) <- paste("NumGen", gen, dats2, sep = "_")
    AdultsExcl1_wtd_sublist[[j]] <- AdultsExcl1_wtd_sub
    #AdultsExcl2
    AdultsExcl2_wtd_sub <- overlay(AdultsExcl2_wtd, NumGen_msk, 
      fun = function(x, y) {
        x[is.na(y[])] <- NA
        return(x)
    })
    names(AdultsExcl2_wtd_sub) <- paste("NumGen", gen, dats2, sep = "_")
    AdultsExcl2_wtd_sublist[[j]] <- AdultsExcl2_wtd_sub
  }
  j <- j + 1
}

# Make one big stack from list of results
NumGen_Adults_wtd <- do.call(stack, Adults_wtd_sublist)
if (exclusions_stressunits) {
  NumGen_AdultsExcl1_wtd <- do.call(stack, AdultsExcl1_wtd_sublist)
  NumGen_AdultsExcl2_wtd <- do.call(stack, AdultsExcl2_wtd_sublist)
}

# Subset raster stack by date, extract generation data from layer name, 
# convert to a data frame, and put it in a list
if (exclusions_stressunits) {
  AdultStg_byGen_list <- list(NumGen_Adults_wtd, NumGen_AdultsExcl1_wtd, 
                              NumGen_AdultsExcl2_wtd)
  names(AdultStg_byGen_list) <- c("Adults_brk", "AdultsExcl1_brk", 
                                  "AdultsExcl2_brk")
} else {
  AdultStg_byGen_list <- list(NumGen_Adults_wtd)
  names(AdultStg_byGen_list) <- c("Adults_brk")
}

#### * Life stage with no. of generations plots ####
if (exclusions_stressunits) {
  cat("\n\nDone with Lifestage with NumGen analysis\n\n", 
      str_wrap("### SUMMARY MAP OUTPUT: LIFESTAGE W/ NO. OF GENS AND CLIMATE 
               STRESS EXCL.", width = 80), " ###", sep = "",
      file = Model_rlogging, append = TRUE)
  cat("\n\nDone with Lifestage with NumGen analysis\n\n", 
      str_wrap("SUMMARY MAP OUTPUT: LIFESTAGE W/ NO. OF GENS AND CLIMATE STRESS 
               EXCL.\n", width = 80), "\n", sep = "")
} else {
  cat("\n\n### SUMMARY MAP OUTPUT: LIFESTAGE W/ NO. OF GENS. ###", sep = "",
      file = Model_rlogging, append = TRUE)
  cat("\n\nDone with Lifestage with NumGen analysis\n\n", 
      str_wrap("SUMMARY MAP OUTPUT: LIFESTAGE W/ NO. OF GENS.\n", width = 80), 
      "\n", sep = "")
}

if (odd_gen_map == 1) {
  cat("\n\nPlotting odd generations only", file = Model_rlogging, 
      append = TRUE) 
}

# Generate and save summary plots for "Lifestage by Generation" (currenlty only
# doing these for adults)
RegCluster(4)
AdultStg_byGen_sum_maps <- foreach(j = 1:length(AdultStg_byGen_list), 
  .packages = pkgs, .inorder = TRUE) %dopar% {
#for (j in 1:length(AdultStg_byGen_list)) {
  nam <- paste0(names(AdultStg_byGen_list[j]))
 # print(nam)
  for (d in dats2) {
  #  print(d)
    df_list <- list()
    stk_sub <- raster::subset(AdultStg_byGen_list[[j]], 
              grep(d, names(AdultStg_byGen_list[[j]]))) # subet by date

    # For each stack layer, identify the generation, convert data to a 
    # data frame and add it to a list
    for (i in 1:nlayers(stk_sub)) {
      r <- stk_sub[[i]]
      gen <- substr(names(r), start = 8, stop = 8)
      # Only convert to data frame if any values are not NA 
      # and either greater than 0, or less than 0 (which indicates climate 
      # stress exclusions)
      if (any(values(r) >= 0 & !is.na(values(r))) | 
          any(values(r) < 0 & !is.na(values(r)))) {
        lyr_df <- ConvDF(r) # convert raster to a data frame
        lyr_df$gen <- gen
        colnames(lyr_df)[1] <- "value"
        df_list[[i]] <- lyr_df # add to the list 
      }
    }
    
    # Merge the list into a single data frame - this has data from all 
    # generations for a given date. Remove generations for which all values 
    # are 0, otherwise will obscure data from other layers (other gens)
    # Not doing this will produce erroneous bands of adults at southern edge
    mrgd <- do.call(rbind, df_list) 

    # Optional: create summary maps for odd generations only - beginning for 
    # 1st gen (i.e., 1, 3, 5, ..)
    if (odd_gen_map == 1) { # should odd gens be plotted instead of all gens?
      mrgd <- mrgd %>% filter(as.numeric(gen) %% 2 != 0)
    }
    
    # Plot results as long as there are data in "mrgd2" - this data frame will 
    # be empty only if "odd_gen_map == 1" and there are no data for Gen1, Gen3, 
    # ... etc. (e.g., if there are only data for GenOW, which has been removed)
    # Currently "other stages" (not adults) are colored gray - may want to 
    # consider coloring them more similarly to which generation they belong to
    if (nrow(mrgd) > 0) {
      if (nam == "Adults_brk") {
        PlotMap(mrgd, d, "Adult relative pop. size for each gen.", 
                "Adult relative\npop. size (peak)", "Misc_output/Adult_byGen")
        } else if (nam == "AdultsExcl1_brk") {
          PlotMap(mrgd, d, 
          "Adult relative pop. size for each gen. w/ climate stress exclusion",
                  "Adult relative\npop. size (peak)", 
                  "Misc_output/Adult_Excl1_byGen")
        } else if (nam == "AdultsExcl2_brk") {
          PlotMap(mrgd, d, 
          "Adult relative pop. size for each gen. w/ climate stress exclusion",
                  "Adult relative\npop. size (peak)", 
                  "Misc_output/Adult_Excl2_byGen")
      }
    } else {
      cat("\n\nWARNING: Adult w/ NumGen results for", nam, "on", d, 
          "\nnot plotted - no odd generation data for this date", 
          file = Model_rlogging, append = TRUE)
    }
  }
}

stopCluster(cl)

cat("\n\nDone with Lifestage with NumGen summary maps\n", 
    file = Model_rlogging, append = TRUE)
cat("\n\nDone with Lifestage with NumGen summary maps\n\n")

#### * Analyses and map production all done - wrap-up ####
processing_exectime <- toc(quiet = TRUE)
processing_exectime <- (processing_exectime$toc - processing_exectime$tic) / 60 

cat("\n### Done w/ final analyses and map production ###\n", 
    "Run time for analyses and map production = ", 
    round(processing_exectime, digits = 2), " min\n", sep = "", 
    "Deleting, renaming, and moving some remaining files\n",
    file = Model_rlogging, append = TRUE)
cat("\nDone w/ final analyses and map production\n\n", 
    "Run time for analyses and mapping run time = ", 
    round(processing_exectime, digits = 2),
    " min\n\n", "Deleting, renaming, and moving some remaining files\n\n", 
    sep = "")

# Delete any remaining output files from daily loop now that all have been 
# processed
unlink(list.files(pattern = glob2rx(paste0("*_cohort.tif$"))))

# Rename files for last day of year
last_dat_fls <- list.files(pattern = glob2rx(paste0("*", last_date, "*.png$")))
new_names <- paste0(spp, "_", last_dat_fls)
invisible(file.rename(last_dat_fls, new_names))

cat("Renamed all files for last day (", last(dats2), ") to include ", spp, 
    " in file name\n", sep = "", file = Model_rlogging, append = TRUE)
cat("\nRenamed all files for last day (", last(dats2), ") to include ", spp, 
    " in file name\n", sep = "")

# Move all leftover files without "LBAM" in file name to "Misc_output" 
# (grep finds the inverse of pattern here)
misc_fls <- grep(list.files(path = output_dir), 
                 pattern = glob2rx(paste0("*", last_date, "*.png$")), 
                 invert = TRUE, value = TRUE)
invisible(file.copy(misc_fls, paste0(output_dir, "/Misc_output/")))
invisible(file.remove(misc_fls))

# Wrap up log file and report run time for entire model
cat("\nMODEL RUN DONE\n", file = Model_rlogging, append = TRUE)
cat("\nMODEL RUN DONE\n")
total_exectime <- toc(quiet = TRUE) # Execution time for entire run
total_exectime <- round((total_exectime$toc - total_exectime$tic) / 60, 
                        digits = 2)
cat("Run time for entire model =", total_exectime, "min", 
    file = Model_rlogging, append = TRUE)
cat("\nRun time for entire model =", total_exectime, "min\n\n")
