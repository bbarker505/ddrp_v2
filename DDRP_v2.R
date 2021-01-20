#!/usr/bin/Rscript
#.libPaths("/usr/lib64/R/library/")
# Log of recent edits
# 
# 11/10/20: Modified Adult by Gen code inc. summary map function because of 
#   errors in plotting FCM maps and also some clim. stress vals not correct.
#   Also removed "R.utils" package and just coded manually for backup folder.
# 10/27/20: Added code to backup old run files if a run with the same folder 
#   name is conducted, fixed bug related to sampling the current day, etc.
# 7/24/20: Replaced "mclapply" function with a foreach loop because mclapply
#  cannot be used on Windows system; improved method for registering cluster so
#  that it is based on number of cores in the computer or server being used.
# 7/1/20: Added "StageCount" raster and map outputs, added map outputs for 
# current day, improved map legends, added more input param checks 
#   TO DO: "odd_map_gen" param doesn't work well at small scales becuase 
#   certain gens may be missing. Maybe address in future versions of DDRP.  
# On 4/24/20: Changed the parameter name "chill stress" to "cold stress"
# 3/30/20: fixed typo/missing object in log file text for Adult by Gen
# 2/17/20: major overhaul of NumGen weighting and Adult by Lifestage analyses. 
# Experimenting showed that saving large raster bricks to disk may slightly
# improve run times (compared to holding them in memory). Also removed FORK
# option in RegCluster because was causing crashes on Hopper.
# 2/7/20: modified code for Adult by Gen summary maps so that temp rasters
# are saved to file instead of being held in memory; was resulting in "can not
# open connection errors" and subsequent run crashes
# 1/28/20: detect CRS from template data, not hard-code it
# 1/23/20: finished fixing bugs w/ closing connections for clusters, and
# resolving issues with memory
# 1/17/20: inc. no. of gens for mapping; increase memory w/ FORK etc.
# 1/3/20: fixed bug that was deleting output raster files
# 12/16/19: renamed file names and uploaded to GitHub
# 11/26/19: fixed bug w/ loading 30 yr climate normals

# DDRP v2
options(echo = FALSE)

# Load the required packages
pkgs <- c("doParallel", "dplyr", "foreach", "ggplot2", "ggthemes", 
          "lubridate", "mapdata", "mgsub", "optparse", "parallel",
          "purrr", "RColorBrewer", "rgdal", "raster", "readr", "sp", 
          "stringr", "tidyr", "tictoc", "tools", "toOrdinal")
ld_pkgs <- lapply(pkgs, library, 
                  lib.loc = "/usr/lib64/R/library/", character.only = TRUE)

# Load collection of functions for this model
source("/usr/local/dds/DDRP_B1/DDRP_v2_funcs.R")

# Bring in states feature for summary maps (PNG files)
# Requires these libraries: "mapdata" and "maptools"
cat("\n\nDownloading US states feature\n")
states <- map_data("state")

# Start timing the model run
tic("Total run time")

################################################################################
########                  Header Documentation Section             #############
#  Cohorts DDRP: Degree-Day, establishment Risk, and Pest event mapping system #
########  By Brittany Barker, Len Coop, Gericke Cook, Dan Upper, and ###########
########  Tyson Wepprich for APHIS PPQ and IPM needs ###########################
################################################################################

# DDRP v2 is an update from the previous version of DDRP 
# (v24proDDRP_B1.R = DDRP_v1) that includes:
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
              default = NA, help = "study region: CONUS, EAST, WEST, or 
              state (2-letter abbr.)"),
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
  spp           <- "TABS" # Default species to use
  forecast_data <- "PRISM" # Forecast data to use (PRISM or NMME)
  start_year    <- "2020" # Year to use
  start_doy     <- 1 # Start day of year          
  end_doy       <- 365 # End day of year - need 365 if voltinism map 
  keep_leap     <- 1 # Should leap day be kept?
  region_param  <- "ND" # Region [CONUS, EAST, WEST, or state (2-letter abbr.)]
  exclusions_stressunits    <- 1 # Turn on/off climate stress unit exclusions
  pems          <- 0 # Turn on/off pest event maps
  mapA          <- 1 # Make maps for adult stage
  mapE          <- 0 # Make maps for egg stage
  mapL          <- 0 # Make maps for larval stage
  mapP          <- 0 # Make maps for pupal stage
  out_dir       <- "TABS_test" # Output dir
  out_option    <- 1 # Sampling frequency
  ncohort       <- 1 # Number of cohorts to approximate end of OW stage
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

#output_dir <- paste0("/home/httpd/html/CAPS/", spp, "_cohorts")
output_dir <- paste0("/usr/local/dds/DDRP_B1/DDRP_results/", out_dir)

# If the directory already exists, then a backup directory will be created that
# contains the old run files. Old backup directories will be removed if present.
if (file.exists(output_dir)) {
  
  # Create a directory for the previous run to backup old run files to and 
  # delete old backup (if present)
  backup_dir <- paste0(output_dir, "/previous_run")
  unlink(backup_dir, recursive = TRUE)
  dir.create(backup_dir)
  
  # Make list of old directories from most recent run to be backed up
  old_subdirs <- list.dirs(path = output_dir, recursive = FALSE) 
  old_subdirs <- old_subdirs[!grepl("previous_run", old_subdirs)]
  
  if (any(grepl("previous_run", old_subdirs))) {
    pos <- which(grepl("previous_run", old_subdirs))
    unlink(paste0(old_subdirs[pos], "/"), recursive = TRUE)
    old_subdirs <- old_subdirs[-pos] # Remove backup dir from subdir list
  }
  
  # Copy files from main dir and then subdirs to backup directory
  file.copy(list.files(output_dir, full.names = TRUE), backup_dir) # main dir
  for (i in 1:length(old_subdirs)) {
    subdir <-  old_subdirs[i] %>%
      str_split(pattern = "/") %>%
      unlist %>%
      last()
    subdir_copy <- paste(backup_dir, subdir, sep = "/")
    dir.create(subdir_copy)
    file.copy(list.files(old_subdirs[i], full.names = TRUE), subdir_copy)
    unlink(paste0(old_subdirs[i], "/"), recursive = TRUE)
  }
  
  # Remove old files now that they have been copied to backup folder
  unlink(paste0(output_dir, "/*"))  
  cat("\n", str_wrap(paste0("EXISTING OUTPUT DIR: ", output_dir, ";\n", 
                            "copied old run files to", backup_dir, "\n"), 
                     width = 80), sep = "") 
  
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
    "### Log file for DDRP v2 ###\n", 
    paste0(rep("#", 36), collapse = ""), "\n\n", sep = "", 
    file = Model_rlogging)

# Record PRISM and output dir
cat("BASE DIR: ", base_dir, "\n", file = Model_rlogging, append = TRUE)
cat("WORKING DIR: ", prism_dir, "\n", file = Model_rlogging, append = TRUE)
cat(str_wrap(paste0("EXISTING OUTPUT DIR: ", output_dir, 
    "; removing all files"), width = 80), "\n\n", sep = "", 
    file = Model_rlogging, append = TRUE)

# Push out a message file with all R error messages
msg <- file(paste0(output_dir, "/Logs_metadata/rmessages.txt"), open = "wt")
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
  cat("Species params: ", species_params, "\n", file = Model_rlogging, 
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
  start_year <- "30yr" # This needs to be changed depending on folder name
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

# Check for appropriate command line parameters
# Exit program if no pest event maps have been specified but users want pest
# event maps (pems = 1)
if (pems == 1 & !(1 %in% c(mapA, mapE, mapL, mapP))) {
  cat("\n", str_wrap("No pest event maps (mapA, mapE, mapL, mapP) specified; 
                     exiting program", width = 80), sep = "", 
      file = Model_rlogging, append = TRUE)
  cat("\n", str_wrap("No pest event maps (mapA, mapE, mapL, mapP) specified; 
          exiting program", width = 80), sep = "")
  q()
}

# Exit program if an incorrect sampling frequency has been specified
if (out_option %in% !c(1, 2, 3, 4, 5, 6)) {
  cat("Out_option =", out_option, "is unacceptable; exiting program\n", 
      file = Model_rlogging, append = TRUE)
  cat("Out_option =", out_option, "is unacceptable; exiting program\n")
  q() 
}

# Exit program if ncohort is < 1 (must be at least 1)
if (ncohort < 1) {
  cat("\n", str_wrap("Number of cohorts must be greater than or equal to 1; 
                     exiting program", width = 80), sep = "", 
      file = Model_rlogging, append = TRUE)
  cat("\n", str_wrap("Number of cohorts must be greater than or equal to 1; 
                     exiting program", width = 80), sep = "")
  q()
}

# Exit if end day of year is inappropriate
if (end_doy > 366) {
  cat("\n", str_wrap(paste("End day of year (end_doy) of", end_doy, "is 
                     unacceptable; exiting program"), width = 80), sep = "", 
      file = Model_rlogging, append = TRUE)
  cat("\n", str_wrap(paste("End day of year (end_doy) of", end_doy, "is 
                     unacceptable; exiting program"), width = 80), sep = "")
  q()
}

# Create a list of days to use for daily loop
sublist <- start_doy:end_doy

#### * Format threshold and DD params for Daily Loop ####

# Need to match length and order of stgorder, which is species specific

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
cat("### Metadata for DDRP v2 ###\n", file = metadata)

# Document run date and time
cat("\nRun date and time:", strftime(Sys.time(), format = "%m/%d/%Y %H:%M"),
    file = metadata, append = TRUE)

# Document species information and method used to calculate degree-days
cat("\n\n### Model Species Parameters ###\n Species Abbrev:", spp, 
    "\n Full Name:", fullname, 
    "\n Pest of:", pestof,
    "\n Overwintering Stage:", owstage, 
    "\n Degree-day calculation method:", calctype, 
    file = metadata, append = TRUE)

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
    "\n Adult DDs:", adultDD, 
    file = metadata, append = TRUE)

# Document climate stress exclusion parameter values, if applicable
if (exclusions_stressunits) {
  cat("\n \n Climate stress parameters",
      "\n Lower Cold Threshold:", coldstress_threshold, 
      "\n Upper Heat Threshold:", heatstress_threshold,
      "\n Max Cold Units (lower bound):", coldstress_units_max1, 
      "\n Max Cold Units (upper bound):", coldstress_units_max2,
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

cat("\n\n### Durations (in degree-days) of stages in each of", 
    ncohort, "cohorts ###\n", file = metadata, append = TRUE) 
suppressWarnings(write.table(stage_dd.print, file = metadata, 
                             row.names = FALSE, 
                             col.names = colnames(stage_dd.print), 
                             append = TRUE))

cat("\nDurations (degree-days) of stages in each of", ncohort, "cohorts:\n\n") 
print(stage_dd.print, row.names = FALSE)

cat("Done writing metadata file\n\n", forecast_data, " DATA PROCESSING\n", 
    sep = "", file = Model_rlogging, append = TRUE)
cat("\nDone writing metadata file\n\n", forecast_data, " DATA PROCESSING\n",
    sep = "")

# (5). WEATHER DATA LOADING AND PROCESSING -----

# Weather inputs and outputs - PRISM climate data w/subdirs 4-digit year
# New feature - choose whether to use PRISM or NMME for weather forecasts 
# (forecast_data = PRISM, or forecast_data = NMME)
tminfiles <- list.files(path = prism_dir, 
                        pattern = glob2rx(paste0("*PRISM_tmin_*", 
                                                 start_year, "*.bil$*")), 
                        all.files = FALSE, full.names = TRUE, recursive = TRUE)
if (length(tminfiles) == 0) {
  cat("Could not find tmin files - exiting program\n", 
      file = Model_rlogging, append = TRUE) 
  cat("Could not find tmin files - exiting program\n") 
  q()
}

tminfiles <- ExtractBestPRISM(tminfiles, forecast_data, 
                              keep_leap)[start_doy:end_doy]

tmaxfiles <- list.files(path = prism_dir, 
                        pattern = glob2rx(paste0("*PRISM_tmax_*",
                                                 start_year, "*.bil$*")), 
                        all.files = FALSE, full.names = TRUE, recursive = TRUE)

if (length(tmaxfiles) == 0) {
  cat("Could not find tmax files - exiting program\n", 
      file = Model_rlogging, append = TRUE) 
  cat("Could not find tmax files - exiting program\n") 
  q()
}

tmaxfiles <- ExtractBestPRISM(tmaxfiles, forecast_data, 
                              keep_leap) [start_doy:end_doy]

## Extract date from temperature files using regex pattern matching
dats <- unique(regmatches(tminfiles, regexpr(pattern = "[0-9]{8}", 
                                             text = tminfiles)))

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
}

# Make vector of dates to use when processing results 
# The current date will be sampled if it's the current year AND if 
# the current day falls within the range of start_doy and end_doy.
# The last date of year will always be sampled.
# Using "unique" will only keep date if it doesn't already occur in vector
# This happens if the end day of year is a multiple of the sampling frequency 
# (e.g. 1 to 300, w/ a 30 day sampling frequency), or if the current date falls
# within the sampling frequency
today_dat <- strftime(Sys.time(), format = "%Y%m%d")
current_year <- strftime(Sys.time(), format = "%Y")

if (start_year == current_year & 
    yday(Sys.time()) >= start_doy &
    yday(Sys.time()) <= end_doy) {
  dats2 <- sort(as.numeric(unique(c(dats[seq(0, length(dats), sample_freq)], 
                  today_dat, last(dats)))))
} else {
   dats2 <- sort(as.numeric(unique(c(dats[seq(0, length(dats), sample_freq)],
                                      last(dats)))))
}

dats2 <- as.character(dats2) # Need to be in character format for plotting
num_dats <- length(dats2) # How many sampled dates? 

# Create vector of days in the sublist that will be sampled (rasters are saved 
# for those days) in the Daily Loop, and also tack on the last day in the list. 
sample_pts <- c(sublist[seq(0, length(sublist), sample_freq)],
                last(sublist))

# Add the present day if DDRP run is being run for the current year AND if 
# the current day falls within the range of start_doy and end_doy. 
if (start_year == current_year & 
    yday(Sys.time()) >= start_doy &
    yday(Sys.time()) <= end_doy) {
  today_doy <- strftime(Sys.time(), format = "%j") # Day of year
  sample_pts <- sort(as.numeric(unique(c(sample_pts, today_doy))))
}

# Keep only unique sampling points (there may be duplicates for example
# if the present day is already in the list).
sample_pts <- unique(sample_pts)

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

#### * If CONUS or EAST, split template into tiles (and run in parallel)
# Benefit of tiles is lost for smaller regions, so these are not split
# SpaDES.tools requires the 'sf' package, which requires a newer version of GDAL
# .inorder must be set to TRUE so that output files are in correct order!

# Register DoParallel
# The "RegCluster" function creates a set of copies of R running in parallel and 
# communicating over sockets (parallel socket clusters). The value may be 
# specified manually; however, here the value is estimated based on the number 
# of available cores on the computer or server DDRP is being run on. 
# Specifying too many clusters will overload the computer.
ncores <- detectCores()

# The "RegCluster" function determines an appropriate # of cores depending on 
# the "region_param" and "ncohort" parameters, so the server doesn't become
# overloaded
RegCluster(round(ncores/4))

if (region_param %in% c("CONUS", "EAST")) {
  # Split template (2 pieces per side)
  tile_list <- SplitRas(template, ppside = 2, save = FALSE, plot = FALSE) 
  tile_n <- 1:length(tile_list) # How many tiles?
  cat("Splitting template into", length(tile_list), "tiles\n", 
      file = Model_rlogging, append = TRUE)
  
  # Name the 4 tiles (tile1, tile2, tile3, tile4)
  template <- mapply(function(n, t) {
    names(n) <- paste0("tile", t)
    return(n)
  }, n = tile_list, t = tile_n )
  
  rm(tile_list)
  
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
  
}

stopCluster(cl)
rm(cl)

cat("Done processing ", forecast_data, " data\n\nDAILY LOOP\n", sep = "",
    file = Model_rlogging, append = TRUE)
cat("\nDone processing ", forecast_data, " data\n\nDAILY LOOP\n", sep = "")

## (6). RUN THE DAILY LOOP -----
# First set up number of cores to use
# IMPORTANT: using too many cores will result in low memory, killing the daily 
# loop part-way through.

# For mclapply: Can not use >1 core on Windows, so detect OS
# TO DO: Resolve "foreach" issue with Daily Loop for CONUS so don't need this
if (grepl("Windows", Sys.info()[1])) {
  mc.cores <- 1
} else {
  mc.cores <- 4 # use 4 here, because there are 4 tiles being run in parallel
}

# Split cohorts into smaller chunks for CONUS and EAST to avoid overloading 
# memory when running in parallel.
if (region_param %in% c("CONUS", "EAST")) {
  cohort_chunks <- split(1:ncohort, ceiling(1:length(1:ncohort)/2)) 
} else {
  cohort_chunks <- split(1:ncohort, ceiling(1:length(1:ncohort)/7))
}

tic("Daily loop run time") # Start timing the daily loop run-time
# cat("DAILY LOOP: daily loop log files show loop progress and 
# output file info\n", file = Model_rlogging, append = TRUE)
cat("Sampling every", sample_freq, "days between", first(dats), "and", 
    last(dats), "\n", file = Model_rlogging, append = TRUE) 
cat("\nSampling every", sample_freq, "days between", first(dats), "and", 
    last(dats), "\n") 

# Run it! If there is an error the program will stop

tryCatch(
  {
    
    RegCluster(round(ncores/4))
    
    # If the region is CONUS or EAST, then both cohorts and tiles will be run in
    # parallel. To avoid overloading the server, mc.cores = 4 (for the 4 tiles,
    # which keeps load at appropriate level. If a smaller 
    if (region_param %in% c("CONUS", "EAST")) {

      # Total number of nodes is mc.cores * 2 because use mclapply twice in loop
      for (c in cohort_chunks) {
        cat("Running daily loop for cohorts", as.character(c), "\n", 
            file = Model_rlogging, append = TRUE)
        cat("\nRunning daily loop for cohorts", as.character(c), "\n")
        cohort_vec <- unname(unlist(c)) # change to an unnamed vector
        # TO DO: Can not get "foreach" loop to run this analysis. This issue 
        # should be resolved if DDRP will be run on Windows, because Windows 
        # is incapable of forking with the "mclapply" function. 
        #foreach(cohort = cohort_vec, .packages = pkgs, .inorder = FALSE) %:% 
        #  foreach(tile_num = 1:length(template), .packages = pkgs, 
        #        .inorder = FALSE) %dopar% {
        mclapply(cohort_vec, function(cohort) {
          mclapply(1:length(template), function(tile_num) {
            tile <- template[[tile_num]]
            DailyLoop(cohort, tile_num, tile)
          }, mc.cores = mc.cores)
         }, mc.cores = mc.cores)  
        }
      #}
      
      stopCluster(cl)
      rm(cl)
      
    } else {

      # If the region is not CONUS or EAST, then we don't need to run function 
      # for multiple tiles.
      for (c in cohort_chunks) {
        cat("Running daily loop for cohorts", as.character(c), "\n", 
            file = Model_rlogging, append = TRUE)
        cat("\nRunning daily loop for cohorts", as.character(c), "\n")
        cohort_vec <- unname(unlist(c)) # change to an unnamed vector
        # Only need to run cohorts in parallel
        foreach(cohort = cohort_vec, .packages = pkgs, 
                .inorder = FALSE) %dopar% {
          DailyLoop(cohort, NA, template)
        }
        
        stopCluster(cl)
        rm(cl)
      }
    }
  },
  
  error = function(e) {
    cat("Error in Daily Loop\n", 
        file = Model_rlogging, append = TRUE) 
    cat("\nError in Daily Loop\n")
})

# Free up memory - delete unneeded large objects
rm(DailyLoop, template, tmaxfiles, tmax_list, tminfiles, tmin_list)

# Document daily loop execution time
loop_exectime <- toc(quiet = TRUE)
loop_exectime <- (loop_exectime$toc - loop_exectime$tic) / 60 

cat("Daily loop done (run time = ", round(loop_exectime, digits = 2), " min)",
    "\n\nFINAL ANALYSES AND MAP PRODUCTION\n", sep = "",
    file = Model_rlogging, append = TRUE) 
cat("\nDaily loop done (run time = ", round(loop_exectime, digits = 2), " min)",
    "\n\nFINAL ANALYSES AND MAP PRODUCTION\n", sep = "")

#(7). PROCESS DAILY LOOP RESULTS -----
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
  RegCluster(4)
  
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
        foreach(c = cohorts, .packages = pkgs, .inorder = TRUE) %dopar% {
          CombineMaps(brick_files, t, c)
          cat("Merged", t, "tiles for cohort", c, "\n",
              file = Model_rlogging, append = TRUE)
        }

      } 
    }
  }
  
  stopCluster(cl)
  rm(cl)
  
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
  
  # Remove "_all" from file names - not needed
  fls <- list.files(pattern = "*tif$")
  file.rename(from = fls, to = sub(pattern = "_all.tif", 
                                  replacement = ".tif", fls))
  
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
} else if (asp < 1.2 & asp >= 0.6) {
  base_size <- 7
  legend_units <- 1
} else if (asp < 0.6 & asp >= 0.45) {
  base_size <- 5.7
  legend_units <- 0.6
} else if (asp < 0.45) {
  base_size <- 5.2
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

# Remove "cohort1" from DDtotal and climate stress raster file names
fls <- list.files(pattern = glob2rx("*Stress_Excl|Stress_Units|DDtotal*tif$"))
fl_rename <- lapply(fls, function(fl) {
  file.rename(from = fl, to = sub(pattern = "_cohort1", replacement = "", fl))
})

# Split up the dates into chunks (no chunks have single dates - this results in
# warning messages. Splitting up the dates avoids overloading the server w/ 
# running too many dates in parallel.
dats_list <- split(dats2, ceiling(seq_along(dats2)/(length(dats2)/4)))

# For each date in a date chunk, plot and save summary maps for:
# degree-day accumulation, cold stress unit accumulation, cold stress 
# exclusion, heat stress unit accumulation, heat stress exclusion, and all 
# stress exclusion

RegCluster(round(ncores/3))

#for (dat in dats_list) {
dd_stress_results <- foreach(dat = dats_list, .packages = pkgs, 
                         .inorder = TRUE) %:%
  foreach(d = unname(unlist(dat)), .packages = pkgs, .inorder = TRUE) %dopar% {
  
  dat_vec <- unname(unlist(dat)) # change to an unnamed date vector
  #for (d in dat_vec) {
      # Get position (layer) of date in raster brick
      lyr <- which(dats2 == d)
      # Make the plots
      PlotMap(subset(brick("DDtotal.tif"), lyr), d, 
              "Degree day (DD) accumulation", "Degree Days", 
              "Misc_output/DDtotal")

      if (exclusions_stressunits) {
        
        # Cold stress unit accumulation
         PlotMap_stress(subset(brick("Cold_Stress_Units.tif"), lyr), d, 
                       coldstress_units_max1, coldstress_units_max2, 
                       "Cold stress units", "Cold Stress Units", 
                       "Misc_output/Cold_Stress_Units")

        # Cold stress exlusions (-1 = moderate; -2 = severe)
        PlotMap(subset(brick("Cold_Stress_Excl.tif"), lyr), d, 
                "Cold stress exclusion", "Exclusion status", 
                "Misc_output/Cold_Stress_Excl")

        # Heat unit accumulation
        PlotMap_stress(subset(brick("Heat_Stress_Units.tif"), lyr), d, 
                       heatstress_units_max1, heatstress_units_max2, 
                       "Heat stress units", "Heat Stress Units", 
                       "Misc_output/Heat_Stress_Units")

        # Heat stress exclusions (-1 = moderate; -2 = severe)
        PlotMap(subset(brick("Heat_Stress_Excl.tif"), lyr), d, 
                "Heat stress exclusion", "Exclusion status", 
                "Misc_output/Heat_Stress_Excl")

        # All stress exclusions (cold stress + heat stress exclusions)
        PlotMap(subset(brick("All_Stress_Excl.tif"), lyr), d, 
                "All stress exclusion", "Exclusion status", "All_Stress_Excl")

      }
      
}

stopCluster(cl)
rm(cl)

# Log messages
if (exclusions_stressunits) {
  cat("\n\n", str_wrap("Done with DDtotal, climate stress exclusions, and 
                       climate stress unit maps", width = 80), "\n", sep = "",
      file = Model_rlogging, append = TRUE)
  cat("\n", str_wrap("Done with DDtotal, climate stress exclusions, and climate 
                     stress unit maps", width = 80), "\n", sep = "")
  cat("\n### ", str_wrap("RASTER AND SUMMARY MAP OUTPUTS: STAGE COUNT W/ 
                        CLIM. STRESS EXCL. ###", width = 80), sep = "", 
      file = Model_rlogging, append = TRUE)
  cat("\nRASTER AND SUMMARY MAP OUTPUTS: STAGE COUNT W/ CLIM. STRESS EXCL.\n")
} else {
  cat("\n\nDone with DDtotal maps\n", file = Model_rlogging, append = TRUE) 
  cat("\n### RASTER AND SUMMARY MAP OUTPUTS: STAGE COUNT ###", 
      file = Model_rlogging, append = TRUE)
  cat("\nRASTER AND SUMMARY MAP OUTPUTS: STAGE COUNT\n")
}

#### * Stage count analysis ####

# This analysis produces gridded and summary map outputs that depicts where 
# each life stage of each generation occurs. Outputs are saved only for the 
# middle cohort, as most of the population will be in the middle, and depicting
# the results across all cohorts is too complicated. The maps will show all 
# stages of each generation that are present on the sampled day. The StageCount 
# raster is formatted as follows: the number before the decimal is the 
# generation, and the number after the decimal is the life stage, where 
# 1 = OW stage and 2-5 is the remaining stages in consecutive order 
# (e.g. 1 = OWlarvae, 2 = pupae, 3 = adult, 4 = egg, 5 = larvae)
middle_cohort <- ceiling(ncohort/2)
Lfstg <- brick(paste0("Lifestage_cohort", middle_cohort, ".tif"))/10
NumGen <- brick(paste0("NumGen_cohort", middle_cohort, ".tif"))
StageCt <- overlay(Lfstg, NumGen, fun = function(x, y){ x + y })
writeRaster(StageCt, file = "StageCount", format = "GTiff",  datatype = "FLT4S", 
            overwrite = TRUE)
rm(Lfstg, NumGen, StageCt) # Free up memory

# Create a data frame of life stages spelled out to be used for plotting
# Stage order is specific to the species, but also need a column for order
# that stages actually occur (life cycle steps)
stg_vals <- data.frame("stg_name" = stgorder, 
                       "stg_num" = as.character(c(1:5))) 
stg_vals <- stg_vals %>% 
  mutate(stg_name = gsub("O", "", stg_name),
         stg_name = mgsub(stg_name, c("E", "L", "P", "A"), 
                          c("eggs", "larvae", "pupae", "adults")),
         life_cycle = case_when((stg_name == "eggs") ~ 1, 
                                (stg_name == "larvae") ~ 2,
                                (stg_name == "pupae") ~ 3,
                                (stg_name == "adults") ~ 4)) 

# If climate stress exclusions are specified, then take results from above and 
# substitute values where the species is under severe stress only with 
# -1 (Excl1), and areas where the species is both moderate and severe stress 
# with -1 and -2, respectively (Excl2)
if (exclusions_stressunits) {
  # Tack on climate stress data onto stage values data frame
  stg_vals <- rbind(data.frame(stg_name = c("excl.-moderate", "excl.-severe"), 
                    life_cycle = c(-1, -2), stg_num = c(-1, -2)), stg_vals)
  StageCt_excl1 <- brick(Rast_Subs_Excl(brick("StageCount.tif"), "Excl1")) 
  StageCt_excl2 <- brick(Rast_Subs_Excl(brick("StageCount.tif"), "Excl2"))
  # Save raster brick results
  writeRaster(StageCt_excl1, file = "StageCount_Excl1", format = "GTiff",  
              datatype = "FLT4S", overwrite = TRUE)
  writeRaster(StageCt_excl2, file = "StageCount_Excl2", format = "GTiff",  
              datatype = "FLT4S", overwrite = TRUE)
  rm(StageCt_excl1, StageCt_excl2) # Free up memory
}

# Make list of Stage Count raster bricks for plotting
if (exclusions_stressunits) {
  StageCt_lst <- c("StageCount.tif", "StageCount_Excl1.tif", 
                   "StageCount_Excl2.tif")
} else {
  StageCt_lst <- c("StageCount.tif")
}

# Generate summary maps for Stage Count results
# TO DO: see about making this step faster. Increasing parallel processing
# here (e.g. for going through the StageCt_lst) overloads server too much.
RegCluster(round(ncores/3))

for (i in 1:length(StageCt_lst)) {
  fl <- StageCt_lst[i]
  #for (dat in dats_list) {
  foreach(dat = dats_list, .packages = pkgs, .inorder = TRUE) %:%
    foreach(d = unname(unlist(dat)), 
            .packages = pkgs, .inorder = TRUE) %dopar% {
    
    #dat_vec <- unname(unlist(dat)) # Change to an unnamed date vector
    
    #for (d in dat_vec) {
      
      # Get position (layer) of date in raster brick, subset layer from bricks,
      # convert this layer to a data frame, and mutate data to extract gen 
      # number and stage number. Then join to the stg_vals data frame to get
      # the stage name. These operations are done only if the data are not
      # climate stress exclusion values (i.e. are greater than 0). The stage
      # order (stg_order) column is for ordering stages correctly in map legend.
      lyr <- which(dats2 == d)
      
      StageCt_df <- ConvDF(brick(StageCt_lst[i])[[lyr]]) %>% 
        mutate(value_orig = value) %>% # Keep original value (for plotting)
        mutate(value = round(value, 1)) %>% # Round decimal (must do this)
        mutate(gen = as.numeric(ifelse(value > 0, 
                            str_split_fixed(value, "[.]", 2)[,1], value))) %>%
        mutate(stg_num = as.character(ifelse(value > 0, 
                              str_split_fixed(value, "[.]", 2)[,2], value))) %>%
        left_join(., stg_vals, by = "stg_num") %>%
        mutate(gen_stg = paste0(gen, ".", life_cycle)) # Correctly sort legend
          
      # Format data values for plotting - Gen 0 = OW gen and ordinal values for
      # other generations (1st, 2nd, 3rd, etc.). This step is pretty slow - 
      # see if it can be sped up in future version.
      OW_gen <- StageCt_df %>% 
        filter(gen == 0) %>%
        mutate(value = paste("OW gen.", stg_name))
      
      # Filter out climate stress values to tack on after formatting other data
      excl_df <- StageCt_df %>% 
        dplyr::filter(value < 0) %>%
        mutate(gen_stg = ifelse(value == -2, -2, -1))
          
      if (any(StageCt_df$gen > 0)) {
        StageCt_df2 <- StageCt_df %>% 
          filter(gen > 0) %>%
          mutate(value = 
                   map_chr(gen, function(x) paste(toOrdinal(x), "gen."))) %>% 
          unnest() %>% 
          mutate(value = paste(value, stg_name)) %>%
          rbind(OW_gen, .) 
      } else {
        StageCt_df2 <- OW_gen
      }
      
      # Add climate stress values back in
      if (exclusions_stressunits) {
        StageCt_df2 <- rbind(StageCt_df2, excl_df)
      }
      
      # Plot the results
      if (fl == "StageCount.tif") {
        PlotMap(StageCt_df2, d, "Generation and stage", "Gen. x stage", 
                "StageCount")
      } else if (fl == "StageCount_Excl1.tif") {
        PlotMap(StageCt_df2, d, 
                "Generation and stage w/ climate stress exclusion", 
                "Gen. x stage", "StageCount_Excl1")
      } else if (fl == "StageCount_Excl2.tif") {
        PlotMap(StageCt_df2, d, 
                "Generation and stage w/ climate stress exclusion", 
                "Gen. x stage", "StageCount_Excl2")
      }
  
    }
  }
#}

stopCluster(cl)
rm(cl)

rm(StageCt_lst)

# Log file messages
if (exclusions_stressunits) {
    cat("\n\n", str_wrap("Done with raster and summary map outputs for 
      StageCount, StageCount_Excl1, and StageCount_Excl2", width = 80), 
    file = Model_rlogging, sep = "", append = TRUE) 
  cat("\n", str_wrap("Done with raster and summary map outputs for StageCount,
      StageCount_Excl1, and StageCount_Excl2", width = 80), sep = "")
} else {
  cat("\n\nDone with raster and summary map outputs for StageCount\n", 
    file = Model_rlogging, append = TRUE)
  cat("\nDone with raster and summary map outputs for StageCount\n")
}

# If no PEMS, then moving on to Lifestage analyses 
if (!pems & !exclusions_stressunits) {
  cat("\n", str_wrap("### WEIGHTED RASTER AND SUMMARY MAP OUTPUTS: LIFESTAGE
                     ###", width = 80), "\n\nStages: ", 
      paste(stgorder, collapse = ", "), 
      sep = "", file = Model_rlogging, append = TRUE)
  cat("\nWEIGHTED RASTER AND SUMMARY MAP OUTPUTS: LIFESTAGE ###","\n\nStages: ",
      paste(stgorder, collapse = ", "), "\n")
  # If no PEMS but climate stress exclusions, then moving on to Lifestage 
  # analyses that also include climate stress exclusions
} else if (!pems & exclusions_stressunits) {
  cat("\n\n", str_wrap("### WEIGHTED RASTER AND SUMMARY MAP OUTPUTS:
      LIFESTAGE W/ CLIM. STRESS EXCL. ###", width = 80), "\n\nStages: ", 
      paste(stgorder, collapse = ", "), sep = "",
      file = Model_rlogging, append = TRUE)
  cat("\n\n", str_wrap("### WEIGHTED RASTER AND SUMMARY MAP OUTPUTS:
      LIFESTAGE W/ CLIM. STRESS EXCL. ###", width = 80), "\nStages: ", 
      paste(stgorder, collapse = ", "), sep = "")
  # If PEMS but no climate stress exclusions, then conducting PEM analyses
} else if (pems & !exclusions_stressunits) {
  cat("\n### RASTER AND SUMMARY MAP OUTPUTS: PEST EVENT MAPS ###", sep = "",
      file = Model_rlogging, append = TRUE)
  cat("\nRASTER AND SUMMARY MAP OUTPUTS: PEST EVENT MAPS\n", sep = "")
  # If PEMS and climate stress exclusions, then conducting PEM analyses that 
  # also include climate stress exclusions
} else if (pems & exclusions_stressunits) {
  cat("\n\n", str_wrap("### RASTER AND SUMMARY MAP OUTPUTS: PEST EVENT MAPS W/
                       CLIMATE STRESS EXCL. ###", width = 80), sep = "",
      file = Model_rlogging, append = TRUE)
  cat("\n\n", str_wrap("RASTER AND SUMMARY MAP OUTPUTS: PEST EVENT MAPS W/
                CLIMATE STRESS EXCL.\n", width = 80), sep = "")
}

#### * Pest Event Maps ####

# Process and plot the Pest Event Maps (PEMs) 
# Currently, the earliest date across cohorts and average data across cohorts 
# is calculated for the last day (last day is last element of "sublist" vector)

if (pems) {
  # Get all PEM files, split them by type (e.g., PEMe1, PEMe2) and stage 
  # (e.g., "egg", "adult")
  PEM_files <-  list.files(pattern = glob2rx("*PEM*.tif$")) # all PEM files
  PEM_types <- unique(substr(PEM_files, start = 1, stop = 5)) # split by type
  
  # Create a data frame with PEM labels - the labels will be joined to the 
  # appropriate PEM file below
  PEM_event_labels <- cbind(data.frame("pem_types" = PEM_types), 
    data.frame("gen" = substr(PEM_types, start = 5, stop = 5)), 
    data.frame("stg" = substr(PEM_types, start = 4, stop = 4)))
  PEM_event_labels <- PEM_event_labels %>%
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
  
  # Analyze the PEMs for each cohort in parallel, and then export resulting 
  # rasters and generate summary maps
  RegCluster(length(PEM_types))
  
  foreach(type = PEM_types, .packages = pkgs, .inorder = FALSE) %dopar% {
  #for (type in PEM_types) {
   #print(type)
    # Find files by type (e.g., "PEMe1" for each cohort) 
    files_by_type <- PEM_files[grep(pattern = type, x = PEM_files, 
                                    fixed = TRUE)] 
    # Change 0 values to NA so they are not averaged 
    PEM_brk <- brick(raster::stack(files_by_type))
    PEM_brk[PEM_brk == 0] <- NA
    
    # Remove PEM raster brick files if all PEM values are 0 because 
    # there's no point in plotting an all zero result
    if (sum(matrix(PEM_brk), na.rm = TRUE) == 0) {
      unlink(list.files(pattern = glob2rx(paste0("*", type, "_*tif$"))))
    # Remove PEM raster brick file if there is only a single unique value
    # This may happen for the last day of sampling period? (check on this)
    } else if (length(unique(values(PEM_brk))) < 3) {
        vals <- unique(getValues(PEM_brk))
        vals <- vals[!is.na(vals)]
        if (length(vals) == 1) {
          unlink(list.files(pattern = glob2rx(paste0("*", type, "_*tif$"))))
        }
      # If PEM raster brick has data, then process and plot it
    } else {
    # Create event label to be used for making summary maps
      eventLabel_df <- dplyr::filter(PEM_event_labels, PEM_types == type) %>% 
        dplyr::select(finalLabel) %>%
        mutate(., finalLabel = ifelse(type == OW_pem, paste("date of OW gen.", 
                                              OWEventLabel), finalLabel))          
        eventLabel <- paste(eventLabel_df$finalLabel)
        
      # Remove layers in the PEM stack if they are all NA, and print warning 
      # If this happens, should maybe change cohort emergence params
      zero_sum <- cellStats(PEM_brk, sum)
      pem_na <- PEM_brk[[which(zero_sum == 0)]]
      PEM_brk <- PEM_brk[[which(zero_sum > 0)]]
      if (nlayers(PEM_brk) < ncohort) {
        cat("\n\n", str_wrap(paste0("WARNING: check emergence params - missing 
            cohorts for ", type), width = 80), sep = "", file = Model_rlogging, 
            append = TRUE)
      }

      # Calc. avg. date of pest event across cohorts
      # Then save raster brick, and create and save summary maps
      avg_PEM <- calc(PEM_brk, fun = function(x, na.rm= TRUE) { 
        mean(x, na.rm = TRUE) 
        }) # average in day of event among cohorts
      names(avg_PEM) <- "Avg" # name layer for use below
      SaveRaster2(avg_PEM, paste("Avg", type, last(dats2), sep = "_"), 
                  "INT2U", paste("- Avg.", eventLabel))
      PlotMap(avg_PEM, last(dats2), paste("Avg.", eventLabel, sep = " "), 
              paste("Avg.", eventLabel, sep = " "), 
              paste("Avg", type, sep = "_"))
      
      # Calc. the earliest date of pest event across cohorts 
      # Then save raster brick, and create and save summary maps
      # Need to edit event labels if they contain the word "first" because this
      # is highly redundant (e.g. "earliest first adult emergence")
      min_PEM <- calc(PEM_brk, fun = function(x, na.rm= TRUE) { 
        min(x) 
        })
      names(min_PEM) <- "Earliest" # name layer for use below
      SaveRaster2(min_PEM, paste("Earliest", type, last(dats2), sep = "_"), 
                  "INT2U", paste("- Earliest", eventLabel))
      PlotMap(min_PEM, last(dats2), paste("Earliest", eventLabel, sep = " "), 
              paste("Earliest", eventLabel, sep = " "), 
              paste("Earliest", type, sep = "_"))
      
      # If climate stress exclusions are specified, then take PEM results from 
      # above and substitute values where the species is under severe stress 
      # only with -1 (Excl1), and areas where the species is under both
      # moderate and severe stress with -1 and -2, respectively (Excl2)
      if (exclusions_stressunits) {
        PEM_list <- list(avg_PEM, min_PEM)
        foreach(PEM = PEM_list, .packages = pkgs, .inorder = FALSE) %dopar% {
        #for (PEM in PEM_list) {
          nam <- names(PEM)
          # Do calculations
          PEM_excl1 <- Rast_Subs_Excl(PEM, "Excl1")[[1]] # Sev. stress (Excl1) 
          PEM_excl2 <- Rast_Subs_Excl(PEM, "Excl2")[[1]] # Both stress (Excl2) 
              
          # Save raster brick results; create and save summary maps
          SaveRaster2(PEM_excl1, paste0(nam, "_", type, "Excl1_", last(dats2)), 
                      "INT2S", paste("-", nam, eventLabel))
          SaveRaster2(PEM_excl2, paste0(nam, "_", type, "Excl2_", last(dats2)), 
                      "INT2S", paste("-", nam, eventLabel))
          PlotMap(PEM_excl1, last(dats2), paste0(nam, " ", eventLabel, 
                  " w/ climate stress exclusion"), paste0(nam, " ", eventLabel),
                  paste0(nam, "_", type, "Excl1"))
          PlotMap(PEM_excl2, last(dats2), paste0(nam, " ", eventLabel, 
                  " w/ climate stress exclusion"), paste0(nam, " ", eventLabel), 
                  paste0(nam, "_", type, "Excl2"))
         }
        }
      }
    }
  
  stopCluster(cl)
  rm(cl)
  
}

# Remove PEM objects to free up memory, and delete 
# PEM cohort rasters now that they have been processed
unlink(list.files(pattern = glob2rx(paste0("*PEM*cohort*"))))
rm(list = ls(pattern = "PEM|pem_")) 
  
# Log file messages
if (pems & exclusions_stressunits) {
  cat("\n\nDone with Pest Event Maps\n\n", str_wrap("### WEIGHTED RASTER AND 
      SUMMARY MAP OUTPUTS: LIFESTAGE W/ CLIM. STRESS EXCL. ###", width = 80), 
      "\n\nStages: ", paste(stgorder, collapse = ", "), sep = "",
      file = Model_rlogging, append = TRUE)  
  cat("\n\nDone with Pest Event Maps\n\n", str_wrap("WEIGHTED RASTER AND SUMMARY 
      MAP OUTPUTS: LIFESTAGE W/ CLIM. STRESS EXCL.", width = 80), "\nStages: ",
      paste(stgorder, collapse = ", "), "\n", sep = "")
} else if (pems & !exclusions_stressunits) {
  cat("\n\nDone with Pest Event Maps\n\n", str_wrap("### WEIGHTED RASTER AND  
      SUMMARY MAP OUTPUTS: LIFESTAGE ###", width = 80), "\n\nStages: ", 
      paste(stgorder, collapse = ", "), sep = "",
      file = Model_rlogging, append = TRUE) 
  cat("\nDone with Pest Event Maps\n\n", 
      "WEIGHTED RASTER AND SUMMARY MAP OUTPUTS: LIFESTAGE", "\nStages: ",
      paste(stgorder, collapse = ", "), "\n", sep = "")
}

# Make file lists for weighting the rasters by relative population size
Lfstg_fls <- list.files(pattern = glob2rx("*Lifestage_*.tif$"))

#### * Lifestage raster processing and plots ####
# Output weighted rasters and summary maps for ALL stages - this splits outs 
# the OW stage separately. Next, output weighted rasters and summary maps for 
# stages that combine OW stage w/ actual stage (e.g. OW adult = adult).

# Match OW stage to actual stage (e.g., OA = adult)
if (owstage == "OL") {
  stage_list <- c("OL","P","A","E","L")
} else if (owstage == "OP") {
  stage_list <- c("OP","A","E","L","P")
} else if (owstage == "OA") {
  stage_list <- c("OA","E","L","P","A")
} else if (owstage == "OE") {
  stage_list <- c("OE","L","P","A","E")
}

# What is the non-OW form of the OW stage? (e.g., OWadult = Adult)
stg_nonOW <- substring(owstage, 2) 

# Weight the lifestage rasters to calc. the relative size of the population 
# in any given life stage. For example, if 7 cohorts are run, then results from
# the 7 cohorts for each date will be combined and "weighted" according the 
# relative proportion of population represented by that cohort. 
# Results for the 5 stages (E, L, P, A, and OW stage) are run in parallel.
RegCluster(round(ncores/5))

foreach(stg = stage_list, .packages = pkgs, .inorder = TRUE) %dopar% {
#for (stg in stage_list) {
  #print(stg)
  # Get stage number of stage, and then rename to a more descriptive name
  stg_nam <- mgsub(string = stg, pattern = 
                     c("OE", "OL", "OP", "OA", "E", "L", "P", "A"), 
                   replacement = c("OWegg", "OWlarvae", "OWpupae", "OWadult",
                                   "Egg", "Larvae", "Pupae", "Adult"))

  if (stg != stg_nonOW) {
    
    # Weight the rasters according the proportion of the population 
    # represented by each cohort in that life stage
    # Weight_rasts accepts 3 arguments: 1) the cohort files; 2) the type
    # of files; and the life stage being analyzed in numerical form (e.g.
    # Egg = 2 if the stage order is OA, E, L, P, A)
    stg_num <- match(stg, stgorder)
    Lfstg_wtd <- Weight_rasts(Lfstg_fls, "Lifestage")
      
    # Save raster results
    SaveRaster2(Lfstg_wtd, paste0("Misc_output/", stg_nam), "INT2U",  
                paste("-", stg_nam, "relative pop. size for all", 
                      num_dats, "dates")) 

    # Create and save summary maps
    Lfstg_plots <- foreach(lyr = 1:nlayers(Lfstg_wtd), 
                           .packages = pkgs, .inorder = TRUE) %dopar% {
        lyr_name <- paste0(dats2[[lyr]])
        PlotMap(Lfstg_wtd[[lyr]], lyr_name, 
                paste0(stg_nam, " relative pop. size"), 
                "Relative pop. size", paste0("Misc_output/", stg_nam))
    }

    # If climate stress exclusions are specified, then take weighted lifestage 
    # results from above and substitute values where the species is under severe 
    # stress only with -1 (Excl1), and areas where the species is both moderate 
    # and severe stress with -1 and -2, respectively (Excl2)
    if (exclusions_stressunits) {
      
      rm(Lfstg_plots) # Free up memory
      
      # Overlay the All Stress Exclusion maps onto the weighted Lifestage
      # raster bricks. This is much more memory efficient than weighting
      # the the LifestageEXCL1 and LifestageEXCL2 rasters separately
      # NOTE: using do.call w/ brick results in smaller object size than using
      # raster::stack(Rast_Subs_Excl(Lfstg_wtd, "Excl1"))
      Lfstg_wtd_excl1 <- do.call(brick, Rast_Subs_Excl(Lfstg_wtd, "Excl1"))
      
      # Severe stress exclusion only
      SaveRaster2(Lfstg_wtd_excl1, 
                  paste0("Misc_output/", stg_nam, "_Excl1"), 
                  "INT2S", paste("-", stg_nam, "relative pop. size for all", 
                  num_dats, "dates"))
      
      Lfstg_Excl1_plots <- foreach(lyr = 1:nlayers(Lfstg_wtd_excl1),
                                   .packages = pkgs, .inorder = TRUE) %dopar% {
          lyr_name <- paste0(dats2[[lyr]])
          PlotMap(Lfstg_wtd_excl1[[lyr]], lyr_name, paste(stg_nam, 
                 "relative pop. size w/ climate stress exclusion", sep = " "), 
                 "Relative pop. size", 
                 paste0("Misc_output/", stg_nam, "_Excl1"))
      }
      
      rm(Lfstg_wtd_excl1, Lfstg_Excl1_plots) # Free memory
      
      # Severe and moderate stress exclusions
      Lfstg_wtd_excl2 <- do.call(brick, Rast_Subs_Excl(Lfstg_wtd, "Excl2"))
      
      SaveRaster2(Lfstg_wtd_excl2, 
                  paste0("Misc_output/", stg_nam, "_Excl2"), 
                  "INT2S", paste("-", stg_nam, "relative pop. size for all", 
                                 num_dats, "dates")) 
      
      Lfstg_Excl2_plots <- foreach(lyr = 1:nlayers(Lfstg_wtd_excl2), 
                                   .packages = pkgs, .inorder = TRUE) %dopar% {
          lyr_name <- paste0(dats2[[lyr]])
          PlotMap(Lfstg_wtd_excl2[[lyr]], lyr_name, paste(stg_nam, 
                  "relative pop. size w/ climate stress exclusion", sep = " "), 
                  "Relative pop. size", 
                  paste0("Misc_output/", stg_nam, "_Excl2"))
      }
            
    rm(Lfstg_wtd_excl2, Lfstg_Excl2_plots) # Free memory
    
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
    stg_num <- match(stg_nonOW, stgorder) # Get stage no. of non-OW stage
    stg_nonOW_nam <- mgsub(string = stg_nonOW, pattern = c("E", "L", "P", "A"), 
                           replacement = c("Egg", "Larvae", "Pupae", "Adult"))
    
    # Weight the rasters to get relative population size of the stage in the
    # population, and save and plot results. The Weight_rasts function is
    # written to account for the instance where stg == non_stg_nonOW.
    Lfstg_incOW_wtd <- Weight_rasts(Lfstg_fls, "Lifestage")
    SaveRaster2(Lfstg_incOW_wtd, paste0("Misc_output/", stg_nonOW_nam), 
                "INT2U", paste("-", stg_nonOW_nam, "relative pop. size for", 
                               num_dats, "dates"))
    
    Lfstg_incOW_plots <- foreach(lyr = 1:nlayers(Lfstg_incOW_wtd), 
                                 .packages = pkgs, .inorder = TRUE) %dopar% {
        lyr_name <- paste0(dats2[[lyr]])
        PlotMap(Lfstg_incOW_wtd[[lyr]], lyr_name, 
                paste(stg_nonOW_nam, "relative pop. size"), 
                "Relative pop. size", paste0("Misc_output/", stg_nonOW_nam))
    }
        
    rm(Lfstg_incOW_plots) # Free memory
    
    # If climate stress exclusions are specified, then take substitute values 
    # where the species is under severe stress with -1 (Excl1), and areas where 
    # the species is under both moderate and severe stress with -1 and -2, 
    # respectively (Excl2)
    if (exclusions_stressunits) {
      
      # Severe stress exclusion only
      # Weight rasters, save results, and plot the results
      #Lfstg_incOW_wtd_excl1_brk <- Weight_rasts(Lfstg_Excl1_fls, "Lifestage")
      Lfstg_incOW_wtd_excl1 <- do.call(brick, 
                                       Rast_Subs_Excl(Lfstg_incOW_wtd, "Excl1")) 
        
      SaveRaster2(Lfstg_incOW_wtd_excl1, 
        paste0("Misc_output/", stg_nonOW_nam, "_Excl1"), "INT2S", 
        str_wrap(paste("-", stg_nonOW_nam, " relative pop. size w/ sev. climate 
                       stress exclusion for", num_dats, "dates"), width = 80))
      
      Lfstg_incOW_Excl1_plots <- foreach(lyr = 1:nlayers(Lfstg_incOW_wtd_excl1), 
                                    .packages = pkgs, .inorder = TRUE) %dopar% {
          lyr_name <- paste0(dats2[[lyr]])
          PlotMap(Lfstg_incOW_wtd_excl1[[lyr]], 
                  lyr_name, paste(stg_nonOW_nam, 
                  "relative pop. size w/ climate stress exclusion", sep = " "), 
                  "Relative pop. size", 
                  paste0("Misc_output/", stg_nonOW_nam, "_Excl1"))
      }
            
      rm(Lfstg_incOW_wtd_excl1) # Free memory
      
      # Moderate and severe stress exclusions
      # Lfstg_incOW_wtd_excl2_brk <- Weight_rasts(Lfstg_Excl2_fls, "Lifestage")
      Lfstg_incOW_wtd_excl2 <- do.call(brick, 
                                       Rast_Subs_Excl(Lfstg_incOW_wtd, "Excl2")) 
      
      SaveRaster2(Lfstg_incOW_wtd_excl2, 
        paste0("Misc_output/", stg_nonOW_nam, "_Excl2"), "INT2S",
        str_wrap(paste("-", stg_nonOW_nam, "relative pop. size w/ sev. and mod. 
                       climate stress exclusion for", num_dats, "dates"), 
                 width = 80))
      
      Lfstg_incOW_Excl2_plots <- foreach(lyr = 1:nlayers(Lfstg_incOW_wtd_excl2), 
        .packages = pkgs, .inorder = TRUE) %dopar% {
          lyr_name <- paste0(dats2[[lyr]])
          PlotMap(Lfstg_incOW_wtd_excl2[[lyr]], lyr_name, 
                  paste("All", tolower(stg_nonOW_nam), 
                  "relative pop. size w/ climate stress exclusion", sep = " "), 
                  "Relative pop. size", paste0("Misc_output/", stg_nonOW_nam, 
                                               "_Excl2"))
      }
      
      rm(Lfstg_incOW_wtd_excl2) # Free memory
    }
  }
}

stopCluster(cl)
rm(cl)

# Delete Lifestage cohort rasters now that they have been processed
unlink(list.files(pattern = glob2rx(paste0("*Lifestage*cohort*"))))

# Log file messages
if (exclusions_stressunits) {
  cat("\n\n", str_wrap("Done with weighted raster summary map outputs for 
                       Lifestage, Lifestage_Excl1, and Lifestage_Excl2", 
                       width = 80), "\n\n", 
      str_wrap(paste("### WEIGHTED RASTER OUTPUT: NUMGEN WITH CLIMATE STRESS 
                     EXCL. ###"), width = 80), sep = "", 
      file = Model_rlogging, append = TRUE) 
  cat("\n", str_wrap("Done with weighted raster summary map outputs for 
                       Lifestage, Lifestage_Excl1, and Lifestage_Excl2", 
                       width = 80), "\n", sep = "")
  cat("\nWEIGHTED RASTER OUTPUT: NUMGEN WITH CLIMATE STRESS EXCL.", sep = "")
} else {
  cat("\n\n", str_wrap("Done with weighted raster summary map outputs for 
                       Lifestage", width = 80), "\n\n", 
      "### WEIGHTED RASTER OUTPUT: NUMGEN ###", sep = "", 
      file = Model_rlogging, append = TRUE) 
  cat("\n", str_wrap("Done with weighted raster and summary map outputs for 
                       Lifestage", width = 80), "\n\n", 
      "WEIGHTED RASTER OUTPUT: NUMGEN\n", sep = "")
}

#### * NumGen raster processing ####
# Note that each generation is output to it's own raster brick (.tif) file, 
# whereas the summary plots show every generation on a given date. There will 
# likely be some overlap between generations, and showing this is not possible
# if a raster brick combined mulitple generations.

# Calculate the maximum number of generations over sampled period 
# Split out NumGen brick by generation and save 
# First calculate the maximum number of generations over sampled period 
NumGen_fls <- list.files(pattern = glob2rx(paste0("NumGen_", "*tif$"))) 
maxgens <- as.numeric(max(maxValue(raster::stack(NumGen_fls))))

RegCluster(round(ncores/12))

foreach(i = 0:maxgens, .packages = pkgs, 
        .inorder = TRUE) %:%
  foreach(j = 1:length(NumGen_fls), .packages = pkgs, .inorder = TRUE) %dopar% {
    
    #for (j in 1:length(NumGen_fls)) {
    fl <- NumGen_fls[j]
    cohort <-  unique(str_split_fixed(fl, "_|[.]", 3)[,2])
    #for (i in 0:maxgens) {
    Gen_brick <- raster::stack(fl) == i
    SaveRaster2(Gen_brick, paste("Gen", i, cohort, sep = "_"), "INT2U", 
                paste("- Gen.", i, "for all", num_dats, "dates"))
  }
#}

stopCluster(cl)
rm(cl)

# Delete NumGen raster bricks - no longer needed
unlink(list.files(pattern = glob2rx(paste0("*NumGen*cohort*"))))

# Make a list of cohort bricks for each generation
gen_fls_lst <- list()
for (gen in 0:maxgens) {
  gen_fls <- list.files(pattern = paste0("Gen_", gen, "_"))
  gen_fls_lst[[gen + 1]] <- gen_fls 
}

names(gen_fls_lst) <- paste(c(rep("Gen", 1 + maxgens)), 0:maxgens, sep = "_")

# Weight the NumGen raster bricks for each generation according to the relative
# population size of each cohort. For example, cohort 1 emerged sooner in
# the year so may have completed more generations, but they comprise a small
# proportion of the population. The result of the analysis will depict this.
RegCluster(round(ncores/10))
foreach(i = 1:length(gen_fls_lst), .packages = pkgs, 
                          .inorder = TRUE) %dopar% {
#for (i in 1:length(gen_fls_lst)) { 
  # Which generation number?
  gen_nam <- names(gen_fls_lst)[i]
  gen <- as.numeric(str_split_fixed(gen_nam, "_", 2)[,2])
  gen_cohort_fls <- unlist(unname(gen_fls_lst[i]))
  # The value is NA here, becaue the raster bricks have already been split out
  # by generation (as opposed to the Lifestage bricks needing to be split 
  # by stage).
  NumGen_wtd <- Weight_rasts(gen_cohort_fls, "NumGen")
  SaveRaster2(NumGen_wtd, paste("Misc_output/Gen", gen, sep = "_"), "INT2U", 
               paste("- Gen.", gen, "for all", num_dats, "dates"))
  #cat("Finished NumGen_wtd", gen, "\n")
  
  # As for the Lifestage results, it is more memory efficient to overlay the
  # All Stress Exclusion raster with the weighted NumGen brick than to weight
  # the NumGenEXCL1 and NumGenEXCL2 files separately.
  if (exclusions_stressunits) {
    NumGen_wtd_excl1 <- do.call(brick, Rast_Subs_Excl(NumGen_wtd, "Excl1"))
    SaveRaster2(NumGen_wtd_excl1,
                paste("Misc_output/GenExcl1", gen, sep = "_"), "INT2S",
                str_wrap(paste("- Gen.", gen,
                               "with severe climate stress excl. for all",
                               num_dats, "dates"), width = 80))
    rm(NumGen_wtd_excl1) # Free memory
       
    #cat("Finished NumGen_wtd_excl1 - gen", gen, "\n")
       
    NumGen_wtd_excl2 <- do.call(brick, Rast_Subs_Excl(NumGen_wtd, "Excl2"))
    rm(NumGen_wtd) # Free up memory
    SaveRaster2(NumGen_wtd_excl2,
                paste("Misc_output/GenExcl2", gen, sep = "_"), "INT2S",
                str_wrap(paste("- Gen.", gen,
                "with severe and moderate climate stress excl. for all",
                num_dats, "dates"), width = 80))
    rm(NumGen_wtd_excl2)
    cat("Finished NumGen_wtd_excl2 - gen", gen, "\n")
  }

}

stopCluster(cl)
rm(cl)

# Delete cohort raster bricks split by generation - no longer needed
unlink(list.files(pattern = glob2rx(paste0("*Gen_*cohort*"))))

### * Create summary maps of NumGen results, weighted across cohorts

# Log file messages
if (exclusions_stressunits) {
  cat("\n\n", str_wrap("Done with weighted raster outputs for 
                       NumGen, NumGen_Excl1, and NumGen_Excl2", width = 80),
      sep = "", file = Model_rlogging, append = TRUE) 
  cat("\n\n### SUMMARY MAP OUTPUT: NUMGEN WITH CLIMATE STRESS EXCLUSIONS ###", 
      file = Model_rlogging, append = TRUE)
  cat("\n", str_wrap("Done with weighted raster outputs for NumGen, 
      NumGen_Excl1, and NumGen_Excl2", width = 80), sep = "")
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

# For each type (NumGen, NumGenExcl1, and NumGenExcl2, stack all generations 
# together, and then save the stack to a file. This allows the stacked results 
# to be worked with outside of the program, freeing up memory. First a list of 
# files to stack is created (by type), then the stacked files are written to 
# file. Writing the files is done in parallel if exclusions_stressunits = 1 
# (NumGenExcl1 and NumGenExcl2); otherwise the process is very, very slow.

# Make the list of files to stack by type
fls_to_stack <- list(list.files(paste0(getwd(), "/Misc_output"), 
                           pattern = glob2rx("*Gen_*.tif$")))

if (exclusions_stressunits) {
  fls_to_stack <-  append(fls_to_stack, 
                         list(list.files(paste0(getwd(), "/Misc_output"), 
                                    pattern = glob2rx("*GenExcl1_*.tif$")),
                         list.files(paste0(getwd(), "/Misc_output"), 
                                 pattern = glob2rx("*GenExcl2_*.tif$"))))
  names(fls_to_stack) <- c("NumGen", "NumGenExcl1", "NumGenExcl2")
} else {
 names(fls_to_stack) <- c("NumGen")
}

# Add directory ("Misc_output/") to every element in the list
fls_to_stack[] <- lapply(fls_to_stack, function(x) paste0("Misc_output/", x))

# For each type (NumGen, NumGenExcl1, NumGenExcl2), stack all 
# generations together and write the results to file
if (exclusions_stressunits) {
  RegCluster(round(ncores/10))

  foreach(i = 1:length(fls_to_stack), .packages = pkgs,
          .inorder = TRUE) %dopar% {
    fl_type <- names(fls_to_stack[i])
    writeRaster(stack(unlist(fls_to_stack[i], use.names = FALSE)),
              filename = paste0(fl_type, "_all_merged.grd"))
  }

  stopCluster(cl)
  rm(cl)

} else {
  writeRaster(stack(unlist(fls_to_stack, use.names = FALSE)),
              filename = "NumGen_all_merged.grd")
}

# Summary maps will be produced for each generation (saved from previous step), 
# for each date. This involves looping through all dates, creating
# a dataframe that contains data from all generations for a given date, and 
# plotting the results. This is done in parallel for increased speed [by data
# and by type (NumGen, NumGenExcl1, and NumGenExcl2)].
NumGen_mrgd_fls <- list("NumGen_all_merged.grd")
if (exclusions_stressunits) {
  NumGen_mrgd_fls <- append(NumGen_mrgd_fls, 
                            list("NumGenExcl1_all_merged.grd",
                                 "NumGenExcl2_all_merged.grd"))
}

RegCluster(round(ncores/4))

#for (i in 1:length(NumGen_mrgd_fls)) {
foreach(i = 1:length(NumGen_mrgd_fls), .packages = pkgs,
     .inorder = TRUE) %:%
  foreach(d = dats_list, .packages = pkgs, .inorder = TRUE) %dopar% {
    
    # Get the brick for the file type
    brk_fl <- NumGen_mrgd_fls[[i]]
    fl_type <- names(brick(NumGen_mrgd_fls[[i]])[[1]])
    fl_type <- str_split_fixed(fl_type, pattern = "_", 2)[,1]
    
    #for (d in dats_list) { 
    #print(d)
    
    # Create a vector of dates from each chunk in the dates list
    # Then loop through date vector, extract (subset) the raster brick for the
    # file type by a given date, and then create a data frame that has all data
    # from all generations for that date.
    dat_vec <- unname(unlist(d))
    for (dat in dat_vec) {
      #print(dat)
      # Which layer # in the stack corresponds to the date? Then subset brick.
      lyr_no <- which(dats2 == dat) 
      lyr_name <- paste0("\\b[.]", lyr_no, "\\b") # exact match
      brk_sub <- brick(brk_fl)[[grep(lyr_name, names(brick(brk_fl)))]]
      
      # In order for all completed generations to show up in legend key,
      # need to extract info from "NumGen_all_merged" brick for the date -
      # the climate stress values mask out this information. (TO DO: figure
      # out a way to avoid this in earlier steps?)
      # If a generation has completed or currently present, then there will be
      # non-zero values.
      if (exclusions_stressunits) {
        NumGen_brk <- brick(NumGen_mrgd_fls[[1]])
        brk_sub2 <- NumGen_brk[[grep(lyr_name, names(NumGen_brk))]]
        maxgens <- data.frame(as(brk_sub2, "SpatialPixelsDataFrame")) %>%
          dplyr::select(-x, -y) %>% 
          gather() %>% # Combine columns and make column of generation data
          mutate(gen = str_split_fixed(key, pattern = "_", 2)[,2]) %>% 
          mutate(gen = sub('\\..*', '', gen)) %>%
          filter(value > 0) %>% # Remove 0 values (gens not present)
          distinct(gen) %>% arrange %>% # Returns the gen(s) present on date
          pull() %>% last()
        maxgens <- as.numeric(maxgens) # This will be incorporated below
      }
        
      # Convert each layer of raster brick to a data frame; add generation num.
      # The add each data frame to a list; these will be merged below.
      NumGen_lyrs_toPlot <- list()
      #j <- 1
      for (lyr in 1:nlayers(brk_sub)) {
        df <- ConvDF(brk_sub[[lyr]])
        # Extract generation number from layers for that date
        lyr_name <- sub('\\..*', '', names(brk_sub[[lyr]]))
        gen <- str_split_fixed(lyr_name, pattern = "_", 2)[,2]
        df$gen <- as.numeric(sub("*\\.[0-9]", "", gen))

        # Don't include data if all values are >= 0 - don't want them to be 
        # in legend key
        if (any(df$value > 0)) {
          NumGen_lyrs_toPlot[[lyr]] <- df
        }
        
      }
      
      # Format data if pops of NO gens are present (no value > 0) AND there are 
      # climate stress exclusions - i.e. the spp is excluded from entire area. 
      # Then take the last data frame in the list; the generation column will 
      # be replaced with simply "excl. severe" and/or "excl. moderate" in plot.
      # Don't want to show generation in legend (b/c all would be excluded); 
      # just show "excl. severe" and/or "excl. moderate"
      brk_sub_uniqueVals <- unique(
        c(as.matrix(brk_sub)))[!is.na(unique(c(as.matrix(brk_sub))))]
      
      if (all(brk_sub_uniqueVals < 0)) {
        mrgd2 <- df
      }
      
      # Merge data frames in the list - if list is empty b/c all data frames 
      # had value = 0, then just use most recent data frame
      if (any(brk_sub_uniqueVals > -1) & length(NumGen_lyrs_toPlot) > 0) {
        mrgd <- do.call(rbind, NumGen_lyrs_toPlot)
      } else {
        mrgd <- df
      }
      
      rm(NumGen_lyrs_toPlot) # Free up memory
      
      # Create a layer for OW generation, using dataframe from latest 
      # generation, if present in data
      if (any(brk_sub_uniqueVals > -1) & any(mrgd$gen == 0)) {
        OW <- data.frame(mrgd %>% dplyr::filter(gen == 0))
      }
      
      # From merged data frame, remove duplicate cells, showing the most 
      # recent generation in cases of overlap
      if (any(brk_sub_uniqueVals > -1) & any(mrgd$value > 0)) {
        noZero <- data.frame(mrgd %>% group_by(x, y) %>% arrange(gen))
        noZero <- dplyr::filter(noZero, !value == 0)
        if (any(mrgd$gen == 0)) { 
          mrgd2 <- rbind(OW, noZero) 
        } else {
          mrgd2 <- noZero
        }
        
        # Format data if pops of any gen are present (value > 0) AND there are 
        # climate stress exclusions, but first check that there are any values 
        # in data >= 0 (if all values < 0, data are handled differently above)
      } else if (any(brk_sub_uniqueVals > 0) & any(mrgd$value < 0)) {
        if (any(mrgd$value < 0)) {
          excl_vals <- data.frame(mrgd %>% dplyr::filter(value != 0))
          #excl_vals$gen <- "GenOW"
          if (any(df$gen == 0)) {
            mrgd2 <- rbind(excl_vals, OW) 
          } else {
            mrgd2 <- excl_vals
          }
        } else {
          mrgd2 <- OW
        }
      }
      
      # If specified, create summary maps for odd generations only (1, 3, 5, ..)
      # ISSUE: no generations will be available to plot if even gens only are
      # present on a given date! This feature is therefore not useful on small
      # scales, since many gens may be missing at that scale.
      if (odd_gen_map == 1 & any(mrgd2$gen > 0)) {
        mrgd2 <- mrgd2[(mrgd2$gen %% 2 != 0),]
      }
      
      # If the input data are the files with climate stress values, then
      # the completed number of gens as computed above need to be added back in 
      if (i %in% c(2, 3)) {
        mrgd2 <- mrgd2 %>% 
          add_row(gen = 0:maxgens, value = 0) %>%
          mutate(gen = ifelse(value == -1, -1, ifelse(value == -2, -2, gen)))
      }
        
      # Plot results as long as there are data in "mrgd2" - this data frame
      # will be empty only if "odd_gen_map == 1," and there are no data for 
      # Gen1, Gen3, ... etc. (e.g., if there are only data for GenOW, which 
      # has been removed)
      if (nrow(mrgd2) > 0) {
        # Create and save summary maps
        if (fl_type == "GenExcl1") {
          PlotMap(mrgd2, dat, 
                  "Number of generations w/ climate stress exclusions",
                  "No. of\ngenerations", "NumGen_Excl1")
        } else if (fl_type == "GenExcl2") {
          PlotMap(mrgd2, dat, 
                  "Number of generations w/ climate stress exclusions",
                  "No. of\ngenerations", "NumGen_Excl2")
        } else if (fl_type == "Gen") {
          PlotMap(mrgd2, dat, "Number of generations", 
                  "No. of\ngenerations", "NumGen")
        }
      } else {
        cat("\n\nWARNING: NumGen results for", fl_type, "on", dat, 
            "\nnot plotted - no odd generation data for this date", 
            file = Model_rlogging, append = TRUE)
      }
    }
  }
#}

stopCluster(cl)
rm(cl)

# Log file messages
if (exclusions_stressunits) {
  cat("\n\n", str_wrap("Done with summary maps for NumGen, NumGen_Excl1, and 
                       NumGen_Excl2", width = 80), sep = "", 
      file = Model_rlogging, append = TRUE) 
  cat("\n\n", str_wrap("### ANALYSIS: LIFESTAGE W/ NO. OF GENS. AND CLIMATE 
                       STRESS EXCLUSIONS ###\n", 
                       width = 80), sep = "", 
      file = Model_rlogging, append = TRUE)
  cat("\nDone with summary maps for NumGen, NumGen_Excl1, and NumGen_Excl2\n\n")
  cat("ANALYSIS: LIFESTAGE W/ NO. OF GENS. AND CLIMATE STRESS EXCLUSIONS\n\n")
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
# TO DO: find a more efficient way to overlay generation combos w/o several 
# lines of code here
# Make vector of gen numbers
gens_noOW <- 1:maxgens # remove OW generation

# If more than 2 generations, split even and odd gens, make combos, and then 
# paste together
if (length(gens_noOW) > 2) {
  # Take every other gen no., starting with element 1 (odd), and again with 
  # element 2 (even)
  gens_even <- gens_noOW[gens_noOW %% 2 == 0] # if divisible by 2, is even 
  gens_even <- paste("Gen", gens_even, sep = "_")
  gens_odd <- gens_noOW[gens_noOW %% 2 != 0] # if not divisible by 2, is odd
  gens_odd <- paste("Gen", gens_odd, sep = "_")
  gens_odd2 <- gens_odd[2:length(gens_odd)]
  # First combo - consecutive numbers starting at Gen1 
  # (e.g. Gen_2|Gen_1, Gen_4|Gen_3...)
  # If uneven no. of gens, need to trim odd and even to same length
  if (length(gens_odd) > length(gens_even)) {
    gens_combo1 <- paste(gens_even, gens_odd[1:length(gens_even)], sep = "|")
  } else {
    gens_combo1 <- paste(gens_even, gens_odd, sep = "|")
  }
  # Second combo - consecutive numbers starting at Gen2 
  # (e.g., Gen_3|Gen_2, Gen_5|Gen_4)
  gens_combo2 <- paste(gens_odd2, gens_even[1:length(gens_odd2)], sep = "|")
  # Combine the two combo vectors and sort, then add on OW gen
  gens_combo_all <- sort(c(gens_combo1, gens_combo2)) 
  gens_combo_all <- append("Gen_1|Gen_0", gens_combo_all)
  # If only two generations, then just make this combo
  } else if (length(gens_noOW) == 2) {
    gens_combo_all <- c("Gen_1|Gen_0", "Gen_2|Gen_1")
  # If only one generation, then make this combo
  } else if (length(gens_noOW) == 1) {
    gens_combo_all <- c("Gen_1|Gen_0")
}

# For each combination, overlay the combos for each date and replace older 
# generation values (e.g., for combo "NumGen_2|NumGen_1" - NumGen_2 will replace 
# NumGen1 where there is overlap)
cat("\n\n", str_wrap("Replacing older generation vals with newer gen. 
                   vals in areas of overlap", width = 80), "\n", sep = "",
    file = Model_rlogging, append = TRUE)

RegCluster(round(ncores/10))
  
corrected_NumGen <- foreach(d = 1:length(dats2), .packages = pkgs, 
                            .inorder = FALSE) %dopar% {
#corrected_NumGen <- for (d in 1:length(dats2)) { 
  
  # Get file and layer names
  NumGen_all_fl <- "NumGen_all_merged.grd"
  all_lyr_names <- names(brick(NumGen_all_fl))
                               
  corrected_brick_list <- list()
  
  for (i in 1:length(gens_combo_all)) {
    
    # Which layer # in the stack corresponds to the date? Then subset brick
    # and replace all non-zero values w/ 1, and 0 values with NA
    lyr_no <- which(dats2 == dats2[d]) 
    lyr_name <- paste0("\\b", lyr_no, "$\\b") # regex for getting exact match
    sub1 <- raster::stack(NumGen_all_fl)[[grep(lyr_name, all_lyr_names)]]
    sub1[sub1 > 0] <- 1 
    sub1[sub1 == 0] <- NA
    
    # Get each gen in the combo
    genA <- str_split_fixed(gens_combo_all[i], pattern = "\\|", 2)[,1]
    genB <- str_split_fixed(gens_combo_all[i], pattern = "\\|", 2)[,2]
    
    # Search for each of those gens in the stack and combine them
    # The order matters here, genA must be first layer
    sub1A <- raster::subset(sub1, grep(paste0(genA, "[.]"), names(sub1)))
    sub1B <- raster::subset(sub1, grep(paste0(genB, "[.]"), names(sub1)))
    sub2 <- raster::stack(sub1A, sub1B)
    rm(sub1A, sub1B) # Free up memory
    
    # Identify areas where the two gens overlap by summing them
    # Areas of overlap = 2
    gen_ovlp <- stackApply(sub2, indices = c(1), fun = sum) 
    
    # Crop the layer of the later generation if it overlaps w/ earlier one
    # (but if values are all NA, just keep it)
    if (!is.na(any(values(sub2[[2]] > 0)))) {
      gen_ovlp_NA <- overlay(sub2[[1]], sub2[[2]], gen_ovlp,
        fun = function(x, y, z) {
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
      gen_ovlp_NA <- raster::stack(gen0, gen_ovlp_NA)
    }
    
    # "Corrected" combo is put back into a list
    corrected_brick_list[i] <- gen_ovlp_NA 
  }
    
  # Create a raster stack from the list
  NumGen_corrected <- do.call(stack, corrected_brick_list)
    
}

# Combine results of the previous analysis and overwrite the 
# "NumGen_all_merged.grd" file. This raster file has the "corrected" layers.
# Alternatively the above output can be held in memory as a huge raster
# stack. The run times are essentially the same, but will use this method
# instead in case there may be memory issues.
writeRaster(do.call(stack, corrected_NumGen),
  filename = "NumGen_all_merged.grd", overwrite = TRUE)

rm(corrected_NumGen)  # Free up memory                           

# All done - print messages and stop clusters  
cat("Done\n", file = Model_rlogging, append = TRUE)
cat("\nDone\n")
  
stopCluster(cl)
rm(cl)

# Now use resulting ordered NumGen brick layers to mask out Lifestage 
# (adult) layers. These will be plotted so that the Lifestage (adult) for each
# generation is depicted with a distinct color.
if (exclusions_stressunits) {
  cat("\n", str_wrap("Assigning a generation number to pixels in the Adult, 
                   Adult_Excl1, and Adult_Excl2 raster bricks", width = 80), 
      sep = "", file = Model_rlogging, append = TRUE)
  cat("\n", str_wrap("Assigning a generation number to pixels in the Adult, 
                   Adult_Excl1, and Adult_Excl2 raster bricks", width = 80),
      sep = "")
} else {
  cat("\n", str_wrap("Assigning a generation number to pixels in the Adult 
                     raster brick", width = 80), sep = "", 
      file = Model_rlogging, append = TRUE)
  cat("\n", str_wrap("Assigning a generation number to pixels in the Adult 
                     raster brick", width = 80), sep = "")
}

# Subset NumGen raster stack by generation and then mask out areas in weighted
# Adult rasters that do not belong to that generation 
# Result will be a list of raster stacks for each generation of adults
RegCluster(round(ncores/6))

# Run the overlay analysis and save results as raster bricks
mskGenAdult <- foreach(gen = 0:maxgens, 
                       .packages = pkgs, .inorder = TRUE) %dopar% {    
#for (gen in 0:maxgens) {
     #print(gen)
    # Extract data for each generation
    NumGen_msk <- raster::subset(brick("NumGen_all_merged.grd"),
    grep(paste0("Gen_", gen, "[.]"),
         names(brick("NumGen_all_merged.grd"))))
    
    # Mask out areas in adults raster that do not belong to the gen. of interest
    # Then name each layer by the generation no. and the date
    Adults_byGen <- overlay(brick("Misc_output/Adult.tif"), NumGen_msk, 
      fun = function(x, y) {
        x[is.na(y[])] <- NA
        names(x) <- names(y)
        return(x)
    }, datatype = "INT2S", filename = paste0("Adult_Gen", gen, ".tif"))
    names(Adults_byGen) <- paste("Gen", gen, dats2, sep = "_")
    
    # Add climate stress exclusions if selected
    if (exclusions_stressunits == 1) {
      Adults_byGen_excl1 <- stack(Rast_Subs_Excl(Adults_byGen, "Excl1")) 
      writeRaster(Adults_byGen_excl1, paste0("Adult_Excl1_Gen", gen, ".tif"))
      Adults_byGen_excl2 <- stack(Rast_Subs_Excl(Adults_byGen, "Excl2"))
      writeRaster(Adults_byGen_excl2, paste0("Adult_Excl2_Gen", gen, ".tif"))
    }
    
}
#}

stopCluster(cl)
rm(cl)

# All done - print messages and stop clusters  
cat("\nDone\n", file = Model_rlogging, append = TRUE)
cat("\nDone")
  
# Delete NumGen grids now that they are no longer needed
unlink(list.files(path = output_dir, 
                  pattern = glob2rx(paste0("*.gri$|*.grd$"))))

#### * Life stage with no. of generations plots ####
if (exclusions_stressunits) {
  cat("\nDone with Lifestage with NumGen analysis\n\n", 
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

# Get files to plot
if (exclusions_stressunits) {
  Adult_byGen_fls <-  list(list.files(pattern = glob2rx("*Adult_Gen*.tif$")), 
    list.files(pattern = glob2rx("*Adult_Excl1_Gen*.tif$")), 
    list.files(pattern = glob2rx("*Adult_Excl2_Gen*.tif$")))
  names(Adult_byGen_fls) <- c("Adult", "Adult_Excl1", "Adult_Excl2")
} else {
  Adult_byGen_fls <- list(list.files(pattern = glob2rx("*Adult_Gen*.tif$")))
  names(Adult_byGen_fls) <- c("Adult")
}

# Generate and save summary plots for "Lifestage by Generation" (currently only
# doing these for adults). TO DO: maybe change way the results are displayed; 
# a bit confusing to have "other stages" plus gen number, but gen number is 
# not actually dispalyed on map.
RegCluster(round(ncores/6))

Adult_byGen_sum_maps <- foreach(j = 1:length(Adult_byGen_fls),
                                .packages = pkgs, .inorder = TRUE) %:%
  foreach(d = dats2, .packages = pkgs, .inorder = TRUE) %dopar% {

#for (j in 1:length(Adult_byGen_fls)) {
 fl_type <- paste0(names(Adult_byGen_fls[j]))
   #for (d in dats2) {
    #print(d)
    #print(j)

    # Which layer # in the stack corresponds to the date? 
    lyr_no <- which(dats2 == d) 
    lyr_name <- paste0("\\b", lyr_no, "$\\b") # regex for getting exact match
    
    # Subset the brick by the layer name (date)
    # Only include the layer if there are other data besides NA
    # TO DO: for some versions of R or an R package (unknown which), the 
    # "ConvDF" function will fail if there is only 1 pixel and that pixel == 0.  
    # Tried to modify the function but it did not resolve the issue.
    fls <- Adult_byGen_fls[[j]]
    brk_sub <- brick()
    
    for (f in 1:length(fls)) {
      brk <- brick(fls[[f]])[[grep(lyr_name, names(brick(fls[[f]])))]]
      # Bug in "ConvDF" requires more than 1 row of data if the value == 0
      # Not sure if same error will be thrown for -1 and -2 values so used
      # all(freq$value <= 0) $ freq$count > 1
      if (any(!is.na(values(brk)))) { # Don't include layers if all NA
        freq <- data.frame(freq(brk, useNA = "no")) # Get frequency of all vals
        if (all(freq$value <= 0) & freq$count > 1) { 
          brk_sub <- addLayer(brk_sub, brk)
        } else if (any(freq$value > 0)) { # Don't need to worry about non-0 vals
          brk_sub <- addLayer(brk_sub, brk)
        }
      }
    }
    
    # For each stack layer, identify the generation, convert data to a 
    # data frame and add it to a list
    df_list <- list()
    for (i in 1:nlayers(brk_sub)) {
      r <- brk_sub[[i]]
      gen <- unlist(str_split(names(r), paste0("_Gen|[.]")))[2]
      #print(gen)
      lyr_df <- ConvDF(r) # convert raster to a data frame
      lyr_df$gen <- as.numeric(gen)
      colnames(lyr_df)[1] <- "value"
      df_list[[i]] <- lyr_df # add to the list 
    }
    
    # Merge the list into a single data frame - this has data from all 
    # generations for a given date. Remove generations for which all values 
    # are 0, otherwise will obscure data from other layers (other gens)
    # Not doing this will produce erroneous bands of adults at southern edge
    mrgd <- do.call(rbind, df_list) 
    
    # Free up memory
    rm(df_list, brk_sub, r)
    
    # Optional: create summary maps for odd generations only - beginning for 
    # 1st gen (i.e., 1, 3, 5, ..)
    if (odd_gen_map == 1) { # should odd gens be plotted instead of all gens?
     mrgd <- mrgd[(mrgd$gen %% 2 != 0),]
    }
    
    # Plot results as long as there are data in "mrgd2" - this data frame will 
    # be empty only if "odd_gen_map == 1" and there are no data for Gen1, Gen3, 
    # ... etc. (e.g., if there are only data for GenOW, which has been removed)
    # Currently "other stages" (not adults) are colored gray - may want to 
    # consider coloring them more similarly to which generation they belong to.
    # Note that output maps may have a generation in the legend that is not 
    # visible on the map - this is because other life stages (in light gray) are 
    # present, not the adults.
    if (nrow(mrgd) > 0) {
      if (fl_type == "Adult") {
        PlotMap(mrgd, d, "Adult relative pop. size for each gen.", 
                "Adult relative\npop. size (peak)", "Misc_output/Adult_byGen")
        } else if (fl_type == "Adult_Excl1") {
          PlotMap(mrgd, d, 
          "Adult relative pop. size for each gen. w/ climate stress exclusion",
                  "Adult relative\npop. size (peak)", 
                  "Misc_output/Adult_Excl1_byGen")
        } else if (fl_type == "Adult_Excl2") {
          PlotMap(mrgd, d, 
          "Adult relative pop. size for each gen. w/ climate stress exclusion",
                  "Adult relative\npop. size (peak)", 
                  "Misc_output/Adult_Excl2_byGen")
      }
    } else {
      cat("\n\nWARNING: Adult w/ NumGen results for each gen. on", d, 
          "\nnot plotted - no odd generation data for this date", 
          file = Model_rlogging, append = TRUE)
    }
  }
#}

stopCluster(cl)
rm(cl)

# Delete temp files 
unlink(list.files(pattern = glob2rx(paste0("*Adult*Gen*.tif$"))))

cat("\n\nDone with Lifestage with NumGen summary maps\n", 
    file = Model_rlogging, append = TRUE)
cat("\nDone with Lifestage with NumGen summary maps\n")

#### * Analyses and map production all done - wrap-up ####
processing_exectime <- toc(quiet = TRUE)
processing_exectime <- (processing_exectime$toc - processing_exectime$tic) / 60 

cat("\n### Done w/ final analyses and map production ###\n", 
    "Run time for analyses and map production = ", 
    round(processing_exectime, digits = 2), " min\n", sep = "", 
    "Deleting, renaming, and moving miscellaneous files\n",
    file = Model_rlogging, append = TRUE)
cat("\nDone w/ final analyses and map production\n\n", 
    "Run time for analyses and mapping run time = ", 
    round(processing_exectime, digits = 2),
    " min\n\n", "Deleting, renaming, and moving miscellaneous files\n\n", 
    sep = "")

#### * Rename final files and move misc files ####

# Create list of files that will be kept in the main output folder. These 
# include outputs for the last day of the sampled time period, with the 
# exception of Stage Count outputs.
last_dat_fls <- list.files(pattern = glob2rx(paste0("*", last(dats2), 
                                                    "*.png$")))
stgCnt_remove <- grep(pattern = glob2rx(paste0("*StageCount*", last(dats2), 
                                              "*")), last_dat_fls, value = TRUE)
last_dat_fls <- last_dat_fls[!last_dat_fls %in% stgCnt_remove]

# If current year was sampled then keep Stage Count outfile for the current day 
# in the main output folder. Then make a list of final output files to rename.
if (start_year == current_year) {
  today_dat_fls <- list.files(pattern = glob2rx(paste0("*StageCount*", 
                                                       today_dat, "*.png$")))
  final_fls <- c(last_dat_fls, today_dat_fls)
} else {
  final_fls <- c(last_dat_fls)
}

# Put species abbreviation in final output files (rename)
new_names <- paste0(spp, "_", final_fls)
if (length(final_fls) > 0) {
  invisible(file.rename(final_fls, new_names))  
} else {
  cat("\nNo PNG files for final outputs - check for errors\n", 
      file = Model_rlogging, append = TRUE)
  cat("\nNo PNG files for final outputs - check for errors\n")
}

cat("Renamed all final PNG files to include ", spp, " in file name\n", sep = "", 
    file = Model_rlogging, append = TRUE)
cat("Renamed all final PNG files to include ", spp, " in file name\n", sep = "")

# All other misc files (w/out spp name in file name) are moved to "/Misc_output"
misc_fls <- grep(list.files(path = output_dir), 
                 pattern = spp, invert = TRUE, value = TRUE) 
misc_fls <- misc_fls[!(misc_fls %in% 
                         c("Misc_output", "previous_run", "Logs_metadata"))]
invisible(file.copy(misc_fls, paste0(output_dir, "/Misc_output/")))
invisible(file.remove(misc_fls))

# Wrap up log file and report time for entire model run
cat("\nMODEL RUN DONE\n", file = Model_rlogging, append = TRUE)
cat("\nMODEL RUN DONE\n")
total_exectime <- toc(quiet = TRUE) # Execution time for entire run
total_exectime <- round((total_exectime$toc - total_exectime$tic) / 60, 
                        digits = 2)
cat("Run time for entire model =", total_exectime, "min", 
    file = Model_rlogging, append = TRUE)
cat("\nRun time for entire model =", total_exectime, "min\n\n")

# Clean up
rm(list = ls(all.names = TRUE)) # Clear all objects including hidden objects
gc()

