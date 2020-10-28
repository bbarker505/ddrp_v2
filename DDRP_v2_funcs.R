# Functions used for DDRP_v2.R -----
# 
# This file must accompany DDRP_v2.R; it contains the majority of functions 
# needed to run the program.
# 
# Log of most recent changes -----
# 10/26/20: Fixed code that allows for different DD calc methods to be used and
#   removed "SimpDD" calculation (need to add Sine DD calc method later)
# 8/15/20: Fixed bug in Stage Count plotting function
# 7/22/20: Minor edits to RegCluster function
# 7/16/20: Improved some legend colors to better discern categories
# 6/26/20: Added "StageCount" summary maps, improved map legends, cleaned
# up PlotMap function
# 4/24/20: Changed the parameter name "chill stress" to "cold stress"
# 3/31/20: Fixed issue w/ PEMs showing Dec of previous year
# 2/25/20: Minor edits to PEM legend colors so weeks of year are always 
# assigned the same color
# 2/18/20: Changed RegCluster back from FORK (to default = PSOCK) due to
# apparent issues on Hopper (cannot open connection issues whenever the
# process required access to numerous raster files). Updated Cut_interval func.
# 2/15/20: Removed EXCl1 and EXCL2 steps for NumGen and Lifestage in 
# Daily Loop function because those raster bricks are not used - it is more
# memory efficient to calculate them in the Data Processing section
# 2/12/20: streamlined Rast_Subs_Excl functions; added Weight_rasts function
# 2/7/20: fixed minor bugs in PlotMap function & changed a color
# 1/28/20: detect CRS from template data, not hard-code it
# 1/23/20: changed RegCluster func; fixed memory issues
# 1/3/20: changed naming of clim. suit. output files
# 12/9/19: had to fix code in PEM plotting - error for some spp
#
# Issues to resolve: boundary of CONUS doesn't line up completely with raster
# May not be an issue, but the color key in PEMs is really convoluted - should
# look into simplifying the code if possible.

#### (1). Assign_extent: assign geographic extent ####
# Add new extent definitions here for use in models and plots
# Set up regions
# Use switch() (works like a single use hash) 
Assign_extent <- function(region_param = paste0(region_param)) {
  REGION <- switch(region_param,
                "CONUS"        = extent(-125.0, -66.5, 24.54, 49.4),
                "WEST"         = extent(-125.0, -102, 31.1892, 49.4),
                "EAST"         = extent(-106.8, -66.5, 24.54, 49.4),
                "MIDWEST"      = extent(-104.2, -87, 30, 49.3),
                "NORTHWEST"    = extent(-125.1, -103.8, 40.6, 49.15),
                "SOUTHWEST"    = extent(-124.6, -101.5, 31.2, 42.3),
                "SOUTHCENTRAL" = extent(-83.6, -78.3, 31.8, 35.3),
                "NORTHCENTRAL" = extent(-104.3, -80.2, 35.7, 49.4),
                "SOUTHEAST"    = extent(-107.1, -75.0, 24.54, 39.6),
                "NORTHEAST"    = extent(-84.2, -64.3, 36.9, 48.1),
                "AL"           = extent(-88.5294, -84.7506, 30.1186, 35.1911),
                "AR"           = extent(-94.8878, -89.5094, 32.8796, 36.6936),
                "AZ"           = extent(-115, -108.98, 31.2, 37),
                "CA"           = extent(-124.6211, -113.7428, 32.2978, 42.2931),
                "CO"           = extent(-109.2625, -101.8625, 36.7461, 41.2214),
                "CT"           = extent(-73.7700, -71.7870, 40.9529, 42.0355),
                "DL"           = extent(-76.1392, -74.1761, 38.3508, 39.9919),
                "FL"           = extent(-87.8064, -79.9003, 24.54, 31.1214),
                "GA"           = extent(-85.7850, -80.5917, 30.1767, 35.1594),
                "IA"           = extent(-96.8617, -89.9697, 40.1147, 43.7353),
                "ID"           = extent(-117.3917, -110.6167, 41.4500, 49.15),
                "IL"           = extent(-91.5897, -87.0461, 36.8903, 42.6375),
                "IN"           = extent(-88.1686, -84.4686, 37.7836, 41.9794),
                "KS"           = extent(-102.3342, -94.1756, 36.6369, 40.2836),
                "KY"           = extent(-89.3581, -81.8425, 36.4208, 39.3347),
                "LA"           = extent(-94.3019, -88.7758, 28.8333, 33.2994),
                "MA"           = extent(-73.5639, -69.7961, 41.1689, 42.9525),
                "MD"           = extent(-79.7014, -74.8833, 37.0631, 39.9075),
                "ME"           = extent(-71.4056, -66.6667, 42.9525, 47.5228),
                "MI"           = extent(-90.5542, -82.3047, 41.6311, 47.5739),
                "MN"           = extent(-97.4000, -89.3786, 43.2550, 49.4),
                "MO"           = extent(-95.8803, -88.9883, 35.8822, 40.7058),
                "MS"           = extent(-91.7475, -87.8522, 29.9842, 35.2631),
                "MT"           = extent(-116.3667, -103.8250, 44.0667, 49.15),
                "NC"           = extent(-84.44092, -75.3003, 33.6829, 36.6461),
                "ND"           = extent(-104.2708, -96.3075, 45.6403, 49.15),
                "NE"           = extent(-104.3553, -95.0464, 39.7506, 43.2022),
                "NH"           = extent(-72.6617, -70.6142, 42.6256, 45.4700),
                "NJ"           = extent(-75.9175, -73.1892, 38.8944, 41.5806),
                "NM"           = extent(-109.2942, -102.6383, 31.1892, 37.2000),
                "NV"           = extent(-120.3358, -113.6803, 34.7356, 42.2981),
                "NY"           = extent(-80.0867, -71.7381, 40.4828, 45.1692),
                "OH"           = extent(-85.0439, -80.2464, 38.2797, 42.0217),
                "OK"           = extent(-103.2850, -94.1964, 33.3839, 37.2850),
                "OR"           = extent(-124.7294, -116.2949, 41.7150, 46.4612),
                "PA"           = extent(-80.7672, -74.5033, 39.4694, 42.5094),
                "RI"           = extent(-71.8628, -71.1206, 41.1463, 42.0188),
                "SC"           = extent(-83.6422, -78.3275, 31.8814, 35.3811),
                "SD"           = extent(-104.3553, -96.0806, 42.3050, 46.2050),
                "TN"           = extent(-90.3239, -81.5047, 34.5578, 37.1125),
                "TX"           = extent(-107.1592, -93.2411, 25.8614, 36.7200),
                "UT"           = extent(-114.2925, -108.7450, 36.7778, 42.2347),
                "VA"           = extent(-83.8322, -75.6200, 36.3892, 39.7886),
                "VT"           = extent(-73.6747, -71.4108, 42.5886, 45.1956),
                "WA"           = extent(-124.9585, -116.8364, 45.4554, 49.15),
                "WI"           = extent(-93.1572, -86.6822, 42.2733, 46.9914),
                "WV"           = extent(-82.8783, -77.5114, 37.1158, 40.7836),
                "WY"           = extent(-111.6167, -103.7333, 40.6667, 45.4833))
  return(REGION)
}

# autoStopCluster <- function(cl) {
#   stopifnot(inherits(cl, "cluster"))
#   env <- new.env()
#   env$cluster <- cl
#   attr(cl, "gcMe") <- env
#   reg.finalizer(env, function(e) {
#     message("Finalizing cluster ...")
#     #message(capture.output(print(e$cluster)))
#     try(parallel::stopCluster(e$cluster), silent = FALSE)
#     message("Finalizing cluster ... done")
#   })
#   cl
# }

#### (2). Base_map: base map for summary plots ####
# Base features used for all summary (PNG) maps in "PlotMap" function
# The "coord_quickmap" function allows the state/region of interest to be 
# plotted (otherwise CONUS is plotted)
# geom_raster is faster than geom_tile
# Input data are in a data frame (= df) format
Base_map <- function(df) {
  p <- ggplot(states, aes(x = long, y = lat)) + 
    geom_raster(data = df, aes(x = x, y = y, fill = value)) + 
    geom_path(aes(group = group), color = "black", lwd = 0.4) +
    #theme_map(base_size = base_size) +
    coord_quickmap(xlim = c(REGION@xmin, REGION@xmax), 
                   ylim = c(REGION@ymin, REGION@ymax), expand = FALSE) 
    #coord_cartesian(xlim = c(REGION@xmin, REGION@xmax), 
    #ylim = c(REGION@ymin, REGION@ymax))
}

#### (3). CohortDistrib: cohort emergence distribution ####
# This is an approximation from GAM predictions. 
CohortDistrib <- function(dist, numstage, perc) {
    ReturnClosestValue <- function(dist, xval) {
    out <- dist$CDF[which(dist$x > xval)[1]]
  }
  
  low <- (1 - perc)/2
  high <- 1 - (1 - perc) / 2
  low <- dist$x[which(dist$CDF > low)[1]]
  high <- dist$x[which(dist$CDF > high)[1]]
  
  bounds <- seq(low, high, length.out = numstage + 1)
  means <- (bounds[1:numstage] + bounds[2:(numstage + 1)]) / 2
  weights <- diff(sapply(X = bounds, FUN = ReturnClosestValue, dist = dist), 
                  lag = 1)
  return(data.frame(means, weights))
}

#### (4). CombineMaps: merge raster tiles ####
# Merge tiles back together for CONUS or EAST
# SpaDES.tools requires 'sf' package (needs GDAL update), so just use 
# "raster::merge"
CombineMaps <- function(brick_files, type, cohort) {
  # Search for file type in brick file list
  fls_by_type <- brick_files[grep(pattern = paste0(type, "_"), x = brick_files, 
                                  fixed = TRUE)]
  # Merge tiles back together for the input cohort
  fls_by_cohort <- fls_by_type[grep(pattern = paste0("cohort", cohort), 
                                    x = fls_by_type, fixed = TRUE)]
  mrgd <- Reduce(raster::merge, lapply(fls_by_cohort, brick))
  #mrgd <- SpaDES.tools::mergeRaster(brk_by_cohort)
  writeRaster(mrgd, filename = paste0(type, "_cohort", cohort, "_all"),
              overwrite = TRUE, datatype = "INT2S", format = "GTiff")
}

#### (5). Cond: if then else (conditional) ####
# If then else raster function [sim. to GRASS r.mapcalc if (x,a,b)]
Cond <- function(condition, trueValue, falseValue) {
  return(condition * trueValue + (!condition) * falseValue)
}

#### (6). CohortVals: simple way to assign weights for substages (cohorts) ####
# Choose number of substages (= numstage) and the decimal (0, 1) (= perc)
# of population to include
# Outputs standard deviations from mean and weights for substage
CohortVals <- function(numstage, perc) {
  low <- qnorm((1 - perc)/2)
  high <- qnorm(1 - (1 - perc) / 2)
  bounds <- seq(low, high, length.out = numstage + 1)
  means <- (bounds[1:numstage] + bounds[2:(numstage + 1)]) / 2
  weights <- diff(pnorm(bounds), lag = 1)
  return(data.frame(means, weights))
}

#### (7). Colfunc: gradient color ramp for summary maps ####
# Creates a gradient color ramp consisting of n colors
# x and y are color1 and color2
Colfunc <- function(x, y, n) {
  colorRampPalette(c(x, y))(n)
}

#### (8). ConvDF: convert raster to data frame ####
# First convert raster (= rast) to a spatial pixels data frame and 
# then to a data frame
ConvDF <- function(rast) {
  spdf <- as(rast, "SpatialPixelsDataFrame")
  df <- as.data.frame(spdf)
  colnames(df) <- c("value", "x", "y")
  return(df)
}

#### (9). Cut_bins: classify data into bins for plotting ####
# Classify data (= df) so it can be visualized in categories 
# (e.g., 1-10, 11-20, 21-30) 
Cut_bins <- function(df, breaks) {
  df$value_orig <- df$value # Keep old value so can sort factors against it
  # Round up max value to highest number divisible by 10
  df$value[df$value == max(df$value)] <- 10 * ceiling(max(df$value)/10)
  # Cut values into bins and format results
  df2 <- df %>% mutate(value = cut_interval(df$value, n = breaks),
            value = gsub("[()]|\\[|\\]", "", value),
            bin1 = ceiling(as.numeric(str_split_fixed(value, ",", 2)[,1])),
            bin2 = ceiling(as.numeric(str_split_fixed(value, ",", 2)[,2])),
            value = paste(bin1, bin2, sep = "-"))
  return(df2)
}

# For data potentially ranging from 0 - 100 (Lifestage pop. size), 
# bins them by groups of 10 always starting at 0 and ending at 100.
# TO DO: see about combining the two "Cut_bin" functions; simplify
Cut_bins2 <- function(df) {
  df$value_orig <- df$value # Keep old value so can sort factors against it
  df <- mutate(df, value = ifelse(value < 10, "0-10", 
          ifelse(value < 20, "10-20", ifelse(value < 30, "20-30", 
          ifelse(value < 40, "30-40", ifelse(value < 50, "40-50", 
          ifelse(value < 60, "50-60", ifelse(value < 70, "60-70", 
          ifelse(value < 80, "70-80", ifelse(value < 90, "80-90", 
          "90-100"))))))))))
  return(df)
}

#### (10). DailyLoop: daily loop model ####
# The daily loop is the daily time step used to generate all results for DDRP
# The loop is run for each cohort (= cohort) in parallel. If the study region 
# is CONUS or EAST, then each of 4 tiles (= tile_num) are run in parallel as 
# well. The template (= template) is needed for setting up data sets to 
# populate during the model run, and to convert matrix outputs into raster 
# format.
DailyLoop <- function(cohort, tile_num, template) { 

  # Generate a daily log file for each cohort - 
  # this is useful only for trouble-shooting
  # if (region_param %in% c("CONUS", "EAST")) {
  #   daily_logFile <- paste0("Daily_loop_cohort", cohort, "_", 
  #   tile_num, ".txt")
  # } else {
  #   daily_logFile <- paste0("Daily_loop_cohort", cohort, ".txt")
  # }
  
  #### * Initialize rasters for all variables ####
  
  # Main rasters - these are built upon within daily loop
  DDaccum <- as.matrix(template)
  # Track total degree days accumulated, for a single Lifestage (larvae)
  DDtotal <- as.matrix(template)
  # Track Lifestage for each cell per day (all cells start in OW = stage1)
  Lifestage <- as.matrix(template) + 1
  # Track voltinism per cell per day, starting at 0
  NumGen <- as.matrix(template)
  
  # Additional rasters - created depending on input setting
  if (exclusions_stressunits) {
    # Create masks for each variable
    coldmask         <- as.matrix(template)  # Daily cold units mask
    coldstress       <- as.matrix(template)  # Count of daily cold units
    coldstressTHRESH  <- as.matrix(template)  # Mask for coldstrs units thres
    coldstressTHRESH  <- coldstress_threshold # Mask for coldstrs units thres
    coldunitsCUM     <- as.matrix(template)  # Cumulative cold units
    coldstressMAX1    <- as.matrix(template)  # Max cold before most die
    coldstressMAX1    <- coldstress_units_max1 # Max cold before most die
    coldstressMAX2    <- as.matrix(template)  # Max cold before all die
    coldstressMAX2    <- coldstress_units_max2 # Max cold before all die
    coldEXCL         <- as.matrix(template)  # Cold stress exclusion
    heatmask          <- as.matrix(template)  # Daily heat stress units mask
    heatstress        <- as.matrix(template)  # Count of daily heat stress units
    heatstressTHRESH  <- as.matrix(template)  # Mask for heatstress units thres
    heatstressTHRESH  <- heatstress_threshold # Mask for heatstress units thres
    heatunitsCUM      <- as.matrix(template)  # Cumulative heat stress units
    heatstressMAX1    <- as.matrix(template)  # Max heat before most die
    heatstressMAX1    <- heatstress_units_max1 # Max heat before most die
    heatstressMAX2    <- as.matrix(template)  # Max heat before all die
    heatstressMAX2    <- heatstress_units_max2 # Max heat before all die
    heatEXCL          <- as.matrix(template)  # Heat stress exclusions
    AllEXCL           <- as.matrix(template)  # Combined stress exclusions
  }
  
  if (pems) {
    
    if (mapE == 1 & eggEventDD) {  
      # Event DD for all must be specified in spp.params file for this to run
      if (PEMnumgens > 0) {
        # Egg DOYs for when cumDDs > eggEvent threshold OW generation
        PEMe0 <- as.matrix(template)  
        # Egg DOYs for when cumDDs > eggEvent threshold  1st Gen
        PEMe1 <- as.matrix(template)  
      }
      if (PEMnumgens > 1) {
        # Egg DOYs for when cumDDs > eggEvent threshold  2nd Gen
        PEMe2 <- as.matrix(template)  
      }
      if (PEMnumgens > 2) {
        # Egg DOYs for when cumDDs > eggEvent threshold  3rd Gen
        PEMe3 <- as.matrix(template)  
      }
      if (PEMnumgens > 3) {
        # Egg DOYs for when cumDDs > eggEvent threshold  4th Gen
        PEMe4 <- as.matrix(template)  
      }
    }
    
    if (mapL == 1 & larvaeEventDD) {
       if (PEMnumgens > 0) {
        # Larval DOYs for when cumDDs > larvaeEvent threshold OW generation
        PEMl0 <- as.matrix(template)  
        # Larval DOYs for when cumDDs > larvaeEvent threshold  1st Gen
        PEMl1 <- as.matrix(template)  
      }
      if (PEMnumgens > 1) {
        # Larval DOYs for when cumDDs > larvaeEvent threshold  2nd Gen
        PEMl2 <- as.matrix(template)  
      }
      if (PEMnumgens > 2) {
        # Larval DOYs for when cumDDs > larvaeEvent threshold  3rd Gen
        PEMl3 <- as.matrix(template)  
      }
      if (PEMnumgens > 3) {
        # Larval DOYs for when cumDDs > larvaeEvent threshold  4th Gen
        PEMl4 <- as.matrix(template)  
      }
    }
    
    if (mapP == 1 & pupaeEventDD) {
      if (PEMnumgens > 0) {
        # Pupal DOYs for when cumDDs > pupalEvent threshold OW generation
        PEMp0 <- as.matrix(template)  
        # Pupal DOYs for when cumDDs > pupalEvent threshold  1st Gen
        PEMp1 <- as.matrix(template)  
      }
      if (PEMnumgens > 1) {
        # Pupal DOYs for when cumDDs > pupalEvent threshold  2nd Gen
        PEMp2 <- as.matrix(template)  
      }
      if (PEMnumgens > 2) {
        # Pupal DOYs for when cumDDs > pupalEvent threshold  3rd Gen
        PEMp3 <- as.matrix(template)  
      }
      if (PEMnumgens > 3) {
        # Pupal DOYs for when cumDDs > pupalEvent threshold  4th Gen
        PEMp4 <- as.matrix(template)  
      }
    }
    
    if (mapA == 1 & adultEventDD) {
      if (PEMnumgens > 0) {
        # Adult DOYs for when cumDDs > adultEvent threshold OW generation
        PEMa0 <- as.matrix(template)  
        # Adult DOYs for when cumDDs > adultEvent threshold  1st Gen
        PEMa1 <- as.matrix(template)  
      }
      if (PEMnumgens > 1) {
        # Adult DOYs for when cumDDs > adultEvent threshold  2nd Gen
        PEMa2 <- as.matrix(template)  
      }
      if (PEMnumgens > 2) {
        # Adult DOYs for when cumDDs > adultEvent threshold  3rd Gen
        PEMa3 <- as.matrix(template)  
      }
      if (PEMnumgens > 3) {
        # Adult DOYs for when cumDDs > adultEvent threshold  4th Gen
        PEMa4 <- as.matrix(template)  
      }
    }
    
  }
  
  #### * Step through days ####
 # tryCatch(
  for (d in 1:length(sublist)) {
    #cat("\n\ndoy: ", sublist[d], "\n", file=daily_logFile, append=TRUE) 
    stage_dd_cohort <- stage_dd[as.integer(cohort), ]  
    
    # Get temperature matrices for the day
    if (region_param %in% c("CONUS", "EAST")) {
      tmax <- as.numeric(tmax_list[[tile_num]][[d]])
      tmin <- as.numeric(tmin_list[[tile_num]][[d]])
    } else {
      tmax <- as.numeric(tmax_list[[d]])
      tmin <- as.numeric(tmin_list[[d]])
    }
    
    # Assign each raster cell to a Lifestage
    # These three matrices assign physiological parameters by cell
    ls_ldt <- stage_ldt[Lifestage]
    ls_udt <- stage_udt[Lifestage]
    ls_dd <- stage_dd_cohort[Lifestage]
    
    # Accumulate total DDs across year, using larvae Lifestage
    # Can not use DDaccum because this one has values reset when progress = 1
    ls_ldt_larv <- stage_ldt[which(stgorder == "L")]
    ls_udt_larv <- stage_udt[which(stgorder == "L")]

    # Calculate stage-specific degree-days for each cell per day
    if (calctype == "average") {
      dd_tmp <- AvgDD(tmax, tmin, ls_ldt, ls_udt)
      dd_tmp_larv <- AvgDD(tmax, tmin, ls_ldt_larv, ls_udt_larv)
      } else if (calctype == "triangle") {
        dd_tmp <- TriDD(tmax, tmin, ls_ldt, ls_udt)
        dd_tmp_larv <- TriDD(tmax, tmin, ls_ldt_larv, ls_udt_larv)
    }
    
    # Accumulate degree days
    DDaccum <- DDaccum + dd_tmp
    
    # Calculate total degree days for larvae
    DDtotal <- DDtotal + dd_tmp_larv
    
    # Climate stress exclusions - results will be same for all cohorts, 
    # so just calculate exclusions for cohort 1
    if (exclusions_stressunits) {
      # Cold stress accumulation
      # Make today's cold mask and calculate today's cold stress DDs
      coldmask <- tmin < coldstressTHRESH  
      coldstress <- coldmask * abs(coldstressTHRESH - tmin) 
      coldunitsCUM <- coldunitsCUM + coldstress
      # ASSUME NEW -2=severe -1=mod 0=none throughout
      coldEXCL <- Cond(coldunitsCUM >= coldstressMAX2, -2, 
                        Cond(coldunitsCUM >= coldstressMAX1, -1, 0))
      # Heat stress accumulation
      # Make today's heat mask and calculate today's heat stress DDs
      heatmask <- tmax > heatstressTHRESH  
      heatstress <- heatmask * abs(tmax - heatstressTHRESH) 
      heatunitsCUM <- heatunitsCUM + heatstress
      heatEXCL <- Cond(heatunitsCUM >= heatstressMAX2, -2, 
                       Cond(heatunitsCUM >= heatstressMAX1, -1, 0))
      AllEXCL <- Cond((coldEXCL == 0) & (heatEXCL == 0), 0,
                      Cond((coldEXCL == -1) & (heatEXCL >= -1),-1,
                           Cond((coldEXCL >= -1) & (heatEXCL == -1), -1, -2)))
    }
    
    # Calculate pest events
    # DOYs for when cumDDs > event threshold for a given generation
    if (pems) {
      
     # The OW pest event is a percentage of the OW stage DDs 
     # (e.g. OWEventP = 0.5 would be 50% of the OWlarvaeDD, or half-way 
     # through OWlarvaeDD). It is specified in the species param file.
      OWEventDD <- stage_dd_cohort[1] * OWEventP
      
      # Egg PEMs
      if (mapE == 1 & eggEventDD) {  
        if (PEMnumgens > 0) {
          if (owstage == "OE") {
          # If owstage = egg, then eggs finish developing after the DD of OWegg 
          # for that cohort (do NOT use OWeggDD from param file - this is for 
          # DDRP v2)
          PEMe0 <- Cond(PEMe0 == 0 & NumGen == 0 & (DDaccum >= OWEventDD), 
                          d * (Lifestage == which(stgorder == "OE")), PEMe0)
          }
          # Egg DOYs for when cumDDs > eggEvent threshold 1st gen
          PEMe1 <- Cond(PEMe1 == 0 & NumGen == 1 & (DDaccum >= eggEventDD), 
                        d * (Lifestage == which(stgorder == "E")), PEMe1) 
        }
        if (PEMnumgens > 1) {
          # Egg DOYs for when cumDDs > eggEvent threshold 2nd gen
          PEMe2 <- Cond(PEMe2 == 0 & NumGen == 2 & (DDaccum >= eggEventDD), 
                        d * (Lifestage == which(stgorder == "E")), PEMe2)
        }
        if (PEMnumgens > 2) {
          # Egg DOYs for when cumDDs > eggEvent threshold 3rd gen
          PEMe3 <- Cond(PEMe3 == 0 & NumGen == 3 & (DDaccum >= eggEventDD), 
                        d * (Lifestage == which(stgorder == "E")), PEMe3) 
        }
        if (PEMnumgens > 3) {
          # Egg DOYs for when cumDDs > eggEvent threshold 4th gen
          PEMe4 <- Cond(PEMe4 == 0 & NumGen == 4 & (DDaccum >= eggEventDD), 
                        d * (Lifestage == which(stgorder == "E")), PEMe4) 
        }
      }
      
      # Larvae PEMs
      if (mapL == 1 & larvaeEventDD) {
        if (PEMnumgens > 0) {
          if (owstage == "OL") {
          # If owstage = larvae, then larvae finish developing after the DD of 
          # OWlarvae for that cohort (do NOT use OWlarvaeDD from species
          # param file - this is for DDRP v2 only)
            PEMl0 <- Cond(PEMl0 == 0 & NumGen == 0 & (DDaccum >= OWEventDD), 
                          d * (Lifestage == which(stgorder == "OL")), PEMl0) 
          # If owstage = egg, then larvae of this OW gen will have to go through 
          # full development 
          } else if (owstage %in% c("OE")) {
            PEMl0 <- Cond(PEMl0 == 0 & NumGen == 0 & (DDaccum >= larvaeEventDD), 
                          d * (Lifestage == which(stgorder == "L")), PEMl0) 
          }
          # Larvae DOYs for when cumDDs > larvaeEvent threshold 1st gen
          PEMl1 <- Cond(PEMl1 == 0 & NumGen == 1 & (DDaccum >= larvaeEventDD), 
                        d * (Lifestage == which(stgorder == "L")), PEMl1) 
        }
        if (PEMnumgens > 1) {
          # Larvae DOYs for when cumDDs > larvaeEvent threshold 2nd gen
          PEMl2 <- Cond(PEMl2 == 0 & NumGen == 2 & (DDaccum >= larvaeEventDD), 
                        d * (Lifestage == which(stgorder == "L")), PEMl2) 
        }
        if (PEMnumgens > 2) {
          # Larvae DOYs for when cumDDs > larvaeEvent threshold 3rd gen
          PEMl3 <- Cond(PEMl3 == 0 & NumGen == 3 & (DDaccum >= larvaeEventDD), 
                        d * (Lifestage == which(stgorder == "L")), PEMl3) 
        }
        if (PEMnumgens > 3) {
          # Larvae DOYs for when cumDDs > larvaeEvent threshold 4th gen
          PEMl4 <- Cond(PEMl4 == 0 & NumGen == 4 & (DDaccum >= larvaeEventDD), 
                        d * (Lifestage == which(stgorder == "L")), PEMl4) 
         }
      }
      
      # Pupae PEMs
      if (mapP == 1 & pupaeEventDD) {
        if (PEMnumgens > 0) {
          if (owstage == "OP") {
            # If owstage = pupae, then pupae finish developing after the DD of 
            # OWpupae for that cohort (do NOT use OWpupaeDD from species param 
            # file - this is for DDRP v2)
            PEMp0 <- Cond(PEMp0 == 0 & NumGen == 0 & (DDaccum >= OWEventDD), 
                          d * (Lifestage == which(stgorder == "OP")), PEMp0)
            } else if (owstage %in% c("OE", "OL")) {
            # If owstage = egg or larvae, then pupae of the OW gen will have to 
            # go through full development 
            PEMp0 <- Cond(PEMp0 == 0 & NumGen == 0 & (DDaccum >= pupaeEventDD), 
                          d * (Lifestage == which(stgorder == "P")), PEMp0) 
            }  
          # Pupae DOYs for when cumDDs > pupaeEvent threshold 1st gen
          PEMp1 <- Cond(PEMp1 == 0 & NumGen == 1 & (DDaccum >= pupaeEventDD), 
                        d * (Lifestage == which(stgorder == "P")), PEMp1) 
        }
        if (PEMnumgens > 1) {
          # Pupae DOYs for when cumDDs > pupaeEvent threshold 2nd gen
          PEMp2 <- Cond(PEMp2 == 0 & NumGen == 2 & (DDaccum >= pupaeEventDD), 
                        d * (Lifestage == which(stgorder == "P")), PEMp2) 
        }
        if (PEMnumgens > 2) {
          # Pupae DOYs for when cumDDs > pupaeEvent threshold 3rd gen
          PEMp3 <- Cond(PEMp3 == 0 & NumGen == 3 & (DDaccum >= pupaeEventDD), 
                        d * (Lifestage == which(stgorder == "P")), PEMp3) 
        }
        if (PEMnumgens > 3) {
          # Pupae DOYs for when cumDDs > pupaeEvent threshold 4th gen
          PEMp4 <- Cond(PEMp4 == 0 & NumGen == 4 & (DDaccum >= pupaeEventDD), 
                        d * (Lifestage == which(stgorder == "P")), PEMp4) 
        }
      }
      
      # Adult PEMs
      if (mapA == 1 & adultEventDD) {
        if (PEMnumgens > 0) {
          if (owstage == "OA") {
            # If owstage = adult, then adults finish developing after the DD of
            # OWadult for that cohort (do NOT use OWadultDD from species param 
            # file - this is for DDRP v2)
            PEMa0 <- Cond(PEMa0 == 0 & NumGen == 0 & (DDaccum >= OWEventDD), 
                          d * (Lifestage == which(stgorder == "OA")), PEMa0) 
            } else if (owstage %in% c("OL", "OP")) {
            # If owstage = larvae or pupae, then adults of the OW gen will 
            # have to go through full development 
            PEMa0 <- Cond(PEMa0 == 0 & NumGen == 0 & (DDaccum >= adultEventDD), 
                          d * (Lifestage == which(stgorder == "A")), PEMa0) 
            }
          # Adult DOYs for when cumDDs > adultEvent threshold 1st gen
          PEMa1 <- Cond(PEMa1 == 0 & NumGen == 1 & (DDaccum >= adultEventDD), 
                        d * (Lifestage == which(stgorder == "A")), PEMa1) 
        } 
        if (PEMnumgens > 1) {
          # Adult DOYs for when cumDDs > adultEvent threshold 2nd gen
          PEMa2 <- Cond(PEMa2 == 0 & NumGen == 2 & (DDaccum >= adultEventDD), 
                        d * (Lifestage == which(stgorder == "A")), PEMa2) 
        }
        if (PEMnumgens > 2) {
          # Adult DOYs for when cumDDs > adultEvent threshold 3rd gen
          PEMa3 <- Cond(PEMa3 == 0 & NumGen == 3 & (DDaccum >= adultEventDD), 
                        d * (Lifestage == which(stgorder == "A")), PEMa3) 
        }
        if (PEMnumgens > 3) {
          # Adult DOYs for when cumDDs > adultEvent threshold 4th gen
          PEMa4 <- Cond(PEMa4 == 0 & NumGen == 4 & (DDaccum >= adultEventDD), 
                        d * (Lifestage == which(stgorder == "A")), PEMa4) 
        }
      }
      
    }
    
    # Calculate Lifestage progression: Is accumulation > Lifestage requirement 
    # (0 = FALSE, 1 = TRUE)? If species has obligate diapause, then don't 
    # allow it to progress past the 1st gen overwintering stage.
    if (exists("obligate_diapause")) {
      if (obligate_diapause == 1) {
        if (owstage == "OL") {
          progress <- Cond(NumGen <= 1 & Lifestage != which(stgorder == "L"), 
                           as.integer(DDaccum >= ls_dd), 0)
          } else if (owstage == "OP") {
            progress <- Cond(NumGen <= 1 & Lifestage != which(stgorder == "P"), 
                             as.integer(DDaccum >= ls_dd), 0)
          } else if (owstage == "OA") {
            progress <- Cond(NumGen <= 1 & Lifestage != which(stgorder == "A"), 
                             as.integer(DDaccum >= ls_dd), 0)
          } else if (owstage == "OE") {
            progress <- Cond(NumGen <= 1 & Lifestage != which(stgorder == "E"), 
                             as.integer(DDaccum >= ls_dd), 0)
        }
      }
    # If no obligate diapause, then there are no limits on no. of generations
    } else {
      progress <- as.integer(DDaccum >= ls_dd)
    }
    
    # If reproductive adult stage progresses, then that cell has oviposition 
    # and the generation count increases. If species has OW adults, then need 
    # to change stage value to "adult" to allow NumGen to increase when it 
    # progresses. "OA" = 1, and "adult" = 5 for species with OW adults
    if (owstage == "OA") {
      Lifestage2 <- Lifestage
      Lifestage2[Lifestage2 == 1] <- 5
      NumGen <- NumGen + (progress == 1 & Lifestage2 == 5)
    } else {
      # Value for "adult" varies depending on OW stage
      NumGen <- NumGen + (progress == 1 & Lifestage == which(stgorder == "A")) 
    }
    #cat("No. gen (max): ", max(NumGen, na.rm=T), "\n", file=daily_logFile, 
    #append=TRUE) 
    
    # If progress is 1, then there is progression to the next life stage
    Lifestage <- Lifestage + progress
    
    # Reset the DDaccum cells to zero for cells that progressed to next stage
    DDaccum <- DDaccum - (progress * ls_dd)
    #cat("DDaccum (max): ", max(DDaccum, na.rm=T), "\n", file=daily_logFile, 
    #append=TRUE)
    
    # Reassign cells that progressed past end of stgorder to first non-OW stage
    Lifestage <- Cond(Lifestage == (length(stgorder) + 1), 2, Lifestage)
    #cat("Lifestage (max): ", max(Lifestage, na.rm=T), file=daily_logFile, 
    #append=TRUE)
    
    #### * Save data for certain days, specified by sampling frequency ####
    # Data from last sampling day of year is also saved
    if (sublist[d] %in% sample_pts) {
      #cat("sampled", sublist[d])
      # Convert Lifestage and Numgen matrices to rasters and put into a brick
      mat_list <- list(Lifestage, NumGen, DDtotal)
      ext <- as.data.frame(as.matrix(extent(template)))
      rast_list <- lapply(mat_list, Mat_to_rast, ext = ext, template = template)
      names(rast_list) <- c("Lifestage_rast", "NumGen_rast", "DDtotal_rast")
      
      #cat("\n\n### Adding layers to Lifestage brick for cohort", cohort, ": 
      #doy =", sublist[d], "\n", file=daily_logFile, append=TRUE)
      # Lifestage brick
      if (!exists("Lifestage_brick")) {
        Lifestage_brick <- brick(rast_list$Lifestage_rast, crs = crs)
      } else {
        Lifestage_brick <- addLayer(Lifestage_brick, rast_list$Lifestage_rast)
      }
      
      #cat("### Adding layers to NumGen brick for cohort", cohort, ": doy =", 
      #sublist[d], "\n", file=daily_logFile, append=TRUE)
      # NumGen brick
      if (!exists("NumGen_brick")) {
        NumGen_brick <- brick(rast_list$NumGen_rast, crs = crs)
      } else {
        NumGen_brick <- addLayer(NumGen_brick, rast_list$NumGen_rast)
      }
      
      # Only need to do DDtotal brick for a single cohort, because it will be 
      # the same across all cohorts
      if (cohort == 1) {
        #cat("### Adding layers to DDtotal brick for cohort", cohort, ": doy =",
        # sublist[d], file=daily_logFile, append=TRUE)
        if (!exists("DDtotal_brick")) {
          DDtotal_brick <- brick(rast_list$DDtotal_rast, crs = crs)
        } else {
          DDtotal_brick <- addLayer(DDtotal_brick, rast_list$DDtotal_rast)
        }
      }
      
      rm(rast_list) # Free up memory
      
      # If exclusions_stressunits = 1, then do the same thing for LifestageEXCL 
      # and NumGenEXCL, after they are calculated
      if (exclusions_stressunits) {

        # Do the same for cold/heat units and cold/heat exclusion, but just 
        # for cohort 1, because results will be same for all cohorts
        if (cohort == 1) {
          # Convert matrices to rasters and put them into a raster brick
          mat_list3 <- list(coldunitsCUM, coldEXCL, heatunitsCUM,
                            heatEXCL, AllEXCL)
          ext <- as.data.frame(as.matrix(extent(template)))
          rast_list3 <- lapply(mat_list3, Mat_to_rast, ext = ext, 
                               template = template)
          names(rast_list3) <- c("coldunitsCUM_rast", "coldEXCL_rast", 
            "heatunitsCUM_rast", "heatEXCL_rast", "AllEXCL_rast")
          
          #cat("\n### Adding layers to Cold Stress Units brick for cohort", 
          #cohort, ": doy =", sublist[d], "\n", file=daily_logFile, append=TRUE)
          # Cold stress unit accumulation brick
          if (!exists("coldunitsCUM_brick")) {
            coldunitsCUM_brick <- brick(rast_list3$coldunitsCUM_rast, 
                                         crs = crs)
          } else {
            coldunitsCUM_brick <- addLayer(coldunitsCUM_brick, 
                                            rast_list3$coldunitsCUM_rast)
          }
          
          #cat("### Adding layers to Cold Stress Exclusion brick for cohort", 
          #cohort, ": doy =", sublist[d], "\n", file=daily_logFile, append=TRUE)
          # Cold stress exclusion brick
          if (!exists("coldEXCL_brick")) {
            coldEXCL_brick <- brick(rast_list3$coldEXCL_rast, crs = crs)
          } else {
            coldEXCL_brick <- addLayer(coldEXCL_brick, 
                                        rast_list3$coldEXCL_rast)
          }
          
          #cat("### Adding layers to Heat Stress Units brick for cohort", 
          #cohort, ": doy =", sublist[d], "\n", file=daily_logFile, append=TRUE)
          # Heat stress unit accumulation brick
          if (!exists("heatunitsCUM_brick")) {
            heatunitsCUM_brick <- brick(rast_list3$heatunitsCUM_rast, crs = crs)
          } else {
            heatunitsCUM_brick <- addLayer(heatunitsCUM_brick, 
                                           rast_list3$heatunitsCUM_rast)
          }
          
          #cat("### Adding layers to Heat Stress Exclusion brick for cohort", 
          #cohort, ": doy =", sublist[d], "\n", file=daily_logFile, append=TRUE)
          # Heat stress exclusion brick
          if (!exists("heatEXCL_brick")) {
            heatEXCL_brick <- brick(rast_list3$heatEXCL_rast, crs = crs)
          } else {
            heatEXCL_brick <- addLayer(heatEXCL_brick, rast_list3$heatEXCL_rast)
          }
          
          #cat("### Adding layers to All Stress Exclusion brick for cohort",
          # cohort, ": doy =", sublist[d], file=daily_logFile, append=TRUE)
          # All stress exclusion brick (cold stress + heat stress exclusions)
          if (!exists("AllEXCL_brick")) {
            AllEXCL_brick <- brick(rast_list3$AllEXCL_rast, crs = crs)
          } else {
            AllEXCL_brick <- addLayer(AllEXCL_brick, rast_list3$AllEXCL_rast)
          }
          
          rm(rast_list3) # Free up memory
        }
      }
    }
   }#,
  # error = function(e) {
  #     # Add error message to the error log file
  #     write(toString(e), msg, append=TRUE)
  # }
  # )
  
  ### * Daily loop done - save raster bricks ###
  # Save non-optional raster bricks = Lifestage and NumGen
  SaveRaster(Lifestage_brick, cohort, tile_num, "Lifestage", "INT1U")
  SaveRaster(NumGen_brick, cohort, tile_num, "NumGen", "INT1U")

  # Save DDtotal brick (only for cohort 1)
  if (cohort == 1) {
    SaveRaster(DDtotal_brick, cohort, tile_num, "DDtotal", "INT2S")
  }
  
  # Free up memory
  rm(Lifestage_brick, NumGen_brick, DDtotal_brick)
  
  # If Pest Event Maps are specified (pems = 1), then convert PEM matrices 
  # to rasters and save them
  if (pems) {
    pem_list <- mget(ls(pattern = "PEMe|PEMl|PEMp|PEMa"))
    # Convert each matrix in the list to a raster and save it
    for (i in 1:length(pem_list)) {
      pem_mat <- pem_list[[i]]
      pem_rast <- Mat_to_rast(pem_mat, ext, template)
      if (region_param %in% c("CONUS", "EAST")) {
        SaveRaster(pem_rast, cohort, tile_num, names(pem_list[i]), "INT2U")
      } else {
        SaveRaster(pem_rast, cohort, NA, names(pem_list[i]), "INT2U")
      }
    }
  }
  
  rm(list = ls(pattern = "PEM|pem_list|pem_rast")) # Free up memory
  
  # If exclusions_stressunits = 1, then save stress unit and exclusions bricks
  if (exclusions_stressunits) {

    # Cold and heat stress unit and exclusion bricks will be the same for 
    # all cohorts, so take only 1st one
    if (cohort == 1) {
      stress_excl_brick_list <- c(coldunitsCUM_brick, coldEXCL_brick,
        heatunitsCUM_brick, heatEXCL_brick, AllEXCL_brick)
      names(stress_excl_brick_list) <- c("Cold_Stress_Units", 
        "Cold_Stress_Excl", "Heat_Stress_Units", "Heat_Stress_Excl", 
        "All_Stress_Excl")
      
      # Save each raster brick product in the list
      for (i in 1:length(stress_excl_brick_list)) {
        brk <- stress_excl_brick_list[[i]]  
        SaveRaster(brk, cohort, tile_num, 
                   paste0(names(stress_excl_brick_list[i])), "INT2S")
      
      }
      
      # Free up memory
      rm(stress_excl_brick_list, coldunitsCUM_brick, coldEXCL_brick,
         heatunitsCUM_brick, heatEXCL_brick, AllEXCL_brick) 
      
    }
    #cat("\n### Finished climate exclusions and stress units raster output 
    #cohort", cohort, "###\n\n", file = Model_rlogging, append=TRUE)
  }
  
  # Remove .xml files generated w/ .tif files for certain raster bricks
  # Haven't yet figured out a way to prevent these from being created. The
  # only solution I have found is to change a GDAL setting.
  delfiles <- dir(pattern = "*xml")
  suppressWarnings(file.remove(delfiles))
  gc() # Clear items from the environment - is this necessary?
  
}


#### (11). Degree Day calculation methods #### 
# Equations used to calculate degree-days
# tmax = max. temp. data; tmin = min. temp data; LDT = lower developmental 
# temperature threshold; UDT = upper developmental temperature threshold

# Averaging DD Calc method (max + min/2 - tlow) but with horizontal 
# (substitution) upper threshold:
AvgDD <- function(tmax, tmin, LDT, UDT) {
  return(Cond(tmax < LDT, 0, Cond(tmin > UDT, 0, 
                                  Cond(tmax > UDT, (UDT + tmin)/2 - LDT, 
                                       Cond((tmax + tmin)/2 - LDT < 0, 0, 
                                            (tmax + tmin)/2 - LDT)))))
}

# Single triangle with upper threshold (Sevachurian et al. 1977) - 
# also a good substitution for the single sine method
TriDD <- function(tmax, tmin, LDT, UDT) {
  tmax <- Cond(tmax == tmin, tmax + 0.01, tmax)
  Tmp1 = 6 * ((tmax - LDT) * (tmax - LDT))/(tmax - tmin)
  Tmp2 = 6 * ((tmax - UDT) * (tmax - UDT))/(tmax - tmin)
  Cond(tmax < LDT, 0,
       Cond(tmin >= UDT, UDT - LDT,
            Cond((tmax < UDT) & (tmin <= LDT), Tmp1/12,
                 Cond((tmin <= LDT) & (tmax >= UDT), (Tmp1 - Tmp2)/12,
                      Cond((tmin > LDT) & (tmax >= UDT), 
                           6 * (tmax + tmin - 2 * LDT)/12 - (Tmp2/12),
                           Cond((tmin > LDT) & (tmax < UDT), 
                                6 * (tmax + tmin - 2 * LDT)/12, 0))))))
} 

#### (12). ExtractBestPRISM: get best PRISM/NMME file from directory ####
# Take .bil files from PRISM or NMME (= forecast_data) yearly directories 
# (= files). The function returns the best data for each day. If data are 
# from a leap year, then leap day data may be removed or kept (= keep_leap).
ExtractBestPRISM <- function(files, forecast_data, keep_leap) {
  numsplits <- str_count(string = files[1], pattern = "/")
  pfile <- str_split(string = files, pattern = coll("/"), numsplits) %>% 
    purrr::map(numsplits)
  qa <- str_split(string = pfile, pattern = coll("_"), 6) %>% purrr::map(3) %>% 
    unlist()

  # Extract dates - the dates will always be the largest number, so sort and
  # then take first element of vector
  dates <- regexpr(pattern = "[0-9]{8}", text = files)
  df <- data.frame(dates = regmatches(files, dates),
                   quality = substr(qa, start = 1, stop = 4),
                   rownum = 1:length(qa))
  
  # Added to Tyson's version of this function - ability to choose PRISM vs. 
  # NMME for future dates
  if (forecast_data == "PRISM") {
    df <- mutate(df, rank = ifelse(quality == "stab", 1, 
                            ifelse(quality == "prov", 2, 
                            ifelse(quality == "earl", 3, 
                            ifelse(quality == "10yr", 4, 
                            ifelse(quality == "30yr", 5, 
                            ifelse(quality == "nmme", 6, NA)))))))
  } else if (dat == "NMME") {
    df <- mutate(df, rank = ifelse(quality == "stab", 1, 
                            ifelse(quality == "prov", 2, 
                            ifelse(quality == "earl", 3, 
                            ifelse(quality == "nmme", 4, 
                            ifelse(quality == "10yr", 5, 
                            ifelse(quality == "30yr", 6, NA)))))))
  }

  # Sorting backwards matches data hierarchy
  # If PRISM, then stable > provisional > early > 10yr > 30yr > nmme
  # If NMME, then stable > provisional > early > nmme > 10yr > 30yr
  df2 <- df %>% group_by(dates) %>% 
    dplyr::filter(rank == min(rank)) %>%
    dplyr::filter(1:n() == 1)

  best <- files[df2$rownum]
  dates <- regexpr(pattern = "[0-9]{8}", text = best)
  
  fileorder <- order(regmatches(best, dates))
  files <- best[fileorder]
  
  # Remove leap day (2/29) if the start_year is not a leap year, or if it is 
  # a leap year but user wants it removed (keep_leap == 0). This doesn't apply 
  # if 30 yr average data or other non-specific year data are being used
  # (in this case, start_year is non-numeric)
  if (is.numeric(start_year)) {
    if (start_year %% 4 != 0 | start_year %% 4 == 0 & keep_leap == 0) {
      files <- files[!grepl(paste0(start_year, "0229"), files)]
    }
  }
  
  return(files)
}

#### (13). Mat_to_rast: matrix to raster conversion ####
# Converts a matrix (= m) to a raster, which involves specifying the extent 
# (= ext) from the template (= template), setting the coordinate system, and 
# assigning it a spatial resolution (from the template)
Mat_to_rast <- function(m, ext, template) {
  rast <- raster(m, xmn = ext[1,1], xmx = ext[1,2], 
                    ymn = ext[2,1], ymx = ext[2,2])
  crs <- crs(template)
  res(rast) <- res(template)
  NAvalue(rast) <- NaN
  return(rast)
}

#### (14). PlotMap: summary map plotting - main ####
# A VERY large function that generates summary plots for all products generated
# in the DDRP model run
# r = raster input, d = date, lgd = legend title, outfl = outfile name
# TO DO: clean up code - remove redundancies
PlotMap <- function(r, d, titl, lgd, outfl) {
  
  # If data are in raster format, then convert to a data frame
  if (grepl("NumGen|Adult_byGen|Adult_Excl1_byGen|Adult_Excl2_byGen|StageCount", 
            outfl)) {
    df <- r # data are already in a data frame
  } else {
    df <- ConvDF(r) # convert raster to data frame
  }
  
  # Shift x and y grid cells by 1 grid interval (res = 0.0417) because for some
  # reason the states boundary does not line up quite right
  #df$x <- df$x + 0.0417
  #df$y <- df$y + 0.0417
  
  # Format all labels used for the plot
  sp <- paste0(fullname, ":") # Species full name
  dat <- as.character(format(strptime(d, format = "%Y%m%d"), 
                             format = "%m/%d/%Y")) # Date
  titl_orig <- titl # Used for some log captions
  
  # Plot title - if it's not a PEM, then put sampling date in title
  if (!grepl("Avg|Earliest", outfl)) {
    titl <- paste(titl, dat, sep = " ")
  }
  
  if (grepl("PEM", outfl)) {
    start_year <- strtrim(d, 4) # if using 30 yr data, need to trim other chars
    titl <- paste(titl, start_year, sep = " ")
  }
  
  # Subtitle (same for all plots)
  subtitl <- paste("Maps and modeling",
    format(Sys.Date(), "%m/%d/%Y"), str_wrap("by Oregon State University IPPC 
    USPEST.ORG and USDA-APHIS-PPQ; climate data from OSU PRISM Climate Group",
    width = 150))
  
  # Need to enforce a rule for wrapping title and subtitle on plot
  # The title and subtitle go off of page for some states (VT...any else?)
  if (asp > 1.55) {
    titl_width <- 45
    subtitl_width <- 55
  } else if (asp >= 1.4 & asp < 1.55) {
      titl_width <- 55
      subtitl_width <- 65
  } else {
    titl_width <- 65
    subtitl_width <- 75
  }
  
  # Code for plots will vary depending on the product type, as specified below
  #
  #### * DDtotal ####
  if (grepl("DDtotal", outfl)) {
    # Caption for log file
    log_capt <- paste("- Number of accumulated degree-days on", 
                         format(as.Date(d, "%Y%m%d"), "%m/%d/%Y"))
    
    # Create plot separately for rasters where all DDtotal values = 0  
    if (all(df$value == 0)) {
      df$value <- factor(df$value)
      # If there are any non-zero values, then cut values into bins
    } else {
      df <- Cut_bins(df, 10)  
      df$value <- factor(df$value, 
                         levels = unique(df$value[order(df$value_orig)]))

    }
    
    p <- Base_map(df) +       
      scale_fill_brewer(palette = "Spectral", direction = -1, 
                        name = paste0(lgd)) +
      labs(title = str_wrap(paste(sp, titl), width = titl_width), 
            subtitle = str_wrap(paste(subtitl), width = subtitl_width)) +
      theme_map(base_size = base_size) + 
      mytheme
    
    #### * Lifestage relative pop size ####
  } else if (grepl("OWegg|OWlarvae|OWpupae|OWadult|Egg|Larvae|Pupae|Adult",
                   outfl) & !grepl("byGen", outfl)) {
    # Caption for log file
    # TO DO: some log captions get placed in the wrong line because the 
    # function is run in parallel. Not sure why this is happening only for 
    # Lifestage and not other output types, but maybe because there are 
    # nested parallel processes.
    log_capt <- paste("-", titl_orig, "on", format(as.Date(d, "%Y%m%d"), 
                                                      "%m/%d/%Y"))
    df$value_orig <- df$value # Keep original values for sorting later
    
    # If data include climate exclusions, then format accordingly
    # Need to remove -2 and -1 values prior to binning values for plot
    if (exclusions_stressunits) {
      if (any(df$value_orig >= 0)) {
        df2 <- Cut_bins2(dplyr::filter(df, !value < 0))
 
        # Filter out climate stress exclusion values
        excl_df <- df %>% dplyr::filter(value < 0) # Take only -2 and -1 values
        excl_df$value_orig <- excl_df$value
        excl_df <- mutate(excl_df, value = ifelse(value == -2, "excl.-severe", 
                      ifelse(value == -1, "excl.-moderate", value)))
        
        # Put exlcusion values and Lifestage rel. pop size (binned) values back
        # together, then order by original values so plots in numerical order
        df3 <- rbind(excl_df, df2) 
        df <- df3 # Rename data frame
          
        # If clim. exclusions masks out all non-zero values, then just plot 
        # climate stress exclusions
      } else if (all(df$value_orig < 0)) {
          df <- mutate(df, value = ifelse(value == -2, "excl.-severe", 
                                  ifelse(value == -1, "excl.-moderate", value)))
      }
      
      # If stress values are missing in data, then add a row so the legend
      # still shows the stress category (Excl1 = excl.-severe, Excl2 = 
      # excl.-severe and excl.-moderate). Otherwise just recode stress values.
      if (grepl("Excl1", outfl) & (!(-2 %in% df$value_orig))) {
        df <- df %>% 
          add_row(value = "excl.-severe", x = NA, y = NA, value_orig = -2)
      } else if (grepl("Excl2", outfl)) {
        if (!(-2 %in% df$value_orig)) {
          df <- df %>% 
              add_row(value = "excl.-severe", x = NA, y = NA, value_orig = -2)
          }
          if (!(-1 %in% df$value_orig)) {
            df <- df %>% 
              add_row(value = "excl.-moderate", x = NA, y = NA, value_orig = -1)
          }
        }
   
    # If there are no climate stress exclusions
    } else {
      df <- Cut_bins2(df)
    }
  
    # Make legend color keys and assign colors
    if (any(df$value_orig >= 0)) {
      
      # Generate the color key
      col_key <- data.frame(cols = c(colorRampPalette(c("dodgerblue3", "yellow", 
                                                        "red3"))(10)),
                            value_low = seq(0, 90, 10),
                            value_high = seq(10, 100, 10))
      col_key <- col_key %>% 
        mutate(value = paste(value_low, value_high, sep = "-")) %>%
        dplyr::select(cols, value)
      
      # Create "fake" data rows if data are missing any bins so that key always 
      # shows 10 bins (0-10, 10-20, 20-30, etc.). Only bins below the max value
      # in data are kept.
      missing <- col_key %>% dplyr::select(value) %>%
        filter(!(value %in% df$value)) %>%
        filter(value < max(df$value)) %>% 
        mutate(x = NA, y = NA,
            value_orig = as.numeric(str_split_fixed(value, "-", 2)[,1], value))
      df <- rbind(df, missing)
      df$value <- factor(df$value, levels = 
                               unique(df$value[order(df$value_orig)]))
      
      # Need to add grayscale cols to color key if there are
      # climate stress exclusions (grayscale colors)
      if (exclusions_stressunits) {
        if (any(df$value == "excl.-moderate")) {
          col_key <- rbind(data.frame(cols = "gray70",
                                      value = "excl.-moderate"), col_key)
        }
        if (any(df$value == "excl.-severe")) {
          col_key <- rbind(data.frame(cols = "gray30",
                                      value = "excl.-severe"), col_key)
        }
      }
      
      # If all values are <= 0, then just show climate exclusions (grayscale)
    } else if (all(df$value_orig < 0)) {
      col_key <- data.frame(cols = c("gray30", "gray70", "dodgerblue3"), 
                            value = c("excl.-severe", "excl.-moderate", "0"))
      col_key <- semi_join(col_key, df, by = "value") # keep only vals in data
    }
    
    # Convert legend color key to a vector and make the plot
    cols <- setNames(as.character(col_key$cols), col_key$value)
    
    p <- Base_map(df) +       
      scale_fill_manual(values = cols, name = paste0(lgd)) +
      labs(title = str_wrap(paste(sp, titl), width = titl_width), 
           subtitle = str_wrap(subtitl, width = subtitl_width)) +
      theme_map(base_size = base_size) + 
      mytheme

    #### * Number of generations ####
  } else if (grepl("NumGen|NumGen_Excl1|NumGen_Excl2", outfl)) {
    
    # Caption for logging file
    if (grepl("NumGen_Excl1|NumGen_Excl2", outfl)) { 
      log_capt <- paste("-", str_wrap("Number of gens. with climate stress excl. 
                                      on", width = 80),
                        format(as.Date(d, "%Y%m%d"), "%m/%d/%Y"))
    } else {
      log_capt <- paste("-", "Numbers of generations on", 
                        format(as.Date(d, "%Y%m%d"), "%m/%d/%Y"))
    }
    
    # Extract info on how many generations have been completed to date. This
    # makes it possible to see all completed gens in the legend key even if 
    # they're not present on a given date.
    if (any(df$value >= 0)) {
      all_gens <- c(0:max(df$gen))
      gens_df <- data.frame("value" = paste(all_gens, "gens."), "x" = NA, 
                          "y" = NA, "gen" = all_gens)
    }
    
    # If only odd gens are plotted then need to remove even gens.
    if (odd_gen_map == 1) {
      gens_df <- gens_df %>% filter(gen %% 2 != 0)
    }
    
    # Format values that are not climate stress exclusions (gen >= 0)
    # Rel. pop sizes are grouped into 5 bins
    df2 <- df %>% filter(value >= 0) %>% # Remove clim. stress values
      mutate(value = ifelse(value < 20, 0, ifelse(value < 40, 20, 
                            ifelse(value < 60, 40, ifelse(value < 80, 60, 
                            ifelse(value < 100, 80, value))))),
             value = paste(gen, "gens.:", value))
    
    if (any(df$value >= 0)) {
      df2 <- rbind(df2, gens_df) # Add on all completed generations to date
    }
    
    # Extract and back in climate stress exclusion values
    excl_df <- df %>% dplyr::filter(value < 0) %>% 
      mutate(value = ifelse(value == -2, "excl.-severe", 
                                ifelse(value == -1, "excl.-moderate", value)))
    df <- rbind(excl_df, df2)
    
    # If stress values are missing in data, then add a row ("fake" data)
    # so the legend still shows the stress category (Excl1 = excl.-severe, 
    # Excl2 = excl.-severe and excl.-moderate).
    if (grepl("Excl1", outfl) & (!("excl.-severe" %in% df$value))) {
      df <- df %>% add_row(value = "excl.-severe", x = NA, y = NA, gen = -2)
    } else if (grepl("Excl2", outfl)) {
      if (!("excl.-severe" %in% df$value)) {
        df <- df %>% add_row(value = "excl.-severe", x = NA, y = NA, gen = -2)
      }
      if (!("excl.-moderate" %in% df$value)) {
        df <- df %>% 
          add_row(value = "excl.-moderate", x = NA, y = NA, gen = -1)
      }
    }
    
    # Order by original values so plots in numerical order
    df$value <- factor(df$value, 
                        levels = unique(df$value[order(df$gen)]))
    
    # Make the color key for the legend, rel. pop. size in bins of 20 (5 total)
    # Currently enough colors for 20 generations
    cols_df <- data.frame("cols" = 
      c(Colfunc("deepskyblue", "blue3", 5), # Gen 0 
        Colfunc("orangered", "firebrick4", 5), # Gen 1
        Colfunc("yellow", "gold3", 5), # Gen 2
        Colfunc("lightgreen", "darkgreen", 5), # Gen 3
        Colfunc("magenta", "magenta4", 5), # Gen 4
        Colfunc("tan1", "darkorange3", 5), # Gen 5
        Colfunc("cyan", "cyan4", 5), # Gen 6
        Colfunc("greenyellow", "chartreuse4", 5), # Gen 7
        Colfunc("mediumpurple1", "purple3", 5), # Gen 8
        Colfunc("lightgoldenrod", "gold4", 5), # Gen 9
        Colfunc("cadetblue1", "cornflowerblue", 5), # Gen 10
        Colfunc("mistyrose", "palevioletred2", 5), # Gen 11
        Colfunc("seagreen1", "seagreen4", 5), # Gen 12
        Colfunc("lemonchiffon", "gold", 5), # Gen 13
        Colfunc("red", "darkred", 5), # Gen 14
        Colfunc("lightpink", "deeppink3", 5), # Gen 15
        Colfunc("lightgreen", "darkgreen", 5), # Gen 16
        Colfunc("sienna", "sienna4", 5), # Gen 17
        Colfunc("maroon1", "maroon4", 5), # Gen 18
        Colfunc("royalblue1", "royalblue4", 5), # Gen 19
        Colfunc("thistle1", "thistle4", 5))) # Gen 20
    bins_df <- data.frame(gen = c(rep(0, 5), rep(1, 5), rep(2, 5),
        rep(3, 5), rep(4, 5), rep(5, 5), rep(6, 5), rep(7, 5), rep(8, 5), 
        rep(9, 5), rep(10, 5), rep(11, 5), rep(12, 5), rep(13, 5), rep(14, 5),
        rep(15, 5), rep(16, 5), rep(17, 5), rep(18, 5), rep(19, 5), 
        rep(20, 5)))
    col_key <- cbind(cols_df, bins_df) %>% 
      add_row(cols = c("gray70", "gray30"), gen = c(-1, -2))

    # Remove unneded colors, create bins of 5 to represent rel. pop. size. 
    # For legend, show only one color for each generation (map will have a 
    # gradation of this color). This is only done if there are values other
    # than climate stress exclusions. The color for rel. pop. size = 60 is used.
    # Add on a color for the "other stages" category (non-adult stages) - this
    # is coded as -0.1 in order to properly sort it in the legend key.
    if (any(df$gen >= 0)) {
      col_key2 <- suppressWarnings(semi_join(col_key, df, by = "gen"))
      num_gens <- length(unique(col_key2$gen)) # how many unique gens in data?
      col_key2 <- col_key2 %>% 
        mutate(value = ifelse(gen >= 0, rep(c(0, 20, 40, 60, 80), 
                                          num_gens), gen), # 5 bins
             value = ifelse(gen >= 0, paste(gen, "gens.:", value),
                            ifelse(gen == -1, "excl.-moderate", 
                                   ifelse(gen == -2, "excl.-severe", NA))))
      
    # For legend, show only one color for each generation 
    # (map will have a gradation of this color)
    lgnd_cols <- col_key2 %>% dplyr::filter(grepl(": 60", value))
    lgnd_cols$value <- str_split_fixed(lgnd_cols$value, 
                                    pattern = ":", 2)[,1] # Which gen?
    col_key2 <- rbind(col_key2, lgnd_cols)
    
    # Breaks to use in plotting function, so only one shade per gen 
    # is shown in legend. Need to use str_sort so that the vector is sorted
    # by generation number, not alphabetically.
    lgnd_brks <- col_key2 %>% distinct(gen) %>% 
      arrange(gen) %>% 
      mutate(gen = ifelse(gen >= 0, paste(gen, "gens."), 
                          ifelse(gen == -1, "excl.-moderate",
                                 ifelse(gen == -2, "excl.-severe", gen)))) %>%
      pull() # Convert to a vector

    # If data only has climate stress values (no bins), create empty data
    # frame for color key and populate based on stress values present in data.
    } else {
      col_key2 <- data.frame(cols = as.character(), value = as.character())
      lgnd_brks <- df %>% arrange(gen) %>% distinct(value) %>% pull()
      if (any(df$value == "excl.-moderate")) {
        col_key2 <- col_key2 %>% 
          add_row(cols = "gray70", value = "excl.-moderate")
      }
      if (any(df$value == "excl.-severe")) {
        col_key2 <- col_key2 %>% 
          add_row(cols = "gray30", value = "excl.-severe")
      }
      
    }
      
    # Create lgd. color vector and plot
    cols <- setNames(as.character(col_key2$cols), col_key2$value) 
    p <- Base_map(df) + 
      scale_fill_manual(values = cols, breaks = lgnd_brks, 
                        name = str_wrap(paste0(lgd), width = 15)) +
      labs(title = str_wrap(paste(sp, titl), width = titl_width), 
           subtitle = str_wrap(subtitl, width = subtitl_width)) +
      theme_map(base_size = base_size) + 
      mytheme
     
    #### * Stage count ####
  } else if (grepl("StageCount", outfl)) {
    # Caption for logging file
      if (grepl("StageCount_Excl1|StageCount_Excl2", outfl)) { 
        log_capt <- paste("-", 
          str_wrap("Stage count with climate stress excl. on", width = 80),
          format(as.Date(d, "%Y%m%d"), "%m/%d/%Y"))
      } else {
        log_capt <- paste("-", "Stage count on", 
                             format(as.Date(d, "%Y%m%d"), "%m/%d/%Y"))
      }    
      
    # Format data if there are climate stress exclusion values
      if (exclusions_stressunits) {
        df <- df %>% mutate(value = ifelse(value_orig == -2, "excl.-severe", 
                        ifelse(value_orig == -1, "excl.-moderate", value)))
        
        if (any(df$value_orig > 0)) {
          # Need to remove -2 and -1 values prior to binning values for plot
          df2 <- dplyr::filter(df, !value_orig < 0) 
          excl_df <- df %>% dplyr::filter(value_orig < 0) # Take only -2 and -1 
          # Put exlcusion values and stage count values back together
          df <- rbind(df2, excl_df) # Rename data frame
        
          # If clim. exclusions masks out all non-zero values, then just plot 
          # climate stress exclusions
        } else if (all(df$value_orig <= 0)) {
          df <- mutate(df, value = ifelse(value_orig == -2, "excl.-severe", 
                            ifelse(value_orig == -1, "excl.-moderate", value)))
            
        }
        
        # If stress values are missing in data, then add a row so the legend
        # still shows the stress category (Excl1 = excl.-severe, Excl2 = 
        # excl.-severe and excl.-moderate). Otherwise just recode stress values.
        if (grepl("StageCount_Excl1", outfl) & (!(-2 %in% df$value_orig))) {
          df <- df %>% 
            add_row(value = "excl.-severe", gen_stg = factor(-2))
        } else if (grepl("StageCount_Excl2", outfl)) {
          if (!(-2 %in% df$value_orig)) {
            df <- df %>% 
              add_row(value = "excl.-severe", gen_stg = factor(-2))
          }
          if (!(-1 %in% df$value_orig)) {
            df <- df %>% 
              add_row(value = "excl.-moderate",  gen_stg = factor(-1))
          }
        }
        df <- arrange(df, as.numeric(gen_stg))
      }
      
      # Define factor levels to order legend key properly
      sorted <- unique(as.numeric(df$gen_stg))  
      df$gen_stg <- factor(df$gen_stg, levels = sorted)
      
      # Make the color key for the legend 
      # Currently enough colors for 20 generations
      cols_df <- data.frame("cols" = 
        c(Colfunc("deepskyblue", "blue3", 4), # Gen 0 
          Colfunc("orangered", "firebrick4", 4), # Gen 1
          Colfunc("yellow", "gold3", 4), # Gen 2
          Colfunc("lightgreen", "darkgreen", 4), # Gen 3
          Colfunc("magenta", "magenta4", 4), # Gen 4
          Colfunc("tan1", "darkorange3", 4), # Gen 5
          Colfunc("cyan", "cyan4", 4), # Gen 6
          Colfunc("greenyellow", "chartreuse4", 4), # Gen 7
          Colfunc("mediumpurple1", "purple3", 4), # Gen 8
          Colfunc("lightgoldenrod", "gold4", 4), # Gen 9
          Colfunc("cadetblue1", "cornflowerblue", 4), # Gen 10
          Colfunc("mistyrose", "palevioletred2", 4), # Gen 11
          Colfunc("seagreen1", "seagreen4", 4), # Gen 12
          Colfunc("lemonchiffon", "gold", 4), # Gen 13
          Colfunc("red", "darkred", 4), # Gen 14
          Colfunc("lightpink", "deeppink3", 4), # Gen 15
          Colfunc("lightgreen", "darkgreen", 4), # Gen 16
          Colfunc("sienna", "sienna4", 4), # Gen 17
          Colfunc("maroon1", "maroon4", 4), # Gen 18
          Colfunc("royalblue1", "royalblue4", 4), # Gen 19
          Colfunc("thistle1", "thistle4", 4))) # Gen 20
        gens_df <- data.frame(gen = c(rep(1, 4), rep(2, 4),
          rep(3, 4), rep(4, 4), rep(5, 4), rep(6, 4), rep(7, 4), rep(8, 4), 
          rep(9, 4), rep(10, 4), rep(11, 4), rep(12, 4), rep(13, 4), rep(14, 4),
          rep(15, 4), rep(16, 4), rep(17, 4), rep(18, 4), rep(19, 4), 
          rep(20, 4)))
        gens_df$gen <- sapply(gens_df$gen, function(x) {
          paste(toOrdinal(x), "gen.") 
          })
        gens_df <- rbind(data.frame(gen = c(rep("OW gen.", 4))), gens_df)
        
        # Create the color key and named vector of the key
        #col_key <- cols_df %>% mutate(gen_name = paste0("G", gens_df$gen)) %>%
        col_key <- cols_df %>% mutate(gen = gens_df$gen) %>%
          mutate(stg_name = 
            rep_len(c("eggs", "larvae", "pupae", "adults"), nrow(gens_df))) %>%
          mutate(value = paste(gen, stg_name)) %>%
          semi_join(., df, by = "value") %>%
          dplyr::select(cols, value)
        
        # Order values by generationa nd life cycle stage order (i.e. egg, 
        # larvae, pupae, adult) or won't show up correctly in legend key
        df$value <- factor(df$value, 
                  levels = unique(df$value[order(df$gen_stg)]))
        
        # Add grayscale colors to legend colors if climate stress exclusions
        # Moderate stress exclusion
        if (any(df$value == "excl.-moderate")) {
          col_key <- rbind(data.frame("cols" = "gray70", 
                                       "value" = "excl.-moderate"), col_key)
        }
        # Severe stress exclusions
        if (any(df$value == "excl.-severe")) {
          col_key <- rbind(data.frame("cols" = "gray30", 
                                      "value" = "excl.-severe"), col_key)
        }
        
      # Make the plot
      cols <- setNames(as.character(col_key$cols), levels(df$value)) 
      p <- Base_map(df) + 
        scale_fill_manual(values = cols, 
                            name = str_wrap(paste0(lgd), width = 15)) +
        labs(title = str_wrap(paste(sp, titl), width = titl_width), 
              subtitle = str_wrap(subtitl, width = subtitl_width)) +
        theme_map(base_size = base_size) + 
        mytheme
    
     # Need to adjust number of rows in legend for very small plots or 
     # the legend will go off the page
     if (asp < 0.5) {
       p <- p + guides(fill = guide_legend(nrow = 15))
     }
      
    #### * Climate stress exclusion maps ####
  } else if (grepl("Heat_Stress_Excl|Cold_Stress_Excl|All_Stress_Excl", 
                   outfl)) {
    # Caption for log file
    log_capt <- paste("-", titl_orig, 
                  "based on stress temperature thresholds and unit limits on",
                  format(as.Date(d, "%Y%m%d"), "%m/%d/%Y"))
    # Re-assign values (0, -1, -2) to their corresponding description
    df <- mutate(df, value = factor(ifelse(value == -2, "excl.-severe", 
                                    ifelse(value == -1, "excl.-moderate", 
                                    ifelse(value == 0, "not excluded",NA)))))    
    df$value <- factor(df$value, levels = c("excl.-severe", "excl.-moderate",
                                           "not excluded")) # Order by values
    # Make the plot
    p <- Base_map(df) +        
      scale_fill_manual(values = 
                          c("excl.-severe" = "gray30",
                            "excl.-moderate" = "gray70",
                            "not excluded" = "green2"), 
                        name = paste0(lgd), drop = FALSE) +
      labs(title = str_wrap(paste(sp, titl), width = titl_width),
           subtitle = str_wrap(subtitl, width = subtitl_width)) +
      theme_map(base_size = base_size) + 
      mytheme + 
      theme(legend.text = element_text(size = rel(1.5)), 
            legend.title = element_text(size = rel(1.4), face = "bold"))
      
    #### * Lifestage w/ NumGen maps ####
  } else if (grepl("Adult_byGen|Adult_Excl1_byGen|Adult_Excl2_byGen", outfl)) {
    # Caption for log file
    log_capt <- paste("-", "Relative pop. size of adults for each gen. on", 
                      format(as.Date(d, "%Y%m%d"), "%m/%d/%Y"))
    
    # Extract info on how many generations have been completed to date
    all_gens <- c(0:max(df$gen))
    gens_df <- data.frame("value" = paste(all_gens, "gens."), 
                          "x" = NA, "y" = NA, "gen" = all_gens)
        
    # If only odd gens are plotted then need to remove even gens.
    if (odd_gen_map == 1) {
      gens_df <- gens_df %>% filter(gen %% 2 != 0)
    }
    
    # Create rows for previous generations so that they appear in the legend
    # key even if they are absent on the sampling day
    df <- df %>% mutate(gen = ifelse(value == -2, -2, 
                                     ifelse(value == -1, -1, gen)))
    
    # Format values that are not climate stress exclusions (gen >= 0)
    df2 <- df %>% filter(gen >= 0) %>% # Remove clim. stress values
      mutate(value = ifelse(value == 0, 0, # Create bins
                            ifelse(value != 0 & value < 20, 1, 
                            ifelse(value < 40, 20, ifelse(value < 60, 40, 
                            ifelse(value < 80, 60, ifelse(value < 100, 80, 
                                   value))))))) %>%
      mutate(value = paste(gen, "gens.:", value)) %>% 
      mutate(value = ifelse(grepl(": 0", value), "other stages", value))

    df2 <- rbind(df2, gens_df) # Add on all completed generations to date
        
    # Extract and back in climate stress exclusion values
    excl_df <- df %>% dplyr::filter(gen < 0) %>%  
      mutate(value = ifelse(value == -2, "excl.-severe", 
                            ifelse(value == -1, "excl.-moderate", value)))
    df <- rbind(excl_df, df2)
      
    # If stress values are missing in data (when climate stress exclusions 
    # are specified), then add a row ("fake" data) so the legend still shows 
    # the stress category (Excl1 = excl.-severe, 
    # Excl2 = excl.-severe and excl.-moderate).
    if (grepl("Excl1", outfl) & (!("excl.-severe" %in% df$value))) {
      df <- df %>% 
        add_row(value = "excl.-severe", x = NA, y = NA, gen = -2)
    } else if (grepl("Excl2", outfl)) {
      if (!("excl.-severe" %in% df$value)) {
        df <- df %>% 
          add_row(value = "excl.-severe", x = NA, y = NA, gen = -2)
      }
      if (!("excl.-moderate" %in% df$value)) {
        df <- df %>% 
          add_row(value = "excl.-moderate", x = NA, y = NA, gen = -1)
      }
    }  
    
    # Order formatted values by original values so legend is in correct order
    df$value <- factor(df$value, 
                       levels = unique(df$value[order(df$gen, df$value)]))
    
    # Make the color key for the legend 
    # Currently enough colors for 20 generations
    cols_df <- data.frame("cols" = 
      c(Colfunc("deepskyblue", "blue3", 5), # Gen 0 
        Colfunc("orangered", "firebrick4", 5), # Gen 1
        Colfunc("yellow", "gold3", 5), # Gen 2
        Colfunc("lightgreen", "darkgreen", 5), # Gen 3
        Colfunc("magenta", "magenta4", 5), # Gen 4
        Colfunc("tan1", "darkorange3", 5), # Gen 5
        Colfunc("cyan", "cyan4", 5), # Gen 6
        Colfunc("greenyellow", "chartreuse4", 5), # Gen 7
        Colfunc("mediumpurple1", "purple3", 5), # Gen 8
        Colfunc("lightgoldenrod", "gold4", 5), # Gen 9
        Colfunc("cadetblue1", "cornflowerblue", 5), # Gen 10
        Colfunc("mistyrose", "palevioletred2", 5), # Gen 11
        Colfunc("seagreen1", "seagreen4", 5), # Gen 12
        Colfunc("lemonchiffon", "gold", 5), # Gen 13
        Colfunc("red", "darkred", 5), # Gen 14
        Colfunc("lightpink", "deeppink3", 5), # Gen 15
        Colfunc("lightgreen", "darkgreen", 5), # Gen 16
        Colfunc("sienna", "sienna4", 5), # Gen 17
        Colfunc("maroon1", "maroon4", 5), # Gen 18
        Colfunc("royalblue1", "royalblue4", 5), # Gen 19
        Colfunc("thistle1", "thistle4", 5))) # Gen 20    
     bins_df <- data.frame(gen = c(rep(0, 5), rep(1, 5), rep(2, 5),
        rep(3, 5), rep(4, 5), rep(5, 5), rep(6, 5), rep(7, 5), rep(8, 5), 
        rep(9, 5), rep(10, 5), rep(11, 5), rep(12, 5), rep(13, 5), rep(14, 5),
        rep(15, 5), rep(16, 5), rep(17, 5), rep(18, 5), rep(19, 5), 
        rep(20, 5)))
    col_key <- cbind(cols_df, bins_df) %>% 
      add_row(cols = c("gray70", "gray30"), gen = c(-1, -2))
        
    # Remove unneded colors, create bins of 5 to represent rel. pop. size. 
    # For legend, show only one color for each generation (map will have a 
    # gradation of this color). This is only done if there are values other
    # than climate stress exclusions. The color for rel. pop. size = 60 is used.
    # Add on a color for the "other stages" category (non-adult stages) - this
    # is coded as -0.1 in order to properly sort it in the legend key.
    if (any(df$gen >= 0)) {
      col_key2 <- semi_join(col_key, df, by = "gen") 
      num_gens <- length(unique(col_key2$gen)) # how many unique gens in data?
      col_key2 <- col_key2 %>% 
        mutate(value = ifelse(gen >= 0, rep(c(1, 20, 40, 60, 80), 
                                            num_gens), gen), # 5 bins
               value = ifelse(gen >= 0, paste(gen, "gens.:", value),
                              ifelse(gen == -1, "excl.-moderate", 
                              ifelse(gen == -2, "excl.-severe", NA)))) %>%
        add_row(cols = "gray90", gen = -0.1, value = "other stages")
      col_key2$value <- factor(col_key2$value, 
                        levels = unique(col_key2$value[order(col_key2$gen)]))
      
      # For legend, show only one color for each generation 
      # (map will have a gradation of this color)
      lgnd_cols <- col_key2 %>% dplyr::filter(grepl(": 60", value))
      lgnd_cols$value <- str_split_fixed(lgnd_cols$value, pattern = ":", 2)[,1]
      
      # Bind actual colors and legend colors together and create a 
      # named vector of these
      col_key2 <- rbind(col_key2, lgnd_cols)
      
      # Breaks to use in plotting function, so only one shade per gen 
      # is shown in legend. Need to use str_sort so that the vector is sorted
      # by generation number, not alphabetically.
      lgnd_brks <- col_key2 %>% distinct(gen) %>% 
        arrange(gen) %>% 
        mutate(gen = ifelse(gen >= 0, paste(gen, "gens."), 
                            ifelse(gen == -1, "excl.-moderate",
                                   ifelse(gen == -2, "excl.-severe", 
                                          "other stages")))) %>%
        pull() # Convert to a vector

      # If data only has climate stress values (no bins), create empty data
      # frame for color key and populate based on stress values present in data.
      } else {
        col_key2 <- data.frame(cols = as.character(), value = as.character())
        lgnd_brks <- as.character(unique(df$value))
        if (any(df$value == "excl.-moderate")) {
          col_key2 <- col_key2 %>% 
              add_row(cols = "gray70", value = "excl.-moderate")
        }
        if (any(df$value == "excl.-severe")) {
          col_key2 <- col_key2 %>% 
            add_row(cols = "gray30", value = "excl.-severe")
        }
      
    }
    
    # Create lgd. color vector and plot
    cols <- setNames(as.character(col_key2$cols), col_key2$value) 
    p <- Base_map(df) + 
      scale_fill_manual(values = cols, breaks = lgnd_brks, 
                        name = str_wrap(paste0(lgd), width = 15)) +
      labs(title = str_wrap(paste(sp, titl), width = titl_width), 
           subtitle = str_wrap(subtitl, width = subtitl_width)) +
      theme_map(base_size = base_size) + 
      mytheme

    #### * Pest Event Maps ####
  } else if (grepl("Avg|Earliest", outfl)) {
    log_capt <- paste("-", titl_orig) # Caption for log file
    start_year <- as.numeric(start_year)
    
    # Format the data value column
    df <- df %>% 
      dplyr::filter(!(value %in% c(0, 366))) # remove day 0 and day 366
    df$value <- round(df$value)
    # TO DO: try to fix this so that don't end up with 6 weeks in Dec
    # Currently must change value for day 364 and 365 to day 363, or end up w/
    # 6 weeks in December for some years
    df <- df %>% mutate(value = ifelse(value >= 364, 363, value))
    df$value_orig <- df$value # Create for ordering recoded values later
    
    # Convert day of year to a date, convert date to a week of the year, and 
    # format to "month-day" format. First need to subtract 1 from all day of 
    # year values, because as.Date starts at day 1, not day 0
    df$value <- df$value - 1
    df$value <- as.Date(df$value, origin = 
                          as.Date(paste0(start_year, "-01-01"))) 

    # The resulting values may have dates before Jan-1 (Dec-30, Dec-31) because 
    # they occur in the same week as Jan-1. The ceiling_date function (dplyr)
    # rounds up to the next month so that they begin on Jan-1 
    # (e.g., 2019-12-31 == 2020-01-01)
    df$value <- as.character(cut.POSIXt(strptime(df$value, format = "%Y-%m-%d"), 
                                        breaks = "1 weeks"))
    badDates <- df %>% filter(grepl(as.character(start_year - 1), value))
    badDates$value <- as.character(ceiling_date(as.Date(badDates$value, 
                                                format = "%Y-%m-%d"), "month"))
    
    # Now replace old data with data that have fixed date
    # Then add week of month column
    df <- df %>% filter(!(grepl(as.character(start_year - 1), value))) %>%
      bind_rows(., badDates)    
    df$week <- ceiling(mday(df$value)/7)
    
    # Reformat the dates to month-day (e.g., Jan-01, Jan-06, ...)
    # Clim. exc. values (-1 and -2) will become NA
    df$value <- format(strptime(df$value, format = "%Y-%m-%d"), 
                      format = "%b-%d")
    
    # Data frames w/ January may have 2 dates (Jan-01 + another) for week 1
    # This will result in the key having 2 dates w/ same color
    # Check to see if this is true, and if so, then add 1 week onto weeks for
    # January
    week1 <- df %>% filter(week == 1 & value_orig < 30) %>% distinct(value)
    
    if (nrow(week1) > 1) {
      df <- df %>% 
        mutate(week = ifelse(grepl("Jan-", value) & !grepl("Jan-01", value), 
                             week + 1, week))
    }
    
    # Generate a key for colors for every week of the year, allowing up to 
    # 5 weeks per month
    cols_df <- data.frame("cols" = 
      c(Colfunc("deepskyblue", "blue3", 5),
        Colfunc("red", "darkred", 5), 
        Colfunc("yellow", "gold3", 5),
        Colfunc("lightgreen", "darkgreen", 5), 
        Colfunc("magenta", "magenta4", 5),
        Colfunc("sienna1", "sienna4", 5),
        Colfunc("cyan", "cyan4", 5),
        Colfunc("greenyellow", "chartreuse4", 5),
        Colfunc("mediumpurple1", "purple3", 5),
        Colfunc("lightpink", "deeppink4", 5),
        Colfunc("lightgoldenrod", "gold4", 5), 
        Colfunc("cadetblue1", "cornflowerblue", 5))) # Colors
    weeks_df <- data.frame("mnth" = c(rep("Jan", 5), rep("Feb", 5), 
      rep("Mar", 5), rep("Apr", 5), rep("May", 5), rep("Jun", 5), rep("Jul", 5),
      rep("Aug", 5), rep("Sep", 5), rep("Oct", 5), rep("Nov", 5), 
      rep("Dec", 5))) # 5 weeks per month
    weeks_df <- data.frame(weeks_df %>% group_by(mnth) %>% # Group by month
      mutate(mnth_wk = row_number()) %>% # Assign unique row # to rep. week #
      mutate(mnth_wk = paste(mnth, mnth_wk, sep = "_")))
    
    # Attach those data frames to make the key
    col_key <- cbind(cols_df, weeks_df)
    col_key$mnth <- as.character(col_key$mnth) # Add which month
    
    # Extract all unique weeks from data,and count no. of bins (months)
    dats <- df %>% distinct(value) 
    dats$mnth <- str_split_fixed(dats$value, pattern = "-", 2)[,1]
    dats <- dats %>% arrange(., value) %>%
      group_by(mnth) %>% # Group by month
      # Assign a unique value to each row in a month - used for joining later
      # This will also be done for other data frames below 
      
      #mutate(week = ceiling(day(mnth_day) / 7)) %>%
      left_join(dplyr::select(df, value, week), by = "value") %>%
      mutate(mnth_day = format(as.Date(value, "%b-%d"))) %>%
      mutate(mnth_wk = paste(mnth, week, sep = "_")) %>%
      distinct(., .keep_all = TRUE) %>%
      arrange(mnth_day)

    # Necessary for removing weeks that are not in the data in the col_key2
    mnth_ct <- data.frame(dats %>% group_by(mnth) %>% 
      dplyr::mutate(freq = n_distinct(value))) %>%
      group_by(mnth) #%>% # Group by month

    # Finally filter unncessary weeks out of generic color key (some months 
    # have only 4 weeks)
    col_key2 <- dplyr::semi_join(col_key, dats, by = "mnth") %>% 
      dplyr::left_join(., mnth_ct, by = c("mnth_wk")) %>%
      na.omit %>%
      dplyr::select(cols, value)
    
    # Format the dates dataframe (dats2) for joining to col_key2 (color key)
    dats2 <- data.frame(dplyr::select(dats, value, mnth) %>% 
                          arrange(mnth, value))

    # Attach the colors to the value and format with needed colunms, etc.
    col_key2 <- left_join(col_key2, dplyr::select(dats2, -mnth), by = "value")
    col_key2$year <- start_year
    col_key2$date <- paste0(col_key2$year, "-", col_key2$value)
    col_key2$date <- as.Date(col_key2$date, format = "%Y-%b-%d")
    col_key2 <- col_key2 %>% dplyr::select(value, cols)

    # If data have climate stress exclusion values, need to reformat data
    # because values are non-dates; recode them, then bind back to original
    # data. Finally add grayscale shades to the color key for legend.
    if (any(df$value_orig < 0)) {
      df2 <- filter(df, value_orig < 0)
      df2 <- mutate(df2, value = ifelse(df2$value_orig == -2, "excl.-sev.", 
                                 ifelse(df2$value_orig == -1, "excl.-mod.", 
                                        df2$value)))
      df3 <- rbind(filter(df, value_orig > 0), df2)
      # Order according to orig. vals (DOY)
      df3$value <- factor(df3$value, levels = 
        unique(df3$value[order(as.numeric(as.character(df3$value_orig)))]))
      } else {
      df3 <- df
      df3$value <- factor(df3$value, levels = 
        unique(df3$value[order(as.numeric(as.character(df3$value_orig)))]))
      }
      if (any(df$value_orig == -1)) {
        col_key2 <- rbind(data.frame("value" = "excl.-mod.", "cols" = "gray70"), 
                          col_key2)
      }
      if (any(df$value_orig == -2)) {
        col_key2 <- rbind(data.frame("value" = "excl.-sev.", "cols" = "gray30"), 
                          col_key2)
     }

    # Make legend colors df a vector and plot results
    cols <- setNames(as.character(col_key2$cols), col_key2$value)
    p <- Base_map(df3) + 
      scale_fill_manual(values = cols, name = str_wrap(paste0(lgd), 
                                                       width = 15)) +
      labs(title = str_wrap(paste(sp, titl), width = titl_width), 
           subtitle = str_wrap(paste(subtitl), width = subtitl_width)) +
      theme_map(base_size = base_size) + 
      mytheme
     
     # Need to adjust number of rows in legend for small aspect plots or 
     # the legend will go off the page
     if (asp < 0.6) {
       p <- p + guides(fill = guide_legend(nrow = 15))
     }
     
  }

  #### * Save the plots ####
  # Save the plot, or else report that there was an error and skip
  # See "rmessages.txt" for error report
  tryCatch(
    {
      suppressMessages(ggsave(p, file = paste0(outfl, "_", d, ".png"), 
                              height = asp * 7, units = c('in'), dpi = 300))
      cat(paste0("\n\nSaving summary map: ", outfl, "_", d, ".png\n"),  
          str_wrap(paste0(log_capt, "\n"), width = 80, exdent = 2), sep = "",
          file = Model_rlogging, append = TRUE) # print progress in log file
      # cat("\n\nSaving summary map: ", outfl, "_", d, ".png\n", sep = "", 
      #     file = Model_rlogging, append = TRUE) # print progress in log file
      # cat(str_wrap(paste0(log_capt, "\n"), width = 80, exdent = 2),
      #     file = Model_rlogging, append = TRUE) # plot caption for log file
    },
    error = function(e) {
      cat("\nCould not create plot for ", outfl, "_", d, ".png\n", sep = "", 
          file = Model_rlogging, append = TRUE) 
    } )
}

#### (15). PlotMap_stress: summary map plotting - stress units ####
# Create summary maps (PNG) of heat stress and cold stress units, 
# with max1 (Stress limit 1) and max2 (Stress limit 2) shown as "countour" lines
# r = raster input; d = date; max1 = stress limit 1; max2 = stress limit 2;
# titl = plot title; lgd = legend title; outfl = outfile name
PlotMap_stress <- function(r, d, max1, max2, titl, lgd, outfl) {
  log_capt <- paste("- Number of accumulated", tolower(lgd), "on", 
                       format(as.Date(d, "%Y%m%d"), "%m/%d/%Y")) # Log file cap
  sp <- paste0(gsub(pattern = "_", replacement = " ", fullname), ":")
  dat <- as.character(format(strptime(d, format = "%Y%m%d"), 
                             format = "%m/%d/%Y"))
  titl <- paste(titl, dat, sep = " ")
  subtitl <- paste("Maps and modeling", format(Sys.Date(), "%m/%d/%Y"), 
    str_wrap("by Oregon State University IPPC USPEST.ORG and USDA-APHIS-PPQ; 
             climate data from OSU PRISM Climate Group",  width = 150))  
  df <- ConvDF(r)
  df$value_orig <- df$value
  df2 <- Stress_Val_Conv(df) # Properly formats values
  
  # Need to wrap title and subtitle for narrow plots (e.g., RI)
  if (asp > 1.55) {
    titl_width <- 45
    subtitl_width <- 55
  } else {
    titl_width <- 55
    subtitl_width <- 75
  }
  
  # Create contours from raster values greater than limit 1 and limit 2, if 
  # raster values are greater than max1 and/or max2
  max1_c <- tryCatch(
    {
      # max_1 is object of class "SpatialLinesDataFrame"
      max1_c <- rasterToContour(r > max1) 
    },
    error = function(e) {
      max1_c <- 0 
    } )
  max2_c <- tryCatch(
    {
      # max_2 is object of class "SpatialLinesDataFrame"
      max2_c <- rasterToContour(r > max2) 
    },
    error = function(e) {
      max2_c <- 0 
    } )
  
  # If all values are 0 or are less than 10 (if stress limit < 10), then don't 
  # include contours (must include this code or it will throw an error)
  if (all(df$value == 0 | all(df$value < 10 & all(df$value < max1)))) {
    p <- Base_map(df2) +
      scale_fill_manual(values = c("#5E4FA2"), name = paste0(lgd)) +
      labs(title = str_wrap(paste(sp, titl), width = titl_width), 
           subtitle = str_wrap(subtitl, width = subtitl_width)) +
      theme_map(base_size = base_size) + 
      mytheme 
    
    # If any values are greater than 0, but not greater than max1, then don't 
    # plot either countour (must include or it will thrown an error)
  } else if (any(df$value > 0) & is.numeric(max1_c)) {
    p <- Base_map(df2) +
      scale_fill_brewer(palette = "Spectral", direction = -1, 
                        name = str_wrap(paste0(lgd), width = 15)) +
      labs(title = str_wrap(paste(sp, titl), width = titl_width), 
           subtitle = str_wrap(subtitl, width = subtitl_width)) +
      theme_map(base_size = base_size) + 
      mytheme
    
    # If any values are greater than limit 1 but less than limit 2, then 
    # plot countour line for just limit 1
  } else if (any(df$value > 0) & !is.numeric(max1_c) & is.numeric(max2_c)) { 
    # max1_c is class "SpatialLinesDataFrame" but max2_c is class "numeric" 
    # (max2_c = 0)
    p <- Base_map(df2) +
      scale_fill_brewer(palette = "Spectral", direction = -1, 
                        name = str_wrap(paste0(lgd), width = 15)) +
      scale_color_manual(name = "Stress Limits", 
                         values = c("Stress limit 1" = "magenta")) +
      geom_path(data = max1_c, aes(x = long, y = lat, group = group, 
                                   color = "Stress limit 1"), lwd = 0.15) +
      labs(title = str_wrap(paste(sp, titl), width = titl_width), 
           subtitle = str_wrap(subtitl, width = subtitl_width)) +
      theme_map(base_size = base_size) + 
      mytheme +
      guides(colour = guide_legend(override.aes = list(size = 1), order = 1)) 
    
    # If any values are greater than limit1 and limit2, then plot 
    # countour lines for both limit1 and limit2
  } else if (!is.numeric(max1_c) & !is.numeric(max2_c)) {
    p <- Base_map(df2) +
      scale_fill_brewer(palette = "Spectral", direction = -1, 
                        name = str_wrap(paste0(lgd), width = 15)) +
      scale_color_manual(name = "Stress Limits", 
                         values = c("Stress limit 1" = "magenta", 
                                    "Stress limit 2" = "mediumblue")) +
      geom_path(data = max1_c, aes(x = long, y = lat, group = group, 
                                   color = "Stress limit 1"), lwd = 0.15) +
      geom_path(data = max2_c, aes(x = long, y = lat, group = group, 
                                   color = "Stress limit 2"), lwd = 0.15) +
      labs(title = str_wrap(paste(sp, titl), width = titl_width), 
           subtitle = str_wrap(subtitl, width = subtitl_width)) +
      theme_map(base_size = base_size) + 
      mytheme +
      guides(colour = guide_legend(override.aes = list(size = 1), order = 1))
  }
  
  # Save the plot, or else report that there was an error and skip - 
  # See "rmessages.txt" for error report
  tryCatch(
    {
      ggsave(p, file = paste0(outfl, "_", d, ".png"), height = 7 * asp, 
             units = c('in'), dpi = 200) 
      cat("\n\nSaving summary map: ", outfl, "_", d, ".png\n", 
          str_wrap(paste0(log_capt, "\n"), width = 80), sep = "",
          file = Model_rlogging, append = TRUE)
    },
    error = function(e) {
      cat("Could not create plot for ", outfl, "_", d, ".png\n", sep = "", 
          file = Model_rlogging, append = TRUE) 
    } )
}

#### (16). Rast_Subs_Excl: lifestage raster weighting + clim. stress excl. ####
# Substitutes weighted lifestage raster values with severe (-2) 
# and moderate (-1) climate stress exclusions (Excl2) in areas under climate 
# stress (i.e., where Lifestage overlaps with Excl2)
# Two types: severe stress only (Excl1) or severe and moderate stress (Excl2)
Rast_Subs_Excl <- function(brk, type) {
  
  # Get all climate stress exclusion data
  allEXCL_brk <- brick(list.files(pattern = glob2rx("*All_Stress_Excl*.tif$")))
  
  # If PEM, then only take the last layer of allEXCL_brk (last date)
  # because PEMs are created only for the last date
  if (deparse(substitute(brk)) == "PEM") {
    allEXCL_brk <- allEXCL_brk[[last(nlayers(allEXCL_brk))]] # Keep last layer
  }
  
  # Identify pixels that have moderate (-1) or severe (-2) stress values.
  # The corresponding pixel in the input raster brick will be replaced with a
  # stress value. 
  lapply(1:nlayers(brk), function(lyr) {

    # For each layer in the brick, get the corresponding layer
    # in the All Stress Exclusion brick (allEXCL_brk)
    brk_lyr <- brk[[lyr]]
    Excl <- allEXCL_brk[[lyr]] 
    
    # Replace pixels in brick layer with the stress values in
    # areas of overlap
    
    if (type == "Excl2") {
      Excl2 <- Excl < 0   # Both severe and moderate exclusion
      brk_lyr[Excl2] <- Excl[Excl2] 
    } else if (type == "Excl1") {
      Excl1 <- Excl == -2   # Only severe exclusion
      brk_lyr[Excl1] <- Excl[Excl1]
    }
      
    return(brk_lyr)
  })
  
}

#### (17). RegCluster: register cluster for parallel computing ####
# Specifies the number of clusters to use for parallel computation. The 
# function will be different depending on the OS.
RegCluster <- function(value) {
  
  # Change the value if it is 1 (this could happen if number of cores is low)
  # Otherwise the whole process will not be run in parallel
  if (value == 1) {
    value <- 2
  }
  
  if (grepl("Windows", Sys.info()[1])) {
    cl <<- makePSOCKcluster(value)
  } else {
    cl <<- makeCluster(value)
  }
  
  # If run is being done on Hopper, need to specify the library for each worker
  if (Sys.info()["nodename"] == "hopper.science.oregonstate.edu") {
    clusterEvalQ(cl, .libPaths("/usr/local/lib64/R/library/"))
  }
  
  doParallel::registerDoParallel(cl)
  #on.exit(stopCluster(cl))
  return(cl)
}

#### (18). SaveRaster: save rasters ####
# Simply the "writeRaster" function from raster library but prints progress 
# in daily loop log file for a given cohort, if desired
# r = raster, tile_num = tile number (only relevant to CONUS and EAST),
# outnam = output file name; datatype = number of digits int the 
# output rasters (see "raster" library specificatoins)
SaveRaster <- function(r, cohort, tile_num, outnam, datatype) {
  if (region_param %in% c("CONUS", "EAST")) {
    #daily_logFile <- paste0("Daily_loop_cohort", cohort, "_", tile_num, ".txt")
    writeRaster(r, file = paste0(outnam, "_cohort", cohort, "_tile", tile_num),
                format = "GTiff",  datatype = datatype, overwrite = TRUE)
  } else {
    #daily_logFile <- paste0("Daily_loop_cohort", cohort, ".txt")
    writeRaster(r, file = paste0(outnam, "_cohort", cohort), 
                format = "GTiff", datatype = datatype, overwrite = TRUE)
  }
  #cat("Saving raster: ", outnam, "_cohort", cohort, ".tif\n\n", sep = "", 
  #file=daily_logFile, append=TRUE) 
}

# Same as above but for saving rasters and raster bricks in the 
# "Data Processing" section, and prints caption in the log file
# r = raster, outnam = output file name; datatype = number of digits int the 
# output rasters (see "raster" library specificatoins); log_capt = caption
SaveRaster2 <- function(r, outnam, datatype, log_capt) {
  writeRaster(r, file = outnam, format = "GTiff", datatype = datatype, 
              overwrite = TRUE)
  if (nlayers(r) > 1) {
    outnam2 <- paste0("\n\nSaving raster brick: ", outnam, ".tif\n")
  } else {
    outnam2 <- paste0("\n\nSaving raster: ", outnam, ".tif\n")
  }
  cat(outnam2, str_wrap(paste0(log_capt, "\n"), width = 80, exdent = 2), 
      sep = "", file = Model_rlogging, append = TRUE) 
}

#### (19). Stress_Val_Conv #### 
# Deal with 0 vs non-0 values when plotting cold and heat stress unit rasters
# Only create bins (0-10, etc...) if there are non-zero values in the data 
Stress_Val_Conv <- function(x) {
  if (all(x$value == 0)) {
    x2 <- x
  } else {
    x2 <- Cut_bins(x, 10)
  }
  # Need to fix bins if all data are < 10 
  if (all(x2$value_orig < 10)) {
    x2$value <- "0-10"
  }
  x2$value <- factor(x2$value, 
                     levels = unique(x2$value[order(x2$value_orig)]))
  return(x2)
}

#### (20). SplitRas: split raster into tiles ####
# The function spatially aggregates the original raster
# from https://stackoverflow.com/questions/29784829/
# r-raster-package-split-image-into-multiples
# It turns each aggregated cell into a polygon, then the extent of each polygon 
# is used to crop the original raster. The function returns a list with all the 
# pieces in case you want to keep them in the memory. The arguments are:
# raster = raster to be chopped            (raster object)
# ppside = pieces per side                 (integer)
# save   = write raster                    (TRUE or FALSE)
# plot   = do you want to plot the output? (TRUE or FALSE)
SplitRas <- function(raster, ppside, save, plot) {
  h        <- ceiling(ncol(raster)/ppside)
  v        <- ceiling(nrow(raster)/ppside)
  agg      <- aggregate(raster, fact = c(h, v))
  agg[]    <- 1:ncell(agg)
  agg_poly <- rasterToPolygons(agg)
  names(agg_poly) <- "polis"
  r_list <- list()
  for (i in 1:ncell(agg)) {
    e1          <- extent(agg_poly[agg_poly$polis == i,])
    r_list[[i]] <- crop(raster, e1)
  }
  if (save == TRUE) {
    for (i in 1:length(r_list)) {
      ii <- formatC(i, width = 2, format = "d", flag = "0")
      writeRaster(r_list[[i]], filename = paste("SplitRas", ii, sep = ""),
                  format = "GTiff", datatype = "FLT4S", overwrite = TRUE)  
    }
  }
  if (plot == TRUE) {
    par(mfrow = c(ppside, ppside))
    for (i in 1:length(r_list)) {
      plot(r_list[[i]], axes = FALSE, legend = FALSE, bty = "n", box = FALSE)  
    }
  }
  return(r_list)
}

#### (21). Weight_rasts: weight cohort rasters ####
# Two input lists are required: 1) either "Lifestage" or "NumGen" raster bricks
# for all cohorts, and 2) a vector of the relative population size comprised
# by each cohort. The function weights the cohort bricks according the size. 
# If there are 7 cohorts sampled each month of yr (12 mon + end of year), 
# then rast_wtd will be a list of 7 cohorts each w/ 13 layers 
# (13 * 7 = 91 layers). 

# Weight the cohorts according to their relative size in the population, 
# exporting each generation as a single brick file
# "type" variable will be life stage (stg) or generation number (gen)
# "value" variable is not needed for the function to work; could remove
# (stage number is in global environment), so maybe remove.
Weight_rasts <- function(cohort_fls, fl_type) { 
  
  # For each life stage or generation, extract data from raster brick, and 
  # multiple it by the rel. pop size of each cohort. The result is a list of 
  # raster bricks that provide the relative pop. size of each cohort for each 
  # date.
  if (fl_type == "Lifestage") {
    
    # Function to recode results from the OW stage (stg_num = 1) to the same 
    # value as the non-OW stage, and then calculate the rel. pop. size of the
    # combined stages (e.g., "Adult" size + "OWadult" size)

      wtd_brk_lst <- list()
      i <- 1
      for (j in 1:length(cohort_fls)) {
        
        brk <- brick(cohort_fls[[j]])
        
        if (stg == stg_nonOW) {
          brk[brk == 1] <- stg_num
        }
        
        brk_stg <- brk == stg_num
        wtd_brk <- round(100 * brk_stg) * relpopsize[i]
        wtd_brk_lst[[j]] <- wtd_brk
        i <- i + 1

      }
 
    # Same as analysis as above but according to generation
  } else if (fl_type == "NumGen") {

    wtd_brk_lst <- list()
    i <- 1
    for (j in 1:length(cohort_fls)) {
    
      brk <- brick(cohort_fls[[j]])
      wtd_brk <- round(100 * brk) * relpopsize[i]
      wtd_brk_lst[[j]] <- wtd_brk
      i <- i + 1
    
    }
  }
    
  # The results from previous step need to be summed across all cohorts. 
  # This produces a single raster brick in which each layer represents the 
  # relative population size of each cohort for each date.
  # "Reduce" accomplishes this task.
  wtd_sum <- Reduce("+", wtd_brk_lst)
  #gc()
  
  return(wtd_sum)
}
