# Functions used for DDRP_v2 R----
# Last modified on 1/3/20: changed naming of clim. suit. output files
# Modified on 12/9/19: had to fix code in PEM plotting - error for some spp
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

#### (2). Base_map: base map for summary plots ####
# Base features used for all summary (PNG) maps in "PlotMap" function
# The "coord_quickmap" function allows the state/region of interest to be 
# plotted (otherwise CONUS is plotted)
# geom_raster is faster than geom_tile
# Input data are in a data frame (= df) format
Base_map <- function(df) {
  p <- ggplot(states, aes(x = long, y = lat)) + 
    geom_raster(data = df, aes(x = x, y = y, fill = value)) + 
    geom_path(aes(group = group), color = "gray20", lwd = 0.4) +
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
  df$value_orig <- df$value # keep old value so can sort factors against it
  # round decimal places appropriately
  df$value <- ifelse(df$value < 0.1, round_any(df$value, 0.01),
                    ifelse(df$value < 1 & df$value > 0.1, round_any(df$value, .1),
                           round_any(df$value, 1)))
  #df$value <- ifelse(df$value < 1, round_any(df$value, .1), round_any(df$value, 1)) 
  # Cut values into bins; remove brackets, parentheses and dashes; then order
  # values will plotted in numerical order
  df2 <- df %>% mutate(value = cut(value, breaks = breaks, dig.lab = 4)) 
  df2$value <- gsub(pattern = "[()]|\\[|\\]", replacement = "", df2$value)
  df2$value <- gsub(pattern = "[,]", replacement = "-", df2$value)
  df2$value <- factor(df2$value, 
                      levels = unique(df2$value[order(df2$value_orig)])) 
  return(df2)
}

#### (10). DailyLoop: daily loop model ####
# The daily loop is the daily time step used to generate all results for DDRP
# The loop is run for each cohort (= cohort) in parallel. If the study region 
# is CONUS or EAST, then each of 4 tiles (= tile_num) are run in parallel as 
# well. The template (= template) is needed for setting up data sets to 
# populate during the model run, and to convert matrix outputs into raster 
# format.
DailyLoop <- function(cohort, tile_num, template) { 
  
  setwd(output_dir) 
  
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
  FullGen <- as.matrix(template)
  
  # Additional rasters - created depending on input setting
  if (exclusions_stressunits) {
    # Create masks for each variable
    chillmask         <- as.matrix(template)  # Daily chill units mask
    chillstress       <- as.matrix(template)  # Count of daily chill units
    chillstressTHRESH  <- as.matrix(template)  # Mask for chillstrs units thres
    chillstressTHRESH  <- chillstress_threshold # Mask for chillstrs units thres
    chillunitsCUM     <- as.matrix(template)  # Cumulative chill units
    chillstressMAX1    <- as.matrix(template)  # Max chill before most die
    chillstressMAX1    <- chillstress_units_max1 # Max chill before most die
    chillstressMAX2    <- as.matrix(template)  # Max chill before all die
    chillstressMAX2    <- chillstress_units_max2 # Max chill before all die
    chillEXCL         <- as.matrix(template)  # Chill stress exclusion
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
    
    # Calculate stage-specific degree-days for each cell per day
    dd_tmp <- TriDD(tmax, tmin, ls_ldt, ls_udt)
    
    # Accumulate degree days
    DDaccum <- DDaccum + dd_tmp
    
    # Accumulate total DDs across year, using larvae Lifestage
    # Can not use DDaccum because this one has values reset when progress = 1
    ls_ldt_larv <- stage_ldt[which(stgorder == "L")]
    ls_udt_larv <- stage_udt[which(stgorder == "L")]
    dd_tmp_larv <- TriDD(tmax, tmin, ls_ldt_larv, ls_udt_larv)
    DDtotal <- DDtotal + dd_tmp_larv
    
    # Climate stress exclusions - results will be same for all cohorts, 
    # so just calculate exclusions for cohort 1
    if (exclusions_stressunits) {
      # Chill stress accumulation
      # Make today's chill mask and calculate today's chill stress DDs
      chillmask <- tmin < chillstressTHRESH  
      chillstress <- chillmask * abs(chillstressTHRESH - tmin) 
      chillunitsCUM <- chillunitsCUM + chillstress
      # ASSUME NEW -2=severe -1=mod 0=none throughout
      chillEXCL <- Cond(chillunitsCUM >= chillstressMAX2, -2, 
                        Cond(chillunitsCUM >= chillstressMAX1, -1, 0))
      # Heat stress accumulation
      # Make today's heat mask and calculate today's heat stress DDs
      heatmask <- tmax > heatstressTHRESH  
      heatstress <- heatmask * abs(tmax - heatstressTHRESH) 
      heatunitsCUM <- heatunitsCUM + heatstress
      heatEXCL <- Cond(heatunitsCUM >= heatstressMAX2, -2, 
                       Cond(heatunitsCUM >= heatstressMAX1, -1, 0))
      AllEXCL <- Cond((chillEXCL == 0) & (heatEXCL == 0), 0,
                      Cond((chillEXCL == -1) & (heatEXCL >= -1),-1,
                           Cond((chillEXCL >= -1) & (heatEXCL == -1), -1, -2)))
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
    
    # FullGen means it reaches the OW stage
    FullGen <- FullGen + (progress == 1 & Lifestage == (length(stgorder) - 1)) 
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
      # Convert Lifestage and Numgen matrices to rasters and put into a brick
      mat_list <- list(Lifestage,NumGen,DDtotal)
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
      
      # If exclusions_stressunits = 1, then do the same thing for LifestageEXCL 
      # and NumGenEXCL, after they are calculated
      if (exclusions_stressunits) {
        # Exclusions included: -2=severe stress, -1=moderate stress
        LifestageEXCL1 <- Cond((AllEXCL > -2), Lifestage, -2)
        LifestageEXCL2 <- Cond((AllEXCL == -2), -2, 
                           Cond((AllEXCL == -1), -1, Lifestage))
        NumGenEXCL1 <- Cond((AllEXCL > -2), NumGen, -2)
        NumGenEXCL2 <- Cond((AllEXCL == -2), -2, 
                            Cond((AllEXCL == -1), -1, NumGen)) 
        
        # Convert LifestageEXCL and NumgenEXCL matrices to rasters and 
        # put into a brick
        mat_list2 <- list(LifestageEXCL1,LifestageEXCL2,
                          NumGenEXCL1,NumGenEXCL2)
        ext <- as.data.frame(as.matrix(extent(template)))
        rast_list2 <- lapply(mat_list2, Mat_to_rast, ext = ext, 
                             template = template)
        names(rast_list2) <- c("LifestageEXCL1_rast", "LifestageEXCL2_rast", 
                               "NumGenEXCL1_rast", "NumGenEXCL2_rast")
        
        #cat("### Adding layers to Lifestage Stress Exclusion 1 brick for 
        #cohort", cohort, ": doy =", sublist[d], "\n", 
        #file=daily_logFile, append=TRUE)
        # LifestageEXCL1 brick
        if (!exists("LifestageEXCL1_brick")) {
          LifestageEXCL1_brick <- brick(rast_list2$LifestageEXCL1_rast, 
                                        crs = crs)
        } else {
          LifestageEXCL1_brick <- addLayer(LifestageEXCL1_brick, 
                                       rast_list2$LifestageEXCL1_rast)
        }
        
        #cat("### Adding layers to Lifestage Stress Exclusion 2 brick 
        #for cohort", cohort, ": doy =", sublist[d], "\n", 
        #file=daily_logFile, append=TRUE)
        # LifestageEXCL2 brick
        if (!exists("LifestageEXCL2_brick")) {
          LifestageEXCL2_brick <- brick(rast_list2$LifestageEXCL2_rast, 
                                        crs = crs)
        } else {
          LifestageEXCL2_brick <- addLayer(LifestageEXCL2_brick, 
                                       rast_list2$LifestageEXCL2_rast)
        }
        
        #cat("### Adding layers to NumGen Stress Exclusion 1 brick for cohort",
        # cohort, ": doy =", sublist[d], "\n", file=daily_logFile, append=TRUE)
        # NumGenEXCL1 brick
        if (!exists("NumGenEXCL1_brick")) {
          NumGenEXCL1_brick <- brick(rast_list2$NumGenEXCL1_rast, crs = crs)
        } else {
          NumGenEXCL1_brick <- addLayer(NumGenEXCL1_brick, 
                                        rast_list2$NumGenEXCL1_rast)
        }
        
        #cat("### Adding layers to NumGen Stress Exclusion 2 brick for cohort", 
        #cohort, ": doy =", sublist[d], file=daily_logFile, append=TRUE)
        # NumGenEXCL2 brick
        if (!exists("NumGenEXCL2_brick")) {
          NumGenEXCL2_brick <- brick(rast_list2$NumGenEXCL2_rast, crs = crs)
        } else {
          NumGenEXCL2_brick <- addLayer(NumGenEXCL2_brick, 
                                        rast_list2$NumGenEXCL2_rast)
        }
        
        # Do the same for chill/heat units and chill/heat exclusion, but just 
        # for cohort 1, because results will be same for all cohorts
        if (cohort == 1) {
          # Convert matrices to rasters and put them into a raster brick
          mat_list3 <- list(chillunitsCUM, chillEXCL, heatunitsCUM,
                            heatEXCL,AllEXCL)
          ext <- as.data.frame(as.matrix(extent(template)))
          rast_list3 <- lapply(mat_list3, Mat_to_rast, ext = ext, 
                               template = template)
          names(rast_list3) <- c("chillunitsCUM_rast", "chillEXCL_rast", 
            "heatunitsCUM_rast", "heatEXCL_rast", "AllEXCL_rast")
          
          #cat("\n### Adding layers to Chill Stress Units brick for cohort", 
          #cohort, ": doy =", sublist[d], "\n", file=daily_logFile, append=TRUE)
          # Chill stress unit accumulation brick
          if (!exists("chillunitsCUM_brick")) {
            chillunitsCUM_brick <- brick(rast_list3$chillunitsCUM_rast, 
                                         crs = crs)
          } else {
            chillunitsCUM_brick <- addLayer(chillunitsCUM_brick, 
                                            rast_list3$chillunitsCUM_rast)
          }
          
          #cat("### Adding layers to Chill Stress Exclusion brick for cohort", 
          #cohort, ": doy =", sublist[d], "\n", file=daily_logFile, append=TRUE)
          # Chill stress exclusion brick
          if (!exists("chillEXCL_brick")) {
            chillEXCL_brick <- brick(rast_list3$chillEXCL_rast, crs = crs)
          } else {
            chillEXCL_brick <- addLayer(chillEXCL_brick, 
                                        rast_list3$chillEXCL_rast)
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
          # All stress exclusion brick (chill stress + heat stress exclusions)
          if (!exists("AllEXCL_brick")) {
            AllEXCL_brick <- brick(rast_list3$AllEXCL_rast, crs = crs)
          } else {
            AllEXCL_brick <- addLayer(AllEXCL_brick, rast_list3$AllEXCL_rast)
          }
          
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
  
  # If exclusions_stressunits = 1, then save stress unit and exclusions bricks
  if (exclusions_stressunits) {
    # Lifestage and NumGen climate stress exclusion files will vary across 
    # cohorts, so they need to be processed to produce final results in "Data
    # Processing" section
    SaveRaster(LifestageEXCL1_brick, cohort, tile_num, "LifestageEXCL1", 
               "INT2S")
    SaveRaster(LifestageEXCL2_brick, cohort, tile_num, "LifestageEXCL2", 
               "INT2S")
    SaveRaster(NumGenEXCL1_brick, cohort, tile_num, "NumGenExcl1", "INT2S")
    SaveRaster(NumGenEXCL2_brick, cohort, tile_num, "NumGenExcl2", "INT2S")
    
    # Chill and heat stress unit and exclusion bricks will be the same for 
    # all cohorts, so take only 1st one
    if (cohort == 1) {
      stress_excl_brick_list <- c(chillunitsCUM_brick, chillEXCL_brick,
        heatunitsCUM_brick, heatEXCL_brick,AllEXCL_brick)
      names(stress_excl_brick_list) <- c("Chill_Stress_Units", 
        "Chill_Stress_Excl", "Heat_Stress_Units", "Heat_Stress_Excl", 
        "All_Stress_Excl")
      
      # Save each raster brick product in the list
      for (i in 1:length(stress_excl_brick_list)) {
        brk <- stress_excl_brick_list[[i]]  
        SaveRaster(brk, cohort, tile_num, 
                   paste0(names(stress_excl_brick_list[i])), "INT2S")
      }
      
    }
    #cat("\n### Finished climate exclusions and stress units raster output 
    #cohort", cohort, "###\n\n", file = Model_rlogging, append=TRUE)
  }
  
  # Remove .xml files generated w/ .tif files for certain raster bricks
  # Haven't yet figured out a way to prevent these from being created. The
  # only solution I have found is to change a GDAL setting.
  delfiles <- dir(path = output_dir, pattern = "*xml")
  suppressWarnings(file.remove(file.path = output_dir, delfiles))
  gc() # Clear items from the environment - is this necessary?
  
}


#### (11). Degree Day calculation methods #### 
# Equations used to calculate degree-days
# tmax = max. temp. data; tmin = min. temp data; LDT = lower developmental 
# temperature threshold; UDT = upper developmental temperature threshold

# Simple Mean Temp DD Calc method: ((tmean > LDT) * (tmean - LDT))
# Same result as max((tmax + tmin)/2 - LDT,0), so no need for tmean PRISM data. 
SimpDD <- function(tmax, tmin,LDT) {
  return(max((tmax + tmin)/2 - LDT,0))
}

# Averaging DD Calc method (max + min/2 - tlow) but with horizontal 
# (substitution) upper threshold:
AvgDD <- function(tmax, tmin, LDT, UDT) {
  return(Cond(tmax < LDT, 0, Cond(tmin > UDT, 0, 
                                  Cond(tmax > UDT, (UDT + tmin)/2 - LDT, 
                                       Cond((tmax + tmin)/2 - LDT < 0,0, 
                                            (tmax + tmin)/2 - LDT)))))
}

# Single triangle with upper threshold (Sevachurian et al. 1977) - 
# also a good substitution for the single sine method
TriDD <- function(tmax, tmin, LDT, UDT) {
  tmax <- Cond(tmax == tmin, tmax + 0.01, tmax)
  Tmp1 = 6*((tmax - LDT)*(tmax - LDT))/(tmax - tmin)
  Tmp2 = 6*((tmax - UDT)*(tmax - UDT))/(tmax - tmin)
  Cond(tmax < LDT,0,
       Cond(tmin >= UDT,UDT - LDT,
            Cond((tmax < UDT) & (tmin <= LDT), Tmp1/12,
                 Cond((tmin <= LDT) & (tmax >= UDT), (Tmp1 - Tmp2)/12,
                      Cond((tmin > LDT) & (tmax >= UDT), 
                           6*(tmax + tmin - 2*LDT)/12 - (Tmp2/12),
                           Cond((tmin > LDT) & (tmax < UDT), 
                                6*(tmax + tmin - 2*LDT)/12,0))))))
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
  crs(rast) <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
  res(rast) <- res(template)
  NAvalue(rast) <- NaN
  return(rast)
}

#### (14). PlotMap: summary map plotting - main ####
# A VERY large function that generates summary plots for all products generated
# in the DDRP model run
# r = raster input, d = date, lgd = legend title, outfl = outfile name
PlotMap <- function(r, d, titl, lgd, outfl) {
  
  # If data are in raster format, then convert to a data frame
  if (grepl("NumGen|Adult_byGen|Adult_Excl1_byGen|Adult_Excl2_byGen", outfl)) {
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
      p <- Base_map(df) +       
        scale_fill_brewer(palette = "Spectral", direction = -1, 
                          name = paste0(lgd)) +
        labs(title = str_wrap(paste(sp, titl), width = titl_width), 
             subtitle = str_wrap(paste(subtitl), width = subtitl_width)) +
        theme_map(base_size = base_size) + mytheme
      
      # If there are any non-zero values, then cut values into bins and plot
    } else {
      df <- Cut_bins(df, 10)  
      df$value <- factor(df$value)
      p <- Base_map(df) +       
        scale_fill_brewer(palette = "Spectral", direction = -1, 
                          name = paste0(lgd)) +
        labs(title = str_wrap(paste(sp, titl), width = titl_width), 
             subtitle = str_wrap(paste(subtitl), width = subtitl_width)) +
        theme_map(base_size = base_size) + 
        mytheme
    }
    
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
    
    # Create a plot separately for rasters where all rel. pop size values = 0
    if (all(df$value == 0)) {
      df$value <- factor(df$value)
      cols <- c("mediumblue")
      p <- Base_map(df) +
        scale_fill_manual(values = cols, name = paste0(lgd)) +
        labs(title = str_wrap(paste(sp, titl), width = titl_width), 
             subtitle = str_wrap(subtitl, width = subtitl_width)) +
        theme_map(base_size = base_size) + 
        mytheme
      
      # If data include climate exclusions, then format accordingly
    } else if (exclusions_stressunits) {
      if (any(df$value_orig > 0)) {
        # Need to remove -2 and -1 values prior to binning values for plot
        df2 <- dplyr::filter(df, !value < 0) 
        df2 <- Cut_bins(df2, 10) 
        excl_df <- df %>% dplyr::filter(value < 0) # Take only -2 and -1 values
        excl_df$value_orig <- excl_df$value
        excl_df <- mutate(excl_df, value = ifelse(value == -2, "excl.-severe", 
                                           ifelse(value == -1, "excl.-moderate", 
                                                  value)))
        # Put exlcusion values and Lifestage rel. pop size (binned) values 
        # back together, then order by original values so plots in numerical 
        # order
        df3 <- rbind(excl_df, df2) 
        df3$value <- factor(df3$value, 
                            levels = unique(df3$value[order(df3$value_orig)])) 
        df <- df3 # Rename data frame
        
        # If clim. exclusions masks out all non-zero values, then just plot 
        # climate stress exclusions
      } else if (all(df$value_orig <= 0)) {
        df <- mutate(df, value = ifelse(value == -2, "excl.-severe", 
                                 ifelse(value == -1, "excl.-moderate", value)))
        df$value <- factor(df$value, levels = 
                             unique(df$value[order(df$value_orig)]))
      }
      
      # If there are no climate stress exclusions, and relative pop size 
      # values do not all = 0, then cut data into bins (e.g. 0-10, ...)
    } else {
      df <- Cut_bins(df, 10)  
      df$value <- factor(df$value)
    }
    
    # Make legend color keys and assign colors
    if (any(df$value_orig > 0)) {
      # First remove values less than 0 (-1 and -2 for clim. stress exclusion),
      # because want gray shades for those
      df_noStress <- dplyr::filter(df, value_orig >= 0)
      df_noStress$value <- factor(df_noStress$value)
      col_key <- cbind(data.frame("cols" = 
                        c(colorRampPalette(c("dodgerblue3", "yellow", "red3"))
                          (length(levels(df_noStress$value))))),
                       data.frame("value" = c(levels(df_noStress$value))))
      #col_df <- suppressWarnings(suppressMessages(semi_join(col_key, df,
      #by = "value"))) # join the colors to the data
      
      # Need to add grayscale cols to color key if there are
      # climate stress exclusions (grayscale colors)
      if (exclusions_stressunits) {
        if (any(df$value == "excl.-moderate")) {
          col_key <- rbind(data.frame(cols = "gray70", 
                                      value = "excl.-moderate"), col_key)
        }
        if (any(df$value == "excl.-severe")) {
          col_key <- rbind(data.frame(cols = "gray40", 
                                      value = "excl.-severe"), col_key)
        }
      }
      
      # If all values are <= 0, then just show climate exclusions (grayscale) 
      # and 0 values (blue)
    } else if (all(df$value_orig <= 0)) {
      col_key <- data.frame(cols = c("gray40", "gray70", "mediumblue"), 
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
        log_capt <- paste("-", 
          str_wrap("Number of gens. with climate stress excl. 
                   on", width = 80),
          format(as.Date(d, "%Y%m%d"), "%m/%d/%Y"))
      } else {
        log_capt <- paste("-", "Numbers of generations on", 
                             format(as.Date(d, "%Y%m%d"), "%m/%d/%Y"))
      }
    
    # If no data besides Gen0, then make entire map gray
    if (all(df$value == 0)) {
      df$value <- "0 gens."
      df$value <- factor(df$value)
      cols <- "mediumblue"
      cols <- setNames(as.character(cols), "0 gens.") 
      # Make the plot
      p <- Base_map(df) +
        scale_fill_manual(values = cols, name = str_wrap(paste0(lgd), 
                                                         width = 15)) +
        labs(title = str_wrap(paste(sp, titl), width = titl_width), 
             subtitle = str_wrap(subtitl, width = subtitl_width)) +
        theme_map(base_size = base_size) + 
        mytheme
      
      # Else if all values are below 0, then just plot those climate stress
      # exclusions (-1 and -2)
    } else if (all(df$value < 0)) {
        df <- mutate(df, value = ifelse(value == -2, "excl.-severe", 
                        ifelse(value == -1, "excl.-moderate", NA)))
        df <- mutate(df, gen = ifelse(value == "excl.-severe", "excl.-severe", 
                      ifelse(value == "excl.-moderate", "excl.-moderate", NA)))
        # Order by original values so plots in numerical order
        df$value <- factor(df$value, levels = unique(df$value[order(df$value)])) 
        lgnd_brks <- data.frame("gen" = unique(df$value))
        lgnd_brks <- sort(dplyr::pull(lgnd_brks, gen)) # Convert df to a vector
        
        # Assign climate stress exclusion colors (grayscale)
        if (all(df$value == "excl.-moderate")) {
          cols <- c("gray70")
          names(cols) <- c("excl.-moderate")
        } else if (all(df$value == "excl.-severe")) {
          cols <- c("gray40")
          names(cols) <- c("excl.-severe")
        } else {
          cols <- c("gray70", "gray40")
          names(cols) <- c("excl.-moderate", "excl.-severe")
        }
        
        # Make the plot
        p <- Base_map(df) + 
          scale_fill_manual(values = cols, breaks = lgnd_brks, 
                            name = str_wrap(paste0(lgd), width = 15)) +
          labs(title = str_wrap(paste(sp, titl), width = titl_width), 
               subtitle = str_wrap(subtitl, width = subtitl_width)) +
          theme_map(base_size = base_size) + 
          mytheme
        
        # If there are any values > 0, need to format data accordingly
      } else if (any(df$value > 0)) {
        # Want to just show non-zero values if they're available, 
        # so remove remove rows (i.e. generations) with zero values
        # Anti-join removes those Gen0 values present in other dataframe
        vals0 <- dplyr::filter(df, value == 0)
        vals_no0 <- dplyr::filter(df, value != 0)
        removeDups <- anti_join(vals0, vals_no0, by = c("x", "y")) 
        df <- rbind(vals_no0, removeDups)
        
        # If all values are >= 0
        if (all(df$value >= 0)) {
          df <- mutate(df, value = ifelse(value < 20, 0, ifelse(value < 40, 20, 
                                  ifelse(value < 60, 40, ifelse(value < 80, 60, 
                                  ifelse(value < 100, 80, value))))))
          df$value <- paste0(df$gen, ": ", df$value)
          df$value <- factor(df$value, 
                        levels = unique(df$value[order(df$gen_num, df$value)]))
          df$gen <- factor(df$gen, 
                             levels = unique(df$gen[order(df$gen_num, df$gen)]))
          
        # If data has climate stress exclusion values (AdultExcl1_byGen or 
        # AdultExcl2_byGen), but not all values are climate stress exclusions
        } else if (any(df$value < 0)) {
          if (any(df$value > 0)) {
            df2 <- dplyr::filter(df, !value < 0) # need to remove -2 and -1 vals
            df2 <- mutate(df2, value = ifelse(value < 20, 0, 
                                       ifelse(value < 40, 20, 
                                       ifelse(value < 60, 40, 
                                       ifelse(value < 80, 60, 
                                       ifelse(value < 100, 80, value))))))
            df2$value <- paste0(df2$gen, ": ", df2$value)

            # Recode exclusion values
            excl_df <- df %>% dplyr::filter(value < 0) # take only -2 and -1
            excl_df <- mutate(excl_df, 
                              value = ifelse(value == -2, "excl.-severe", 
                                      ifelse(value == -1, "excl.-moderate", 
                                             value)))
            excl_df$value <- factor(excl_df$value)
            
            # Recombine the processed data frames
            # put exlcusion values and rel. pop size (binned) values
            # back together
            df3 <- rbind(excl_df, df2) 
            df <- df3 # rename dataframe
            df$value <- factor(df$value, 
                        levels = unique(df$value[order(df$gen_num, df$value)]))
            df$gen <- factor(df$gen, 
                             levels = unique(df$gen[order(df$gen_num, df$gen)]))
            
            # If all values are 0
          } else if (all(df$value == 0)) {
            df <- mutate(df, value = ifelse(value == -2, "excl.-severe", 
                                     ifelse(value == -1, "excl.-moderate", 
                                     ifelse(value == 0, "0 gens.", value))))
            # Order by original values so plots in numerical order
            df$value <- factor(df$value, 
                               levels = unique(df$value[order(df$gen_num, 
                                                              df$value)]))
            df$gen <- factor(df$gen, 
                             levels = unique(df$gen[order(df$gen_num, df$gen)]))
          } 
        }
        
        # Make the color key for the legend 
        cols_df <- data.frame("cols" = c(Colfunc("deepskyblue", "blue3", 5), 
          Colfunc("red", "darkred", 5), Colfunc("yellow", "darkgoldenrod3", 5),
          Colfunc("cyan", "cyan4", 5), Colfunc("lightgreen", "darkgreen", 5),
          Colfunc("sienna4", "sienna1", 5), 
          Colfunc("mediumpurple1", "mediumpurple4", 5),
          Colfunc("magenta", "magenta4", 5), 
          Colfunc("burlywood", "burlywood4", 5),
          Colfunc("olivedrab4", "olivedrab1", 5), 
          Colfunc("lightblue4", "lightblue1", 5), 
          Colfunc("lightpink", "deeppink4", 5), 
          Colfunc("slateblue1", "slateblue4", 5)))
        weeks_df <- data.frame(gen = c(rep(0, 5), rep(1, 5), rep(2, 5),
          rep(3, 5), rep(4, 5), rep(5, 5), rep(6, 5), rep(7, 5), rep(8, 5), 
          rep(9, 5), rep(10, 5), rep(11, 5), rep(12, 5)))
        weeks_df$gen <- paste(weeks_df$gen, "gens.")
        col_key <- cbind(cols_df, weeks_df)
        col_key2 <- suppressWarnings(semi_join(col_key, df, by = "gen"))
        # Create bins of 5 to represent abundance of adults 
        # (0 to 100, or just 0 if OWGen)
        col_key2 <- data.frame(col_key2 %>% group_by(gen) %>% 
                                 mutate(value = c(0, 20, 40, 60, 80)))
        col_key2$value <- paste0(col_key2$gen, ": ", col_key2$value)
        
        # For legend, show only one color for each generation 
        # (map will have a gradation of this color)
        # Color for bin value 20 is used for legend (an intermediate color)
        leg_cols <- col_key2 %>% dplyr::filter(grepl(": 60", value))
        leg_cols$value <- str_split_fixed(leg_cols$value, 
                                          pattern = ":", 2)[,1] # Which gen?
        # Bind actual colors and legend colors together and create a named 
        # vector of these
        col_key2 <- rbind(leg_cols, col_key2)
        
        # Create legend breaks to use in plotting function, so only one shade 
        # per gen is shown in legend.
        # Create a fake data frame and attach to real data in order to create
        # a custom legend that shows only gens in the fake on
        gens_df <- data.frame("value" = unique(df$gen),"x" = NA, "y" = NA, 
                              "gen" = unique(df$gen), 
                              "gen_num" = unique(df$gen_num))
        gens_df <- arrange(gens_df, gen_num)
        lgnd_brks <- data.frame("gen" = gens_df$gen) # Legend breaks to use
        
        # Attach the fake data to the real data, and set factor levels according
        # to generation numbers to they are sorted correctly in legend
        df <- rbind(df, gens_df)
        df$value <- factor(df$value, 
                        levels = unique(df$value[order(df$gen_num, df$value)]))
         
        # Add grayscale colors to legend colors if climate stress exclusions
        # Moderate stress exclusion
        if (any(df$value == "excl.-moderate")) {
          col_key2 <- rbind(data.frame("cols" = "gray70", "gen" = NA, 
                                       "value" = "excl.-moderate"), col_key2)
          lgnd_brks <- rbind(
            data.frame("gen" = c(as.character("excl.-moderate"))), lgnd_brks)
        }
        # Severe stress exclusions
        if (any(df$value == "excl.-severe")) {
          col_key2 <- rbind(data.frame("cols" = "gray40", "gen" = NA, 
                                       "value" = "excl.-severe"), col_key2)
          lgnd_brks <- rbind(
            data.frame("gen" = c(as.character("excl.-severe"))), lgnd_brks)
        }
        
        # Convert legend breaks to vector, create lgd. color vector, and plot
        lgnd_brks <- as.vector(lgnd_brks$gen)
        cols <- setNames(as.character(col_key2$cols), col_key2$value) 
        p <- Base_map(df) + 
          scale_fill_manual(values = cols, breaks = lgnd_brks, 
                             name = str_wrap(paste0(lgd), width = 15)) +
          labs(title = str_wrap(paste(sp, titl), width = titl_width), 
               subtitle = str_wrap(subtitl, width = subtitl_width)) +
          theme_map(base_size = base_size) + 
          mytheme
      }
    
    #### * Climate exclusion maps ####
  } else if (grepl("Heat_Stress_Excl|Chill_Stress_Excl|All_Stress_Excl", 
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
                          c("excl.-severe" = "gray40",
                            "excl.-moderate" = "gray70",
                            "not excluded" = "green2"), name = paste0(lgd)) +
      labs(title = str_wrap(paste(sp, titl), width = titl_width),
           subtitle = str_wrap(subtitl, width = subtitl_width)) +
      theme_map(base_size = base_size) + 
      mytheme + 
      theme(legend.text = element_text(size = rel(1.5)), 
            legend.title = element_text(size = rel(1.4), face = "bold"))
      
    #### * Lifestage (currently adults) w/ NumGen maps ####
  } else if (grepl("Adult_byGen|Adult_Excl1_byGen|Adult_Excl2_byGen", outfl)) {
    # Caption for log file
    log_capt <- paste("-", "Relative pop. size of adults for each gen. on", 
                      format(as.Date(d, "%Y%m%d"), "%m/%d/%Y"))
    df$gen <- as.numeric(df$gen) # Generation column must be numeric
    
    # If all values are climate stress exclusions then output map with only 
    # these exclusions
    if (all(df$value < 0)) {
      df <- mutate(df, value = ifelse(value == -2, "excl.-severe", 
                               ifelse(value == -1, "excl.-moderate", NA)))
      df <- mutate(df, gen = ifelse(value == "excl.-severe", "excl.-severe", 
                             ifelse(value == "excl.-moderate", "excl.-moderate", 
                                    NA))) 
      df$value <- factor(df$value, levels = unique(df$value[order(df$value)])) 
      lgnd_brks <- data.frame("gen" = unique(df$value))
      lgnd_brks <- sort(dplyr::pull(lgnd_brks, gen)) # convert df to a vector
      
      # Assign grayscale colors to climate stress exclusion values
      if (all(df$value == "excl.-moderate")) {
        cols <- c("gray70")
        names(cols) <- c("excl.-moderate")
      } else if (all(df$value == "excl.-severe")) {
        cols <- c("gray40")
        names(cols) <- c("excl.-severe")
      } else {
        cols <- c("gray70", "gray40")
        names(cols) <- c("excl.-moderate", "excl.-severe")
      }
      
      # Make the plot
      # NEED TO FIX - why is the scaling for these plots different than others? 
      # Had to adjust scaling of text size to deal with this.. may only be issue 
      # for some states
      p <- Base_map(df) + 
        scale_fill_manual(values = cols, breaks = lgnd_brks, 
                          name = str_wrap(paste0(lgd), width = 15)) +
        labs(title = str_wrap(paste(sp, titl), width = titl_width), 
             subtitle = str_wrap(subtitl, width = subtitl_width)) +
        theme_map(base_size = base_size) + 
        mytheme
      
    # If all values are 0 then output a "blank" map
    } else if (all(df$value == 0)) {
      df$value <- "other stages"
      df$value <- factor(df$value)
      p <- Base_map(df) +        
        scale_fill_manual(values = c("other stages" = "gray90"), 
                          name = paste0(lgd)) +
        labs(title = str_wrap(paste(sp, titl), width = titl_width), 
             subtitle = str_wrap(paste(subtitl), width = subtitl_width)) +
        theme_map(base_size = base_size) + 
        mytheme
      
      # If values are not all 0, check if climate stress exclusions 
      # values (-2, -1) are in data
    } else if (any(df$value > 0)) { 
      
      # No climate stress exclusion data (Adult_byGen)
      if (all(df$value >= 0)) {
        df <- mutate(df, value = ifelse(value == 0, 0, 
                                 ifelse(value != 0 & value < 20, 1, 
                                 ifelse(value < 40, 20, 
                                 ifelse(value < 60, 40, 
                                 ifelse(value < 80, 60, 
                                 ifelse(value < 100, 80, value)))))))
        df$value <- paste(df$gen, "gens.:", df$value)
        df <- mutate(df, value = ifelse(grepl(": 0", value), 
                                        "other stages", value))
        gens_df <- data.frame("value" = paste(unique(df$gen), "gens."), 
                              "x" = NA, "y" = NA, "gen" = unique(df$gen))
        df <- rbind(df, gens_df)
        df$value <- factor(df$value, levels = 
                             unique(df$value[order(df$gen, df$value)]))
        
        # If data has climate stress exclusion values (AdultExcl1_byGen or 
        # AdultExcl2_byGen), then need to format accordingly
      } else if (any(df$value < 0)) {
        
        # If any value is > 0, then format this way
        if (any(df$value > 0)) {
          df2 <- dplyr::filter(df, !value < 0) # need to remove -2 and -1 vals
          df2 <- mutate(df2, value = ifelse(value == 0, 0, 
                                     ifelse(value != 0 & value < 20, 1, 
                                     ifelse(value < 40, 20, 
                                     ifelse(value < 60, 40, 
                                     ifelse(value < 80, 60, 
                                     ifelse(value < 100, 80, value)))))))
          df2$value <- paste(df2$gen, "gens.:", df2$value)
          df2 <- mutate(df2, value = ifelse(grepl(": 0", value), 
                                          "other stages", value))
          gens_df <- data.frame("value" = paste(unique(df2$gen), "gens."), 
                                "x" = NA, "y" = NA, "gen" = unique(df2$gen))
          df2 <- rbind(df2, gens_df)
          df2$value <- factor(df2$value, levels = 
                               unique(df2$value[order(df2$gen, df2$value)]))          
          # Recode exclusion values
          excl_df <- df %>% dplyr::filter(value < 0) # take only -2 and -1 vals
          excl_df <- mutate(excl_df, value = ifelse(value == -2, "excl.-severe", 
            ifelse(value == -1, "excl.-moderate", value)))
          excl_df$gen <- -9
          excl_df$value <- factor(excl_df$value)
          
          # Recombine the processed data frames (puts stress exclusion values 
          # and Lifestage rel. pop size (binned) values back together
          df3 <- rbind(excl_df, df2) 
          df <- df3 # rename dataframe
          df$gen <- as.numeric(df$gen)
          
          # Else if data contains nothing but climate stress exclusion values,
          # then needs to be formatted differently
        } else if (all(df$value <= 0)) {
          df <- mutate(df, value = ifelse(value == 0, "other stages", 
            ifelse(value == -2, "excl.-severe", 
            ifelse(value == -1, "excl.-moderate", value))))
          df$value <- factor(df$value, 
                             levels = unique(df$value[order(df$value)]))
        }
      }
      
      # Convert df$gen to numeric, or will get errors below
      df$gen <- as.numeric(df$gen)
      
      # Make the color key 
      cols_df <- data.frame("cols" = c(Colfunc("deepskyblue", "blue3", 5), 
        Colfunc("red", "darkred", 5), Colfunc("yellow", "darkgoldenrod3", 5),
        Colfunc("cyan", "cyan4", 5), Colfunc("lightgreen", "darkgreen", 5),
        Colfunc("sienna4", "sienna1", 5), 
        Colfunc("mediumpurple1", "mediumpurple4", 5),
        Colfunc("magenta", "magenta4", 5), 
        Colfunc("burlywood", "burlywood4", 5),
        Colfunc("olivedrab4", "olivedrab1", 5), 
        Colfunc("lightblue4", "lightblue1", 5), 
        Colfunc("lightpink", "deeppink4", 5), 
        Colfunc("slateblue1", "slateblue4", 5)))
      weeks_df <- data.frame(gen = c(rep(0, 5), rep(1, 5), rep(2, 5),
        rep(3, 5), rep(4, 5), rep(5, 5), rep(6, 5), rep(7, 5), rep(8, 5), 
        rep(9, 5), rep(10, 5), rep(11, 5), rep(12, 5)))
      #weeks_df$gen <- paste(weeks_df$gen, "gens.")
      col_key <- cbind(cols_df, weeks_df)
      col_key2 <- semi_join(col_key, df, by = "gen") 
      #col_key2 <- mutate(col_key2, gen = ifelse(gen == 0, "OW", gen))
      #col_key2$gen <- as.factor(col_key2$gen)
      # Create bins of 5 to represent abundance of adults 
      num_gens <- length(unique(col_key2$gen)) # how many unique gens in data?
      col_key2$value <- rep(c(1, 20, 40, 60, 80), num_gens) # give unique gen 5 bins
      col_key2$value <- paste(col_key2$gen, "gens.:", col_key2$value)
      col_key2$value <- factor(col_key2$value, 
                        levels = unique(col_key2$value[order(col_key2$value)])) 
      
      #col_key2$value <- paste0("Gen", col_key2$gen, ": ", col_key2$value)
      # Add on a color for the "other stages" category
      noAdults_col <- data.frame(cols = c("gray90"), gen = NA, 
                                 value = as.factor("other stages"))
      col_key2 <- rbind(noAdults_col, col_key2)
      
      # For legend, show only one color for each generation 
      # (map will have a gradation of this color)
      leg_cols <- col_key2 %>% dplyr::filter(grepl(": 60", value))
      leg_cols$value <- str_split_fixed(leg_cols$value, pattern = ":", 2)[,1]
      # Bind actual colors and legend colors together and create a 
      # named vector of these
      col_key2 <- rbind(col_key2, leg_cols)
      # Breaks to use in plotting function, so only one shade per gen 
      # is shown in legend
      lgnd_brks <- as.character(gens_df$value)
      lgnd_brks <- append(c("other stages"), lgnd_brks) # add "other stages"
      
      # Add grayscale colors to legend colors if climate stress exclusions
      if (any(df$value == "excl.-moderate")) {
        col_key2 <- rbind(data.frame("cols" = "gray70", "gen" = NA, 
                                     "value" = "excl.-moderate"), col_key2)
        lgnd_brks <- append("excl.-moderate", lgnd_brks)
      }
      if (any(df$value == "excl.-severe")) {
        col_key2 <- rbind(data.frame("cols" = "gray40", "gen" = NA,
                                     "value" = "excl.-severe"), col_key2)
        lgnd_brks <- append("excl.-severe", lgnd_brks)
      }
      
      # Convert colors to a vector and plot
      cols <- setNames(as.character(col_key2$cols), col_key2$value) 
      p <- Base_map(df) + 
        scale_fill_manual(values = cols, breaks = lgnd_brks,
                          name = str_wrap(paste0(lgd), width = 15)) +
        labs(title = str_wrap(paste(sp, titl), width = titl_width), 
             subtitle = str_wrap(subtitl, width = subtitl_width)) +
        theme_map(base_size = base_size) + 
        mytheme     
      }
    
    #### * Pest Event Maps ####
  } else if (grepl("Avg|Earliest", outfl)) {
    log_capt <- paste("-", titl_orig) # Caption for log file
    
    # Format the data, especially the value column
    df <- df %>% 
      dplyr::filter(!(value %in% c(0, 366))) # remove day 0 and day 366
    df$value <- round(df$value)
    # TO DO: try to fix this so that don't end up with 6 weeks in Dec
    # Currently must change value for day 364 and 365 to day 363, or end up w/
    # 6 weeks in December for some years
    df <- df %>% mutate(value = ifelse(value >= 364, 363, value))
    df$value_orig <- df$value # Create for ordering recoded values later
    # Convert day of year to a date, convert date to a week of the year and 
    # format to "month-day" format
    # NEED TO FIX? - why does as.Date always get the date 1 day off? 
    df$value <- as.Date(df$value, origin = 
                          as.Date(paste0(start_year, "-01-01"))) - 1
    df$value <- cut.POSIXt(strptime(df$value, format = "%Y-%m-%d"), 
                           breaks = "1 weeks") 
    # Reformat the dates to this format: 2019-Jan-01
    # Clim. exc. values (-1 and -2) will become NA
    df$value <- format(strptime(df$value, format = "%Y-%m-%d"), 
                       format = "%b-%d")
    
    # Generate a key for colors for every week of the year, allowing up to 
    # 5 weeks per month
    cols_df <- data.frame("cols" = 
      c(Colfunc("darkseagreen1", "darkseagreen4", 5),
        Colfunc("rosybrown1", "rosybrown4", 5),
        Colfunc("royalblue1", "royalblue4", 5),
        Colfunc("mediumpurple1", "mediumpurple4", 5), 
        Colfunc("magenta", "magenta4", 5), Colfunc("cyan", "cyan4", 5),
        Colfunc("greenyellow", "green4", 5), 
        Colfunc("yellow", "darkgoldenrod3", 5), 
        Colfunc("sienna1", "sienna4", 5),
        Colfunc("red", "darkred", 5), Colfunc("lightpink", "deeppink4", 5),
        Colfunc("skyblue", "skyblue4", 5))) # Colors
    weeks_df <- data.frame("mnth" = c(rep("Jan", 5), rep("Feb", 5), 
      rep("Mar", 5), rep("Apr", 5), rep("May", 5), rep("Jun", 5), rep("Jul", 5),
      rep("Aug", 5), rep("Sep", 5), rep("Oct", 5), rep("Nov", 5), 
      rep("Dec", 5))) # 5 weeks per month
    weeks_df <- data.frame(weeks_df %>% group_by(mnth) %>% # Group by month
      mutate(mnth_wk = row_number()) %>% # Assign unique row # to represent week #
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
      mutate(mnth_wk = row_number()) %>% 
      mutate(mnth_wk = paste(mnth, mnth_wk, sep = "_"))
    
    # Necessary for removing weeks that are not in the data in the col_key2
    mnth_ct <- data.frame(dats %>% group_by(mnth) %>% 
      dplyr::mutate(freq = n_distinct(value))) %>%
      arrange(., mnth) %>%
      group_by(mnth) %>% # Group by month
      mutate(mnth_wk = row_number()) %>% # Assign unique row # to repr. week #
      mutate(mnth_wk = paste(mnth, mnth_wk, sep = "_"))
    
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
        col_key2 <- rbind(data.frame("value" = "excl.-sev.", "cols" = "gray40"), 
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
     
     # Need to adjust number of rows in legend for very small plots or 
     # the legend will go off the page
     if (asp < 0.5) {
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
      cat("Could not create plot for ", outfl, "_", d, ".png\n", sep = "", 
          file = Model_rlogging, append = TRUE) 
    } )
}

#### (15). PlotMap_stress: summary map plotting - stress units ####
# Create summary maps (PNG) of heat stress and chill stress units, 
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
  
  # If all values are 0, then don't include contours 
  # (must include this code or it will throw an error)
  if (all(df$value == 0)) {
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
# climate stress exclusions (Excl1) in areas under climate 
# stress (i.e., where Lifestage overlaps with Excl1)
Rast_Subs_Excl1 <- function(brk) {
  # Get all climate stress exclusion data
  allEXCL_patrn <- glob2rx("*All_Stress_Excl*.tif$")
  allEXCL_brk <- brick(list.files(pattern = allEXCL_patrn))
  # If PEM, then only take the last layer of allEXCL_brk (last date)
  if (deparse(substitute(brk)) == "PEM") {
    allEXCL_brk <- allEXCL_brk[[last(nlayers(allEXCL_brk))]] # Keep last layer
  }
  lapply(1:nlayers(brk), function(lyr) {
    brk_lyr <- brk[[lyr]]
    Excl <- allEXCL_brk[[lyr]]
    # Create logical index (for later comparison)
    Excl1 <- Excl == -2   # Only severe exclusion
    brk_lyr[Excl1] <- Excl[Excl1] 
    return(brk_lyr)
  })
}

# Substitutes weighted lifestage raster values with severe (-2) 
# and moderate (-1) climate stress exclusions (Excl2) in areas under climate 
# stress (i.e., where Lifestage overlaps with Excl2)
Rast_Subs_Excl2 <- function(brk) {
  # Get all climate stress exclusion data
  allEXCL_patrn <- glob2rx("*All_Stress_Excl*.tif$")
  allEXCL_brk <- brick(list.files(pattern = allEXCL_patrn))
  # If PEM, then only take the last layer (last date)
  # If PEM, then only take the last layer of allEXCL_brk (last date)
  if (deparse(substitute(brk)) == "PEM") {
    allEXCL_brk <- allEXCL_brk[[last(nlayers(allEXCL_brk))]] # Keep last layer
  }
  lapply(1:nlayers(brk), function(lyr) {
    brk_lyr <- brk[[lyr]]
    Excl <- allEXCL_brk[[lyr]]
    Excl2 <- Excl < 0   # both severe and moderate exclusion
    brk_lyr[Excl2] <- Excl[Excl2] 
    return(brk_lyr)
  })
}

#### (17). RegCluster: register cluster for parallel computing ####
# Specifies the number of clusters to use for parallel computation based on 
# the number of cohorts in the model, and whether tiles (for CONUS or EAST)
# are also being run in parallel
RegCluster <- function(ncohort) {
  # If region is CONUS or EAST, need to leave some cores available 
  # for parallel computing of tiles
  if (region_param %in% c("CONUS", "EAST")) {
    ncores <- 10
  # If region is not CONUS or EAST (no tiles in parallel), then have more cores 
  # to devote to cohorts
  } else {
    ncores <- 15
  }
  cl <<- makePSOCKcluster(ncores) # export to global environment
  # If run is being done on Hopper, need to specify the library for each worker
  if (Sys.info()["nodename"] == "hopper.science.oregonstate.edu") {
    clusterEvalQ(cl, .libPaths("/usr/local/lib64/R/library/"))
  }
  registerDoParallel(cl)
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
# Deal with 0 vs non-0 values when plotting chill and heat stress unit rasters
# Only create bins (0-10, etc...) if there are non-zero values in the data 
Stress_Val_Conv <- function(x) {
  if (all(x$value == 0)) {
    x2 <- x
  } else {
    x2 <- Cut_bins(x, 10)
  }
  x2$value <- factor(x2$value)
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
