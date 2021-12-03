#!/bin/sh
# run cron job to create EAB maps for CAPS with DDRP v2 (cohorts)
cd /usr/local/dds/DDRP_B1
./DDRP_v2.R --spp SUNP --forecast_data PRISM --start_year 2012 --start_doy 1 --end_doy 365 --keep_leap 1 --region_param CONUS --exclusions_stressunits 1 --pems 1 --mapA 1 --mapE 1 --mapL 1 --mapP 1 --out_dir SUNP_2012 --out_option 1 --ncohort 7 --odd_gen_map 0
