#!/bin/sh
# run cron job to create LBAM maps for CAPS with DDRP v2 (cohorts)
cd /usr/local/dds/DDRP_B1
./DDRP_v2.R --spp LBAM --forecast_data PRISM --start_year 1990_daily_30yr --start_doy 1 --end_doy 365 --keep_leap 1 --region_param CONUS --exclusions 1 --pems 1 --mapA 1 --mapE 0 --mapL 0 --mapP 1 --out_dir LBAM_30yr --out_option 1 --ncohort 7 --odd_gen_map 0
