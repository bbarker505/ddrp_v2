#!/bin/sh
# run cron job to create FCM2 maps for CAPS with DDRP v2 (cohorts)
cd /usr/local/dds/DDRP_B1
./DDRP_cohorts_v1.R --spp FCM2 --forecast_data PRISM --start_year 2020 --start_doy 1 --end_doy 365 --keep_leap 1 --region_param FL --exclusions_stressunits 1 --pems 0 --mapA 1 --mapE 1 --mapL 1 --mapP 1 --out_dir FCM2_test --out_option 1 --ncohort 1 --odd_gen_map 0
