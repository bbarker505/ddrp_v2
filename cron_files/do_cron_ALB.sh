#!/bin/sh
# run cron job to create ALB maps for CAPS with DDRP v2 (cohorts)
cd /usr/local/dds/DDRP_B1
./DDRP_v2.R --spp ALB --forecast_data PRISM --start_year 2020 --start_doy 1 --end_doy 366 --keep_leap 1 --region_param CO --exclusions_stressunits 1 --pems 1 --mapA 1 --mapE 1 --mapL 0 --mapP 0 --out_dir ALB_2020_new --out_option 1 --ncohort 7 --odd_gen_map 0
