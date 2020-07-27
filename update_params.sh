#!/bin/bash

# Set origin - already did this so not needed
git remote add origin git@github.com:bbarker505/ddrp_v2.git/spp_params

# Get date to put in commit message
currentDate=`date`
currentDate=`date -d "${currentDate}" '+%d-%b-%Y'`

# Add commit and push
git add .
git commit -m "updated spp params ${currentDate}"
git push origin master
