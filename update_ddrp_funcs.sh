#!/bin/bash

# Set origin - already did this so not needed
#git remote add origin git@github.com:bbarker505/ddrp_v2.git

# Get date to put in commit message
currentDate=`date`
currentDate=`date -d "${currentDate}" '+%d-%b-%Y'`

# Add commit and push
git add DDRP_v2_funcs.R
git commit -m "updated DDRP functions ${currentDate}"
git push origin master
