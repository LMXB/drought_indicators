#!/bin/bash

# Nightly push to Bitbucket

HOME=/home/zhoylman/drought_indicators ssh -vT git@github.com
USERNAME='zhoylman'

# Set some variables
DAY=$(date +%F);

# Make sure we run as root
if [ "$(whoami)" != "root" ]; then
    echo "Only root can do this.";
    exit 1;
else
    # Make sure we are in the right directory
    cd /home/zhoylman/drought_indicators;
    # Now add any changes
    git add .;
    # Now commit
    git commit -m "$DAY Daily su push test";
    git push origin master;
fi;