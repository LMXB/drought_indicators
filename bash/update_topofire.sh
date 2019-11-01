#!/bin/bash

scp -r zhoylman@topofire.dbs.umt.edu:/mnt/DataDrive2/data/drought_indices/sedi/current/ /home/zhoylman/drought_indicators/sedi_app/maps/topofire/  >/home/zhoylman/cron-log/topofire_scp 2>&1
ssh zhoylman@topofire.dbs.umt.edu ls -l /mnt/DataDrive2/data/drought_indices/sedi/archive >/home/zhoylman/drought_indicators/sedi_app/maps/topofire/current_time/time 2>&1