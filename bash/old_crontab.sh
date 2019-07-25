####################  Automate app update everyday  ########################

MAILTO="zhoylman@gmail.com"

1 0 * * * Rscript /home/zhoylman/drought_indicators/spi_app/R/SPI_Map.R >/home/zhoylman/cron-log/cron-log-spi 2>&1

1 1 * * * Rscript /home/zhoylman/drought_indicators/spei_app/R/SPEI_Map.R >/home/zhoylman/cron-log/cron-log-spei 2>&1

1 2 * * * Rscript /home/zhoylman/drought_indicators/eddi_app/R/EDDI_Map.R >/home/zhoylman/cron-log/cron-log-eddi 2>&1

1 3 * * * Rscript /home/zhoylman/drought_indicators/snotel/R/download_current.R >/home/zhoylman/cron-log/cron-log-snotel 2>&1

30 3 * * * Rscript /home/zhoylman/drought_indicators/snotel/R/plot_current.R

1 4 * * * \cp -r "/home/zhoylman/drought_indicators" "/srv/shiny-server/"

############################################################################
