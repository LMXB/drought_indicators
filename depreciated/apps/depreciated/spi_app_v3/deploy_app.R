library(rsconnect)
library(rmarkdown)

Sys.setenv(RSTUDIO_PANDOC="C:/Program Files/RStudio/bin/pandoc")

deployApp("D:\\Git_Repo\\drought_indicators\\apps\\spi_app_v3\\UMRB_Markdown.Rmd", 
          appName = "UMRB_Drought_Guide", account = "montana-climate-office-hoylman",
          forceUpdate = T, launch.browser = F)
