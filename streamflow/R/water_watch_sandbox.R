library(imager)

temp = tempfile()

download.file("https://waterwatch.usgs.gov/wwapps/wwdur4.php?sno=12340000&yr=2019&ytp=wy&dt=dv01d&nyr=1&xps=line&nyor=10&legend=0&otp=plot",
              destfile = temp, mode = 'wb')

url = "https://waterwatch.usgs.gov/wwapps/wwdur4.php?sno=12340000&yr=2019&ytp=wy&dt=dv01d&nyr=1&xps=line&nyor=10&legend=0&otp=plot"

###try this

load.image(url) %>% plot

plot(test)

jj <- readPNG(temp,native=TRUE)

plot(0:1,0:1,type="n",ann=FALSE,axes=FALSE)
rasterImage(jj,0,0,1,1)