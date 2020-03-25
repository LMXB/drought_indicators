library(leaflet)
library(htmltools)
library(htmlwidgets)

# This tells htmlwidgets about our plugin name, version, and
# where to find the script. (There's also a stylesheet argument
# if the plugin comes with CSS files.)
esriPlugin <- htmlDependency("leaflet.esri", "1.0.3",
                             src = c(href = "https://cdn.jsdelivr.net/leaflet.esri/1.0.3/"),
                             script = "esri-leaflet.js"
)

gesturePlugin = htmlDependency("Leaflet.GestureHandling", "1.1.8",
                               src = c(href = "https://unpkg.com/leaflet-gesture-handling@1.1.8/dist/"),
                               stylesheet = c(href = "leaflet-gesture-handling.min.css"),
                               script = "leaflet-gesture-handling.min.js"
)

# A function that takes a plugin htmlDependency object and adds
# it to the map. This ensures that however or whenever the map
# gets rendered, the plugin will be loaded into the browser.
registerPlugin <- function(map, plugin) {
  map$dependencies <- c(map$dependencies, list(plugin))
  map
}


leaflet() %>%
  # Register ESRI plugin on this map instance
  registerPlugin(esriPlugin) %>%
  registerPlugin(gesturePlugin) %>%
  # Add your custom JS logic here. The `this` keyword
  # refers to the Leaflet (JS) map object.
  # htmlwidgets::onRender("
  #           function(el,x) {
  #               map = this;
  #               L.esri.basemapLayer('Topographic').addTo(map);
  #           }
  #       ") %>%
  htmlwidgets::onRender("
            function(el,x) {
                this.gestureHandling.enable();
                L.esri.basemapLayer('Topographic').addTo(this);
            }
        ") 


test
htmlwidgets::saveWidget(test, "/home/zhoylman/temp/base_map_test.html")







leafletProviders <- "var OpenStreetMap_Mapnik = L.tileLayer('https://api.maptiler.com/tiles/hillshades/{z}/{x}/{y}.png?key=KZO7rAv96Alr8UVUrd4a', {
                                                maxZoom: 19
                                                });"
# // Map definition
map <- "var map = L.map('map', {
                  center: [-34.7247, -56.1237],
                  zoom: 8,
                  layers: [OpenStreetMap_Mapnik]
                  });"

# // Basemaps
baseMaps <- "var baseMaps = {'OpenStreetMap': OpenStreetMap_Mapnik};"

# // Add a layer control element to the map
layerControl <- "layerControl = L.control.layers(baseMaps, null, {
                                position: 'topleft'});
                                layerControl.addTo(map);"

temp = HTML(paste("<script>", "\n", leafletProviders, "\n", map, "\n", baseMaps, "\n", layerControl,"\n", "</script>", sep = ""))

test = leafletOutput("leaflet", width = "100%", height = 400, expr = temp)


htmlwidgets::saveWidget(test, "/home/zhoylman/temp/base_map_test.html")
