library(RgoogleMaps)
library(servr)
#Carrega Dados e leaflet do MAPA

source("./analisaRandom.R")
coordinates(silencio) <- c("lon","lat")
leaflet() %>% addTiles(group = "OSM (default)") %>% 
  setView(lng = -37.55, lat = -6.97, zoom = 9) %>%
  addCircles(lng = silencio$lon, lat = silencio$lat, radius = 500, 
             label = silencio$Mun, fillOpacity = 0.01, group = "sites") %>%
  addCircles(lng = silencio$lon, lat = silencio$lat, radius = 10, 
             label = silencio$Ruido, group = "sites") %>%
  addCircles(lng = ERBs$Longitude, lat = ERBs$Latitude, weight = 3, radius=40, 
             color="firebrick", stroke = TRUE, fillOpacity = 0.4, group = "ERBs") %>%
  addCircles(lng = AEREOS$lon, lat = AEREOS$lat, radius = 500, color = "violet", 
             label = AEREOS$NOME, group = "Air Traffic") %>%
  addPolylines(lng = linha1$lon, linha1$lat, dashArray = "5, 5, 1, 5" ,opacity = 1, weight = 2, 
               color = "goldenrod", group = "Air Traffic") %>%
  addPolylines(lng = linha2$lon, linha2$lat, dashArray = "5, 5, 1, 5" ,opacity = 1, weight = 2, 
               color = "goldenrod", group = "Air Traffic") %>%
  addPolylines(lng = linha3$lon, linha3$lat, dashArray = "5, 5, 1, 5" ,opacity = 1, weight = 2, 
               color = "goldenrod", group = "Air Traffic") %>%
  addPolylines(lng = linha4$lon, linha4$lat, dashArray = "5, 5, 1, 5" ,opacity = 1, weight = 2, 
               color = "goldenrod", group = "Air Traffic") %>% 
  addPolylines(lng = linha5$lon, linha5$lat, dashArray = "5, 5, 1, 5" ,opacity = 1, weight = 2, 
               color = "goldenrod", group = "Air Traffic") %>%
  addPolygons(data = municipios, col="white" , weight = 2, fill = FALSE, label = municipios$NOME_MUN1, 
              group = "County limits") %>%
  addPolygons(data = imoveisPublicosPB, col="forestgreen" , weight = 2, 
              fill = TRUE, opacity = 1, label = imoveisPublicosPB$NOME_IMO9  ,group = "Public Lands") %>%
  addPolygons(data = imoveisPublicosPB2, col="forestgreen" , weight = 2, 
              fill = TRUE, opacity = 1,label = imoveisPublicosPB2$DETENTOR10 ,group = "Public Lands") %>%
  addPolygons(data = glebasFederais, col="darkmagenta" , weight = 2, 
              fill = TRUE, opacity = 1, group = "Public Lands") %>%
  addPolygons(data = imoveisParticularesPB, col = "sienna1", weight = 1,
              label = imoveisParticularesPB$NOME_IMO9,
              fill = TRUE, opacity = 0.5, group = "Private Lands") %>%
  addPolygons(data = imoveisParticularesPB2, col = "salmon1", weight = 1, 
              label = imoveisParticularesPB2$NOME_ARE9,
              fill = TRUE, opacity = 0.5, group = "Private Lands") %>%
  addPolygons(data = UCs, col = "limegreen", weight = 3, fill = TRUE, opacity = 1, 
              label = UCs$nome, group = "UCs") %>%
  addPolygons(data = RPPNs, col = "limegreen", weight = 3, fill = TRUE, opacity = 1, 
              label = RPPNs$nome, group = "UCs") %>%
  addScaleBar() %>%
  addRasterImage(Paraiba, col=viridis(500) ,opacity = 0.8, maxBytes = 100*1024*1024, 
               project = TRUE, group = "Elevation") %>%
  addLayersControl(
    baseGroups = c("OSM (default)"),
    overlayGroups = c("County limits", "sites", "ERBs", "Air Traffic", 
                      "Public Lands", "Private Lands",
                      "Elevation", "UCs","SitesCalculados" ),
    options = layersControlOptions(collapsed = FALSE)) %>%
      hideGroup("Elevation")



####Get maps

for (Zoom in 10:19){
  GetMapTiles(center=c(lon = -36.6, lat = -7.6),  
              zoom = Zoom, nTiles = round(c(100,100)/(20-Zoom)),
              urlBase = "http://mt1.google.com/vt/lyrs=m", tileDir= "./mapTiles/Google/")
}

for (Zoom in 10:19){
  GetMapTiles(center=c(lon = -36.6, lat = -7.6),  
              zoom = Zoom, nTiles = c(100,100),
              urlBase = "http://mt1.google.com/vt/lyrs=y", tileDir= "./mapTiles/Google/hybrid")
}


for (Zoom in 15:19){
  GetMapTiles(center=c(lon = -36.6, lat = -7.6),  
              zoom = Zoom, nTiles = c(10,10),
              urlBase = "http://mt1.google.com/vt/lyrs=y", tileDir= "./mapTiles/Google/hybrid")
}

#for (Zoom in 13:19){
#  GetMapTiles(center=c(lat = 36.6, lon = -7.6),  
#              zoom = Zoom, nTiles = round(c(50,50)/(20-Zoom)),
#              urlBase = "http://mt1.google.com/vt/lyrs=m", tileDir= "./mapTiles/Google/Sat")
#}

deamon_id <- servr::httd(port = 8000, daemon = TRUE)

library(leaflet)
leaflet() %>% addTiles(urlTemplate = "http:/localhost:8000/mapTiles/{z}_{x}_{y}.png") %>%
  setView(-36.6,  -7.6, zoom = 16)


servr::daemon_stop(deamon_id)

leaflet() %>% addTiles() %>%
  setView(-36.6,  -7.6, zoom = 10)
