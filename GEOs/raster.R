#Map

source("./dataLoading.R")
#-------------

pal <- colorNumeric(c("#0C2C84", "#41B6C4", "#FFFFCC"), values(Paraiba),
                    na.color = "transparent")


map <- leaflet() %>% addTiles(group = "OSM (default)") %>% 
  setView(lng = -37.55, lat = -6.97, zoom = 9) %>%
  addCircles(lng = sites$lon, lat = sites$lat, radius = 5000, 
             label = sites$NOME, fillOpacity = 0.01, group = "sites") %>%
  addCircles(lng = sites$lon, lat = sites$lat, radius = 10, 
             label = sites$NOME, group = "sites") %>%
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
  addRasterImage(Paraiba, col=viridis(500) ,opacity = 0.8, maxBytes = 10*1024*1024, 
               project = TRUE, group = "Elevation") %>%
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
  addLayersControl(
    baseGroups = c("OSM (default)"),
    overlayGroups = c("County limits", "sites", "ERBs", "Air Traffic", 
                      "Public Lands", "Private Lands",
                      "Elevation", "UCs" ),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  hideGroup("Elevation") %>%
  addMeasure(primaryLengthUnit = "meters", primaryAreaUnit = "sqmeters", localization = "pt_BR")

