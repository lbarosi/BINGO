#Leaftlet Shiny Map
#Mapa da Paraíba com diferentes layes
#Função interativa para calculo de atenuiação de ERBs
#Lucian Barosi
#28/Jun/2017
library(shiny)



#Carrega Dados e leaflet do MAPA
source("./dataLoading.R")
#Carrega função para calculo de atenuação
#source("./findSites.R")

#-----------------------------------------------------------------------
ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("BINGOmap", width = "100%", height = "100%")
)

server <- function(input, output, session) {
  
  
  output$map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    leaflet() %>% addTiles(group = "OSM (default)") %>% 
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
      addScaleBar()
      #addRasterImage(Paraiba, col=viridis(500) ,opacity = 0.8, maxBytes = 10*1024*1024, 
      #               project = TRUE, group = "Elevation") %>%
       #%>%
      # addLayersControl(
      #   baseGroups = c("OSM (default)"),
      #   overlayGroups = c("County limits", "sites", "ERBs", "Air Traffic", 
      #                     "Public Lands", "Private Lands",
      #                     "Elevation", "UCs","SitesCalculados" ),
      #   options = layersControlOptions(collapsed = FALSE)
      ## %>%
      #hideGroup("Elevation") %>%
      #addMeasure(primaryLengthUnit = "meters", primaryAreaUnit = "sqmeters", localization = "pt_BR")
    
  })
#--------------------------------------------  
  # # # ## Observe mouse clicks and add circles
  #    observeEvent(input$map_click, {
  #      ## Get the click info like had been doing
  #      click <- input$map_click
  #      clat <- click$lat
  #      clng <- click$lng
  #      geocoord <- c(clng,clat)
  #      noise <- Signal(geocoord)
  #      Ruido <- noise$Ruído
  #      Antenas <- noise$`N. Estações`
  #      Distancia <- noise$`Distância min`
  #      label <- paste(clng,clat,"|dB=-",Ruido,"|N.=",Antenas,"|Dist=",Distancia, sep = "")
  #   
  #      leafletProxy('map') %>% # use the proxy to save computation
  #        addCircles(lng=clng, lat=clat, group='circles',
  #                   weight=1, radius=500, color='black', fillColor='orange',
  #                   popup=label, fillOpacity=0.5, opacity=1)
  #   
  #    })
  #  
   
}
  


shinyApp(ui, server)
