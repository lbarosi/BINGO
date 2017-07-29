library(ggmap)
library(ggsn)
source("./analisaRandom.R")

#'Fazer
#'dataframe para sitios programados para visita
#'erbsonsight para sitios programados
#'levelplot e ggplot para sitios programados
#'informação de ruído para sitios programados
#'Definir sitios para visitar com analisarandom em 50 pontos perto de sitios provaveis 
#'do dataframe silencioCariri

silencioCariri <- filter(silencio, Mun %in% c("SUMÉ","CAMALAÚ","CONGO", "SERRA BRANCA", "SÃO JOSÉ DOS CORDEIROS"))
silencioCariri %<>% arrange(., Ruido)

sitesvisitados <- read.csv("./RandomSites/Visitados-campanha2.csv")
######################
#Insere coluna com dados de propriedades
coordinates(sitesvisitados) <- c("lon","lat")
proj4string(sitesvisitados) <- proj4string(municipios)
sitesvisitados$Particular1  <- over(sitesvisitados,imoveisParticularesPB)$NOME_IMO9
sitesvisitados$Particular2  <- over(sitesvisitados,imoveisParticularesPB2)$NOME_ARE9
sitesvisitados$Publico1     <- over(sitesvisitados,imoveisPublicosPB)$NOME_IMO9
sitesvisitados$Publico2     <- over(sitesvisitados,imoveisPublicosPB2)$NOME_ARE9
sitesvisitados$UCs          <- over(sitesvisitados,UCs)$nome

#usar over
#Insere distancia de linhas aéreas
#Cuidado, tem usar PMIN em mutate para fazer rowwise

sitesvisitados$AEREO1         <- dist2Line(sitesvisitados,l1) %>% as.data.frame() %>% dplyr::select(., distance)
sitesvisitados$AEREO2         <- dist2Line(sitesvisitados,l2) %>% as.data.frame() %>% dplyr::select(., distance)
sitesvisitados$AEREO3         <- dist2Line(sitesvisitados,l3) %>% as.data.frame() %>% dplyr::select(., distance)
sitesvisitados$AEREO4         <- dist2Line(sitesvisitados,l4) %>% as.data.frame() %>% dplyr::select(., distance)
sitesvisitados$AEREO5         <- dist2Line(sitesvisitados,l5) %>% as.data.frame() %>% dplyr::select(., distance)

sitesvisitados %<>% as.data.frame()
names(sitesvisitados) <- c("Mun","lon", "lat", "Ruido", "Antenas", "Dmin", "Opt", 
                     "Part1", "Part2", "Publ1", "Publ2", "UCs", "AEREO1",
                     "AEREO2", "AEREO3", "AEREO4","AEREO5")

sitesvisitados <- sitesvisitados %>% as.data.frame() %>%
  mutate(., AEREO = pmin(AEREO1, AEREO2, AEREO3, AEREO4, AEREO5)) %>%
  dplyr::select(., -AEREO1,-AEREO2, -AEREO3, -AEREO4,-AEREO5)





####################
silencioCariri <- rbind(sitesvisitados, silencioCariri)



sitiosZoomCariri <- function(s, z, h){
  
  sitio <- silencioCariri
  passo <- z
  n <- s  
  
  silencioCaririC <- silencioCariri
  coordinates(silencioCaririC) <- c("lon","lat")
  elev <- silencioCaririC[n,] %>% SpatialPoints() %>% extract(Paraiba,.) %>% mean()
  
  Xlim <- c(sitio$lon[[n]]-passo,sitio$lon[[n]]+passo)
  Ylim <- c(sitio$lat[[n]]-passo,sitio$lat[[n]]+passo)
  
  title <- paste("Lon = ",round(sitio$lon[[n]],4),", Lat = ",round(sitio$lat[[n]],4), ", Elev = ", elev,"m",sep="")
  
  
  Extend <- c(unlist(Xlim),unlist(Ylim))
  
  mapa <- crop(Paraiba, extent(Xlim[[1]],Xlim[[2]], Ylim[[1]], Ylim[[2]]))
  zmin <- (minValue(mapa) %/% 100) * 100
  zmax <- (maxValue(mapa) %/% 100) * 100
  
  my.at <- seq(zmin, zmax, h)
  
  zoomMap<- levelplot(Paraiba, 
                      layers = 1, 
                      colorkey = list(space="bottom"),
                      contour = TRUE,
                      main = title,
                      xlim = Xlim,
                      ylim = Ylim,
                      at = my.at,
                      drop.unused.levels = TRUE,
                      margin = FALSE,
                      par.settings = viridisTheme()
  )
  
  result <- zoomMap + layer(sp.points(ERBs, col = "red", pch="+", cex=1)) + 
    layer(sp.lines(hrr.shp, col = "white", lwd = 0.8))+
    layer(sp.points(silencioDF, col = "darkorange", cex=1, pch = 9))
  
  return(result)
  
}

#---------------------


map <- leaflet() %>% addTiles() %>% 
  setView(lng = -38.2, lat = -7, zoom = 10) %>%
  addCircles(lng = silencioCariri$lon, lat = silencioCariri$lat, radius = 100, 
             label = paste("lon = ",silencioCariri$lon,", lat = ",silencioCariri$lat,sep=""), fillOpacity = 0.01) %>%
  addCircles(lng = silencioCariri$lon, lat = silencioCariri$lat, radius = 10) %>%
  addCircles(lng = ERBs$Longitude, lat = ERBs$Latitude, weight = 3, radius=40, 
             color="firebrick", stroke = TRUE, fillOpacity = 0.4) %>%
  addCircles(lng = AEREOS$lon, lat = AEREOS$lat, radius = 500, color = "violet", label = AEREOS$NOME) %>%
  addPolylines(lng = linha1$lon, linha1$lat, dashArray = "5, 5, 1, 5" ,opacity = 1, weight = 2, color = "goldenrod") %>%
  addPolylines(lng = linha2$lon, linha2$lat, dashArray = "5, 5, 1, 5" ,opacity = 1, weight = 2, color = "goldenrod") %>%
  addPolylines(lng = linha3$lon, linha3$lat, dashArray = "5, 5, 1, 5" ,opacity = 1, weight = 2, color = "goldenrod") %>%
  addPolylines(lng = linha4$lon, linha4$lat, dashArray = "5, 5, 1, 5" ,opacity = 1, weight = 2, color = "goldenrod") %>% 
  addPolylines(lng = linha5$lon, linha5$lat, dashArray = "5, 5, 1, 5" ,opacity = 1, weight = 2, color = "goldenrod") 



pal <- colorNumeric(c("#0C2C84", "#41B6C4", "#FFFFCC"), values(Paraiba),
                    na.color = "transparent")


PBrelevo <-map %>% addRasterImage(Paraiba, col=viridis(500) ,opacity = 0.8, maxBytes = 100*1024*1024, 
                                  project = TRUE) %>%
  addPolygons(data = municipios, col="white" , weight = 2, fill = FALSE) %>%
  addScaleBar()


##############################################
#GoogleMaps


pontoCariri <- function(n, Zoom){
  x <- silencioCariri$lon[n]
  y <- silencioCariri$lat[n]
  erb <- ERBs %>% as.data.frame()
  #distGeo(c(x,y),c(x+0.01,y))
  ponto <- get_map(location= c(lon=x, lat = y),
                   source = "google",
                   maptype = "hybrid",
                   zoom = Zoom
  ) 
  pontoC <- c(x,y) %>%  t() %>% as.data.frame()
  names(pontoC) <- c("lon","lat")
  bbox <- attr(ponto, "bb") %>% as.list()
  pointsC <- filter(silencioCariri, lat > bbox[1] & lat <= bbox[3])
  pointsC <- filter(pointsC, lon > bbox[2] & lon < bbox[4] )
  ggmap(ponto) + 
    geom_point(aes(x = lon, y = lat), data = pointsC, color = "yellow", size = 4, pch = "+")+
    geom_point(aes(x = lon, y = lat), data = pontoC, color = "blue", size = 4)+
    ggtitle(paste("lon= ",round(pontoC$lon,4),",lat= ", round(pontoC$lat,4)))
}

#sitiosZoomCariri(33,0.3,25)
#pontoCariri(33,11)

#sitiosZoomCariri(13,0.1,25)
#pontoCariri(13,15)
