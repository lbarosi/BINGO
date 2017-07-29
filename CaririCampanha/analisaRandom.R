#Analisa dados aleatórios obtidos em randomSites.T
#Carrega dados auxiliares

library(tictoc)
library(ggplot2)
source("./findSites.R")

pal <- colorNumeric(c("#0C2C84", "#41B6C4", "#FFFFCC"), values(Paraiba),
                    na.color = "transparent")


P4S.latlon <- CRS("+proj=longlat +datum=WGS84")
hrr.shp <- readShapePoly("./GEODATA/municipiosPB/i3geomap_municipios_pb", verbose=TRUE, proj4string=P4S.latlon) 

#Carrega todos os arquivos em ./RandomSites
files       <- list.files("./RandomSites") 
sitios      <- lapply(files, function(i) {
                        name <- paste("./RandomSites/",i,sep="");
                        read.csv(name)
                        })
sitios      <- do.call(rbind,sitios) %>% as.data.frame()

silencio    <- sitios %>% filter(., Ruído < -66)

#-----------------------------------------------
#Insere coluna com dados de propriedades
coordinates(silencio) <- c("lon","lat")
proj4string(silencio) <- proj4string(municipios)
silencio$Particular1  <- over(silencio,imoveisParticularesPB)$NOME_IMO9
silencio$Particular2  <- over(silencio,imoveisParticularesPB2)$NOME_ARE9
silencio$Publico1     <- over(silencio,imoveisPublicosPB)$NOME_IMO9
silencio$Publico2     <- over(silencio,imoveisPublicosPB2)$NOME_ARE9
silencio$UCs          <- over(silencio,UCs)$nome

#usar over
#Insere distancia de linhas aéreas
#Cuidado, tem usar PMIN em mutate para fazer rowwise

silencio$AEREO1         <- dist2Line(silencio,l1) %>% as.data.frame() %>% dplyr::select(., distance)
silencio$AEREO2         <- dist2Line(silencio,l2) %>% as.data.frame() %>% dplyr::select(., distance)
silencio$AEREO3         <- dist2Line(silencio,l3) %>% as.data.frame() %>% dplyr::select(., distance)
silencio$AEREO4         <- dist2Line(silencio,l4) %>% as.data.frame() %>% dplyr::select(., distance)
silencio$AEREO5         <- dist2Line(silencio,l5) %>% as.data.frame() %>% dplyr::select(., distance)

silencio %<>% as.data.frame()
names(silencio) <- c("Mun","lon", "lat", "Ruido", "Antenas", "Dmin", "Opt", 
                     "Part1", "Part2", "Publ1", "Publ2", "UCs", "AEREO1",
                     "AEREO2", "AEREO3", "AEREO4","AEREO5")

silencio <- silencio %>% as.data.frame() %>%
  mutate(., AEREO = pmin(AEREO1, AEREO2, AEREO3, AEREO4, AEREO5)) %>%
  dplyr::select(., -AEREO1,-AEREO2, -AEREO3, -AEREO4,-AEREO5)

silencio %<>% filter(., AEREO > 10000)

#silencioCariri <- filter(silencio, Mun %in% c("SUMÉ","CAMALAÚ","SERRA BRANCA"))
#Visualização Geral de sites 
mapageral <- leaflet() %>% addTiles(group = "OSM (default)") %>% 
  setView(lng = -37.55, lat = -6.97, zoom = 9) %>%
  addCircles(lng = silencio$lon, lat = silencio$lat, radius = 5000, 
             label = silencio$mun, color = "green", group = "sites") %>%
  addCircles(lng = sitios$lon, lat = sitios$lat, radius = 40, 
             label = silencio$mun, color = "purple", group = "sites") %>%
  addCircles(lng = ERBs$Longitude, lat = ERBs$Latitude, weight = 3, radius=40, 
             color="firebrick", stroke = TRUE, fillOpacity = 0.4, group = "ERBs") %>%
  addPolygons(data = municipios, col="white" , weight = 2, fill = FALSE, label = municipios$NOME_MUN1, 
              group = "County limits") %>%
  addScaleBar()


silencioDF <- silencio
coordinates(ERBs) <- c("Longitude","Latitude")

coordinates(silencioDF) <- c("lon","lat")
#----------------------------------------------
#Análise
sitiosZoom <- function(s, z, h){
  
  sitio <- silencioDF
  passo <- z
  n <- s  
  
  Xlim <- c(sitio$lon[[n]]-passo,sitio$lon[[n]]+passo)
  Ylim <- c(sitio$lat[[n]]-passo,sitio$lat[[n]]+passo)
  
  Extend <- c(unlist(Xlim),unlist(Ylim))
  
  mapa <- crop(Paraiba, extent(Xlim[[1]],Xlim[[2]], Ylim[[1]], Ylim[[2]]))
 
  # Gaussian filter
  #gf <- focalWeight(mapa, 2, "Gauss")
  #mapa <- focal(mapa, w=gf)
  
  zmin <- (minValue(mapa) %/% 100) * 100
  zmax <- (maxValue(mapa) %/% 100) * 100
  
  my.at <- seq(zmin, zmax, h)
  
  zoomMap<- levelplot(Paraiba, 
                      layers = 1, 
                      contour = TRUE,
                      xlim = Xlim,
                      ylim = Ylim,
                      at = my.at,
                      drop.unused.levels = TRUE,
                      margin = FALSE,
                      par.settings = viridisTheme()
  )
  
  result <- zoomMap + layer(sp.points(ERBs, col = "red", pch="+", cex=1)) + 
    layer(sp.lines(hrr.shp, col = "white", lwd = 0.8))+
    layer(sp.points(silencioDF, col = "darkorange1", cex=1, pch = 9))
  
  print(result)
  
}






#---------------------------------------------


