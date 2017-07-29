#Analisa dados aleatórios obtidos em randomSites.T
#Carrega dados auxiliares

library(tictoc)
library(ggplot2)
library(ggmap)
library(ggsn)
library(gridExtra)
library(grid)
source("./findSites.R")

visitados <- read.csv("./sitesvisited.csv")
pal <- colorNumeric(c("#0C2C84", "#41B6C4", "#FFFFCC"), values(Paraiba),
                    na.color = "transparent")
P4S.latlon <- CRS("+proj=longlat +datum=WGS84")
hrr.shp <- readShapePoly("./GEODATA/municipiosPB/i3geomap_municipios_pb", verbose=FALSE, proj4string=P4S.latlon) 
#Carrega todos os arquivos em ./RandomSites
files       <- list.files("./RandomSites/", pattern = "*.csv") 
sitios      <- lapply(files, function(i) {
                        name <- paste("./RandomSites/",i,sep="");
                        read.csv(name)
                        })
sitios      <- do.call(rbind,sitios) %>% as.data.frame()

silencio    <- sitios %>% filter(., Ruído < -68)
#-----------------------------------------------
#Insere coluna com dados de propriedades
coordinates(silencio) <- c("lon","lat")
proj4string(silencio) <- proj4string(municipios)
silencio$Particular1  <- over(silencio,imoveisParticularesPB)$NOME_IMO9
silencio$Particular2  <- over(silencio,imoveisParticularesPB2)$NOME_ARE9
silencio$Publico1     <- over(silencio,imoveisPublicosPB)$NOME_IMO9
silencio$Publico2     <- over(silencio,imoveisPublicosPB2)$NOME_ARE9
silencio$UCs          <- over(silencio,UCs)$nome
silencio$AEREO1         <- dist2Line(silencio,l1) %>% as.data.frame() %>% select(., distance)
silencio$AEREO2         <- dist2Line(silencio,l2) %>% as.data.frame() %>% select(., distance)
silencio$AEREO3         <- dist2Line(silencio,l3) %>% as.data.frame() %>% select(., distance)
silencio$AEREO4         <- dist2Line(silencio,l4) %>% as.data.frame() %>% select(., distance)
silencio$AEREO5         <- dist2Line(silencio,l5) %>% as.data.frame() %>% select(., distance)
silencio %<>% as.data.frame()
names(silencio) <- c("Mun","lon", "lat", "Ruido", "Antenas", "Dmin", "Opt", 
                     "Part1", "Part2", "Publ1", "Publ2", "UCs", "AEREO1",
                     "AEREO2", "AEREO3", "AEREO4","AEREO5")
silencio <- silencio %>% as.data.frame() %>%
  mutate(., AEREO = pmin(AEREO1, AEREO2, AEREO3, AEREO4, AEREO5)) %>%
  select(., -AEREO1,-AEREO2, -AEREO3, -AEREO4,-AEREO5)
silencio %<>% filter(., AEREO > 10000)
silencioDF <- silencio
coordinates(ERBs) <- c("Longitude","Latitude")
coordinates(silencioDF) <- c("lon","lat")
#----------------------------------------------
sitiosZoomXY <- function(LON, LAT, z, h, title){
  
  sitio <- silencioDF
  passo <- z
  
  Xlim <- c(LON-passo,LON+passo)
  Ylim <- c(LAT-passo,LAT+passo)
  
  Extend <- c(unlist(Xlim),unlist(Ylim))
  
  mapa <- crop(Paraiba, extent(Xlim[[1]],Xlim[[2]], Ylim[[1]], Ylim[[2]]))
 
  zmin <- (minValue(mapa) %/% 100) * 100
  zmax <- (maxValue(mapa) %/% 100) * 100
  
  ponto <- c(LON, LAT) %>% t() %>% as.data.frame()
  names(ponto) <- c("lon","lat")
  
  my.at <- seq(zmin, zmax, h)
  
  result<- levelplot(Paraiba, 
                      layers = 1, 
                      contour = TRUE,
                      xlim = Xlim,
                      ylim = Ylim,
                      at = my.at,
                      drop.unused.levels = TRUE,
                      margin = FALSE,
                      main = title,
                      par.settings = viridisTheme()
  )+
    layer(sp.points(ERBs, col = "red", pch="+", cex=1)) + 
    layer(sp.lines(hrr.shp, col = "white", lwd = 0.8))+
    layer(sp.points(silencioDF, col = "darkorange1", cex=1, pch = 9)) +
    layer({
      ponto <- c(LON, LAT) %>% t() %>% as.data.frame()
      names(ponto) <- c("lon","lat")
      coordinates(ponto) <- c("lon","lat")
      proj4string(ponto) <- P4S.latlon
      sp.points(ponto, col = "red", cex=1, pch = 9)},
      data = list(LON = LON, LAT = LAT, P4S.latlon = P4S.latlon))
  
  
  print(result)
  
}
relevoSitio <- function(n,z,h){

  Lon <- visitados$lon[n] %>% as.numeric()
  Lat <- visitados$lat[n] %>% as.numeric()
  titulo <- visitados$NOME[n] %>% as.character()
  sitiosZoomXY(Lon, Lat,z,h,titulo)
  
}
##############################################
#GoogleMaps
pontoGmap <- function(n, Zoom, mtype){
  x <- visitados$lon[n]
  y <- visitados$lat[n]
  title <- visitados$NOME[n] %>% as.character()
  erb <- ERBs %>% as.data.frame()
  names(erb) <- c("op","lat","lon","dist","azi")
  #distGeo(c(x,y),c(x+0.01,y))
  ponto <- get_map(location= c(lon=x, lat = y),
                   source = "google",
                   maptype = mtype,
                   zoom = Zoom
  ) 
  pontoC <- c(x,y) %>%  t() %>% as.data.frame()
  names(pontoC) <- c("lon","lat")
  bbox <- attr(ponto, "bb") %>% as.list()
  pointsC <- dplyr::filter(silencio, lat > bbox[1] & lat <= bbox[3])
  pointsC <- dplyr::filter(silencio, lon > bbox[2] & lon < bbox[4] )
  erbs    <- dplyr::filter(erb, lat > bbox[1] & lat <= bbox[3])
  erbs    <- dplyr::filter(erb, lon > bbox[2] & lon < bbox[4] )
  ggmap(ponto) + 
    geom_point(aes(x = lon, y = lat), data = pointsC, color = "yellow", size = 4, pch = 13)+
    geom_point(aes(x = lon, y = lat), data = pontoC, color = "darkorange", size = 4, pch = 8)+
    geom_point(aes(x = lon, y = lat), data = erbs, color = "firebrick", size = 4, pch = "+")+
    geom_path(data = municipios, aes(x = long, y = lat, group = group), col = "white", lwd = 0.3, lty = "dotdash")+
    ggtitle(paste("lon= ",round(pontoC$lon,4),",lat= ", round(pontoC$lat,4)))
    
}
mosaico <- function(nponto){
  
  p1 <- relevoSitio(nponto,0.05,25)
  p2 <- pontoGmap(nponto,17,"hybrid")
  p3 <- pontoGmap(nponto,15,"terrain")
  p4 <- pontoGmap(nponto,11,"roadmap")
  
  mosaico <- grid.arrange(p1,p2,p3,p4, ncol=2)
  print(mosaico)
}

##############################################
#Salva Mapas
png(filename="./MAPAS-Campanha3/VaoDoGato.png",
    units = "px",
    width = 5000,
    height = 5000,
    pointsize = 12,
    res = 300)
mosaico(9)
dev.off()



png(filename="./MAPAS-Campanha3/Almas.png",
    units = "px",
    width = 5000,
    height = 5000,
    pointsize = 12,
    res = 300)
mosaico(7)
dev.off()


png(filename="./MAPAS-Campanha3/Urubu.png",
    units = "px",
    width = 5000,
    height = 5000,
    pointsize = 12,
    res = 300)
mosaico(10)
dev.off()


png(filename="./MAPAS-Campanha3/Palestina.png",
    units = "px",
    width = 5000,
    height = 5000,
    pointsize = 12,
    res = 300)
mosaico(11)
dev.off()
