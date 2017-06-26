library(raster)
library(sp)
library(rgdal)
library(rasterVis)
library(viridis)
library(dplyr)
library(maptools)
library(latticeExtra)
library(lattice)
library(readxl)
library(magrittr)
#Loadind Data


#AEREOS H3 e V3 aisweb 2016
AEREOS            <- read_excel("../VisitedSites/AEREOvisited.xlsx")
#Sitios de Interesse Selecionados
sites             <- read_excel("./sitesvisited.xlsx")
nomes             <- sites$NOME
coordinates(sites) <- c("lon","lat")
#Linhas Aereas H3 e V3-------------------------------------------------------------
linha1 <- AEREOS %>% filter(., NOME %in% c("BANGU", "OPABA"))  %>% as.data.frame()
linha2 <- AEREOS %>% filter(., NOME %in% c("BANGU", "EDITE")) %>% as.data.frame()
linha3 <- AEREOS %>% filter(., NOME %in% c("LOMOK", "OPSUS")) %>% as.data.frame()
linha4 <- AEREOS %>% filter(., NOME %in% c("BANGU", "ILNOT")) %>% as.data.frame()
linha5 <- AEREOS %>% filter(., NOME %in% c("VACAR", "ISUKU")) %>% as.data.frame()

l1 <- linha1 %>% select(., lon, lat) %>% as.matrix %>% spLines(., crs='+proj=longlat +datum=WGS84')
l2 <- linha2 %>% select(., lon, lat) %>% as.matrix %>% spLines(., crs='+proj=longlat +datum=WGS84')
l3 <- linha3 %>% select(., lon, lat) %>% as.matrix %>% spLines(., crs='+proj=longlat +datum=WGS84')
l4 <- linha4 %>% select(., lon, lat) %>% as.matrix %>% spLines(., crs='+proj=longlat +datum=WGS84')
l5 <- linha5 %>% select(., lon, lat) %>% as.matrix %>% spLines(., crs='+proj=longlat +datum=WGS84')


ERBs <- read_excel("../ERBs.xlsx")
ERBs$Latitude %<>% as.numeric(.)
ERBs$Longitude %<>% as.numeric(.)

coordinates(ERBs) <- c("Longitude","Latitude")

#----

MAP1 <- raster("./sb-24-z-a/SB-24-Z-A.tif")
MAP2 <- raster("./sb-24-z-c/SB-24-Z-C.tif")
MAP3 <- raster("./sb-24-z-d/SB-24-Z-D.tif")
MAP4 <- raster("./sb-24-z-b/SB-24-Z-B.tif")

Paraiba <- merge(MAP1, MAP2, MAP3, MAP4)

#----------------------------------------------------
P4S.latlon <- CRS("+proj=longlat +datum=WGS84")
hrr.shp <- readShapePoly("./PB_Mun97_region/PB_Mun97_region.shp", verbose=TRUE, proj4string=P4S.latlon) 


ParaibaMap <- levelplot(Paraiba, layers = 1, margin = list(FUN = 'median'), contour = TRUE, par.settings = viridisTheme())


panorama <- ParaibaMap + layer(sp.points(ERBs, col = "red", pch="+", cex=1)) + 
  layer(sp.lines(hrr.shp, col = "white", lwd = 0.8))+
  layer(sp.points(sites, col = "darkorange1", pch=19))+
  layer(sp.lines(l1, col = "coral1", lwd = 1))+
  layer(sp.lines(l2, col = "coral1", lwd = 1))+
  layer(sp.lines(l3, col = "coral1", lwd = 1))+
  layer(sp.lines(l4, col = "coral1", lwd = 1))+
  layer(sp.lines(l5, col = "coral1", lwd = 1))

#-------------
#zooming

sitiosZoom <- function(s, z, h){
  
  sitio <- as.data.frame(sites@coords)
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
                      par.settings = viridisTheme(),
                      main = nomes[[n]]
  )
  
  result <- zoomMap + layer(sp.points(ERBs, col = "red", pch="+", cex=1)) + 
    layer(sp.lines(hrr.shp, col = "white", lwd = 0.8))+
    layer(sp.points(sites, col = "darkorange1", cex=1, pch = 9))+
    layer(sp.lines(l1, col = "coral1", lwd = 1))+
    layer(sp.lines(l2, col = "coral1", lwd = 1))+
    layer(sp.lines(l3, col = "coral1", lwd = 1))+
    layer(sp.lines(l4, col = "coral1", lwd = 1))+
    layer(sp.lines(l5, col = "coral1", lwd = 1))
  
  return(result)
  }


#---------------------


