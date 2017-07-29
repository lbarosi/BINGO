library(ggmap)
library(ggsn)
source("./analisaRandom.R")



silencioCariri <- filter(silencio, Mun %in% c("SUMÉ","CAMALAÚ","SERRA BRANCA", "SÃO JOSÉ DOS CORDEIROS"))

mapaCariri <- leaflet() %>% addTiles(group = "OSM (default)") %>% 
  setView(lng = -36.73, lat = -7.62, zoom = 13) %>%
  addCircles(lng = silencioCariri$lon, lat = silencioCariri$lat, radius = 500, 
             label = silencioCariri$mun, color = "green", group = "sites") %>%
  addCircles(lng = sitios$lon, lat = sitios$lat, radius = 10, 
             label = silencioCariri$mun, color = "purple", group = "sites") %>%
  addCircles(lng = ERBs$Longitude, lat = ERBs$Latitude, weight = 3, radius=40, 
             color="firebrick", stroke = TRUE, fillOpacity = 0.4, group = "ERBs") %>%
  addPolygons(data = municipios, col="white" , weight = 2, fill = FALSE, label = municipios$NOME_MUN1, 
              group = "County limits") %>%
  addScaleBar()



silencioCariri %<>% arrange(., Ruido)

sitiosZoomCariri <- function(s, z, h){
  
  sitio <- silencioCariri
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

sitiosZoomCariri(31,0.3,50)



#----------------------------------------------
MAP1 <- raster("./GEODATA/sb-24-z-a/SB-24-Z-A.tif")
MAP2 <- raster("./GEODATA/sb-24-z-c/SB-24-Z-C.tif")
MAP3 <- raster("./GEODATA/sb-24-z-d/SB-24-Z-D.tif")
MAP4 <- raster("./GEODATA/sb-24-z-b/SB-24-Z-B.tif")

Paraiba <- merge(MAP1, MAP2, MAP3, MAP4)

#----------------------------------------------------
P4S.latlon <- CRS("+proj=longlat +datum=WGS84")
hrr.shp <- readShapePoly("./GEODATA/municipiosPB/i3geomap_municipios_pb.shp", verbose=TRUE, proj4string=P4S.latlon) 

municipios <- readOGR("./GEODATA/municipiosPB/","i3geomap_municipios_pb")

municipios <- spTransform(municipios, CRS("+proj=longlat +datum=WGS84"))


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

cariri <- get_map(location= c(lon=-36.78552, lat = -7.578233),
        source = "google",
        maptype = "hybrid",
        zoom = 11
        )
caririMap <- ggmap(cariri)+geom_point(aes(x = lon, y = lat), data = silencioCariri, color = "purple", size = 5)


pontoCariri <- function(n, Zoom){
  x <- silencioCariri$lon[n]
  y <- silencioCariri$lat[n]
  #distGeo(c(x,y),c(x+0.01,y))
  ponto <- get_map(location= c(lon=x, lat = y),
                   source = "google",
                   maptype = "hybrid",
                   zoom = Zoom
  ) 
  pontoC <- c(x,y) %>%  t() %>% as.data.frame()
  names(pontoC) <- c("lon","lat")
  ggmap(ponto) + 
    geom_point(aes(x = lon, y = lat), data = pontoC, color = "purple", size = 5)
}

pontoCariri(40,12)
