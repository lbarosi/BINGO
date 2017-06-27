
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
suppressMessages(library(reshape2))
library(leaflet)
#Include data from leaflet


#Loadind Data


#AEREOS H3 e V3 aisweb 2016
AEREOS            <- read_excel("./DATA/AEREOvisited.xlsx")
#Sitios de Interesse Selecionados
sites             <- read_excel("./DATA/sitesvisited.xlsx")
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


ERBs <- read_excel("./DATA/ERBs.xlsx")
ERBs$Latitude %<>% as.numeric(.)
ERBs$Longitude %<>% as.numeric(.)

coordinates(ERBs) <- c("Longitude","Latitude")

#----

MAP1 <- raster("./GEODATA/sb-24-z-a/SB-24-Z-A.tif")
MAP2 <- raster("./GEODATA/sb-24-z-c/SB-24-Z-C.tif")
MAP3 <- raster("./GEODATA/sb-24-z-d/SB-24-Z-D.tif")
MAP4 <- raster("./GEODATA/sb-24-z-b/SB-24-Z-B.tif")

Paraiba <- merge(MAP1, MAP2, MAP3, MAP4)

#----------------------------------------------------
P4S.latlon <- CRS("+proj=longlat +datum=WGS84")
hrr.shp <- readShapePoly("./GEODATA/municipiosPB/i3geomap_municipios_pb", verbose=TRUE, proj4string=P4S.latlon) 

#UCSproj <- readShapePoly("./municipiosPB/i3geomap_municipios_pb", verbose=TRUE, CRS("+proj=utm +zone=23 +datum=SAD69")) 

municipios <- readOGR("./GEODATA/municipiosPB","i3geomap_municipios_pb", verbose = FALSE)
municipios <- spTransform(municipios, CRS("+proj=longlat +datum=WGS84"))

glebasFederais <- readOGR("./GEODATA/glebasfederais","i3geomap_glebas_federais", verbose = FALSE)
glebasFederais <- spTransform(glebasFederais, CRS("+proj=longlat +datum=WGS84"))

imoveisPublicosPB <- readOGR("./GEODATA/publicoPB","i3geomap_imoveiscertificados_publico_pb", verbose = FALSE)
imoveisPublicosPB <- spTransform(imoveisPublicosPB, CRS("+proj=longlat +datum=WGS84"))

imoveisPublicosPB2 <- readOGR("./GEODATA/sigefPublicoPB","i3geomap_certificada_sigef_publico_pb", verbose = FALSE)
imoveisPublicosPB2 <- spTransform(imoveisPublicosPB2, CRS("+proj=longlat +datum=WGS84"))


imoveisParticularesPB <- readOGR("./GEODATA/privadosPB","i3geomap_imoveiscertificados_privado_pb", verbose = FALSE)
imoveisParticularesPB <- spTransform(imoveisParticularesPB, CRS("+proj=longlat +datum=WGS84"))


imoveisParticularesPB2 <- readOGR("./GEODATA/particularPB","i3geomap_certificada_sigef_particular_pb", verbose = FALSE)
imoveisParticularesPB2 <- spTransform(imoveisParticularesPB2, CRS("+proj=longlat +datum=WGS84"))

UCs <- readOGR("./GEODATA/UCs","UCs_fed_junho_2017", verbose = FALSE)
UCs <- spTransform(UCs, CRS("+proj=longlat +datum=WGS84"))

RPPNs <- readOGR("./GEODATA/RPPN","PB", verbose = FALSE)
RPPNs <- spTransform(RPPNs, CRS("+proj=longlat +datum=WGS84"))


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


