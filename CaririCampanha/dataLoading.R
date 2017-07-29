#Libraries
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
library(reshape2)
library(leaflet)
library(geosphere)
#------------------------------------

#AEREOS H3 e V3 aisweb 2016
AEREOS            <- read_excel("./DATA/AEREOvisited.xlsx")
#Sitios de Interesse Selecionados
sites             <- read_excel("./DATA/sitesvisited.xlsx")
nomes             <- sites$NOME

#Sites Aleatorios calculados
#silencio          <- read.csv("./silencio.csv")
#coordinates(sites) <- c("lon","lat")
#Linhas Aereas H3 e V3-------------------------------------------------------------
linha1 <- AEREOS %>% filter(., NOME %in% c("BANGU", "OPABA"))  %>% as.data.frame()
linha2 <- AEREOS %>% filter(., NOME %in% c("BANGU", "EDITE")) %>% as.data.frame()
linha3 <- AEREOS %>% filter(., NOME %in% c("LOMOK", "OPSUS")) %>% as.data.frame()
linha4 <- AEREOS %>% filter(., NOME %in% c("BANGU", "ILNOT")) %>% as.data.frame()
linha5 <- AEREOS %>% filter(., NOME %in% c("VACAR", "ISUKU")) %>% as.data.frame()

l1 <- linha1 %>% dplyr::select(., lon, lat) %>% as.matrix %>% spLines(., crs='+proj=longlat +datum=WGS84')
l2 <- linha2 %>% dplyr::select(., lon, lat) %>% as.matrix %>% spLines(., crs='+proj=longlat +datum=WGS84')
l3 <- linha3 %>% dplyr::select(., lon, lat) %>% as.matrix %>% spLines(., crs='+proj=longlat +datum=WGS84')
l4 <- linha4 %>% dplyr::select(., lon, lat) %>% as.matrix %>% spLines(., crs='+proj=longlat +datum=WGS84')
l5 <- linha5 %>% dplyr::select(., lon, lat) %>% as.matrix %>% spLines(., crs='+proj=longlat +datum=WGS84')

#Limite da camada de dados
xmin <- -39.00042
xmax <- -36.00042
ymin <- -8.000416
ymax <- -6.000416

#Antenas
ERBs <- read_excel("./DATA/ERBs.xlsx")
ERBs$Latitude %<>% as.numeric(.)
ERBs$Longitude %<>% as.numeric(.)
#ERBs %<>% filter(., Latitude > ymin & Latitude < ymax & Longitude > xmin & Longitude < xmax  )
ERBs %<>% unique()
#coordinates(ERBs) <- c("Longitude","Latitude")

#----
#Mapas de elevação - geoTIFF
MAP1 <- raster("./GEODATA/sb-24-z-a/SB-24-Z-A.tif")
MAP2 <- raster("./GEODATA/sb-24-z-c/SB-24-Z-C.tif")
MAP3 <- raster("./GEODATA/sb-24-z-d/SB-24-Z-D.tif")
MAP4 <- raster("./GEODATA/sb-24-z-b/SB-24-Z-B.tif")
MAP5 <- raster("./GEODATA/sb-25-y-c/SB-25-Y-C.tif")
MAP6 <- raster("./GEODATA/sb-25-y-a/SB-25-Y-A.tif")
MAP7 <- raster("./GEODATA/sc-24-x-b/SC-24-X-B.tif")
Paraiba <- merge(MAP1, MAP2, MAP3, MAP4, MAP5, MAP6, MAP7)

#-----------------------------------


#----------------------------------------------------
#Malha de municípios da Paraíba
#P4S.latlon <- CRS("+proj=longlat +datum=WGS84")
#hrr.shp <- readShapePoly("./GEODATA/municipiosPB/i3geomap_municipios_pb", verbose=TRUE, proj4string=P4S.latlon) 
#Malha de Municípios da Paraíba
municipios <- readOGR("./GEODATA/municipiosPB","i3geomap_municipios_pb", verbose = FALSE)
municipios <- spTransform(municipios, CRS("+proj=longlat +datum=WGS84"))
#Malha de Glebas Federais
glebasFederais <- readOGR("./GEODATA/glebasfederais","i3geomap_glebas_federais", verbose = FALSE)
glebasFederais <- spTransform(glebasFederais, CRS("+proj=longlat +datum=WGS84"))
#Malha de Imóveis públicos da Paraíba
imoveisPublicosPB <- readOGR("./GEODATA/publicoPB","i3geomap_imoveiscertificados_publico_pb", verbose = FALSE)
imoveisPublicosPB <- spTransform(imoveisPublicosPB, CRS("+proj=longlat +datum=WGS84"))
#Malha de Imóveis públicos da Paraíba SIGEF
imoveisPublicosPB2 <- readOGR("./GEODATA/sigefPublicoPB","i3geomap_certificada_sigef_publico_pb", verbose = FALSE)
imoveisPublicosPB2 <- spTransform(imoveisPublicosPB2, CRS("+proj=longlat +datum=WGS84"))
#Malha de Imóveis Particulares PB
imoveisParticularesPB <- readOGR("./GEODATA/privadosPB","i3geomap_imoveiscertificados_privado_pb", verbose = FALSE)
imoveisParticularesPB <- spTransform(imoveisParticularesPB, CRS("+proj=longlat +datum=WGS84"))
#Malha de Imóveis Particulares PB SIGEF
imoveisParticularesPB2 <- readOGR("./GEODATA/particularPB","i3geomap_certificada_sigef_particular_pb", verbose = FALSE)
imoveisParticularesPB2 <- spTransform(imoveisParticularesPB2, CRS("+proj=longlat +datum=WGS84"))
#Malha de Unidades de Conservação
UCs <- readOGR("./GEODATA/UCs","UCs_fed_junho_2017", verbose = FALSE)
UCs <- spTransform(UCs, CRS("+proj=longlat +datum=WGS84"))
#Malha de RPPNs
RPPNs <- readOGR("./GEODATA/RPPN","PB", verbose = FALSE)
RPPNs <- spTransform(RPPNs, CRS("+proj=longlat +datum=WGS84"))
#----------------------------------------------------------------------------------
