#Pesquisa de Sitios de Interesse para
#BINGO
#Luciano Barosi
#14.jun.2017

#################
#BIBLIOTECAS
suppressMessages(library("ggplot2"))
suppressMessages(library("stringi"))
suppressMessages(library("stringr"))
suppressMessages(library("readxl"))
suppressMessages(library("xlsx"))
suppressMessages(library("xtable"))
suppressMessages(library("dplyr"))
suppressMessages(library("plyr"))
suppressMessages(library(magrittr))
suppressMessages(library(reshape2))
suppressMessages(library("ggthemes"))
suppressMessages(library("dgof"))
suppressMessages(library(readxl))
suppressMessages(library(Hmisc))
suppressMessages(library(reporttools))
suppressMessages(library(gdata))
suppressMessages(library(RgoogleMaps))
suppressMessages(library(ggmap))
suppressMessages(library(scales))
suppressMessages(library(MASS))
################

#Mapa Paraíba
paraiba           <- get_map('Patos, Paraíba', zoom = 7, language = 'pt-BR', maptype = 'hybrid')
mapaPB            <- ggmap(paraiba, extend = "normal", size = c(5400,5400))
########
#AEREOS H3 e V3 aisweb 2016
AEREOS            <- read_excel("./AEREO.xlsx", sheet="data")
#Sitios de Interesse Selecionados
sites             <- read_excel("./sites.xlsx")
#Posição de ERBs 2016
ERBs              <- read_excel("./ERBs.xlsx")
ERBs$Latitude     %<>% as.numeric()
ERBs$Longitude     %<>% as.numeric()
#Linhas Aereas H3 e V3-------------------------------------------------------------
linha1 <- AEREOS %>% filter(., NOME %in% c("RECIFE", "BANGU"))  %>% as.data.frame()
linha2 <- AEREOS %>% filter(., NOME %in% c("RECIFE", "OPABA")) %>% as.data.frame()
linha3 <- AEREOS %>% filter(., NOME %in% c("RECIFE", "FORTALEZA")) %>% as.data.frame()
linha4 <- AEREOS %>% filter(., NOME %in% c("BANGU", "FORTALEZA")) %>% as.data.frame()
linha5 <- AEREOS %>% filter(., NOME %in% c("BANGU", "KIGUK")) %>% as.data.frame()
linha6 <- AEREOS %>% filter(., NOME %in% c("LOMOK", "KODSO")) %>% as.data.frame()
linha7 <- AEREOS %>% filter(., NOME %in% c("RECIFE", "KODSO")) %>% as.data.frame()
linha8 <- AEREOS %>% filter(., NOME %in% c("CAMPINA", "FORTALEZA")) %>% as.data.frame()
linha9 <- AEREOS %>% filter(., NOME %in% c("NATAL", "EVPAB")) %>% as.data.frame()
linha10 <- AEREOS %>% filter(., NOME %in% c("NATAL", "ILNOT")) %>% as.data.frame()
#----------------------------------------------------------------------------------

mapa <- mapaPB +
  geom_path(data = linha1, aes(x = lon, y = lat, group = 1), color = "firebrick", size = 1, linetype = "longdash")+
  geom_path(data = linha2, aes(x = lon, y = lat, group = 1), color = "firebrick", size = 1, linetype = "longdash")+
  geom_path(data = linha3, aes(x = lon, y = lat, group = 1), color = "firebrick", size = 1, linetype = "longdash")+
  geom_path(data = linha4, aes(x = lon, y = lat, group = 1), color = "firebrick", size = 1, linetype = "longdash")+
  geom_path(data = linha5, aes(x = lon, y = lat, group = 1), color = "firebrick", size = 1, linetype = "longdash")+
  geom_path(data = linha6, aes(x = lon, y = lat, group = 1), color = "firebrick", size = 1, linetype = "longdash")+
  geom_path(data = linha7, aes(x = lon, y = lat, group = 1), color = "firebrick", size = 1, linetype = "longdash")+
  geom_path(data = linha8, aes(x = lon, y = lat, group = 1), color = "firebrick", size = 1, linetype = "longdash")+
  geom_path(data = linha9, aes(x = lon, y = lat, group = 1), color = "firebrick", size = 1, linetype = "longdash")+
  geom_path(data = linha10, aes(x = lon, y = lat, group = 1), color = "firebrick", size = 1, linetype = "longdash")+
    geom_point(aes(x = lon, y = lat), 
             color = "blue", shape = 18, size = 4, data = AEREOS, na.rm = TRUE) +
  geom_point(aes(x = lon, y = lat), 
             color = "orchid", shape = 13, size = 4, data = sites, na.rm = TRUE) +
  geom_point(aes(x = Longitude, y = Latitude), 
             color = "khaki1", shape = 3, size = 2, data = ERBs, na.rm = TRUE)  
  
mapa



png(filename="sitios.png",
    units = "in",
    width = 5,
    height = 4,
    res = 300)
mapa
dev.off()

#---------------------------------------------------------------------
#ZOOM sitio1
sitio1           <- get_map(location = c(lon = sites$lon[[1]], lat = sites$lat[[1]] ), zoom = 11, language = 'pt-BR', maptype = 'hybrid')
mapa1            <- ggmap(sitio1, extend = "normal", size = c(5400,5400))


mapa1 <- mapa1 +
  geom_point(aes(x = lon, y = lat), 
             color = "blue", shape = 13, size = 4, data = sites, na.rm = TRUE) +
  geom_point(aes(x = Longitude, y = Latitude), 
             color = "firebrick1", shape = 3, size = 2, data = ERBs, na.rm = TRUE)  

mapa1
png(filename="sitios1.png",
    units = "in",
    width = 5,
    height = 4,
    res = 300)
mapa1
dev.off()

#--------------------------------------------------------------------
#ZOOM sitio2
sitio2           <- get_map(location = c(lon = sites$lon[[2]], lat = sites$lat[[2]] ), zoom = 11, language = 'pt-BR', maptype = 'hybrid')
mapa2            <- ggmap(sitio2, extend = "normal", size = c(5400,5400))


mapa2 <- mapa2 +
  geom_point(aes(x = lon, y = lat), 
             color = "blue", shape = 13, size = 4, data = sites, na.rm = TRUE) +
  geom_point(aes(x = Longitude, y = Latitude), 
             color = "firebrick1", shape = 3, size = 2, data = ERBs, na.rm = TRUE)  

mapa2
png(filename="sitios2.png",
    units = "in",
    width = 5,
    height = 4,
    res = 300)
mapa2
dev.off()
#--------------------------------------------------------------------
#ZOOM sitio3
sitio3           <- get_map(location = c(lon = sites$lon[[3]], lat = sites$lat[[3]] ), zoom = 11, language = 'pt-BR', maptype = 'hybrid')
mapa3            <- ggmap(sitio3, extend = "normal", size = c(5400,5400))


mapa3 <- mapa3 +
  geom_point(aes(x = lon, y = lat), 
             color = "blue", shape = 13, size = 4, data = sites, na.rm = TRUE) +
  geom_point(aes(x = Longitude, y = Latitude), 
             color = "firebrick1", shape = 3, size = 2, data = ERBs, na.rm = TRUE)  

mapa3
png(filename="sitios3.png",
    units = "in",
    width = 5,
    height = 4,
    res = 300)
mapa3
dev.off()
#--------------------------------------------------------------------
#ZOOM sitio4
sitio4           <- get_map(location = c(lon = sites$lon[[4]], lat = sites$lat[[4]] ), zoom = 11, language = 'pt-BR', maptype = 'hybrid')
mapa4            <- ggmap(sitio4, extend = "normal", size = c(5400,5400))


mapa4 <- mapa4 +
  geom_point(aes(x = lon, y = lat), 
             color = "blue", shape = 13, size = 4, data = sites, na.rm = TRUE) +
  geom_point(aes(x = Longitude, y = Latitude), 
             color = "firebrick1", shape = 3, size = 2, data = ERBs, na.rm = TRUE)  

mapa4
png(filename="sitios4.png",
    units = "in",
    width = 5,
    height = 4,
    res = 300)
mapa4
dev.off()
#--------------------------------------------------------------------
#ZOOM sitio5
sitio5           <- get_map(location = c(lon = sites$lon[[5]], lat = sites$lat[[5]] ), zoom = 11, language = 'pt-BR', maptype = 'hybrid')
mapa5            <- ggmap(sitio5, extend = "normal", size = c(5400,5400))


mapa5 <- mapa5 +
  geom_point(aes(x = lon, y = lat), 
             color = "blue", shape = 13, size = 4, data = sites, na.rm = TRUE) +
  geom_point(aes(x = Longitude, y = Latitude), 
             color = "firebrick1", shape = 3, size = 2, data = ERBs, na.rm = TRUE)  

mapa5
png(filename="sitios5.png",
    units = "in",
    width = 5,
    height = 4,
    res = 300)
mapa5
dev.off()
#--------------------------------------------------------------------
#ZOOM sitio6
sitio6           <- get_map(location = c(lon = sites$lon[[6]], lat = sites$lat[[6]] ), zoom = 11, language = 'pt-BR', maptype = 'hybrid')
mapa6            <- ggmap(sitio6, extend = "normal", size = c(5400,5400))


mapa6 <- mapa6 +
  geom_point(aes(x = lon, y = lat), 
             color = "blue", shape = 13, size = 4, data = sites, na.rm = TRUE) +
  geom_point(aes(x = Longitude, y = Latitude), 
             color = "firebrick1", shape = 3, size = 2, data = ERBs, na.rm = TRUE)  

mapa6
png(filename="sitios6.png",
    units = "in",
    width = 5,
    height = 4,
    res = 300)
mapa6
dev.off()
#--------------------------------------------------------------------
#ZOOM sitio7
sitio7           <- get_map(location = c(lon = sites$lon[[7]], lat = sites$lat[[7]] ), zoom = 11, language = 'pt-BR', maptype = 'hybrid')
mapa7            <- ggmap(sitio7, extend = "normal", size = c(5400,5400))


mapa7 <- mapa7 +
  geom_point(aes(x = lon, y = lat), 
             color = "blue", shape = 13, size = 4, data = sites, na.rm = TRUE) +
  geom_point(aes(x = Longitude, y = Latitude), 
             color = "firebrick1", shape = 3, size = 2, data = ERBs, na.rm = TRUE)  

mapa7
png(filename="sitios7.png",
    units = "in",
    width = 5,
    height = 4,
    res = 300)
mapa7
dev.off()
#--------------------------------------------------------------------
#ZOOM sitio8
sitio8           <- get_map(location = c(lon = sites$lon[[8]], lat = sites$lat[[8]] ), zoom = 11, language = 'pt-BR', maptype = 'hybrid')
mapa8            <- ggmap(sitio8, extend = "normal", size = c(5400,5400))


mapa8 <- mapa8 +
  geom_point(aes(x = lon, y = lat), 
             color = "blue", shape = 13, size = 4, data = sites, na.rm = TRUE) +
  geom_point(aes(x = Longitude, y = Latitude), 
             color = "firebrick1", shape = 3, size = 2, data = ERBs, na.rm = TRUE)  

mapa8
png(filename="sitios8.png",
    units = "in",
    width = 5,
    height = 4,
    res = 300)
mapa8
dev.off()
#--------------------------------------------------------------------
#ZOOM sitio9
sitio9           <- get_map(location = c(lon = sites$lon[[9]], lat = sites$lat[[9]] ), zoom = 11, language = 'pt-BR', maptype = 'hybrid')
mapa9            <- ggmap(sitio9, extend = "normal", size = c(5400,5400))


mapa9 <- mapa9 +
  geom_point(aes(x = lon, y = lat), 
             color = "blue", shape = 13, size = 4, data = sites, na.rm = TRUE) +
  geom_point(aes(x = Longitude, y = Latitude), 
             color = "firebrick1", shape = 3, size = 2, data = ERBs, na.rm = TRUE)  

mapa9
png(filename="sitios9.png",
    units = "in",
    width = 5,
    height = 4,
    res = 300)
mapa9
dev.off()
#--------------------------------------------------------------------
#ZOOM sitio10
sitio10           <- get_map(location = c(lon = sites$lon[[10]], lat = sites$lat[[10]] ), zoom = 11, language = 'pt-BR', maptype = 'hybrid')
mapa10            <- ggmap(sitio10, extend = "normal", size = c(5400,5400))


mapa10 <- mapa10 +
  geom_point(aes(x = lon, y = lat), 
             color = "blue", shape = 13, size = 4, data = sites, na.rm = TRUE) +
  geom_point(aes(x = Longitude, y = Latitude), 
             color = "firebrick1", shape = 3, size = 2, data = ERBs, na.rm = TRUE)  

mapa10
png(filename="sitios10.png",
    units = "in",
    width = 5,
    height = 4,
    res = 300)
mapa10
dev.off()
#--------------------------------------------------------------------
#ZOOM sitio11
sitio11           <- get_map(location = c(lon = sites$lon[[11]], lat = sites$lat[[11]] ), zoom = 11, language = 'pt-BR', maptype = 'hybrid')
mapa11            <- ggmap(sitio11, extend = "normal", size = c(5400,5400))


mapa11 <- mapa11 +
  geom_point(aes(x = lon, y = lat), 
             color = "blue", shape = 13, size = 4, data = sites, na.rm = TRUE) +
  geom_point(aes(x = Longitude, y = Latitude), 
             color = "firebrick1", shape = 3, size = 2, data = ERBs, na.rm = TRUE)  

mapa11
png(filename="sitios11.png",
    units = "in",
    width = 5,
    height = 4,
    res = 300)
mapa11
dev.off()
#--------------------------------------------------------------------