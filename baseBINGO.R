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
             color = "blue", shape = 18, size = 2, data = AEREOS, na.rm = TRUE) +
  geom_point(aes(x = lon, y = lat), 
             color = "orchid", shape = 13, size = 2, data = sites, na.rm = TRUE) +
  geom_point(aes(x = Longitude, y = Latitude), 
             color = "khaki1", shape = 3, size = 1, data = ERBs, na.rm = TRUE)  
 


png(filename="./Imagens/sitios.png",
    units = "in",
    width = 5,
    height = 4,
    res = 300)
mapa
dev.off()

#---------------------------------------------------------------------

mapaSitios    <- function(sitio, Zoom) {
  
  titulo    <- sites$NOME[[1]]
  file      <- paste("./Imagens/sitio-",sitio,"-zoom-",Zoom,".png", sep = "")
  lugar     <- c(lon = sites$lon[[sitio]], lat = sites$lat[[sitio]] )
  mapa      <- get_map(location = lugar, zoom = Zoom, language = 'pt-BR', maptype = 'hybrid') %>%
    ggmap(., extend = "normal", size = c(5400,5400))

  mapa <- mapa + 
    geom_point(aes(x = lon, y = lat), 
               color = "orchid", shape = 13, size = 2, data = sites, na.rm = TRUE) +
    geom_point(aes(x = Longitude, y = Latitude), 
               color = "khaki1", shape = 3, size = 1, data = ERBs, na.rm = TRUE)  +
    ggtitle(titulo)
  
  png(filename=file,
      units = "in",
      width = 5,
      height = 4,
      res = 300)
  print(mapa)
  dev.off()
  
}

#ZOOM = 11
lapply(c(1:length(sites$NOME)), function(x) {
  mapaSitios(x,11)
} )

#ZOOM = 18
lapply(c(1:length(sites$NOME)), function(x) {
  mapaSitios(x,18)
} )
