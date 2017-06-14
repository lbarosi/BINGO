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
titulo <- "ERBs PB"
#Mapa Paraíba
paraiba          <- get_map('Patos, Paraíba', zoom = 7, language = 'pt-BR', maptype = 'roadmap')
mapaPB          <- ggmap(paraiba, extend = "normal", size = c(5400,5400))
mapaPB
########
#AEREOS
AEREOS <- read_excel("./AEREO.xlsx", sheet="data")
sites <- read_excel("./sites.xlsx")

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


mapa <- mapaPB +
  geom_path(data = linha1, aes(x = lon, y = lat, group = 1), color = "firebrick", size = 2, linetype = "longdash")+
  geom_path(data = linha2, aes(x = lon, y = lat, group = 1), color = "firebrick", size = 2, linetype = "longdash")+
  geom_path(data = linha3, aes(x = lon, y = lat, group = 1), color = "firebrick", size = 2, linetype = "longdash")+
  geom_path(data = linha4, aes(x = lon, y = lat, group = 1), color = "firebrick", size = 2, linetype = "longdash")+
  geom_path(data = linha5, aes(x = lon, y = lat, group = 1), color = "firebrick", size = 2, linetype = "longdash")+
  geom_path(data = linha6, aes(x = lon, y = lat, group = 1), color = "firebrick", size = 2, linetype = "longdash")+
  geom_path(data = linha7, aes(x = lon, y = lat, group = 1), color = "firebrick", size = 2, linetype = "longdash")+
  geom_path(data = linha8, aes(x = lon, y = lat, group = 1), color = "firebrick", size = 2, linetype = "longdash")+
  geom_path(data = linha9, aes(x = lon, y = lat, group = 1), color = "firebrick", size = 2, linetype = "longdash")+
  geom_path(data = linha10, aes(x = lon, y = lat, group = 1), color = "firebrick", size = 2, linetype = "longdash")+
    geom_point(aes(x = lon, y = lat), 
             color = "blue", shape = 18, size = 4, data = AEREOS, na.rm = TRUE) +
  geom_point(aes(x = lon, y = lat), 
             color = "orchid", shape = 13, size = 4, data = sites, na.rm = TRUE)  
  
mapa

#Localizacao ERBs


