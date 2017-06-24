#Pesquisa de Sitios de Interesse para
#BINGO
#Luciano Barosi
#Visitados 20 - 24 Junho

#################
#BIBLIOTECAS

suppressMessages(library("readxl"))
suppressMessages(library("dplyr"))
suppressMessages(library("plyr"))
suppressMessages(library(magrittr))
suppressMessages(library(reshape2))
library(leaflet)
################


#AEREOS H3 e V3 aisweb 2016
AEREOS            <- read_excel("./AEREOvisited.xlsx", sheet="data")
#Sitios de Interesse Selecionados
sites             <- read_excel("./sitesvisited.xlsx")
#Posição de ERBs 2016
ERBs              <- read_excel("./ERBs.xlsx")
ERBs$Latitude     %<>% as.numeric()
ERBs$Longitude     %<>% as.numeric()
#Linhas Aereas H3 e V3-------------------------------------------------------------
linha1 <- AEREOS %>% filter(., NOME %in% c("BANGU", "OPABA"))  %>% as.data.frame()
linha2 <- AEREOS %>% filter(., NOME %in% c("BANGU", "EDITE")) %>% as.data.frame()
linha3 <- AEREOS %>% filter(., NOME %in% c("LOMOK", "OPSUS")) %>% as.data.frame()
linha4 <- AEREOS %>% filter(., NOME %in% c("BANGU", "ILNOT")) %>% as.data.frame()
linha5 <- AEREOS %>% filter(., NOME %in% c("VACAR", "ISUKU")) %>% as.data.frame()

#----------------------------------------------------------------------------------



map <- leaflet() %>% addTiles() %>% 
  setView(lng = -38.2, lat = -7, zoom = 10) %>%
  addCircles(lng = sites$lon, lat = sites$lat, radius = 5000, label = sites$NOME) %>%
  addCircles(lng = sites$lon, lat = sites$lat, radius = 10, label = sites$NOME) %>%
  addCircles(lng = ERBs$Longitude, lat = ERBs$Latitude, weight = 3, radius=40, 
             color="firebrick", stroke = TRUE, fillOpacity = 0.8) %>%
  addCircles(lng = AEREOS$lon, lat = AEREOS$lat, radius = 500, color = "violet", label = AEREOS$NOME) %>%
  addPolylines(lng = linha1$lon, linha1$lat, dashArray = "5, 5, 1, 5" ,opacity = 1, weight = 2, color = "goldenrod") %>%
  addPolylines(lng = linha2$lon, linha2$lat, dashArray = "5, 5, 1, 5" ,opacity = 1, weight = 2, color = "goldenrod") %>%
  addPolylines(lng = linha3$lon, linha3$lat, dashArray = "5, 5, 1, 5" ,opacity = 1, weight = 2, color = "goldenrod") %>%
  addPolylines(lng = linha4$lon, linha4$lat, dashArray = "5, 5, 1, 5" ,opacity = 1, weight = 2, color = "goldenrod") %>% 
  addPolylines(lng = linha5$lon, linha5$lat, dashArray = "5, 5, 1, 5" ,opacity = 1, weight = 2, color = "goldenrod") 
  


m1 <- leaflet() %>% addTiles() %>% 
  setView(lng = sites$lon[[1]], lat = sites$lat[[1]], zoom = 13) %>%
  addCircles(lng = sites$lon, lat = sites$lat, radius = 10, label = sites$NOME) %>%
  addCircles(lng = sites$lon, lat = sites$lat, radius = 5000, label = sites$NOME) %>%
  addCircles(lng = ERBs$Longitude, lat = ERBs$Latitude, weight = 3, radius=40, 
             color="firebrick", stroke = TRUE, fillOpacity = 0.8) %>%
  addCircles(lng = AEREOS$lon, lat = AEREOS$lat, radius = 500, color = "violet", label = AEREOS$NOME) %>%
  addPolylines(lng = linha1$lon, linha1$lat, dashArray = "5, 5, 1, 5" ,opacity = 1, weight = 2, color = "goldenrod") %>%
  addPolylines(lng = linha2$lon, linha2$lat, dashArray = "5, 5, 1, 5" ,opacity = 1, weight = 2, color = "goldenrod") %>%
  addPolylines(lng = linha3$lon, linha3$lat, dashArray = "5, 5, 1, 5" ,opacity = 1, weight = 2, color = "goldenrod") %>%
  addPolylines(lng = linha4$lon, linha4$lat, dashArray = "5, 5, 1, 5" ,opacity = 1, weight = 2, color = "goldenrod") %>% 
  addPolylines(lng = linha5$lon, linha5$lat, dashArray = "5, 5, 1, 5" ,opacity = 1, weight = 2, color = "goldenrod") 


m2 <- leaflet() %>% addTiles() %>% 
  setView(lng = sites$lon[[2]], lat = sites$lat[[2]], zoom = 13) %>%
  addCircles(lng = sites$lon, lat = sites$lat, radius = 10, label = sites$NOME) %>%
  addCircles(lng = sites$lon, lat = sites$lat, radius = 5000, label = sites$NOME) %>%
  addCircles(lng = ERBs$Longitude, lat = ERBs$Latitude, weight = 3, radius=40, 
             color="firebrick", stroke = TRUE, fillOpacity = 0.8) %>%
  addCircles(lng = AEREOS$lon, lat = AEREOS$lat, radius = 500, color = "violet", label = AEREOS$NOME) %>%
  addPolylines(lng = linha1$lon, linha1$lat, dashArray = "5, 5, 1, 5" ,opacity = 1, weight = 2, color = "goldenrod") %>%
  addPolylines(lng = linha2$lon, linha2$lat, dashArray = "5, 5, 1, 5" ,opacity = 1, weight = 2, color = "goldenrod") %>%
  addPolylines(lng = linha3$lon, linha3$lat, dashArray = "5, 5, 1, 5" ,opacity = 1, weight = 2, color = "goldenrod") %>%
  addPolylines(lng = linha4$lon, linha4$lat, dashArray = "5, 5, 1, 5" ,opacity = 1, weight = 2, color = "goldenrod") %>% 
  addPolylines(lng = linha5$lon, linha5$lat, dashArray = "5, 5, 1, 5" ,opacity = 1, weight = 2, color = "goldenrod") 


m3 <- leaflet() %>% addTiles() %>% 
  setView(lng = sites$lon[[3]], lat = sites$lat[[3]], zoom = 13) %>%
  addCircles(lng = sites$lon, lat = sites$lat, radius = 10, label = sites$NOME) %>%
  addCircles(lng = sites$lon, lat = sites$lat, radius = 5000, label = sites$NOME) %>%
  addCircles(lng = ERBs$Longitude, lat = ERBs$Latitude, weight = 3, radius=40, 
             color="firebrick", stroke = TRUE, fillOpacity = 0.8) %>%
  addCircles(lng = AEREOS$lon, lat = AEREOS$lat, radius = 500, color = "violet", label = AEREOS$NOME) %>%
  addPolylines(lng = linha1$lon, linha1$lat, dashArray = "5, 5, 1, 5" ,opacity = 1, weight = 2, color = "goldenrod") %>%
  addPolylines(lng = linha2$lon, linha2$lat, dashArray = "5, 5, 1, 5" ,opacity = 1, weight = 2, color = "goldenrod") %>%
  addPolylines(lng = linha3$lon, linha3$lat, dashArray = "5, 5, 1, 5" ,opacity = 1, weight = 2, color = "goldenrod") %>%
  addPolylines(lng = linha4$lon, linha4$lat, dashArray = "5, 5, 1, 5" ,opacity = 1, weight = 2, color = "goldenrod") %>% 
  addPolylines(lng = linha5$lon, linha5$lat, dashArray = "5, 5, 1, 5" ,opacity = 1, weight = 2, color = "goldenrod") 


m4 <- leaflet() %>% addTiles() %>% 
  setView(lng = sites$lon[[4]], lat = sites$lat[[4]], zoom = 13) %>%
  addCircles(lng = sites$lon, lat = sites$lat, radius = 10, label = sites$NOME) %>%
  addCircles(lng = sites$lon, lat = sites$lat, radius = 5000, label = sites$NOME) %>%
  addCircles(lng = ERBs$Longitude, lat = ERBs$Latitude, weight = 3, radius=40, 
             color="firebrick", stroke = TRUE, fillOpacity = 0.8) %>%
  addCircles(lng = AEREOS$lon, lat = AEREOS$lat, radius = 500, color = "violet", label = AEREOS$NOME) %>%
  addPolylines(lng = linha1$lon, linha1$lat, dashArray = "5, 5, 1, 5" ,opacity = 1, weight = 2, color = "goldenrod") %>%
  addPolylines(lng = linha2$lon, linha2$lat, dashArray = "5, 5, 1, 5" ,opacity = 1, weight = 2, color = "goldenrod") %>%
  addPolylines(lng = linha3$lon, linha3$lat, dashArray = "5, 5, 1, 5" ,opacity = 1, weight = 2, color = "goldenrod") %>%
  addPolylines(lng = linha4$lon, linha4$lat, dashArray = "5, 5, 1, 5" ,opacity = 1, weight = 2, color = "goldenrod") %>% 
  addPolylines(lng = linha5$lon, linha5$lat, dashArray = "5, 5, 1, 5" ,opacity = 1, weight = 2, color = "goldenrod") 



m5 <- leaflet() %>% addTiles() %>% 
  setView(lng = sites$lon[[5]], lat = sites$lat[[5]], zoom = 13) %>%
  addCircles(lng = sites$lon, lat = sites$lat, radius = 10, label = sites$NOME) %>%
  addCircles(lng = sites$lon, lat = sites$lat, radius = 5000, label = sites$NOME) %>%
  addCircles(lng = ERBs$Longitude, lat = ERBs$Latitude, weight = 3, radius=40, 
             color="firebrick", stroke = TRUE, fillOpacity = 0.8) %>%
  addCircles(lng = AEREOS$lon, lat = AEREOS$lat, radius = 500, color = "violet", label = AEREOS$NOME) %>%
  addPolylines(lng = linha1$lon, linha1$lat, dashArray = "5, 5, 1, 5" ,opacity = 1, weight = 2, color = "goldenrod") %>%
  addPolylines(lng = linha2$lon, linha2$lat, dashArray = "5, 5, 1, 5" ,opacity = 1, weight = 2, color = "goldenrod") %>%
  addPolylines(lng = linha3$lon, linha3$lat, dashArray = "5, 5, 1, 5" ,opacity = 1, weight = 2, color = "goldenrod") %>%
  addPolylines(lng = linha4$lon, linha4$lat, dashArray = "5, 5, 1, 5" ,opacity = 1, weight = 2, color = "goldenrod") %>% 
  addPolylines(lng = linha5$lon, linha5$lat, dashArray = "5, 5, 1, 5" ,opacity = 1, weight = 2, color = "goldenrod") 



m6 <- leaflet() %>% addTiles() %>% 
  setView(lng = sites$lon[[6]], lat = sites$lat[[6]], zoom = 13) %>%
  addCircles(lng = sites$lon, lat = sites$lat, radius = 10, label = sites$NOME) %>%
  addCircles(lng = sites$lon, lat = sites$lat, radius = 5000, label = sites$NOME) %>%
  addCircles(lng = ERBs$Longitude, lat = ERBs$Latitude, weight = 3, radius=40, 
             color="firebrick", stroke = TRUE, fillOpacity = 0.8) %>%
  addCircles(lng = AEREOS$lon, lat = AEREOS$lat, radius = 500, color = "violet", label = AEREOS$NOME) %>%
  addPolylines(lng = linha1$lon, linha1$lat, dashArray = "5, 5, 1, 5" ,opacity = 1, weight = 2, color = "goldenrod") %>%
  addPolylines(lng = linha2$lon, linha2$lat, dashArray = "5, 5, 1, 5" ,opacity = 1, weight = 2, color = "goldenrod") %>%
  addPolylines(lng = linha3$lon, linha3$lat, dashArray = "5, 5, 1, 5" ,opacity = 1, weight = 2, color = "goldenrod") %>%
  addPolylines(lng = linha4$lon, linha4$lat, dashArray = "5, 5, 1, 5" ,opacity = 1, weight = 2, color = "goldenrod") %>% 
  addPolylines(lng = linha5$lon, linha5$lat, dashArray = "5, 5, 1, 5" ,opacity = 1, weight = 2, color = "goldenrod") 


