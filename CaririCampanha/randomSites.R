#Localiza pontos aleatórios

#source("./dataLoading.R")
library(rgeos)
library(ggmap)
library(tictoc)
source("./findSites.R")
#------------------------------------------------------

randomSitios <- function(n,xmin,xmax,ymin,ymax,seed,name){
  set.seed(seed)
  lat <- runif(n,ymin,ymax)
  lon <- runif(n,xmin,xmax)
  RandomSites <- cbind(lon,lat) %>% as.data.frame()
  
  coordinates(RandomSites) <- c("lon","lat")
  proj4string(RandomSites) <- proj4string(municipios)
  RandomSites$mun <- over(RandomSites,municipios)$NOME_MUN1
  
  #RandomSites %<>% select(., lon, lat)
  
  RandomSites <- as.data.frame(RandomSites) %>% filter(.,!is.na(mun))
  
  noiseRandom <- verificaSitios(RandomSites)  
  
  coordinates(noiseRandom) <- c("lon","lat")
  proj4string(noiseRandom) <- proj4string(municipios)
  noiseRandom$mun <- over(noiseRandom,municipios)$NOME_MUN1 %>% as.data.frame()
  
  #silencio    <- filter(noiseRandom, Ruído < -70) 
  write.csv(noiseRandom,name, row.names = FALSE, append = true)
    
  
  return(as.data.frame(noiseRandom))
}

#A little benchmark
#tic()
#Signal(c(sites[1,]$lon,sites[1,]$lat))
#exectime <- toc()
#Signaltime <- exectime$toc - exectime$toc
# 3.414 sec elapsed


#############
seed  <- 12
#n     <- 200
n     <- 100
#delta <- 0.25 #Raio de 120km
delta <- 0.10


#############
seed  <- 24
#n     <- 200
n     <- 100
#delta <- 0.25 #Raio de 120km
delta <- 0.01



cajazeiras      <- geocode("Cajazeiras Paraiba Brasil")
CampinaGrande   <- geocode("Campina Grande Paraiba Brasil")
Sossego         <- geocode("Sossego Paraiba Brasil")
Sume            <- geocode("Sume Paraiba Brasil")
Passagem        <- geocode("Passagem Paraiba Brasil")
Pianco          <- geocode("Pianco Paraiba Brasil")
Balanca         <- geocode("Balanca Paraiba Brasil")
SJTapada        <- geocode("São José da Lagoa Tapada Paraiba Brasil")
SJPeixe         <- geocode("São João do Rio do Peixe Paraiba Brasil")
SJPiranhas      <- geocode("São Jose de Piranhas Paraiba Brasil")
RPPNAlmas       <- geocode("RPPN Almas, São José dos Cordeiros, Paraíba,")
Congo           <- geocode("Congo, Paraíba,")
Jureminha       <- geocode("Jureminha, Paraíba,")
#Sites de interesse
tic()
Rcajazeiras      <- randomSitios(n,cajazeiras[[1]]-delta,cajazeiras[[1]]+delta,
                                   cajazeiras[[2]]-delta,cajazeiras[[2]]+delta,seed,
                                 "./RandomSites/Rcajazeiras.csv")
#-------------------------------------
RCampinaGrande   <- randomSitios(n,CampinaGrande[[1]]-delta,CampinaGrande[[1]]+delta,
                                 CampinaGrande[[2]]-delta,CampinaGrande[[2]]+delta,seed,
                                 "./RandomSites/CampinaGrande.csv")
RSossego         <- randomSitios(n,Sossego[[1]]-delta,Sossego[[1]]+delta,
                                 Sossego[[2]]-delta,Sossego[[2]]+delta,seed,
                                 "./RandomSites/Sossego.csv")
RSume            <- randomSitios(n,Sume[[1]]-delta,Sume[[1]]+delta,
                                 Sume[[2]]-delta,Sume[[2]]+delta,seed,
                                 "./RandomSites/Sume.csv")
RPassagem        <- randomSitios(n,Passagem[[1]]-delta,Passagem[[1]]+delta,
                               Passagem[[2]]-delta,Passagem[[2]]+delta,seed,
                                 "./RandomSites/Passagem.csv")
RPianco          <- randomSitios(n,Pianco[[1]]-delta,Pianco[[1]]+delta,
                                 Pianco[[2]]-delta,Pianco[[2]]+delta,seed,
                               "./RandomSites/Pianco.csv")
RBalanca         <- randomSitios(n,Balanca[[1]]-delta,Balanca[[1]]+delta,
                                 Balanca[[2]]-delta,Balanca[[2]]+delta,seed,
                                 "./RandomSites/Balanca.csv")
RSJTapada        <- randomSitios(n,SJTapada[[1]]-delta,SJTapada[[1]]+delta,
                                 SJTapada[[2]]-delta,SJTapada[[2]]+delta,seed,
                                 "./RandomSites/SJTapada2.csv")
RSPeixe          <- randomSitios(n,SJPeixe[[1]]-delta,SJPeixe[[1]]+delta,
                               SJPeixe[[2]]-delta,SJPeixe[[2]]+delta,seed,
                                 "./RandomSites/SJPeixe.csv")
RSPiranhas       <- randomSitios(n,SJPiranhas[[1]]-delta,SJPiranhas[[1]]+delta,
                                 SJPiranhas[[2]]-delta,SJPiranhas[[2]]+delta,seed,
                               "./RandomSites/SJPiranhas.csv")
RSRPPNAlmas      <- randomSitios(n,RPPNAlmas[[1]]-delta,RPPNAlmas[[1]]+delta,
                                 RPPNAlmas[[2]]-delta,RPPNAlmas[[2]]+delta,seed,
                                 "./RandomSites/RPPNAlmas.csv")
RSCongo         <- randomSitios(n,Congo[[1]]-delta,Congo[[1]]+delta,
                                 Congo[[2]]-delta,Congo[[2]]+delta,seed,
                                 "./RandomSites/Congo.csv")
RSJureminha      <- randomSitios(n,Jureminha[[1]]-delta,Jureminha[[1]]+delta,
                                 Jureminha[[2]]-delta,Jureminha[[2]]+delta,seed,
                                 "./RandomSites/Jureminha.csv")
exectime <- toc()
Signaltime <- exectime$toc - exectime$toc

RSJTapada        <- randomSitios(n,-38.25941-delta,-38.25941+delta,
                                 -7.04674-delta,-7.04674+delta,seed,
                                 "./RandomSites/Catarina.csv")


#########################


dadosSitios <- function(pontosdf, nome){
  
  pontos <- pontosdf
  coordinates(pontos) <- c("lon","lat")
  proj4string(pontos) <- proj4string(municipios)
  pontos$mun <- over(pontos,municipios)$NOME_MUN1
  
  #RandomSites %<>% select(., lon, lat)
  
  pontos <- as.data.frame(pontos) %>% filter(.,!is.na(mun))
  
  noiseRandom <- verificaSitios(pontos)  
  
  coordinates(noiseRandom) <- c("lon","lat")
  proj4string(noiseRandom) <- proj4string(municipios)
  noiseRandom$mun <- over(noiseRandom,municipios)$NOME_MUN1
  noiseRandom %<>% as.data.frame()
  noiseRandom$optional <- TRUE
  #silencio    <- filter(noiseRandom, Ruído < -70) 
  write.csv(noiseRandom,nome, row.names = FALSE)
  return(noiseRandom)
}

visitados <- read.csv("./campanha2.csv")
dadosSitios(visitados, "./RandomSites/Visitados-campanha2.csv")

#################################
#Campanha 3
sitios <- read.csv("./sitesvisited.csv")

#############
seed  <- 1
#n     <- 200
n     <- 100
#delta <- 0.25 #Raio de 120km
delta <- 0.03

Gato         <- randomSitios(n,sitios$lon[9]-delta,sitios$lon[9]+delta,
                             sitios$lat[9]-delta,sitios$lat[9]+delta,seed,
                                 "./RandomSites/Gato.csv")


Urubu         <- randomSitios(n,sitios$lon[10]-delta,sitios$lon[10]+delta,
                             sitios$lat[10]-delta,sitios$lat[10]+delta,seed,
                             "./RandomSites/Urubu.csv")


Almas        <- randomSitios(n,sitios$lon[7]-delta,sitios$lon[7]+delta,
                             sitios$lat[7]-delta,sitios$lat[7]+delta,seed,
                             "./RandomSites/Almas.csv")


Palestina         <- randomSitios(n,sitios$lon[11]-delta,sitios$lon[11]+delta,
                             sitios$lat[11]-delta,sitios$lat[11]+delta,seed,
                             "./RandomSites/Palestina.csv")

dadosSitios(Gato, "./RandomSites/Gato.csv")
dadosSitios(Urubu, "./RandomSites/Urubu.csv")
dadosSitios(Almas, "./RandomSites/Almas.csv")
dadosSitios(Palestina, "./RandomSites/Palestina.csv")
