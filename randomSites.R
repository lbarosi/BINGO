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
n     <- 200
delta <- 0.25 #Raio de 120km

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
                                 "./RandomSites/SJTapada.csv")
RSPeixe          <- randomSitios(n,SJPeixe[[1]]-delta,SJPeixe[[1]]+delta,
                               SJPeixe[[2]]-delta,SJPeixe[[2]]+delta,seed,
                                 "./RandomSites/SJPeixe.csv")
RSPiranhas       <- randomSitios(n,SJPiranhas[[1]]-delta,SJPiranhas[[1]]+delta,
                                 SJPiranhas[[2]]-delta,SJPiranhas[[2]]+delta,seed,
                               "./RandomSites/SJPiranhas.csv")
exectime <- toc()
Signaltime <- exectime$toc - exectime$toc
