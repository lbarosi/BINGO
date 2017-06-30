# Função para calcular atenuação no site
# Luciano Barosi
# 28/jun/2017
#Descomente a linha abaixo para rodas separadamente
source("./dataLoading.R")
#Funções auxiliares
#--------------------------------------------------
pathLoss    <- function(ponto,erb) {
  # PathLoss em linha reta no ar - Friss Model  
  dist  <- distGeo(ponto,erb)
  #Assume Potencia da Antena de 50dBm
  #Se muito perto da antena, 0
  path  <- ifelse(dist>=1000,-20*log10(4*pi*dist/0.21)+50, 0)
  
  return(path) 
}
retaGeod    <- function(ponto,erb){
  #Função determina geodésica com 10 pontos e retorna as altitudes dos pontos  
  pontos      <- gcIntermediate(ponto, erb, n=48, addStartEnd=TRUE) %>% as.data.frame()
  alt         <- sapply(c(1:50), function(x) {
    pontos[x,] %>% SpatialPoints() %>% extract(Paraiba,.)
  }) %>% as.data.frame() # Extrai dados de Altura
  
  names(alt)  <- c("z")
  #Valores de células vazias
  alt[is.na(alt)] <- mean(alt$z, na.rm = TRUE)
  ##########################
  pontos      <- cbind(pontos, alt) %>% as.data.frame()
  pontos      %<>% filter(., !is.na(z)) #Retorna apenas antenas com informação
  
  return(pontos)
  
}
lineOfsight <- function(ponto,erb){
  data <- retaGeod(ponto,erb)
  nL <- length(data$z)
  hmin <- data[1,3]
  k    <- (data[nL,3]-data[1,3])/nL
  data$n <- c(1:nL)
  data1 <- mutate(data, hadj = z-k*(n-1)/nL-50-hmin)
  boole <- max(data1[[5]][2:nL]) < 0
  return(boole)
}
erbsOnsight <- function(ponto){
  erbs    <- ERBs
  erbs    %<>% select(., Longitude, Latitude)
  erbsD   <- mutate(erbs, Dist = distGeo(ponto, erbs), Boole = lineOfsight(ponto,erbs))
  erbsD   <- erbsD %>% arrange(Dist) %>% filter(., Dist < 30000) %>% filter(.,Boole == TRUE)
  
  ifelse(length(erbsD$Boole)>0, return(erbsD), return(FALSE))
  #return(erbsD)
}
#------------------------------------------------
Signal <- function(ponto){

  vazioDF           <- c(-100,0,100) %>% t() %>% as.data.frame()
  names(vazioDF)    <- c("Ruído", "N. Estações", "Distância min")
  
  #Function Body 
  erbs              <- erbsOnsight(ponto)
  
  flag <- ifelse(erbs == FALSE,return(vazioDF),1)
  
    erbsPOS         <- select(erbs, Longitude, Latitude)
    erbsPOS         <- mutate(erbsPOS, Ruido = pathLoss(ponto,erbsPOS), 
                              Dist = distGeo(ponto,erbsPOS))
    nois            <- c(max(erbsPOS$Ruido),length(erbsPOS$Dist),min(erbsPOS$Dist)) %>%
      t() %>% as.data.frame()
    names(nois)    <- c("Ruído", "N. Estações", "Distância min")
    return(nois)
  
}
#-------------------------------------------------
 verificaSitios <- function(df){
 
   sitios    <- select(df, lon, lat)
 
   noise     <- sapply(c(1:length(sitios$lon)), function(x) {
                                     Signal(sitios[x,])
                                     }) %>% 
                                 t() %>% as.data.frame()
   noise$Ruído           %<>% as.numeric()
   noise$`N. Estações`   %<>% as.numeric()
   noise$`Distância min` %<>% as.numeric()
   
   noise <- cbind(df, noise)
   
   return(noise)
   
 }
# #------------------------------------------------
# #Ruido dos sites listados em sites.xlsx
# noise <- verificaSitios(sites)
#------------------------------------------------
