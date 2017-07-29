library(magrittr)
library(reshape2)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(cowplot)
library(gridExtra)

#Spectrum Analiser Data
F_i <- 0
F_f <- 1300

#Função lê sinais de arquivos e prepara dataframes
sinalCariri <- function(pastaC){

pasta <- pastaC
Skip <- 20

files <- list.files(pasta, pattern = "*.csv")

dados <- lapply(files, function(f){
  fname <- paste(pasta,f,sep="")
  tmp <- read.csv(fname,skip = Skip, header = FALSE)  %>% as.data.frame()
  tmp <- tmp[,2]
  
})


dados <- do.call(cbind,dados)

freq <- files[1] %>% paste(pasta,.,sep = "") %>% read.csv(., skip = Skip, header = FALSE,
                                                          stringsAsFactors=FALSE) %>%
  dplyr::select(.,1) %>% filter(., V1 != "END")

freq$V1 %<>% as.numeric()
freq$V1 <- freq$V1/1000000

dados %<>% as.data.frame()
dados %<>% filter(., !is.na(V1))
dataSignal <- dados


#######################
#Calcula Mediana
data_median <- sapply(c(1:length(dataSignal[[1]])), function(x) {
  median(unlist(dataSignal[x,]))
}) %>% as.data.frame()
#Calcula 10% e 90% quantis
data_Q <- sapply(c(1:length(dataSignal[[1]])), function(x) {
  quantile(unlist(dataSignal[x,]), probs = c(0.10,0.90), na.rm = TRUE)
}) %>% t() %>% as.data.frame() 

#udo em um dataframe
names(freq) <- c("Frequency")
names(data_median) <- c("Median")
names(data_Q) <- c("10%", "90%")


dados <- cbind(freq, data_median, data_Q) %>% mutate(., Frequency, Delta = 10^((`90%`-Median)/20),
                                                     Pm = 10^(Median/20))



return(dados)
}
#Função lê sinais de arquivos e prepara dataframes Juntando por SITE
sinalCaririJoin <- function(pastaC){
  
  pasta <- pastaC
  Skip <- 20
  
  files <- list.files(pasta, pattern = "*.csv", recursive = TRUE)
  
  dados <- lapply(files, function(f){
    fname <- paste(pasta,f,sep="")
    tmp <- read.csv(fname,skip = Skip, header = FALSE)  %>% as.data.frame()
    tmp <- tmp[,2]
    
  })
  
  
  dados <- do.call(cbind,dados)
  
  freq <- files[1] %>% paste(pasta,.,sep = "") %>% read.csv(., skip = Skip, header = FALSE,
                                                            stringsAsFactors=FALSE) %>%
    dplyr::select(.,1) %>% filter(., V1 != "END")
  
  freq$V1 %<>% as.numeric()
  freq$V1 <- freq$V1/1000000
  
  dados %<>% as.data.frame()
  dados %<>% filter(., !is.na(V1))
  dataSignal <- dados
  
  
  #######################
  #Calcula Mediana
  data_median <- sapply(c(1:length(dataSignal[[1]])), function(x) {
    median(unlist(dataSignal[x,]))
  }) %>% as.data.frame()
  #Calcula 10% e 90% quantis
  data_Q <- sapply(c(1:length(dataSignal[[1]])), function(x) {
    quantile(unlist(dataSignal[x,]), probs = c(0.10,0.90), na.rm = TRUE)
  }) %>% t() %>% as.data.frame() 
  
  #udo em um dataframe
  names(freq) <- c("Frequency")
  names(data_median) <- c("Median")
  names(data_Q) <- c("10%", "90%")
  
  
  dados <- cbind(freq, data_median, data_Q) %>% mutate(., Frequency, Delta = 10^((`90%`-Median)/20),
                                                       Pm = 10^(Median/20))
  
  
  
  return(dados)
}
#----------------------------------
#Gráficos--------------------------
#Função plota Diferença de 90% e mediana na escala de energia
plotaSiteDelta <- function (df,title){
  
  p1 <- ggplot() +
    geom_line(data = df, aes(x = Frequency, y = Delta, color = "blue")) + 
    theme_bw() +
    scale_x_continuous(limits = c(900, 1300)) +
    ggtitle(title) +
    xlab("Frequency (Mhz)") + 
    ylab("Power (90% - Median))")+
    labs(color = "")+
    theme_bw() +
    theme(legend.position = "bottom") +
    guides(fill=guide_legend(nrow=2,byrow=TRUE)) +
    scale_fill_brewer(palette = "Paired") 
  
  return(p1)
}
#Função plota Mediana na escala de energia
plotaSitePm <- function (df,title){
  
  p1 <- ggplot() +
    geom_line(data = df, aes(x = Frequency, y = Pm, color = "blue")) + 
    geom_smooth(method = "loess", span = 1/20, data = df, aes(x = Frequency, y = Pm, color = "smooth")) + 
    theme_bw() +
    scale_x_continuous(limits = c(F_i, F_f)) +
    scale_y_continuous(limits = c(0, 0.0003)) +
    ggtitle(title) +
    xlab("Frequency (Mhz)") + 
    ylab("Power (median))")+
    labs(color = "")+
    theme_bw() +
    theme(legend.position = "bottom") +
    guides(fill=guide_legend(nrow=2,byrow=TRUE)) +
    scale_fill_brewer(palette = "Paired") 
  
  return(p1)
}
#FUnção plota 4 dataframes 90% na escala de energia
plotaSites90 <- function (df1, df2, df3, df4, F_i){
  
  p1 <- ggplot() +
    geom_line(data = df1, aes(x = Frequency, y = 10^(`90%`/20), color = "site1")) + 
    geom_smooth(method = "loess", span = 1/20, data = df1, aes(x = Frequency, y = 10^(`90%`/20), color = "site1")) + 
    geom_line(data = df2, aes(x = Frequency, y = 10^(`90%`/20), color = "site2")) + 
    geom_smooth(method = "loess", span = 1/20, data = df2, aes(x = Frequency, y = 10^(`90%`/20), color = "site2")) + 
    geom_line(data = df3, aes(x = Frequency, y = 10^(`90%`/20), color = "site3")) + 
    geom_smooth(method = "loess", span = 1/20, data = df3, aes(x = Frequency, y = 10^(`90%`/20), color = "site3")) + 
    geom_line(data = df4, aes(x = Frequency, y = 10^(`90%`/20), color = "site4")) + 
    geom_smooth(method = "loess", span = 1/20, data = df4, aes(x = Frequency, y = 10^(`90%`/20), color = "site4")) + 
    theme_bw() +
    scale_x_continuous(limits = c(F_i, F_f)) +
    scale_y_continuous(limits = c(0, 0.0003)) +
    ggtitle("Dados da antena") +
    xlab("Frequency (Mhz)") + 
    ylab("Power)")+
    labs(color = "")+
    theme_bw() +
    theme(legend.position = "bottom") +
    guides(fill=guide_legend(nrow=2,byrow=TRUE)) +
    scale_fill_brewer(palette = "Paired") 
  
  return(p1)
}
#
plotaSitesDelta <- function (df1, df2, df3, df4, F_i){
  
  p1 <- ggplot() +
    geom_line(data = df1, aes(x = Frequency, y = Delta, color = "site1")) + 
    geom_smooth(method = "loess", span = 1/20, data = df1, aes(x = Frequency, y = Delta, color = "site1")) + 
    geom_line(data = df2, aes(x = Frequency, y = Delta, color = "site2")) + 
    geom_smooth(method = "loess", span = 1/20, data = df2, aes(x = Frequency, y = Delta, color = "site2")) + 
    geom_line(data = df3, aes(x = Frequency, y = Delta, color = "site3")) + 
    geom_smooth(method = "loess", span = 1/20, data = df3, aes(x = Frequency, y = Delta, color = "site3")) + 
    geom_line(data = df4, aes(x = Frequency, y = Delta, color = "site4")) + 
    geom_smooth(method = "loess", span = 1/20, data = df4, aes(x = Frequency, y = Delta, color = "site4")) + 
    theme_bw() +
    scale_x_continuous(limits = c(F_i, F_f)) +
    scale_y_continuous(limits = c(0.5, 2.5)) +
    ggtitle("Dados da antena") +
    xlab("Frequency (Mhz)") + 
    ylab("Power)")+
    labs(color = "")+
    theme_bw() +
    theme(legend.position = "bottom") +
    guides(fill=guide_legend(nrow=2,byrow=TRUE)) +
    scale_fill_brewer(palette = "Paired") 
  
  return(p1)
}
#----------------------------------
#----------------------------------


site01 <- sinalCaririJoin("./Experimentos/site-01/")
site02 <- sinalCaririJoin("./Experimentos/site-02/")
site03 <- sinalCaririJoin("./Experimentos/site-03/")
site04 <- sinalCaririJoin("./Experimentos/site-04/")

#plotaSitesDelta(site01,site02,site03,site04,900)
#plotaSites90(site01,site02,site03,site04,900)

#Dados de todas as medições
site01_SUL <- sinalCariri("./Experimentos/site-01/direcional/Sul/")
site02_SUL <- sinalCariri("./Experimentos/site-02/direcional/Sul/")
site03_SUL <- sinalCariri("./Experimentos/site-03/direcional/Sul/")
site04_SUL <- sinalCariri("./Experimentos/site-04/direcional/Sul/")

site01_OESTE <- sinalCariri("./Experimentos/site-01/direcional/Oeste/")
site02_OESTE <- sinalCariri("./Experimentos/site-02/direcional/Oeste/")
site03_OESTE <- sinalCariri("./Experimentos/site-03/direcional/Oeste/")
site04_OESTE <- sinalCariri("./Experimentos/site-04/direcional/Oeste/")

site01_LESTE <- sinalCariri("./Experimentos/site-01/direcional/Leste/")
site02_LESTE <- sinalCariri("./Experimentos/site-02/direcional/Leste/")
site03_LESTE <- sinalCariri("./Experimentos/site-03/direcional/Leste/")
site04_LESTE <- sinalCariri("./Experimentos/site-04/direcional/Leste/")

site01_NORTE <- sinalCariri("./Experimentos/site-01/direcional/Norte/")
site02_NORTE <- sinalCariri("./Experimentos/site-02/direcional/Norte/")
site03_NORTE <- sinalCariri("./Experimentos/site-03/direcional/Norte/")
site04_NORTE <- sinalCariri("./Experimentos/site-04/direcional/Norte/")

site01 <- sinalCariri("./Experimentos/site-01/omni/")
site02 <- sinalCariri("./Experimentos/site-02/omni/")
site03 <- sinalCariri("./Experimentos/site-03/omni/")
#site04 <- sinalCariri("./Experimentos/site-04/omni/")



