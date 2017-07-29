library(magrittr)
library(reshape2)
library(dplyr)
library(ggplot2)
#Spectrum Analiser Data
F_i <- 900
F_f <- 1200

RBW <- 1
#N'úmero de Canais
n   <- 461


#N'úmero de repeticoes
n_int <- 100

#######################
#Fake Data
#Get data for repetitions, each repetition in a diffferent column
vecdata <- c(1:n_int)

Freq <- as.numeric(RBW*c(1:n)+ F_i - RBW/2) %>% as.data.frame()


data_test <- sapply(vecdata, function(x) {
  set.seed(x);
  runif(n,-90,-60) 
  } 
  ) %>% t() %>%
  as.data.frame()
#######################
#Calcula Mediana
data_median <- sapply(c(1:n), function(x) {
  median(data_test[[x]])
}) %>% as.data.frame()
#Calcula 10% e 90% quantis
data_Q <- sapply(c(1:n), function(x) {
  quantile(data_test[[x]], probs = c(0.10,0.90))
}) %>% t() %>% as.data.frame() 

#udo em um dataframe
names(Freq) <- c("Frequency")
names(data_median) <- c("Median")
names(data_Q) <- c("10%", "90%")

dados <- cbind(Freq, data_median, data_Q) %>% melt(., id.vars = c("Frequency"))
####################
#Gr'áfico
ggplot(data = dados, aes(x = Frequency, y = value, color = variable)) +
  geom_line() + theme_bw() +
  ggtitle("Dados da antena") +
  xlab("Frequency (Mhz)") + 
  ylab("Signal (dB)")

###################