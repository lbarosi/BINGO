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

######Lendo Dados
#Cursos INEP 2015
cursos        <- read.csv2("../INEP/2015/DADOS/DM_CURSO.CSV", sep = "|", fileEncoding = "LATIN1")
chave         <- read_excel("mapeamentoMODELO.xlsx")
IES           <- read.csv2("../INEP/2015/DADOS/DM_IES.CSV", sep = "|", fileEncoding = "LATIN1")
names(chave)  <- c("CO_OCDE_AREA_DETALHADA", "AREA", "GRUPO", "PG")

#Apenas IFES
cursos        <- filter(cursos, CO_CATEGORIA_ADMINISTRATIVA == 1 &
                         CO_ORGANIZACAO_ACADEMICA == 1 & DS_MODALIDADE_ENSINO == "Presencial" &
                   DS_SITUACAO_CURSO == "Em atividade")

cursos        <- merge(cursos, chave, by = "CO_OCDE_AREA_DETALHADA", all.x = TRUE)

IES           <- filter(IES, CO_CATEGORIA_ADMINISTRATIVA == 1 &
                          CO_ORGANIZACAO_ACADEMICA == 1 )

#Selecionando Variaveis
cursosAREAOCDE  <- dplyr::select(cursos, CO_OCDE_AREA_GERAL, CO_OCDE_AREA_DETALHADA, GRUPO, PG, 
                          NO_REGIAO_CURSO, CO_IES, NO_IES,NO_MUNICIPIO_CURSO, SGL_UF_CURSO, 
                         CO_CURSO, NO_CURSO, DS_GRAU_ACADEMICO, QT_MATRICULA_CURSO,
                         QT_CONCLUINTE_CURSO, QT_INGRESSO_CURSO)

cursosAREAOCDE$CO_OCDE_AREA_GERAL     %<>% as.factor()
cursosAREAOCDE$CO_OCDE_AREA_DETALHADA %<>% as.factor()
cursosAREAOCDE$GRUPO                  %<>% as.factor()
cursosAREAOCDE$NO_REGIAO_CURSO        %<>% as.factor()
cursosAREAOCDE$CO_IES                 %<>% as.factor()
cursosAREAOCDE$NO_IES                 %<>% as.factor()
cursosAREAOCDE$NO_MUNICIPIO_CURSO     %<>% as.factor()
cursosAREAOCDE$SGL_UF_CURSO           %<>% as.factor() 
cursosAREAOCDE$CO_CURSO               %<>% as.factor()
cursosAREAOCDE$DS_GRAU_ACADEMICO      %<>% as.factor()

cursosAREAOCDE                        %<>% drop.levels()
#--------------

IES           <- dplyr::select(IES, CO_IES, NO_IES, SGL_IES, NO_MUNICIPIO_IES, SGL_UF_IES, 
                               NO_REGIAO_IES, QT_TEC_TOTAL, VL_RECEITA_PROPRIA, VL_DES_CUSTEIO, 
                               VL_DES_INVESTIMENTO )

IES$CO_IES                            %<>% as.factor()
IES$NO_IES                            %<>% as.factor()
IES$SGL_IES                           %<>% as.factor()
IES$NO_MUNICIPIO_IES                  %<>% as.factor()
IES$SGL_UF_IES                        %<>% as.factor()
IES$NO_REGIAO_IES                     %<>% as.factor()
IES$VL_DES_CUSTEIO                    <- as.numeric(as.character(IES$VL_DES_CUSTEIO))
#------------------------------------------------------
#Sumários
sumCursosnominal      <- dplyr::select(cursosAREAOCDE, CO_OCDE_AREA_GERAL, GRUPO, 
                                NO_REGIAO_CURSO, SGL_UF_CURSO,DS_GRAU_ACADEMICO)

sumCursosnominal2     <- dplyr::select(cursosAREAOCDE, NO_IES)

sumCursosqtde         <- dplyr::select(cursosAREAOCDE, QT_MATRICULA_CURSO,QT_CONCLUINTE_CURSO, 
                                QT_INGRESSO_CURSO, PG)
#----------------------------------------------------
#Totalizando por Município

munCursos             <- ddply(cursosAREAOCDE, c("NO_MUNICIPIO_CURSO","SGL_UF_CURSO"), summarise,
                               Mat = sum(QT_MATRICULA_CURSO))

munCursos$NO_MUNICIPIO_CURSO %<>% toupper()
munCursos$LOC                 <- paste(munCursos$NO_MUNICIPIO_CURSO, munCursos$SGL_UF_CURSO, sep = ",")
munCursos                    %<>% dplyr::select(LOC, Mat)
#Coordenadas de Todos Municípios brasileiros
coordMUN <- read_excel("MunicipiosBrasil.xlsx")

brasil <- c("AC", "AL", "AP", "AM", "BA", "CE", "DF", "ES", "GO", "MA", "MT", "MS", "MG",
            "PA", "PB", "PR", "PE", "PI", "RJ", "RN", "RS", "RO", "RR", "SC", "SP", "SE", "TO")

estadosBRASIL             <- c( "Acre", "Alagoas", "Amapá", "Amazonas",
                                "Bahia","Ceará", "Distrito Federal", 
                                "Espírito Santo", "Goiás","Maranhão",
                                "Mato Grosso", "Mato Grosso do Sul",
                                "Minas Gerais","Pará", "Paraíba", "Paraná",
                                "Pernambuco", "Piauí", "Rio de Janeiro",
                                "Rio Grande do Norte", "Rio Grande do Sul", 
                                "Rondonia", "Roraima","Santa Catarina", 
                                "São Paulo", "Sergipe", "Tocantins" )

estadosBRASIL         <- as.data.frame(cbind(brasil, estadosBRASIL))
names(estadosBRASIL)  <- c("SIGLA", "UF")
estadosBRASIL$UF      <- toupper(estadosBRASIL$UF)

coordMUN              <- merge(coordMUN, estadosBRASIL, by = "UF", all.x = TRUE)
coordMUN$LOC          <- paste(coordMUN$MUNICIPIO, coordMUN$SIGLA, sep = ",")
coordMUN              %<>% dplyr::select(UF, LOC, lon, lat)
#--------------------------------------------------------------------------

#Coordenadas dos Municipios
munCursos <- merge(munCursos, coordMUN, by = "LOC", all.x = TRUE)

#Municípios tem várias entradas de posicionamento
munCursos <- ddply(munCursos, c("UF", "LOC"), summarise, Mat = mean(Mat), lon = mean(as.numeric(lon)), lat = mean(as.numeric(lat)))

titulo    <- "Distribuição de alunos Matriculados - IFES (2015)"
brasil          <- get_map('Brazil', zoom = 4, language = 'pt-BR', maptype = 'hybrid')
mapaBR          <- ggmap(brasil, extend = "normal", size = c(900,900))

munCursos %<>% filter(!is.na(Mat))
munCursos$UF %<>% as.factor()


mapa <- mapaBR +
  geom_point(aes(x = lon, y = lat, size = Mat , colour = UF),  data = munCursos, na.rm = TRUE)+
  ggtitle(titulo)+
  guides(colour = FALSE)+
  theme_minimal() +
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 8),
        legend.position = c(0.8,0.1),
        legend.direction = "horizontal",
        legend.background = element_rect(fill = "transparent", colour = "transparent"),
        strip.background = element_blank(),
        strip.text.y = element_blank(),
        legend.background = element_blank(),
        legend.key = element_blank(),
        axis.text = element_text(size = 6, hjust = 0),
        plot.title = element_text(size=22, colour = "white"))

#---------------------------------------------------------
#Relações entre as categorias

cursosAREAOCDE %<>% filter(!is.na(CO_OCDE_AREA_GERAL))

cursosAREAOCDE %<>% drop.levels

soma          <- sum(cursosAREAOCDE$QT_MATRICULA_CURSO, na.rm = TRUE)
cursosG       <-  cursosAREAOCDE %>% ddply(., c("CO_OCDE_AREA_GERAL", "GRUPO"), summarise, 
                        MatP = 100 * sum(QT_MATRICULA_CURSO, na.rm = TRUE)/soma)

cursosClass  <- acast(cursosG, CO_OCDE_AREA_GERAL ~ GRUPO,  value.var = "MatP")

ANDIFES <- colSums(cursosClass, na.rm = TRUE)
cursosClass <- rbind(cursosClass,ANDIFES)

OCDE <- rowSums(cursosClass, na.rm = TRUE)
cursosClass <- cbind(cursosClass, OCDE)

#--------------------------------------------
#Caracterização das IES

IES <- arrange(IES, NO_REGIAO_IES, SGL_UF_IES, SGL_IES)

custeio <- ggplot(IES, aes(x= reorder(SGL_IES, -VL_DES_CUSTEIO), y = VL_DES_CUSTEIO/1000000, fill = NO_REGIAO_IES)) + 
  geom_bar(stat = "identity")+
  ggtitle("Orçamento de Custeio IFES - 2016 (CENSO)") +
  labs(x = "", y = "milhoẽs R$") +
  scale_y_continuous(labels=dollar)+
  theme_economist_white() + 
  theme(axis.text.x = element_text(angle = 75))+
  theme(legend.position = "bottom") +
  theme(legend.text = element_text(size = 6))+
  theme(axis.text = element_text(size = 6))+
  theme(plot.title = element_text(size=8, face = "bold"))+
  guides(fill=guide_legend(nrow=1,byrow=TRUE))+
  scale_fill_brewer(palette = "Paired")

  
#------------------------------------
#Limpando dados

IES       %<>% filter(VL_DES_CUSTEIO > 0)
cursos    %<>% filter(!is.na(GRUPO))
#----------------------------------
#Agrupando alunos matriculados nas categorias

alunosG <- ddply(cursos, c("CO_IES", "GRUPO"), summarise, Mat = sum(QT_MATRICULA_CURSO))

#--------------------------------
#Nesse dataframe NA é equivalente  a ZERO
alunosG[is.na(alunosG)] <- 0

#reshape

IESmat <- dcast(alunosG, CO_IES ~ GRUPO, value.var = "Mat")

#Nesse dataframe NA é equivalente  a ZERO
IESmat[is.na(IESmat)] <- 0

IESdata <- dplyr::select(IES, CO_IES, SGL_IES, NO_REGIAO_IES, VL_DES_CUSTEIO)

#Agrega dataframe

IESdata <- merge(IESdata, IESmat, by = "CO_IES")

#--------------------------------------------------------------------
#--------------------------------------------------------------------
#--------------------------------------------------------------------
#--------------------------------------------------------------------
# Fitting Ordinary Linerar fit

ols <- lm(VL_DES_CUSTEIO ~ A1 + A2 + A3 + A4, data = IESdata)
summary(ols)
#plot(ols, las = 1)

#UFRJ é um outlier
#Removendo UFRJ

ols1 <- lm(VL_DES_CUSTEIO ~ A1 + A2 + A3 + A4, data = IESdata[-c(40),])
summary(ols1)
#plot(ols1, las = 1)


