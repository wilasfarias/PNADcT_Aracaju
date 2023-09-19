library(jtools)
library(utils)
library(magrittr)
library(PNADcIBGE)
library(survey)
library(readr)
library(magrittr)
library(utils)
library(timeDate)
library(readxl)
library(tibble)
library(ggplot2)
library(convey)
library(tidyverse)
library(plyr)
library(descr)
library(openxlsx)
library(readxl)
library(rjson)
library(sidrar)
library(tidyr)
library(ggplot2)
library(stringr)
library(dplyr)
library(magrittr)

#install.packages("rticles")
#devtools::install_github("rstudio/rticles")

# arredondar casas decimais
# scales::number(x, accuracy = 1, big.mark = ".", decimal.mark = ",")
# [1] "1.050"
# scales::number(x, accuracy = 0.1, big.mark = ".", decimal.mark = ",")
# [1] "1.050,2"
# scales::number(x, accuracy = 0.01, big.mark = ".", decimal.mark = ",")

#"F:/PNADcT_SERGIPE/wilas_relatorio/tabelas/Tabela 1 - Total da populacao por sexo em Aracaju.xlsx"

#"C:/Users/wilasalvesfarias/Desktop/PNADcT_SERGIPE/wilas_relatorio/tabelas/Tabela 1 - Total da populacao por sexo em Aracaju.xlsx")

#-----------------------------TABELA1-------------------------------------------------#
# importar Total da populacao por sexo em Aracaju
#Pop_sex2 <- read_excel("F:/PNADcT_SERGIPE/wilas_relatorio/tabelas/Tabela 1 - Total da populacao por sexo em Aracaju.xlsx")

Pop_sex <- read_excel("F:/PNADcT_SERGIPE/wilas_relatorio/tabelas/Tabela 1 - Total da populacao por sexo em Aracaju.xlsx")
Pop_sex <- as.data.frame(Pop_sex)
Pop_sex[,1] <- NULL
Pop_sex <- Pop_sex %>% mutate(Total = Homem+Mulher)

Pop_sex <- Pop_sex %>% filter(TRI >= "2022.1")
names(Pop_sex)[1] <- "Trimestre"

Pop_sex$Homem <- scales::number(Pop_sex$Homem, 
                                accuracy = 1, big.mark = ".", decimal.mark = ",")
Pop_sex$Mulher <- scales::number(Pop_sex$Mulher, 
                                 accuracy = 1, big.mark = ".", decimal.mark = ",")
Pop_sex$Total <- scales::number(Pop_sex$Total, 
                                accuracy = 1, big.mark = ".", decimal.mark = ",")
Pop_sex$Trimestre <- as.factor(Pop_sex$Trimestre)


# importar Renda Media Trimestral em Aracaju na Capital
Rmed <- read_excel("F:/PNADcT_SERGIPE/wilas_relatorio/tabelas/Tabela 1.1 - Renda Media Trimestral em Aracaju na Capital.xlsx")
Rmed <- as.data.frame(Rmed)
Rmed[1] <- NULL

Rmed <- Rmed %>% filter(TRI >= "2022.1") %>% arrange(TRI)
names(Rmed)[2] <- "Renda Media"
names(Rmed)[1] <- "Trimestre"

Rmed$`Renda Media` <- scales::number(Rmed$`Renda Media`, 
                                                  accuracy = 0.01, big.mark = ".", decimal.mark = ",")

Pop_sex_Rmed <- full_join(Pop_sex, Rmed, "Trimestre")


#---------------------------TABELA2---------------------------------------------------#
# importar População na Força e Fora da Força de Trabalho na Capital
PEA_NPEA <- read_excel("F:/PNADcT_SERGIPE/wilas_relatorio/tabelas/Tabela 2 - População na Força e Fora da Força de Trabalho na Capital.xlsx")
PEA_NPEA <- as.data.frame(PEA_NPEA)
PEA_NPEA[,1] <- NULL
PEA_NPEA

# importar "Total de pessoas ocupadas e desocupadas na Capital.xlsx"
Tocupdes <- read_excel("F:/PNADcT_SERGIPE/wilas_relatorio/tabelas/Tabela 2.1 - Total de pessoas ocupadas e desocupadas na Capital.xlsx")
Tocupdes <- as.data.frame(Tocupdes)
Tocupdes[,1] <- NULL
Tocupdes

# importar Total de Pessoas Desalentadas na Capital
T_desalento <- read_excel("F:/PNADcT_SERGIPE/wilas_relatorio/tabelas/Tabela 2.2 - Total de Pessoas Desalentadas na Capital.xlsx")
T_desalento <- as.data.frame(T_desalento)
T_desalento[,1] <- NULL

PEA_NPEA_ocup_des_desal <- full_join(PEA_NPEA, Tocupdes, by= "TRI")
(PEA_NPEA_ocup_des_desal <- full_join(PEA_NPEA_ocup_des_desal, T_desalento, by= "TRI"))


PEA_NPEA_ocup_des_desal$`Pessoas na força de trabalho` <- scales::number(PEA_NPEA_ocup_des_desal$`Pessoas na força de trabalho`, 
                                                                         accuracy = 1, big.mark = ".", decimal.mark = ",")
PEA_NPEA_ocup_des_desal$`Pessoas fora da força de trabalho` <- scales::number(PEA_NPEA_ocup_des_desal$`Pessoas fora da força de trabalho`, 
                                                                              accuracy = 1, big.mark = ".", decimal.mark = ",")
PEA_NPEA_ocup_des_desal$`Pessoas ocupadas` <- scales::number(PEA_NPEA_ocup_des_desal$`Pessoas ocupadas`, 
                                                             accuracy = 1, big.mark = ".", decimal.mark = ",")
PEA_NPEA_ocup_des_desal$`Pessoas desocupadas` <- scales::number(PEA_NPEA_ocup_des_desal$`Pessoas desocupadas`, 
                                                                accuracy = 1, big.mark = ".", decimal.mark = ",")
PEA_NPEA_ocup_des_desal$`Pessoas desalentadas` <- scales::number(PEA_NPEA_ocup_des_desal$`Pessoas desalentadas`, 
                                                                 accuracy = 1, big.mark = ".", decimal.mark = ",")


(PEA_NPEA_ocup_des_desal <- PEA_NPEA_ocup_des_desal %>% filter(TRI>="2022.1"))
names(PEA_NPEA_ocup_des_desal)[1] <- "Trimestre"

#-------------------------TABELA3-----------------------------------------------------#
# importar Pessoas ocupadas e desocupadas que estudam e nao estudam em Aracaju

T_estDes <- read_excel("F:/PNADcT_SERGIPE/wilas_relatorio/tabelas/Tabela 3 - Pessoas ocupadas e desocupadas que estudam e nao estudam em Aracaju.xlsx")
T_estDes <- as.data.frame(T_estDes)
T_estDes[,1] <- NULL
names(T_estDes)[2:5] <- c("Ocupadas Estudam", 
                          "Ocupadas Não Estudam",
                          "Desocupadas Estudam",
                          "Desocupadas Não Estudam")
T_estDes <- T_estDes %>% filter(TRI>=2022.1)
names(T_estDes)[1] <- "Trimestre"

#-------------------------TABELA4-----------------------------------------------------#
# importar Ocupacao e desocupacao por faixa etaria em Aracaju
(desocupfaixa <- read_excel("F:/PNADcT_SERGIPE/wilas_relatorio/tabelas/Tabela 4 - Ocupacao e desocupacao por faixa etaria em Aracaju.xlsx"))
desocupfaixa <- as.data.frame(desocupfaixa)
desocupfaixa[1] <- NULL

desocupfaixa <- desocupfaixa %>% 
  mutate(Total=`Pessoas ocupadas`+`Pessoas desocupadas`,
         `Taxa de Desocupação`=(`Pessoas desocupadas`/`Total`)*100)

desocupfaixa$`Pessoas ocupadas` <- scales::number(desocupfaixa$`Pessoas ocupadas`, 
                                                  accuracy = 1, big.mark = ".", decimal.mark = ",")

desocupfaixa$`Pessoas desocupadas` <- scales::number(desocupfaixa$`Pessoas desocupadas`, 
                                                     accuracy = 1, big.mark = ".", decimal.mark = ",")

desocupfaixa$Total <- scales::number(desocupfaixa$Total, 
                                     accuracy = 1, big.mark = ".", decimal.mark = ",")

desocupfaixa$`Taxa de Desocupação` <- scales::number(desocupfaixa$`Taxa de Desocupação`, 
                                                 accuracy = 0.1, big.mark = ".", decimal.mark = ",")

desocupfaixa <- desocupfaixa %>% 
  filter(TRI>="2022.4")
names(desocupfaixa)[1] <- "Trimestre"

(desocupfaixa$Faixa[desocupfaixa$Faixa == "15_19"] <- "15 a 19")
(desocupfaixa$Faixa[desocupfaixa$Faixa == "20_24"] <- "20 a 24")
(desocupfaixa$Faixa[desocupfaixa$Faixa == "25_29"] <- "25 a 29")

#-------------------------TABELA5-----------------------------------------------------#
# importar Jovem desocupado e ocupado por escolaridade, Desocupado em Aracaju
esc_des_faAJ <- read_excel("F:/PNADcT_SERGIPE/wilas_relatorio/tabelas/Tabela 5 - Jovem desocupado e ocupado por escolaridade, Desocupado em Aracaju.xlsx")
esc_des_faAJ <- as.data.frame(esc_des_faAJ)
esc_des_faAJ[1] <- NULL

esc_des_faAJ <- esc_des_faAJ %>% filter(TRI >= "2022.4")
names(esc_des_faAJ)[1] <- "Trimestre"

esc_des_faAJ <- esc_des_faAJ %>% 
  mutate(Nivel = factor(Nivel,
                          levels = c("Sem instrução e menos de 1 ano de estudo",
                                     "Fundamental incompleto ou equivalente",
                                     "Fundamental completo ou equivalente",
                                     "Médio incompleto ou equivalente",
                                     "Médio completo ou equivalente",
                                     "Superior incompleto ou equivalente",
                                     "Superior completo"))) %>% 
  arrange(Trimestre, Nivel)


# importar Jovens nem-nem por escolaridade, Desocupado em Aracaju
nem_nem_fa_des <- read_excel("F:/PNADcT_SERGIPE/wilas_relatorio/tabelas/Tabela 5.1 - Jovens nem-nem por escolaridade, Desocupado em Aracaju.xlsx")
nem_nem_fa_des <- as.data.frame(nem_nem_fa_des)
nem_nem_fa_des[1] <- NULL

nem_nem_fa_des <- nem_nem_fa_des %>% filter(TRI >= "2022.4")
names(nem_nem_fa_des)[1] <- "Trimestre"

nem_nem_fa_des <- nem_nem_fa_des %>% 
  mutate(Nivel = factor(Nivel,
                        levels = c("Sem instrução e menos de 1 ano de estudo",
                                   "Fundamental incompleto ou equivalente",
                                   "Fundamental completo ou equivalente",
                                   "Médio incompleto ou equivalente",
                                   "Médio completo ou equivalente",
                                   "Superior incompleto ou equivalente",
                                   "Superior completo"))) %>% 
  arrange(Trimestre, Nivel)


#-------------------------TABELA6-----------------------------------------------------#
# importar Total de Pessoas por Nivel de Instrucao em Aracaju
Instr <- read_excel("F:/PNADcT_SERGIPE/wilas_relatorio/tabelas/Tabela 6 - Total de Pessoas por Nivel de Instrucao em Aracaju.xlsx")
Instr <- as.data.frame(Instr)
Instr[1] <- NULL

Instr <- Instr %>% filter(TRI >= "2022.4") %>% arrange(TRI)

Instr <- Instr %>% spread("TRI", "Total") %>% arrange(Nivel)

Instr <- Instr %>% 
  mutate(Nivel = factor(Nivel,
                        levels = c("Sem instrução e menos de 1 ano de estudo",
                                   "Fundamental incompleto ou equivalente",
                                   "Fundamental completo ou equivalente",
                                   "Médio incompleto ou equivalente",
                                   "Médio completo ou equivalente",
                                   "Superior incompleto ou equivalente",
                                   "Superior completo"))) %>% 
  arrange(Nivel)

#-------------------------TABELA7-----------------------------------------------------#
# importar Nivel de instrucao das pessoas que nao estudam em Aracaju
ins_n_est <- read_excel("F:/PNADcT_SERGIPE/wilas_relatorio/tabelas/Tabela 7 - Nivel de instrucao das pessoas que nao estudam em Aracaju.xlsx")
ins_n_est <- as.data.frame(ins_n_est)
ins_n_est[1] <- NULL

ins_n_est <- ins_n_est %>% filter(TRI >= "2022.4") %>% arrange(TRI)

ins_n_est <- ins_n_est %>% spread("TRI", "Total") %>% arrange(Nivel)
ins_n_est <- ins_n_est %>% 
  mutate(Nivel = factor(Nivel,
                        levels = c("Sem instrução e menos de 1 ano de estudo",
                                   "Fundamental incompleto ou equivalente",
                                   "Fundamental completo ou equivalente",
                                   "Médio incompleto ou equivalente",
                                   "Médio completo ou equivalente",
                                   "Superior incompleto ou equivalente",
                                   "Superior completo"))) %>% 
  arrange(Nivel)

ins_n_est$"2022.4" <- scales::number(ins_n_est$"2022.4", 
                                     accuracy = 1, big.mark = ".", decimal.mark = ",")
ins_n_est$"2023.1" <- scales::number(ins_n_est$"2023.1", 
                                     accuracy = 1, big.mark = ".", decimal.mark = ",")

#-------------------------TABELA8-----------------------------------------------------#
# importar Nem estudam e nem trabalham por nivel de escolaridade em Aracaju
T_nem_nem <- read_excel("F:/PNADcT_SERGIPE/wilas_relatorio/tabelas/Tabela 8 - Nem estudam e nem trabalham por nivel de escolaridade em Aracaju.xlsx")
T_nem_nem <- as.data.frame(T_nem_nem)
T_nem_nem[1] <- NULL

T_nem_nem <- T_nem_nem %>% filter(TRI >= "2022.4") %>% arrange(TRI)

T_nem_nem <- T_nem_nem %>% spread("TRI", "Total") %>% arrange(Nivel)

T_nem_nem <- T_nem_nem %>% 
  mutate(Nivel = factor(Nivel,
                        levels = c("Sem instrução e menos de 1 ano de estudo",
                                   "Fundamental incompleto ou equivalente",
                                   "Fundamental completo ou equivalente",
                                   "Médio incompleto ou equivalente",
                                   "Médio completo ou equivalente",
                                   "Superior incompleto ou equivalente",
                                   "Superior completo"))) %>% 
  arrange(Nivel)

#-------------------------TABELA9-----------------------------------------------------#
# importar "Pessoas ocupadas por setor produtivo, Capital.xlsx"
OcupsetorAJ <- read_excel("F:/PNADcT_SERGIPE/wilas_relatorio/tabelas/Tabela 9 - Pessoas ocupadas por setor produtivo, Capital.xlsx")
OcupsetorAJ <- as.data.frame(OcupsetorAJ)
OcupsetorAJ[1] <- NULL
names(OcupsetorAJ)[3] <- "Pessoas Ocupadas"

OcupsetorAJ <- OcupsetorAJ %>% filter(TRI >= "2022.4") %>% arrange(TRI)

OcupsetorAJ <- OcupsetorAJ %>% spread("TRI", "Pessoas Ocupadas") %>% arrange(Setor)

# OcupsetorAJ_Rmed_Setor <- full_join(OcupsetorAJ, Rmed_Setor, by=c("TRI", "Setor")) %>% arrange(TRI)

# importar Renda Media por Setor Trimestral em Aracaju
Rmed_Setor <- read_excel("F:/PNADcT_SERGIPE/wilas_relatorio/tabelas/Renda Media por Setor Trimestral em Aracaju.xlsx")
Rmed_Setor <- as.data.frame(Rmed_Setor)
Rmed_Setor[1] <- NULL

Rmed_Setor <- Rmed_Setor %>% filter(TRI >= "2022.4") %>% arrange(TRI)
names(Rmed_Setor)[3] <- "Renda Média"

Rmed_Setor <- Rmed_Setor %>% spread("TRI", "Renda Média")

#-------------------------TABELA10-----------------------------------------------------#
# importar Pessoas que estudam em Aracaju
T_estuda <- read_excel("F:/PNADcT_SERGIPE/wilas_relatorio/tabelas/Tabela 10 - Pessoas que estudam em Aracaju.xlsx")
T_estuda <- as.data.frame(T_estuda)
T_estuda[1] <- NULL

T_estuda <- T_estuda %>% filter(TRI >= "2022.1") %>% arrange(TRI)
names(T_estuda)[1] <- "Trimestres"
names(T_estuda)[2] <- "Estudam"
names(T_estuda)[3] <- "Não Estudam"

#T_estuda$"Estudam" <- scales::number(T_estuda$"Estudam", accuracy = 1, big.mark = ".", decimal.mark = ",")
#T_estuda$"Não Estudam" <- scales::number(T_estuda$"Não Estudam", accuracy = 1, big.mark = ".", decimal.mark = ",")

#-------------------------TABELA10-----------------------------------------------------#
# importar Taxa de ocupacao e desocupacao na Capital
txocupdes <- read_excel("F:/PNADcT_SERGIPE/wilas_relatorio/tabelas/Taxa de ocupacao e desocupacao na Capital.xlsx")
txocupdes <- as.data.frame(txocupdes)
txocupdes[1] <- NULL

txocupdes <- txocupdes %>% filter(TRI >= "2022.1") %>% arrange(TRI)
names(txocupdes)[1] <- "Trimestres"
names(txocupdes)[2] <- "Taxa de Desocupação"
names(txocupdes)[3] <- "Taxa de Ocupação"

#txocupdes$`Taxa de Desocupação` <- scales::number(txocupdes$`Taxa de Desocupação`, accuracy = 0.01, big.mark = ".", decimal.mark = ",")
#txocupdes$`Taxa de Ocupação` <- scales::number(txocupdes$`Taxa de Ocupação`, accuracy = 0.01, big.mark = ".", decimal.mark = ",")



# importar Renda Media Trimestral em Aracaju na Capital


# importar Renda Media por Setor Trimestral em Aracaju


# importar Pessoas que estudam em Aracaju


# importar Total de Pessoas por Nivel de Instrucao em Aracaju


# importar Nivel de instrucao das pessoas que nao estudam em Aracaju




# importar Populacao por faixa etaria em Aracaju2


# importar Renda Media com ensino medio e superior completo em Aracaju


# importar Renda media pessoas com medio e superior completo por sexo em Aracaju



















