# autor: Joao Victor Antunes Lopes
# R version 4.2.2 (2022-10-31 ucrt)
# Platform: x86_64-w64-mingw32/x64 (64-bit)

# carregando pacotes
library(data.table)
library(dplyr)
library(plotrix) #piramide etaria
library(tidyverse)

# definindo diretorio
getwd()
setwd('C:/Users/User/Documents/DM026 - Banco de dados em demografia/TrabalhoFinal')

# lendo censo geral (1991 e 2010)
censogeral <- fread('Censo_1991_2010_TodosEstados.csv', header=T, sep= ',')

# informacoes gerais
class(censogeral)
names(censogeral)
head(censogeral)
tail(censogeral)

# separando 1991 e 2010
# 1991
df1991 <- filter(censogeral, YEAR == 1991)
df1991AM <- filter(df1991, GEO1_BR == 76013) # Brasil/Amazonas

# 2010
df2010 <- filter(censogeral, YEAR == 2010) 
df2010AM <- filter(df2010, GEO1_BR == 76013) # Brasil/Amazonas

# verificando as variaveis
names(df1991AM)
names(df2010AM)

# conferindo a distribuicao etaria
hist(df1991AM$AGE) 
hist(df2010AM$AGE)

# criando df para piramide etaria
# 1991 
q1_1991 <- df1991AM %>% select (YEAR, SEX, AGE) %>% # year, sex and age
  mutate(Homem = SEX == 1) %>% # sex = 1 para homens
  mutate(Mulher = SEX == 2) %>% # sex = 2 para mulheres
  mutate(Faixa_Etaria = case_when(AGE %in% 0:4 ~"00 a 04 anos", # grupos quinquenais
                                  AGE %in% 5:9 ~ "05 a 09 anos",
                                  AGE %in% 10:14 ~ "10 a 14 anos", 
                                  AGE %in% 15:19 ~ "15 a 19 anos",
                                  AGE %in% 20:24 ~ "20 a 24 anos",
                                  AGE %in% 25:29 ~ "25 a 29 anos",
                                  AGE %in% 30:34 ~ "30 a 34 anos", 
                                  AGE %in% 35:39 ~ "35 a 39 anos",
                                  AGE %in% 40:44 ~ "40 a 44 anos",
                                  AGE %in% 45:49 ~ "45 a 49 anos",
                                  AGE %in% 50:54 ~ "50 a 54 anos", 
                                  AGE %in% 55:59 ~ "55 a 59 anos",
                                  AGE %in% 60:64 ~ "60 a 64 anos",
                                  AGE %in% 65:69 ~ "65 a 69 anos",
                                  AGE %in% 70:74 ~ "70 a 74 anos", 
                                  AGE %in% 75:79 ~ "75 a 79 anos",
                                  AGE >=80 ~"80 anos e mais") # intervalo aberto
         )%>%
  group_by(Faixa_Etaria) %>% # agrupando por faixa etaria
  summarise(Homem = sum( # contabilizando
    Homem), Mulher = sum(Mulher)
           )

View(q1_1991)

# retirando a primeira coluna
q1_1991 <- q1_1991[,-1]

# piramide
# criando vetor idade em grupos quinquenais
idade = c("0-4", "5-9", "10-14",
        "15-19", "20-24", "25-29",
        "30-34", "35-39", "40-44", 
        "45-49", "50-54", "55-59",
        "60-64", "65-69", "70-74",
        "75-79", "80+")

# gerando percentuais
q1_1991$Homem.p = q1_1991$Homem/sum(q1_1991); # pct homens
q1_1991$Mulher.p = q1_1991$Mulher/sum(q1_1991) # pct mulheres

# plotando a piramide
windows(height = 10, width = 15)

pyramid.plot(q1_1991$Homem.p*100, q1_1991$Mulher.p*100, labels=idade, unit = '%', 
             main = "Piramide Populacional, Amazonas (1991)",
             top.labels = c("Homem", "Idade", "Mulher"),
             lxcol = 'blue', rxcol = 'red', gap = 1, show.values = TRUE)

#

# 2010
q1_2010 <- df2010AM %>% select (YEAR, SEX, AGE) %>% 
  mutate(Homem = SEX == 1) %>% 
  mutate(Mulher = SEX == 2) %>%
  mutate(Faixa_Etaria = case_when(AGE %in% 0:4 ~"00 a 04 anos",
                                  AGE %in% 5:9 ~ "05 a 09 anos",
                                  AGE %in% 10:14 ~ "10 a 14 anos", 
                                  AGE %in% 15:19 ~ "15 a 19 anos",
                                  AGE %in% 20:24 ~ "20 a 24 anos",
                                  AGE %in% 25:29 ~ "25 a 29 anos",
                                  AGE %in% 30:34 ~ "30 a 34 anos", 
                                  AGE %in% 35:39 ~ "35 a 39 anos",
                                  AGE %in% 40:44 ~ "40 a 44 anos",
                                  AGE %in% 45:49 ~ "45 a 49 anos",
                                  AGE %in% 50:54 ~ "50 a 54 anos", 
                                  AGE %in% 55:59 ~ "55 a 59 anos",
                                  AGE %in% 60:64 ~ "60 a 64 anos",
                                  AGE %in% 65:69 ~ "65 a 69 anos",
                                  AGE %in% 70:74 ~ "70 a 74 anos", 
                                  AGE %in% 75:79 ~ "75 a 79 anos",
                                  AGE >=80 ~"80 anos e mais")
  )%>%
  group_by(Faixa_Etaria) %>% 
  summarise(Homem = sum(
    Homem), Mulher = sum(Mulher)
  )

View(q1_2010)

# retirando a primeira coluna
q1_2010 <- q1_2010[ ,-1]

# piramide
# criando vetor idade em grupos quinquenais
idade = c("0-4", "5-9", "10-14",
        "15-19", "20-24", "25-29",
        "30-34", "35-39", "40-44", 
        "45-49", "50-54", "55-59",
        "60-64", "65-69", "70-74",
        "75-79", "80+")

# gerando percentuais
q1_2010$Homem.p = q1_2010$Homem/sum(q1_2010);
q1_2010$Mulher.p = q1_2010$Mulher/sum(q1_2010)

# plotando a piramide
windows(height = 10, width = 15)

pyramid.plot(q1_2010$Homem.p*100, q1_2010$Mulher.p*100, labels = idade, unit = '%',
             main = "Piramide Populacional, Amazonas (2010)", 
             top.labels = c("Homem","Idade","Mulher"),
             lxcol = 'blue',rxcol = 'red', gap = 1, show.values=TRUE)

# 

# questao 2
#Razao de dependencia de jovens (RDJ) = (populacao de 0 a 14 anos) / (populacao de 15 a 64 anos) *100
#Razao de dependencia de idosos (RDI) = (populacao 65 anos e mais) / (populacao de 15 a 64 anos) *100
#Razao de dependencia total: [(populacao 0 a 14 anos) + (populacao 65 anos e mais)] / (populacao de 15 a 64 anos) *100

# 1991
q2_1991 <- df1991AM %>%
  mutate(Jovens = AGE <= 14, # grupo 'jovens' para as idades <= 14 ANOS
         Idosos = AGE >= 65, # grupo idosos para as idades >= 65 ANOS 
         PIA = AGE >= 15 & AGE <= 64) %>% # definindo pia
  summarise(Jovens = sum(Jovens), Idosos = sum(Idosos), # contabilizando
            PIA = sum(PIA)) %>% 
  mutate(RDjovens = (Jovens/PIA)*(100)) %>% # jovens
  mutate(RDidosos = (Idosos/PIA)*(100)) %>% # idosos 
  mutate(RDtotal = (Jovens + Idosos)/(PIA)*(100)) # total

View(q2_1991)

#

# 2010

q2_2010 <- df2010AM %>% 
  mutate(Jovens = AGE <= 14,
         Idosos = AGE >= 65,
         PIA = AGE >= 15 & AGE <= 64) %>%
  summarise(Jovens = sum(Jovens), Idosos = sum(Idosos),
            PIA = sum(PIA)) %>% 
  mutate(RDjovens = (Jovens/PIA)*(100)) %>% 
  mutate(RDidosos = (Idosos/PIA)*(100)) %>% 
  mutate(RDtotal = (Jovens + Idosos)/(PIA)*(100))

View(q2_2010)

# questao 3
# 1991 

names(df1991AM) #SCHOOL - variavel referente a frequencia escolar

q3_1991 <- df1991AM %>% 
  select(SCHOOL, AGE) %>% # selecionando school and age
  filter(SCHOOL %in% c(1,2), AGE %in% 6:18) %>% # desconsiderando NIU
  mutate(Yes = SCHOOL == 1, No = SCHOOL == 2) %>% # 1, sim e 2, nao
  mutate(AGE = case_when(AGE %in% 6:15 ~ '6 a 15 anos', 
                         AGE %in% 16:18 ~ '16 a 18 anos')) %>% #agrupamento 6 a 15 e 16 a 18
  group_by(AGE) %>% # agrupando pela idade
  summarise(Yes = sum(Yes), No = sum(No)) %>% # contabilizando
  mutate(pct_yes = Yes/(Yes+No)*100) %>% # pct sim
  mutate(pct_no = No/(Yes+No)*100)  # pct no

View(q3_1991)

#

# 2010

q3_2010 <- df2010AM %>% 
  select(SCHOOL, AGE) %>%
  filter(SCHOOL %in% c(1, 3, 4), AGE %in% 6:18) %>% # ignorando NIU, considerando 3 e 4 com 'NO', removendo as demais idades
  mutate(Yes = SCHOOL == 1, No = SCHOOL != 1 ) %>% 
  mutate(AGE = case_when(AGE %in% 6:15 ~ '6 a 15 anos',
                         AGE %in% 16:18 ~ '16 a 18 anos')) %>%
  group_by(AGE) %>% 
  summarise(Yes = sum(Yes), No = sum(No)) %>%
  mutate(pct_yes = Yes/(Yes+No)*100) %>% 
  mutate(pct_no = No/(Yes+No)*100)

View(q3_2010)

#

# questao 4
# 1991
# limpando a base, removendo as variaveis de acordo com as definicoesdo IPMUS
#df1991AM %>% filter(INCTOT != 9999999, INCTOT != 9999998) #9999999 = NIU (not in universe), #9999998 = Unknown/missing
df1991AM$INCTOT <- ifelse(df1991AM$INCTOT == 9999999, NA, df1991AM$INCTOT) #9999999 = NIU (not in universe)
df1991AM$INCTOT <- ifelse(df1991AM$INCTOT == 9999998, NA, df1991AM$INCTOT) #9999998 = Unknown/missing

View(df1991AM)

# edattain e inctot
q4_1991_nivel_escolar <- df1991AM %>% 
  select(EDATTAIN, INCTOT) %>% # selecionando EDATTAIN
  filter(EDATTAIN %in% c(1,2,3,4)) %>%
  mutate(
    nivel_escolar = case_when( # compatibilizando
      EDATTAIN == 1 ~ '1. primario incompleto',
      EDATTAIN == 2 ~ '2. primario completo',
      EDATTAIN == 3 ~ '3. secundario completo',
      EDATTAIN == 4 ~ '4. superior completo',
    ), 
    faixa_renda = case_when( # renda SM = 36161.60 cruzeiros [Duarte, Freiras e Bianchini (1998, p. 11)]
      INCTOT < 36161.60 ~ '1. menos de 1 sm', # construindo as categorias de INCTOT...
      INCTOT >= 36161.60 & INCTOT <= 72323.20 ~ '2. de 1 a 2 sm',
      INCTOT > 72323.20 & INCTOT <= 180808 ~ '3. de 2 a 5 sm',
      INCTOT > 180808 ~ '4. mais de 5 sm'
    )
  ) %>% 
  na.omit() %>% # omitindo NAs
  group_by(nivel_escolar, faixa_renda) %>% # agrupando por nivel escolar e faixa renda
  count() %>% 
  pivot_wider( # pivoteando
    names_from = nivel_escolar, 
    values_from = n
  )

View(q4_1991_nivel_escolar)

# ownership e inctot
q4_1991_cond_habitacao <- df1991AM %>% # cond habitacao
  select(OWNERSHIP, INCTOT) %>% 
  filter(OWNERSHIP %in% c(1,2)) %>%
  mutate(
    cond_habitacao = case_when(
      OWNERSHIP == 1 ~ 'propria', # 1 prorpia
      OWNERSHIP == 2 ~ 'nao propria', # 2 nao proria
    ), 
    faixa_renda = case_when( 
      INCTOT < 36161.60 ~ '1. menos de 1 sm', # construindo as categorias de INCTOT
      INCTOT >= 36161.60 & INCTOT <= 72323.20 ~ '2. de 1 a 2 sm',
      INCTOT > 72323.20 & INCTOT <= 180808 ~ '3. de 2 a 5 sm',
      INCTOT > 180808 ~ '4. mais de 5 sm'
    )
  ) %>% 
  na.omit() %>% # omitindo NAS
  group_by(cond_habitacao, faixa_renda) %>% # agrupando
  count() %>%
  pivot_wider( # pivoteando
    names_from = cond_habitacao, 
    values_from = n
  )

View(q4_1991_cond_habitacao)

# 2010
# para 2010 deve-se considerar o salario com base na modera real
df2010AM$INCTOT <- ifelse(df2010AM$INCTOT == 9999999, NA, df2010AM$INCTOT) #9999999 = NIU (not in universe)
df2010AM$INCTOT <- ifelse(df2010AM$INCTOT == 9999998, NA, df2010AM$INCTOT) #9999998 = Unknown/missing

# edattain e inctot
q4_2010_nivel_escolar <- df2010AM %>% 
  select(EDATTAIN, INCTOT) %>% 
  filter(EDATTAIN %in% c(1,2,3,4)) %>%
  mutate(
    nivel_escolar = case_when(
      EDATTAIN == 1 ~ '1. primario incompleto', #NOMEANDO EDATTAIN
      EDATTAIN == 2 ~ '2. primario completo',
      EDATTAIN == 3 ~ '3. secundario completo',
      EDATTAIN == 4 ~ '4. superior completo',
    ), 
    faixa_renda = case_when( # em reais
      INCTOT < 510 ~ '1. menos de 1 sm', # construindo as categorias de renda
      INCTOT >= 510 & INCTOT <= 1020 ~ '2. de 1 a 2 sm',
      INCTOT > 1020 & INCTOT <= 2550 ~ '3. de 2 a 5 sm',
      INCTOT > 2550 ~ '4. mais de 5 sm'
    )
  ) %>% 
  na.omit() %>% # omittindo NAS
  group_by(nivel_escolar, faixa_renda) %>% 
  count() %>% 
  pivot_wider( # pivoteando
    names_from = nivel_escolar, 
    values_from = n
  )

# ownership e inctot 
q4_2010_cond_habitacao <- df2010AM %>% 
  select(OWNERSHIP, INCTOT) %>% 
  filter(OWNERSHIP %in% c(1,2)) %>% 
  mutate(
    cond_habitacao = case_when(
      OWNERSHIP == 1 ~ 'propria',
      OWNERSHIP == 2 ~ 'nao propria',
    ), 
    faixa_renda = case_when(
      INCTOT < 510 ~ '1. menos de 1 sm',
      INCTOT >= 510 & INCTOT <= 1020 ~ '2. de 1 a 2 sm',
      INCTOT > 1020 & INCTOT <= 2550 ~ '3. de 2 a 5 sm',
      INCTOT > 2550 ~ '4. mais de 5 sm'
    )
  ) %>% 
  na.omit() %>%
  group_by(cond_habitacao, faixa_renda) %>% 
  count() %>%
  pivot_wider(
    names_from = cond_habitacao, 
    values_from = n
  )

View(q4_2010_cond_habitacao)

# questao 5
# 1991

q5_1991AM <- df1991AM %>%
  mutate(
        PIA = case_when(
          AGE >= 15 & AGE <= 64 ~ 'sim', # sim, quem esta entre 15 a 64 (PIA)
          TRUE ~ 'nao'
        ),
        PEA = case_when(
          (LABFORCE == 2) & (AGE >= 15 & AGE <= 64) ~ 'sim', # sim, quem possui LABFORCE = 2 e esta entre 15 A 64 (PEA)
          TRUE ~ 'nao'
  )
        ) %>% 
  filter(PIA == 'sim') %>% # filtrando pela pia
  group_by(PIA, PEA) %>% # agrupando
  count() %>%
  pivot_wider(names_from = PEA, values_from = n) %>% # pivoteando
  mutate(pct_PEA = sim/(sim + nao)*100) # pct pea

#

# 2010
q5_2010AM <- df2010AM %>%
  mutate(
    PIA = case_when(
      AGE >= 15 & AGE <= 64 ~ 'sim', 
      TRUE ~ 'nao'
    ),
    PEA = case_when(
      (LABFORCE == 2) & (AGE >= 15 & AGE <= 64) ~ 'sim', 
      TRUE ~ 'nao'
    )
  ) %>% 
  filter(PIA == 'sim') %>%
  group_by(PIA, PEA) %>% 
  count() %>%
  pivot_wider(names_from = PEA, values_from = n) %>%
  mutate(pct_PEA = sim/(sim + nao)*100)

###############FIM###############

