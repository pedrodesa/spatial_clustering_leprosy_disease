# Data wrangling

# Pacotes utilizados
pacotes <- c(
  "tidyverse",
  "read.dbc",
  "lubridate"
  )


if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}


# 1.0 DADOS ----
# Foram utilizados dois bancos de dados do Sinan - Hanseníase
# Os dados foram baixados do site https://datasus.saude.gov.br/transferencia-de-arquivos/#
#df_hans2020 <- read.dbc("HANSBR20.dbc")
#df_hans2021 <- read.dbc("HANSBR21.dbc")
#df_hans2022 <- read.dbc("HANSBR22.dbc")
#df_hans2023 <- read.dbc("HANSBR23.dbc")


# Foi realizada a união dos bancos de dados
#dados_hans <- rbind(df_hans2020, df_hans2021)
#dados_hans <- rbind(dados_hans, df_hans2022)
#dados_hans <- rbind(dados_hans, df_hans2023)

#rm(df_hans2020, df_hans2021, df_hans2022, df_hans2023)

# Salvando os dados em formato RData
#save(dados_hans, file = "dados_hans.RData")


# Dados de hanseníase
load("dados_hans.RData")

glimpse(dados_hans)

dados_hans$ID_MN_RESI <- as.character(dados_hans$ID_MN_RESI)

dados_hans$MUNIRESAT <- as.character(dados_hans$MUNIRESAT)

# Dados da população 2021
bd_pop <- read.csv2(file = "populacao.csv", header = T, sep = ";", dec = ",")

glimpse(bd_pop)


bd_pop <- transform(
  bd_pop,
  COD = as.character(COD),                
  prop_populacao_com_renda_menosqmeio_SM = as.numeric(prop_populacao_com_renda_menosqmeio_SM),
  prop_populacao_pobreza_extrema = as.numeric(prop_populacao_pobreza_extrema),
  Renda_media_domic_per_capita = as.numeric(Renda_media_domic_per_capita),
  Taxa_de_analfabetismo = as.numeric(Taxa_de_analfabetismo),
  Taxa_de_desemprego_16a_emais = as.numeric(Taxa_de_desemprego_16a_emais),
  PIB_per_capita = as.numeric(PIB_per_capita),
  Saneamento_inadequado = as.numeric(Saneamento_inadequado),
  Razao_de_sexos = as.numeric(Razao_de_sexos),
  Taxa_urbanizacao = as.numeric(Taxa_urbanizacao),
  Gini = as.numeric(Gini)
)


# 2.0 TRATAMENTO DE DADOS ----

# Seleção das variáveis de hanseníase
dados_hans2 <- dados_hans %>% dplyr::select(
  DT_DIAG,
  DT_NOTIFIC,
  ID_MN_RESI,
  MODOENTR,
  ESQ_INI_N,
  MUNIRESAT,
  CLASSATUAL, 
  ESQ_ATU_N,
  TPALTA_N,
  FORMACLINI,
  MODODETECT,
  NU_IDADE_N,
  DT_NOTI_AT,
  AVALIA_N
)


# Criação da variável "ANO_DIAG"
dados_hans2$DT_DIAG <- as.Date(dados_hans2$DT_DIAG, format = "%d/%m/%Y")

dados_hans2[,"ANO_DIAG"] <- year(dados_hans2$DT_DIAG)

#df_hans <- filter(df_hans, ANO_DIAG == 2022)

# Criação da variável "ANO_NOTI
dados_hans2$DT_NOTIFIC <- as.Date(dados_hans2$DT_NOTIFIC, format = "%d/%m/%Y")

dados_hans2[,"ANO_NOTI"] <- year(dados_hans2$DT_NOTIFIC)

#df_hans <- filter(df_hans, ANO_NOTI == 2022)


# Criação da variável "ANO_NOTI_AT"
dados_hans2$DT_NOTI_AT <- as.Date(dados_hans2$DT_NOTI_AT, format = "%d/%m/%Y")

dados_hans2[,"ANO_NOTI_AT"] <- year(dados_hans2$DT_NOTI_AT)


# Exclusão dos erros de diagnóssticos
# Os erros de diagnósticos são classificados como 8
dados_hans2 %>% 
  group_by(TPALTA_N) %>% 
  summarize(n = n())

dados_hans2$TPALTA_N <- as.numeric(as.character(as.factor(dados_hans2$TPALTA_N)))

dados_hans2$TPALTA_N <- replace(x = dados_hans2$TPALTA_N, list = is.na(dados_hans2$TPALTA_N),
                       values = 0) # Recodificando o NA para 0 (registro ativo)

dados_hans2 <- filter(dados_hans2, TPALTA_N != 8) # Exclusão dos erros de diagnóstico


# 3.0 CÁLCULO DOS INDICADORES DE HANSENÍASE ----

# Taxa de prevalência
num_prev <- dados_hans2 %>% 
  filter(ANO_NOTI_AT %in% c(2019, 2021, 2022), TPALTA_N == 0) %>% 
  group_by(ID_MN_RESI) %>%
  summarize(prev = n()) %>% 
  ungroup() %>% 
  droplevels(.)

bd_pop <- full_join(bd_pop, num_prev, by = join_by("COD" == "ID_MN_RESI"))

bd_pop$taxa_prevalencia <- (bd_pop$prev / bd_pop$POP_GERAL) * 10000


# Taxa de detecção geral
num_cn <- dados_hans2 %>% 
  filter(MODOENTR == 1, ANO_DIAG == 2022) %>% 
  group_by(ID_MN_RESI) %>%
  summarize(caso_novo = n()) %>% 
  ungroup () %>% 
  droplevels(.)

bd_pop <- full_join(bd_pop, num_cn, by = join_by("COD" == "ID_MN_RESI"))

bd_pop$taxa_geral <- (bd_pop$caso_novo / bd_pop$POP_GERAL) * 100000


# Taxa de detecção em menores de 15 anos
num_menor <- dados_hans2 %>% 
  filter(MODOENTR == 1, ANO_DIAG == 2022, NU_IDADE_N < 4015) %>% 
  group_by(ID_MN_RESI) %>%
  summarize(caso_menor15 = n()) %>% 
  ungroup () %>% 
  droplevels(.)

bd_pop <- full_join(bd_pop, num_menor, by = join_by("COD" == "ID_MN_RESI"))

bd_pop$taxa_menor15 <- (bd_pop$caso_menor15 / bd_pop$POP_MENOR) * 100000


# Casos novos GIF avaliado
num_aval <- dados_hans2 %>% 
  filter(MODOENTR == 1, ANO_DIAG == 2022, AVALIA_N != 3) %>% 
  group_by(ID_MN_RESI) %>%
  summarize(aval = n()) %>% 
  ungroup () %>% 
  droplevels(.)

bd_pop <- full_join(bd_pop, num_aval, by = join_by("COD" == "ID_MN_RESI"))

bd_pop$prop_gifavaliado <- (bd_pop$aval / bd_pop$caso_novo) * 100


# Casos novos com GIF 2
num_gif2 <- dados_hans2 %>% 
  filter(MODOENTR == 1, ANO_DIAG == 2022, AVALIA_N == 2) %>% 
  group_by(ID_MN_RESI) %>%
  summarize(gif2 = n()) %>% 
  ungroup () %>% 
  droplevels(.)

bd_pop <- full_join(bd_pop, num_gif2, by = join_by("COD" == "ID_MN_RESI"))

bd_pop$prop_gif2 <- (bd_pop$gif2 / bd_pop$aval) * 100

# Proporção de cura nos anos das coortes

# Cura
cura_pb <- dados_hans2 %>%
  filter(MODOENTR == 1, ANO_DIAG == 2021, TPALTA_N == 1, CLASSATUAL == 1, ESQ_ATU_N == 1) %>% 
  group_by(MUNIRESAT) %>%
  summarize(cura_pb = n()) %>% 
  ungroup () %>% 
  droplevels(.)


cura_mb <- dados_hans2 %>% 
  filter(MODOENTR == 1, ANO_DIAG == 2020, TPALTA_N == 1, CLASSATUAL == 2, ESQ_ATU_N == 2) %>% 
  group_by(MUNIRESAT) %>%
  summarize(cura_mb = n()) %>% 
  ungroup () %>% 
  droplevels(.)

bd_pop <- full_join(bd_pop, cura_pb, by = join_by("COD" == "MUNIRESAT"))

bd_pop <- full_join(bd_pop, cura_mb, by = join_by("COD" == "MUNIRESAT"))

bd_pop <- replace(x = bd_pop, list = is.na(bd_pop), values = 0) # Recodificando o NA para 0

bd_pop <- bd_pop %>% mutate(cura_coortes = cura_pb + cura_mb)


# Tipo de saída
saida_pb <- dados_hans2 %>% 
  filter(MODOENTR == 1, ANO_DIAG == 2021, CLASSATUAL == 1, ESQ_ATU_N == 1) %>% 
  group_by(MUNIRESAT) %>%
  summarize(saida_pb = n()) %>%
  ungroup () %>% 
  droplevels(.)

saida_mb <- dados_hans2 %>% 
  filter(MODOENTR == 1, ANO_DIAG == 2020, CLASSATUAL == 2, ESQ_ATU_N == 2) %>% 
  group_by(MUNIRESAT) %>%
  summarize(saida_mb = n()) %>% 
  ungroup () %>% 
  droplevels(.)

bd_pop <- full_join(bd_pop, saida_pb, by = join_by("COD" == "MUNIRESAT"))

bd_pop <- full_join(bd_pop, saida_mb, by = join_by("COD" == "MUNIRESAT"))

bd_pop <- replace(x = bd_pop, list = is.na(bd_pop), values = 0) # Recodificando o NA para 0

bd_pop <- bd_pop %>% mutate(saida_coortes = saida_pb + saida_mb)

bd_pop$prop_cura <- (bd_pop$cura_coortes / bd_pop$saida_coortes) * 100


# INSERIR INDICADORES OPERACIONAIS DA APS ----
glimpse(bd_pop)

dados_hans3 <- bd_pop %>% dplyr::select(COD,
                                        MUNI,
                                        prop_cura,
                                        taxa_prevalencia,
                                        taxa_geral,
                                        caso_novo,
                                        POP_GERAL,
                                        taxa_menor15,
                                        prop_gifavaliado,
                                        prop_gif2,
                                        prop_populacao_com_renda_menosqmeio_SM,
                                        prop_populacao_pobreza_extrema,
                                        Renda_media_domic_per_capita,
                                        Taxa_de_analfabetismo,
                                        Taxa_de_desemprego_16a_emais,
                                        PIB_per_capita,
                                        Saneamento_inadequado,
                                        Razao_de_sexos,
                                        Taxa_urbanizacao,
                                        Gini
                                        )

# Dados da APS
dados_APS <- read.csv2(file = "CoberturaABDez2020.csv", header = T, sep = ";", dec = ",")

dados_APS$CoberturaESF <- gsub('%','', dados_APS$CoberturaESF)
dados_APS$CoberturaAB <- gsub('%','', dados_APS$CoberturaAB)

dados_APS2 <- dados_APS %>% dplyr::select(IBGE, CoberturaESF, CoberturaAB)

dados_APS2$IBGE <- as.character(dados_APS2$IBGE)

# Join
dados_analise <- full_join(dados_hans3, dados_APS2, by = join_by("COD" == "IBGE"))

dados_analise <- dados_analise[-c(5571:5579),]

glimpse(dados_analise)

dados_analise$CoberturaESF <- as.numeric(dados_analise$CoberturaESF)
dados_analise$CoberturaAB <- as.numeric(dados_analise$CoberturaAB)

dados_analise <- replace(x = dados_analise, list = is.na(dados_analise), values = 0)


# Salvando o dataframe de análise
# save(dados_analise, file = 'dados_analise.RData')
