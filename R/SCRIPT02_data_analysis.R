###############################################
#   ANÁLISE REGRESSÃO + CLUSTER   #
###############################################

#
## PACOTES ----
pacotes <- c(
  "tidyverse",
  "correlation",
  "see",
  "ggraph",
  "PerformanceAnalytics",
  "ggcorrplot",
  "corrplot",
  "RColorBrewer",
  "psych",
  "MASS",
  "lmtest",
  "rpart",
  "rpart.plot",
  "purrr",
  "plotROC",
  "gridExtra",
  "randomForest",
  "caret",
  "pscl",
  "jtools",
  "stats",
  "overdisp",
  "mvnormtest",
  "cluster",      #função 'agnes' para elaboração de clusters hierárquicos
  "factoextra",   #função 'fviz_dend' para construção de dendrogramas
  "ade4",         #função 'ade4' para matriz de distâncias em var. binárias
  "rgdal",
  "tmap",
  "tmaptools",
  "RColorBrewer",
  "fitdistrplus"
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


#
## 1.0 DADOS ----
load("dados_analise.RData")

# multiplicação da variável Gini
dados_analise <- dados_analise %>% mutate(Gini = Gini * 100)

#
## 2.0 ANÁLISE EXPLORATÓRIA ----
glimpse(dados_analise)

dados_analise <- dados_analise %>% 
  dplyr::select(COD,
                MUNI,
                taxa_geral,
                everything(),
                -prop_cura,
                -taxa_prevalencia)


summary(dados_analise)

map(dados_analise[3:18], sd)

# Distribuição dos dados
dados_analise %>% 
  ggplot() +
  geom_histogram(aes(x = taxa_geral),
                 fill = "deepskyblue4",
                 color = "white") +
  labs(x = "Taxa de detecção de casos novos",
       y = "Frequência absoluta") +
  theme_bw()


dados_analise %>% 
  ggplot() +
  geom_histogram(aes(x = caso_novo),
                 fill = "deepskyblue4",
                 color = "white") +
  labs(x = "Casos novos",
       y = "Frequência absoluta") +
  theme_bw()


# Gráfico de Cullen e Frey
descdist(dados_analise$taxa_geral, discrete = FALSE, boot = 500)

# Matriz de correlação
chart.Correlation(dados_analise[3:18], histogram = TRUE, method = "spearman")

# Teste de correlação de Spearman
df_correl <- dados_analise %>% dplyr::select(-COD, -MUNI)

lapply(df_correl[-1], function(x) cor.test(df_correl[, 3], x, method = 'spearman', exact = F))

# Gráfico para o artigo
corrplot(cor(dados_analise2), 
         type="upper", 
         order="hclust",
         tl.col = "black", 
         tl.srt = 45,
         col = brewer.pal(n = 5, name = "RdBu"))


# MODELOS SUPERVISIONADOS ----

# Dataframe sem as variáveis categóricas
dados_analise2 <- dados_analise %>% dplyr::select(-COD, 
                                                  -MUNI)

#Teste de superdispersão no dataset
overdisp(x = dados_analise2,
         dependent.position = 1,
         predictor.position = 2:16)
# O teste sugere existência de superdispersão nos dados


# Modelo de regressão log-linear binomial negativo

# Modelo bruto
modelos_bivar <- lapply(dados_analise2[2:16], function(x) {
  
  lista_modelos <- glm.nb(log(taxa_geral + 1) ~ x, 
                          data = dados_analise2)
  
  output_modelos <- summary(lista_modelos)
  
  return(output_modelos)
}
)

print(modelos_bivar)


# Modelo bruto - Razão de taxas de incidência
# Razão de Taxas de Incidência (RTI):
# RTI = taxa de incidência no grupo exposto / taxa de incidência no grupo não exposto
# RTI = exp(coeficientes) - 1 em modelos de regressão log-linear binomial negativo com variáveis explicativas contínuas
modelos_bivar_rti <- lapply(dados_analise2[2:16], function(x) {
  
  lista_modelos <- glm.nb(log(taxa_geral + 1) ~ x, data = dados_analise2)
  
  output_modelos <- (exp(coef(lista_modelos)) - 1) * 100
  
  return(output_modelos)
}
)

print(modelos_bivar_rti)


# Intervalos de confiança (95%)
modelos_bivar_ic <- lapply(dados_analise2[2:16], function(x) {
  
  lista_modelos <- glm.nb(log(taxa_geral + 1) ~ x, data = dados_analise2)
  
  output_modelos <- (exp(confint(lista_modelos)) - 1) * 100
  
  return(output_modelos)
}
)

print(modelos_bivar_ic)


# Modelo ajustado - Análise múltipla
dados_analise3 <- dados_analise2 %>% dplyr::select(-prop_populacao_pobreza_extrema,
                                                   -Renda_media_domic_per_capita,
                                                   -Taxa_de_analfabetismo,
                                                   -PIB_per_capita,
                                                   -Saneamento_inadequado,
                                                   -Taxa_urbanizacao,
                                                   -CoberturaAB)

glimpse(dados_analise3)

nb_model <- glm.nb(formula = log(taxa_geral + 1) ~ Gini +
                     Taxa_de_desemprego_16a_emais +
                     taxa_menor15 +
                     prop_gif2,
                   data = dados_analise3)


summary(nb_model)

RTI <- as.data.frame(testando = (exp(coef(nb_model)) - 1) * 100)
teste2 <- as.data.frame((exp(confint(nb_model)) - 1) * 100)
teste3 <- cbind(teste, teste2)

#Parâmetro de forma da distribuição binomial negativa
1 / nb_model$theta #phi
nb_model$theta

#Estatística z de Wald do parâmetro theta para verificação da
#significância estatística
nb_model$theta / nb_model$SE.theta  #maior que 1.96


#Extração do valor de Log-Likelihood (LL)
logLik(nb_model)


#Parâmetros do modelo_bneg
summ(nb_model, digits = 4, confint = T, ci.width = 0.95)



#
## ANÁLISE DE CLUSTER ----

# Teste de esfericidade dos dados
bartlett.test(dados_analise2)

# Esquema de aglomeração hierárquico ----

# Data.Frame
dados_analise4 <- dados_analise3 %>% dplyr::select(-taxa_geral,
                                                   -prop_gifavaliado,
                                                   -prop_populacao_com_renda_menosqmeio_SM,
                                                   -Razao_de_sexos,
                                                   -CoberturaESF)


glimpse(dados_analise4)


# Matriz de dissimilaridades
matriz_D <- dados_analise4 %>% 
  dist(method = "manhattan")


# 1º Teste: Elaboração da clusterização hierárquica como "complete linkage"
cluster_hier_complete <- agnes(x = matriz_D, method = "complete")

# Construção do dendrograma "single linkage"
dev.off()
fviz_dend(x = cluster_hier_complete, show_labels = F)


fviz_dend(x = cluster_hier_complete,
          k = 9,
          k_colors = c("deeppink4", "darkviolet", "deeppink", "cornflowerblue", "salmon", "seagreen", "red", "blue"),
          color_labels_by_k = F,
          rect = T,
          rect_fill = T,
          lwd = 1,
          ggtheme = theme_bw())

coeficientes <- sort(cluster_hier_complete$height, decreasing = FALSE)
esquema <- as.data.frame(cbind(cluster_hier_complete$merge, coeficientes))
names(esquema) <- c("Cluster1", "Cluster2", "Coeficientes")
esquema

## Portanto, vamos gerar uma variável indicando 7 clusters

dados_analise$cluster_H <- factor(cutree(tree = cluster_hier_complete, k = 9))


# A seguir, vamos verificar se todas as variáveis ajudam na formação dos grupos

summary(anova_child_mort <- aov(formula = taxa_menor15 ~ cluster_H,
                                data = dados_analise))

summary(anova_exports <- aov(formula = prop_gif2 ~ cluster_H,
                             data = dados_analise))

summary(anova_health <- aov(formula = Gini ~ cluster_H,
                            data = dados_analise))

summary(anova_imports <- aov(formula = Taxa_de_desemprego_16a_emais ~ cluster_H,
                             data = dados_analise))


# Análise descritiva
table(dados_analise$cluster_H)

media <- group_by(dados_analise, cluster_H) %>%
  summarise(taxa_menor15 = mean(taxa_menor15, na.rm = TRUE),
            prop_gif2 = mean(prop_gif2, na.rm = TRUE),
            desemprego = mean(Taxa_de_desemprego_16a_emais, na.rm = TRUE),
            Gini = mean(Gini, na.rm = TRUE),
            Taxa_geral = mean(taxa_geral, na.rm = T))


t(media)


DP <- group_by(dados_analise, cluster_H) %>%
  summarise(taxa_menor15 = sd(taxa_menor15, na.rm = TRUE),
            prop_gif2 = sd(prop_gif2, na.rm = TRUE),
            desemprego = sd(Taxa_de_desemprego_16a_emais, na.rm = TRUE),
            Gini = sd(Gini, na.rm = TRUE),
            Taxa_geral = sd(taxa_geral, na.rm = T))

t(DP)


mediana <- group_by(dados_analise, cluster_H) %>%
  summarise(taxa_menor15 = median(taxa_menor15, na.rm = TRUE),
            prop_gif2 = median(prop_gif2, na.rm = TRUE),
            desemprego = median(Taxa_de_desemprego_16a_emais, na.rm = TRUE),
            Gini = median(Gini, na.rm = TRUE),
            Taxa_geral = median(taxa_geral, na.rm = TRUE))


t(mediana)

quantil <- group_by(dados_analise, cluster_H) %>%
  summarise(taxa_menor15 = quantile(taxa_menor15, c(0.25, 0.5, 0.75)),
            prop_gif2 = quantile(prop_gif2, c(0.25, 0.5, 0.75)),
            desemprego = quantile(Taxa_de_desemprego_16a_emais, c(0.25, 0.5, 0.75)),
            Gini = quantile(Gini, c(0.25, 0.5, 0.75)),
            Taxa_geral = quantile(taxa_geral, c(0.25, 0.5, 0.75)), q = c(0.25, 0.5, 0.75))


t(quantil)



# Definir os valores das amostras
amostra_cluster <- dados_analise %>% filter(cluster_H == 1)
amostra1 <- amostra_cluster$taxa_geral
amostra2 <- dados_analise$taxa_geral

# Calcular as médias e desvios padrão das amostras
x1 <- mean(amostra1)
x2 <- mean(amostra2)
s1 <- sd(amostra1)
s2 <- sd(amostra2)

# Definir o nível de significância
alpha <- 0.05

# Calcular o valor de t e o valor crítico
t <- (x1 - x2 - s2) / (s1 / sqrt(length(amostra1)))
t_critico <- qt(1 - alpha, df = length(amostra1) - 1)

# Testar a hipótese nula
if (t > t_critico) {
  mensagem <- "Rejeitamos a hipótese nula. A média da amostra 1 está acima de um desvio padrão da média da amostra 2."
} else {
  mensagem <- "Não rejeitamos a hipótese nula. Não há evidências suficientes para afirmar que a média da amostra 1 está acima de um desvio padrão da média da amostra 2."
}

# Imprimir a mensagem
cat(mensagem)



#
## Geoprocessamento

shp_br <- readOGR(dsn = "BR_Municipios_2022", layer = "BR_Municipios_2022")

glimpse(shp_br@data)

shp_br@data <- shp_br@data %>% mutate(COD6 = str_sub(shp_br@data$CD_MUN, 0, 6))


# Combinar o shapefile com o data.frame
shp_dados <- merge(x = shp_br,
                   y = dados_analise,
                   by.x = "COD6",
                   by.y = "COD")




# taxa de detecção geral

# criando uma coluna para agrupar os valores
shp_dados@data$grupo <- cut(shp_dados@data$taxa_geral, c(0.00, 2.00, 10.00, 20.00, 40.00, 747.20), include.lowest = T)

labels <- c("Baixo (<2,00.)", 
            "Médio (2,00 a 9,99)",
            "Alto (10,00 a 19,99)",
            "Muita alto (20,00 a 39,99)",
            "Hiperendêmico (≥40,00)")

tm_shape(shp = shp_dados) +
  tm_fill(col = "grupo",
          palette = "Reds",
          force.limits = TRUE,
          labels = labels,
          colorNA = "white",
          textNA = "Valores ausentes",
          title = "Endemicidade*") +
  tm_layout(frame = F,
            legend.position = c(0.05, 0.2)) +
  tm_borders(col = "black",
             alpha = 0.2) +
  tm_compass(type = "arrow",
             position = c("right", "bottom")) +
  tm_scale_bar(position = c("right", "bottom"))



# Criando uma variável numérica de cluster
shp_dados@data <- shp_dados@data %>% mutate(cluster_H_2 = cluster_H)
shp_dados@data$cluster_H_2 <- as.numeric(shp_dados@data$cluster_H_2)


# Gráfico
tm_shape(shp = shp_dados) + 
  tm_polygons(col = "cluster_H", 
          style = "cat", 
          palette = get_brewer_pal(palette = "Set1", n = 6, plot = FALSE)) +
  tm_layout(legend.text.size = 0.7,
            legend.title.size = 0.9,
            frame = F,
            legend.outside = TRUE) +
  tm_borders(col = "gray93", alpha = 0.3) +
  tm_compass(type = "arrow",
             position = c("left", "bottom")) +
  tm_scale_bar(position = c("left", "bottom"))


# Gráfico dos clusters 1 e 2
shp_dados@data <- mutate(shp_dados@data, 
                         cluster_1_2 = recode(cluster_H_2,
                                              '1' = '1',
                                              '2' = '2',
                                              '3' = '0',
                                              '4' = '0',
                                              '5' = '0',
                                              '6' = '0',
                                              '7' = '0',
                                              '8' = '0',
                                              '9' = '0'))

shp_dados@data$cluster_1_2 <- ifelse(shp_dados@data$cluster_1_2 == '1', '1',
                      ifelse(shp_dados@data$cluster_1_2 == '2', '2', NA))

shp_dados@data$cluster_1_2 <- as.numeric(shp_dados@data$cluster_1_2)

# Gráfico
mapa_1 <- tm_shape(shp = shp_dados) +
  tm_fill(col = "cluster_1_2",
          style = "cat",
          palette = "Set1",
          colorNA = "white",
          textNA = "Outros agrupamentos",
          title = "Agrupamentos") +
  tm_layout(main.title = "(A)",
            frame = F,
            legend.position = c(0.05, 0.2)) +
  tm_borders(col = "black",
             alpha = 0.2) +
  tm_compass(type = "arrow",
             position = c("right", "bottom")) +
  tm_scale_bar(position = c("right", "bottom"))


# Gráfico dos clusters 3, 4, 5, 6, 7
shp_dados@data <- mutate(shp_dados@data, 
                         cluster_345 = recode(cluster_H_2,
                                               '1' = '0',
                                               '2' = '0',
                                               '3' = '3',
                                               '4' = '4',
                                               '5' = '5',
                                               '6' = '0',
                                               '7' = '0',
                                               '8' = '0',
                                               '9' = '0'))

shp_dados@data$cluster_345 <- ifelse(shp_dados@data$cluster_345 == '3', '3',
                                     ifelse(shp_dados@data$cluster_345 == '4', '4',
                                            ifelse(shp_dados@data$cluster_345 == '5', '5',
                                            NA)))

shp_dados@data$cluster_345 <- as.numeric(shp_dados@data$cluster_345)

# Gráfico
mapa_2 <- tm_shape(shp = shp_dados) +
  tm_fill(col = "cluster_345",
          style = "cat",
          palette = "Set2",
          colorNA = "white",
          textNA = "Outros agrupamentos",
          title = "Agrupamentos") +
  tm_layout(main.title = "(B)",
            frame = F,
            legend.position = c(0.05, 0.2)) +
  tm_borders(col = "black",
             alpha = 0.2) +
  tm_compass(type = "arrow",
             position = c("right", "bottom")) +
  tm_scale_bar(position = c("right", "bottom"))



# Gráfico dos clusters 6, 7, 8, 9
shp_dados@data <- mutate(shp_dados@data, 
                         cluster_6789 = recode(cluster_H_2,
                                              '1' = '0',
                                              '2' = '0',
                                              '3' = '0',
                                              '4' = '0',
                                              '5' = '0',
                                              '6' = '6',
                                              '7' = '7',
                                              '8' = '8',
                                              '9' = '9'))

shp_dados@data$cluster_6789 <- ifelse(shp_dados@data$cluster_6789 == '6', '6',
                                     ifelse(shp_dados@data$cluster_6789 == '7', '7',
                                            ifelse(shp_dados@data$cluster_6789 == '8', '8',
                                                   ifelse(shp_dados@data$cluster_6789 == '9', '9',
                                                   NA))))

shp_dados@data$cluster_6789 <- as.numeric(shp_dados@data$cluster_6789)

# Gráfico
mapa_3 <- tm_shape(shp = shp_dados) +
  tm_fill(col = "cluster_6789",
          style = "cat",
          palette = "Set3",
          colorNA = "white",
          textNA = "Outros agrupamentos",
          title = "Agrupamentos") +
  tm_layout(main.title = "(C)",
            frame = F,
            legend.position = c(0.05, 0.2)) +
  tm_borders(col = "black",
             alpha = 0.2) +
  tm_compass(type = "arrow",
             position = c("right", "bottom")) +
  tm_scale_bar(position = c("right", "bottom"))





# Painel de mapas
tmap_arrange(mapa_1, mapa_2, mapa_3, nrow = 1)
