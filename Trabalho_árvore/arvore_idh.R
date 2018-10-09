require(readxl)
require(dplyr)
require(magrittr)

dados <- read_xlsx("Trabalho_árvore/AtlasBrasil_Consulta (1).xlsx")
regiao <- read_xlsx("Trabalho_árvore/ordem_mun.xlsx")

dados %<>%
  select(1:2, idhm = `IDHM 2010`, everything()) %>% 
  mutate(idhm = case_when(
    idhm > 0.7 ~ "alto",
    idhm <= 0.7 & idhm > 0.6 ~ "médio",
    T ~ "baixo"
  ))

dados %<>%
  inner_join(regiao, by = c("Espacialidades" = "Cidade"))
  
# names(dados)[4:length(names(dados))] <- c("gini", "empr_cateira",
#                                           "dom_banheiro_agua", "esp_vida",
#                                           "sem_energia", "populacao",
#                                           "mort_infantil", "anos_estudo",
#                                           "renda_per_capita", "pobres")

dados2 <- dados %>%
  select(-(1:2)) #%>%
  # mutate(regiao = case_when(
  #   Mesorregiao %in% c("Norte Fluminense", "Noroeste Fluminense") ~ "Norte/Noroeste" ,
  #   T ~ Mesorregiao
  # ))

# teste2 <- teste %>% 
#   filter(Mesorregiao == "Metropolitana")

# Modelo com a base toda
modelo <- rpart::rpart(idhm ~ Mesorregiao, data = dados2, method = "class")
rpart.plot::rpart.plot(modelo, yesno = 2,
                       type = 0, extra = 2)

# Dividindo base em treino e teste
index <- sample(1:nrow(dados2), 0.8*nrow(dados2))
dados_treino <- dados2[index, ]
dados_teste <- dados2[-index, ]

# Definindo modelos

modelo1 <- rpart::rpart(idhm ~ ., data = dados_treino, method = "class")
rpart.plot::rpart.plot(x = modelo1, yesno = 2,
           type = 0, extra = 2)
rattle::fancyRpartPlot(modelo1)

modelo2 <- rpart::rpart(idhm ~ Mesorregiao, data = dados_treino, 
                        method = "class")
rpart.plot::rpart.plot(x = modelo2, yesno = 2,
                       type = 0, extra = 2)
rattle::fancyRpartPlot(modelo2)

modelo3 <- rpart::rpart(idhm ~ `Renda per capita 2010`,
                        data = dados_treino, method = "class")
rpart.plot::rpart.plot(x = modelo3, yesno = 2,
                       type = 0, extra = 2)
rattle::fancyRpartPlot(modelo3)

modelo4 <- rpart::rpart(idhm ~ `% de empregados com carteira - 18 anos ou mais 2010` + Mesorregiao + `Rendimento médio dos ocupados - 18 anos ou mais 2010`,
                        data = dados_treino, method = "class")
rpart.plot::rpart.plot(x = modelo4, yesno = 2,
                       type = 0, extra = 2)
rattle::fancyRpartPlot(modelo4)

# Previsões

prev1 <- predict(modelo1, dados_teste, type = "class")
prev2 <- predict(modelo2, dados_teste, type = "class")
prev3 <- predict(modelo3, dados_teste, type = "class")
prev4 <- predict(modelo4, dados_teste, type = "class")

# Calculando medidas de acurácia
mat_conf1 <- caret::confusionMatrix(data = prev1, reference = as.factor(dados_teste$idhm))
mat_conf2 <- caret::confusionMatrix(data = prev2, reference = as.factor(dados_teste$idhm))
mat_conf3 <- caret::confusionMatrix(data = prev3, reference = as.factor(dados_teste$idhm))
mat_conf4 <- caret::confusionMatrix(data = prev4, reference = as.factor(dados_teste$idhm))

# Matriz de confusão
mat_conf1$table
mat_conf2$table
mat_conf3$table
mat_conf4$table

rbind(mat_conf1$overall, mat_conf2$overall, 
      mat_conf3$overall, mat_conf4$overall) %>%
  cbind(rbind(mat_conf1$byClass, mat_conf2$byClass, 
        mat_conf3$byClass, mat_conf4$byClass)) %>% 
  as_data_frame() %>% 
  mutate(Modelo = paste("Modelo", 1:4)) %>% 
  select(Modelo, -Kappa, -starts_with("Accuracy"), Accuracy, Sensitivity, 
         Specificity) %>% 
  arrange_at(vars(-Modelo), desc)

