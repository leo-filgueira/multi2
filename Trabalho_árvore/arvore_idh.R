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

teste <- dados %>% 
  select(-(1:2))

modelo <- rpart::rpart(idhm ~ ., data = teste, method = "class")
rpart.plot::rpart.plot(x = modelo, yesno = 2,
           type = 0, extra = 2)
rattle::fancyRpartPlot(modelo)

modelo_lm <- lm(idhm ~., data = teste)
summary(modelo_lm)
