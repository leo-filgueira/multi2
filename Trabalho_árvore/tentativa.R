require(dplyr)
require(rpart)
require(rpart.plot)

# Leitura das bases
bens_candidatos <- data.table::fread("Trabalho_árvore/bem_candidato_2018/bem_candidato_2018_RJ.csv", dec = ",")
consulta_candidatos <- data.table::fread("Trabalho_árvore/consulta_cand_2018/consulta_cand_2018_RJ.csv", dec = ",")
cassacao <- data.table::fread("Trabalho_árvore/motivo_cassacao_2018/motivo_cassacao_2018_RJ.csv", dec = ",")

# Sumarizando bens
bens_candidatos <- bens_candidatos %>% 
  group_by(SQ_CANDIDATO) %>% 
  summarise(valor_bens = sum(VR_BEM_CANDIDATO))

# Juntando info dos candidatos, bens e info de cassação
candidatos <- consulta_candidatos %>% 
  select(SQ_CANDIDATO, NM_CANDIDATO, DS_GENERO, DS_GRAU_INSTRUCAO,
         DS_ESTADO_CIVIL, DS_COR_RACA, DS_OCUPACAO, NR_IDADE_DATA_POSSE,
         ST_REELEICAO) %>% 
  as_tibble() %>% 
  left_join(bens_candidatos, by = "SQ_CANDIDATO") %>% 
  left_join(
    cassacao %>% 
      distinct(SQ_CANDIDATO) %>% 
      mutate(cassado = 1),
    by = "SQ_CANDIDATO")

# Transnformano cassação em fator e dando labels
candidatos <- candidatos %>% 
  mutate(cassado = case_when(
    is.na(cassado) ~ 0,
    T ~ cassado
  ) %>% 
    factor(labels = c("n_cassado", "cassado"))
  )

candidatos
tentativa <- candidatos[, -(1:2)]

modelo <- rpart(cassado ~ DS_GENERO, 
                data = tentativa, method = "class")
rpart.plot(x = modelo, yesno = 2,
           type = 0, extra = 2)

glimpse(ptitanic)
glimpse(candidatos)
