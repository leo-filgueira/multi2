require(dplyr)
require(MVN)

dados <- starwars %>% 
  select(height, mass) %>% 
  filter_all(all_vars(!is.na(.)))
mvn(as.data.frame(dados))

persp <- mvn(as.data.frame(dados), multivariatePlot = "persp")
cont <- mvn(as.data.frame(dados), multivariatePlot = "contour")

t1 <- dados %>% 
  mutate_all(log)

persp <- mvn(as.data.frame(t1), multivariatePlot = "persp")
cont <- mvn(as.data.frame(t1), multivariatePlot = "contour")
mvn(as.data.frame(t1))

t2 <- dados %>% 
  mutate_all(sqrt)

persp <- mvn(as.data.frame(t2), multivariatePlot = "persp")
cont <- mvn(as.data.frame(t2), multivariatePlot = "contour")
mvn(as.data.frame(t2))

d <- data.frame(a = exp(rnorm(100, 2, 5)),
                a = exp(rnorm(100, 2.5, 15)))

mvn(d)

d %>% 
  mutate_all(log) %>% 
  mvn(multivariatePlot = "persp")
