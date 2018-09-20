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

set.seed(1)

d <- data.frame(a = exp(rnorm(100, 2, 5)),
                b = exp(rnorm(100, 2.5, 15)),
                c = exp(rnorm(100, 2.5, 15)))

mvn(d)

d %>% 
  mutate_all(log) %>% 
  mvn()

x1 <- rchisq(1000, 10) 		
x2 <- rchisq(1000, 20)
x3 <- rchisq(1000, 5)

base <- as_tibble(cbind(x1, x2, x3))

mvn(base, mvnTest = "royston")

base %>% 
  mutate_all(sqrt) %>% 
  mvn(mvnTest = "royston")


qqnorm(x)			# Normal probability plot for original variable
qqline(x, col = "blue")

MASS::boxcox(x~1)

p <- powerTransform(x)    # Estimaton of Box-Cox lambda

y <- bcPower(x,p$lambda)	
qqnorm(y)			# Normal probability plot for original variable
qqline(y, col = "blue")
