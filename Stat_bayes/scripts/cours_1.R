library(ggplot2)
library(LearnBayes)
library(bayesrules)
library(tidyverse)

# Exercice 1

stat = data.frame(data = c(rbeta(1000,1,1), rbeta(1000,16,26)),
                  stat = c(rep("prior",10000), rep("posterior",10000)))

ggplot(stat, aes(x = data, colour = stat))+geom_density()
posterior<- stat %>% 
  filter(stat == "posterior")

## A finir

# Exercice 2

p <- seq(0, 1, by = 0.1)
prior <- (c(0.01,0.20,0.25,0.25,0.16,0.08,0.03,0.01,0.01,0,0))
data <- c(11,26)
post <- pdisc(p,prior,data)
cbind(p,prior,round(post,3))
plot(p,prior,"h")
plot(p,post,"h")

# Exercice 3


# Modèle Gamma-Poisson

# 0.95 de chances que ce nombre soit dans l'intervalle [1,8]

prior <- gamma()