library(tidyverse)
library(xtable)

n = 56
media = 2000
desvio = 500

data = data.frame(
  ID = 1:n,
  Sexo = ifelse(rbinom(n, size = 1,prob = 0.7), "Mas", "Fem"),
  Renda = rnorm(n =n, mean = media, sd = desvio)
)


tabela <- xtable(data, caption = "Tabela de Dados", label = "ID:Renda")
print(tabela, include.rownames=FALSE)

