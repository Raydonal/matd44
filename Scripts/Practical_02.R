# Amostragem por conglomerados

library(survey)


data(api)

help("api")

# O Índice de Desempenho Acadêmico é calculado para todas as 
# escolas da Califórnia com base em testes padronizados de alunos. 
# Os conjuntos de dados contêm informações para todas as escolas com 
# pelo menos 100 alunos e para várias amostras probabilísticas dos dados.

# Os dados populacionais completos no apipop são um quadro de dados com 6.194 
# observações sobre as 37 variáveis a seguir.
# 
# CDs
# Identificador único
# 
# tipo
# Ensino fundamental/médio/superior
# 
# nome
# Nome da escola (15 caracteres)
# 
# nome
# Nome da escola (40 caracteres)
# 
# escória
# Número da escola
# 
# nome
# Nome do distrito
# 
# dnum
# Número do distrito
# 
# nome
# Nome do condado
# 
# cnum
# Número do condado
# 
# bandeira
# motivo para falta de dados
# 
# pcttest
# porcentagem de alunos testados
# 
# API00
# API em 2000
# 
# API99
# API em 1999
# 
# alvo
# alvo para mudança na API
# 
# crescimento
# Mudança na API
# 
# sch.largo
# Atingiu a meta de crescimento em toda a escola?
#   
#   comp.imp
# Atingiu a meta de melhoria comparável
# 
# ambos
# Atingiu ambas as metas
# 
# prêmios
# Elegível para programa de prêmios
# 
# refeições
# Percentagem de estudantes elegíveis para refeições subsidiadas
# 
# ei
# ‘Aprendentes da Língua Inglesa’ (porcentagem)
# 
# ano.rnd
# Escola o ano todo
# 
# mobilidade
# percentagem de alunos para os quais este é o primeiro ano na escola
# 
# acs.k3
# tamanho médio das turmas anos K-3
# 
# acs.46
# tamanho médio das turmas anos 4-6
# 
# acs.core
# Número de cursos acadêmicos básicos
# 
# pct.resp
# percentagem onde o nível de escolaridade dos pais é conhecido
# 
# não.hsg
# porcentagem de pais que não concluíram o ensino médio
# 
# hsg
# porcentagem de pais com ensino médio completo
# 
# algum.col
# porcentagem de pais com alguma faculdade
# 
# col.grad
# porcentagem de pais com diploma universitário
# 
# grad.sch
# porcentagem de pais com pós-graduação
# 
# média.ed
# nível médio de escolaridade dos pais
# 
# completo
# porcentagem de professores totalmente qualificados
# 
# emer
# percentagem de professores com qualificações de emergência
# 
# matricular
# número de alunos matriculados
# 
# API.stu
# número de alunos testados.


head(apipop, 6) # informaçcÕes de nosso conjunto de dados 6 primeiras observações

summary(apipop)
# NOte que há dados faltantes

# Usaremos a variáel distritos escolares (dnum) como sendo a informação do conglomerada

K = dim(apipop)[1]

# Número do distrito
str(apipop$dnum)

# número total de gonglomerados
N = length(unique(apipop$dnum))


# distribuição dos conglomerados
plot(table(apipop$dnum))

# suponhamos que queremos realziar uma amostragem 
# por conglomerados em um estágio de 15 distritos
# escolares

# A obtenção da amostra não é tão direta mas podemos usar
# o pacote sampling

# Pacote de amostragem -  Yves Tillé
library(sampling)

set.seed(31416)

# número de distritos
n = 15

aux1 = sampling::cluster(apipop, # base de dados
                        clustername = c("dnum"), # variável de cluster 
                        n, # tamaho de amostra
                        method = c("srswor"), # método de seleção (srswor = aleatório 
                        #simples sem substituição)
                        description = TRUE # a mensagem fornece o número de clusters 
                        #selecionados, o número de unidades na população e o número 
                        # de unidades selecionadas.
                        )


str(aux1)

# Prob = 15/757 (n_I/N_I)

# Amostra de conglomerados

samplec1 = getdata(apipop, aux1)

# Atributos da base de dados
attributes(samplec1)

L = dim(aux1)[1] # número de unidades selecionados


# O plano amostral agora pode ser definino de forma completa uma vez que 
# temos a variável de cluster

dclus1 <- svydesign(ids = ~dnum, fpc = rep(N,L), data = samplec1)


summary(dclus1)

# Avaliemos novamente o número total de matriculados e a media 
# do escore api para o ano 2000

# Total
svytotal(~enroll, dclus1)

# media
svytotal(~api00, dclus1)

# Note pelos resultados que este plano amostral resulta ser menos preciso
# que AAS ou AASE.



# Consideremos agora uma amostragem en dois estágios por conglomerados 
# em que são selecionadas 40 unidade primarias de amostragem (distritos esolares) 
# e 5 unidades secundarias de amostragem (colegios) por distrito.

set.seed(123456)

Pop = apipop

str(Pop[,6]) # pegar os distritos

# pegar unicamente os registro unicos 
aux0 = aggregate(Pop[,6], by=list(Pop$dnum), function(x)x[1])

# Pegar os registros unicos de numero de colegios por distrito
aux1 = aggregate(Pop[,7], by=list(Pop$dnum), length)

# Base de dados dos distritos
Popd = cbind(aux0, aux1)
names(Popd)[c(2,4)] = c("dname", "Ncdis")

# merge das duas bases de dados mas agora identificando os distritos para 
# a avaliação dos estágios
Pop = merge(Pop, Popd[,c(2,4)], by = c("dname"))

m1 <- sampling::cluster(Pop, clustername = c("dnum"), 
                        size = 40, method= "srswor" )
summary(m1)

m1 <- getdata(Pop,m1) # Amostra do primeiro estágio (distritos 
# como unidades primarias)

# Agora vanos fazer de forma recursiva a seleção das unidades secundarias
# do segundo estágio (colegios)

t=as.numeric(sapply(table(m1$dnum), function(x) min(5,x)  ))

m2 = NULL

for(i in 1:40){
  mx = m1[m1$dnum==unique(m1$dnum)[i],]
  mx$Prob1 = mx$Prob
  m <- sampling::cluster(mx, clustername = c("snum"),
                         size=t[i], method = "srswor")
  m <- getdata(mx,m)
  m2 = rbind(m2,m)
}

str(m2)

m2$w = 1/(m2$Prob1*m2$Prob) # pesos da amostragem

m2$fpc1 = fpc = rep(N, dim(m2)[1]) # fator de correção

# amostragem em dois estagios

dclus2 <- svydesign(ids=~dnum+snum, fpc=~fpc1+Ncdis, data=m2)

summary(dclus2)

# Vejamos como fica a estimação do total de matriculados e a média do indice

svytotal(~enroll, dclus2, na.rm=TRUE)

svymean(~api00, dclus2)






