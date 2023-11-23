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
# NOte que há ddos faltantes

# Relizemos uma aamostragem AAS de escolas públicas de tamamnho 100


# paree efeito de reprodução
# fixe a semente

set.seed(31416)

N= dim(apipop)[1]

n = 1000

index.1 = sample(N,n)

sample.1 = apipop[index.1, ]

# para efeito de expansão criamos uma nova base de dados com 
# os tamanhos de amostra e as probabilidade de inclusão
# Aqui o N é conhecido

aux = data.frame(fpc = rep(N,n), pp = rep(n/N,n))
sample.1 = cbind(sample.1, aux)

head(sample.1,4)

# Agora vamos introduzir estes metadados no objeto adequada para
# o survey design 

# Especificar um plano amostral complexo- svydesign
diseAAS= svydesign(ids = ~1, fpc = ~fpc, data= sample.1)

# Argumentos:

# ids Fórmula ou quadro de dados que especifica IDs de 
# cluster do maior ao menor nível, ~0 ou ~1 é uma fórmula para nenhum cluster.

# fpc Correção de população finita: veja detalhes abaixo

# dados Quadro de dados para procurar variáveis nos argumentos 
# da fórmula, ou nome da  tabela do banco de dados, ou objeto 
# imputationList,


summary(diseAAS)



# Outro plano amostral supondo AAS com substituição

set.seed(12345)

sample.2 = apipop[sample(N,n, replace = TRUE),]
sample.2 = cbind(sample.2, aux)

# Especificar um plano amostral complexo- svydesign com substituição
diseAASc= svydesign(ids = ~1, probs = ~pp, data= sample.2)

summary(diseAASc)


# Definido o plano amostral podemos estaar agora interessados em estimador algum 
# parametro populacional 

# Estimar o total de alunos matriculados
# proporção por tipo de escola
# médias e difrencia de médias del api entre 1999 e 2000 usando os dois 
# anteriores planos amostrais

# Estimação do totol
svytotal(~enroll, diseAAS) 

# Note que há dados faltants produx NA

# Corrigir 
svytotal(~enroll, diseAAS, na.rm = TRUE)

# Estimação das médias
svymean(~stype, diseAAS)

# estimação das médias por ano
means.1 <- svymean(~api00+api99, diseAAS)

means.1 

# Estimaçãoda diferença (contraste)
svycontrast(means.1, c(api00=1, api99=-1))


# Agora vejamos que acontece com uma amostragem com susbtituição

# Estimação do totol
svytotal(~enroll, diseAASc) 

# Note que há dados faltants produx NA

# Corrigir 
svytotal(~enroll, diseAASc, na.rm = TRUE)

# Estimação das médias
svymean(~stype, diseAASc)

# estimação das médias por ano
means.1 <- svymean(~api00+api99, diseAASc)

means.1 

# Estimaçãoda diferença (contraste)
svycontrast(means.1, c(api00=1, api99=-1))




# Dados peniteniarios de Peru - censo nacional penitenciário de 2016

library(foreign)


cp16b <- read.spss(file.choose(), use.value.labels = TRUE)

# saveRDS(cp16b, file="dadosPenitenciariasPeru.Rds")

cp16 <- as.data.frame(cp16b)

cp16_labels <- attr(cp16b, "variable.labels")

cp16_cat <- attr(cp16b, "label.table")

save(cp16,file="cp16.RData")

head(cp16[,1:18])

# AMostragem AAS para estimar a idade media dos internos

# Calculando o tamanho de amostra para uma variavel binária 
# com uma margem de erro não maior a 0.03 e uma confiança de 95%


set.seed(31416)

N = dim(cp16)[1] # 76180 registros

numerador  = 1.96^2*0.5*(1-0.5)*76180
denominador = 1.96^2*0.5*(1-0.5)+0.03^2*76180
  
n = numerador/denominador

n = ceiling(n) # Tamanho de amostra

index = sample(N,n)

sample = cp16[index,]

diseAAS = svydesign(id=~1, fpc=rep(N,n), data=sample)

summary(diseAAS)


# Estimação da idade média

svymean(~EDAD, diseAAS, na.rm = TRUE)

# Idade media na populacão
mean(cp16$EDA, na.rm=TRUE)


# Proporção de internos sentenciados

svymean(~SITUACION_JURIDICA, diseAAS)

# Idade media na populacão
length(cp16$SITUACION_JURIDICA[cp16$SITUACION_JURIDICA=="Procesado"])/N

length(cp16$SITUACION_JURIDICA[cp16$SITUACION_JURIDICA=="Sentenciado"])/N


# Existe relação entre estado civil e tipo de delito


DGEN = as.factor(cp16$DEL_GENERICO_CD)

levels(DGEN)[c("01","02","03","04","05","07","08","09","11","14","16","17","18","19")] = "OUTROS"


DGEN = DGEN[index]

DGEN=factor(DGEN, levels(DGEN)[c(2,3,4,5,1)])

chisq.test(DGEN, sample$E_CIVIL)

# Não há evidencia de associação  entre o estados civil e o tipo de delito




# Voltando a amostragem estatificada com a base de dados api

# Estamos interessados numa amostra de tamanho 200 onde a variável de 
# estatificação será o tipo de colégio fixando n_E=100 escolas elementar,
# n_M = 50 escolas ensino médio e n_H = 50 escolas ensino superior

# interesse é estimar o número total de estudantes matriculadoss e as 
# medias dos indices api

data(api)

attach(apipop)

table(stype) # tipo de colegio

table(stype) / c(100,50,50)# tipo de colegio

set.seed(1357) # proporcoes por estratos

index=c(
  sample(which(stype=="E"),100), 
  sample(which(stype=="H"),50),
  sample(which(stype=="M"),50)
  )

sample1 = apipop[index,]

# vamos construir uma base de dados auxiliar contendo os pesos da amostragem 
# os tamanhois dos estratos  e e o fator de correção por finitude


aux = data.frame( pw = c(rep(44.21,100), rep(15.1, 50), rep(20.36,50)), 
                  fpc = c(rep(4421,100), rep(755,50), rep(1018,50)))

sampleMAE = cbind(sample1, aux)


# amostragem aleatoria simples estratificada

disAASE = svydesign(id=~1, strata=~stype, fp=~fpc, data=sampleMAE)

summary(disAASE)

# Estimação do total de matriculados
svytotal(~enroll, disAASE, na.rm=TRUE)

# media do indice api para 1999 e 2000
svymean(~api99+api00, disAASE)


# note que o o AASE reduz o erro padrão da estimação.


# Mostremos agora a estimação dos dominios e analisemos
# se sob AAS os colegios contam co professores com 
# qualificacões de emergencia ou não tem um pior 
# desempenho api em 2000 (algo parecido a fazer uma avalaição de impacto)


table(as.numeric(apipop$emer>0))

N= dim(apipop)[1]

table(as.numeric(apipop$emer>0))/N * 100

# ao redor de 80% dos colegios possui ao menos um pprofessor comqualificações 
# de emergencia o qual indica a dificuldade que tem as escolas para contratar 
# professores qualificados


# se desejamos estimro sobre o plano amostral a media del api em 2000 e seu erro
# padrão para o dominio de colegios que tem algum professor com qualificacoess de emergencia
# poderiamos fazer o seguinte

# Tamanho dos estratos
Nh = c(4421,755, 1018)

# tamanho de amostra dos estratos
nh = c(100, 50, 50 )

disAASE = update(disAASE, cemer=as.integer(emer>0),
                 apicemer=api00*cemer)

Nd_e = as.numeric(svytotal(~cemer, disAASE))

taud_e = as.numeric(svytotal(~apicemer, disAASE))

(Ybard_e = taud_e/Nd_e)

zh = (sampleMAE$api00-Ybard_e)*(sampleMAE$emer>0)

sigma2zh = as.vector(by(zh, sampleMAE$stype, sd, na.rm=TRUE))^2

sed_e = sqrt(sum(Nh^2*(1-nh/Nh)*sigma2zh/nh)/Nd_e^2)


# de uma form mais simples

discemer = subset(disAASE, cemer==1)

svymean(~api00, discemer)

# Ou de forma geral para os dominios em estudo 

mdom = svyby(~api00, ~cemer, disAASE, svymean)

mdom



# Próxima aula - amostragem por conglomerados