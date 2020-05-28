# Carregando pacotes a serem utilizados

library(readxl)
library(dplyr)
library(corrplot)
library(psych)


# Limpar----

rm(list=ls())


#Diretório

setwd("c:/Users/CAIO AZEVEDO/Documents/Documentos Caio/Github/Monografia/data")


dados<-read_xlsx("BANCO DE DADOS.xlsx")

dados<-dados %>% 
  select(Municipio,x1, x2, x5, x7, x8, x9, x13, x15, x16, x17, x18)

df<-as.matrix(dados[,2:12])
row.names(df)<-dados$Municipio


df<-scale(df)

# Matriz de correlação

matcor<-cor(df)

corrplot(matcor, method="circle")


# Teste de Bartlett

cortest.bartlett(df)

# KMO

KMO(df)


# Variância Explicada
fit<-princomp(df,cor=TRUE)
summary(fit)

# Screeplot

screeplot(fit)

plot(fit,type="lines")



# Rotação

PCAvarimax<-principal(df, nfactors=3,
                           n.obs=92,rotate="varimax",scores=TRUE)

PCAvarimax$loadings


# Diagrama

fa.diagram(PCAvarimax)



bartlett<-factor.scores(df,PCAvarimax, 
              method = c("Bartlett"))

prev<-data.frame(bartlett[["scores"]])


# Combinando os resultados

base<-cbind(dados,prev)
base<-base %>% 
  select(-Municipio)


