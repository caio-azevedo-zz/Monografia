# Carregando pacotes a serem utilizados

library(dplyr)
library(readxl)
library(stringr)
library(read.dbc)


# Limpar----

rm(list=ls())


#Diretório

setwd("c:/Users/CAIO AZEVEDO/Documents/Documentos Caio/Github/Monografia")

# Preparação dos dados

# Produto Interno Bruto

pib<- read_xlsx("data/tabela21.xlsx", sheet = "Tabela 1", col_names = c("mun", "pib"))

pib<-pib[-c(1:5,98),]

# Valor Adicionado bruto a preços correntes

vab<- read_xlsx("data/tabela21.xlsx", sheet = "Tabela 2", col_names = c("mun", "vab"))

vab<-vab[-c(1:5,98),]

# Valor adicionado bruto a preços correntes da agropecuária

agro<- read_xlsx("data/tabela21.xlsx", sheet = "Tabela 4", col_names = c("mun", "agro"))

agro<-agro[-c(1:5,98),]

# Valor adicionado bruto a preços correntes da indústria

ind<- read_xlsx("data/tabela21.xlsx", sheet = "Tabela 6", col_names = c("mun", "ind"))

ind<-ind[-c(1:5,98),]

# Valor adicionado bruto a preços correntes dos serviços, inclusive administração, defesa, educação e saúde públicas e seguridade social

serv<- read_xlsx("data/tabela21.xlsx", sheet = "Tabela 8", col_names = c("mun", "serv"))

serv<-serv[-c(1:5,98),]

# Juntando as tabelas

valor_adicionado<-full_join(vab, agro,c("mun"))
valor_adicionado<-full_join(valor_adicionado, ind,c("mun"))
valor_adicionado<-full_join(valor_adicionado, serv,c("mun"))

rm(vab, agro, ind, serv)


# Criando as variáveis relativas

valor_adicionado<-valor_adicionado %>% 
  mutate("vab"=as.numeric(vab)) %>% 
  mutate("agro"=as.numeric(agro)) %>% 
  mutate("ind"=as.numeric(ind)) %>% 
  mutate("serv"=as.numeric(serv)) %>% 
  mutate(x1=agro/vab) %>% 
  mutate(x2=ind/vab) %>% 
  mutate(x3=serv/vab) %>% 
  select(mun,x1,x2,x3)


#  Taxa de alfabetização das pessoas de 10 anos ou mais de idade (%)

alf<- read_xlsx("data/tabela1383.xlsx", col_names = c("mun", "x5"))

alf<-alf[-c(1:6,99),]

# Juntando as variáveis x1:x5

dados<-full_join(valor_adicionado,alf, c("mun"))

rm(valor_adicionado, alf)


# População residente

urb<- read_xlsx("data/tabela608.xlsx", col_names = c("mun", "sexo", "total", "urbana"))

urb<-urb[-c(1:5,98),-2]

pop<-urb[,1:2]

colnames(pop)<-c("mun","pop")

pop_mun<-toupper(pop$mun)

pop_mun<-chartr("ÁÉÍÓÚÃÕÂÊÔÇ", "AEIOUAOAEOC", pop_mun)

pop<-cbind(pop_mun,pop)
pop<-pop[,-2]
colnames(pop)<-c("mun", "pop")

rm(pop_mun)

urb<-urb %>% 
  mutate("total"=as.numeric(total)) %>% 
  mutate("urbana"=as.numeric(urbana)) %>% 
  mutate(x7=urbana/total) %>% 
  select(mun,x7)



# Frota de veículos

veic<- read_xls("data/Frota Munic DEZ2010.xls", sheet = "DEZ_2010")

colnames(veic)<-c(veic[2,])
veic<-veic[-c(1:2),]

# Filtrando pelo Estado

veic<-veic %>% 
  filter(UF=="RJ") %>% 
  select(MUNICIPIO, TOTAL)


# Inserindo (RJ) para junção das tabelas

mun<-paste(veic$MUNICIPIO, "(RJ)", sep=" ")
veic<-cbind(mun,veic)
veic<-veic[,-2]

# Corrigindo ortagrafia da base 

veic[5,1]<-c("ARMACAO DOS BUZIOS (RJ)")
veic[53,1]<-c("PARATY (RJ)")
veic[87,1]<-c("TRAJANO DE MORAES (RJ)")

rm(mun)

# Juntando as variáveis x6, x7

veic<-left_join(pop, veic, c("mun") )


veic<-veic %>%
  mutate("TOTAL"=as.numeric(TOTAL)) %>% 
  mutate("pop"=as.numeric(pop)) %>% 
  mutate(x6=TOTAL/pop) %>% 
  select("mun", "x6")

rm(x)

# Juntando x6 -x7

x<-cbind(veic,urb)
x<-x[,-1]
rm(veic, urb)


# Juntando a base existente

dados<-full_join(dados, x, c("mun"))

rm(x)


# Leitos por mil habitantes

leitos<- read.csv("data/A182143189_28_143_208.csv",sep = ";", skip = 4)

mun<-str_sub(leitos$Município,start=8)
mun<-paste(mun, "(RJ)", sep=" ")

leitos<-cbind(mun,leitos)
leitos<-leitos[-c(85:96),-2]
rm(mun)

urb<- read_xlsx("data/tabela608.xlsx", col_names = c("mun", "sexo", "total", "urbana"))

urb<-urb[-c(1:5,98),-2]

pop<-urb[,1:2]

colnames(pop)<-c("mun","pop")

leitos<-full_join(pop, leitos, c("mun"))

leitos<-leitos %>% 
  mutate("pop"=as.numeric(pop)) %>% 
  mutate("Quantidade_existente"=as.numeric(Quantidade_existente)) %>% 
  mutate(leitos=(Quantidade_existente/pop)*1000) %>% 
  rename("x10"=leitos) %>% 
  select("mun", "x10")

# Juntando a base existente

dados<-full_join(dados, leitos, c("mun"))





