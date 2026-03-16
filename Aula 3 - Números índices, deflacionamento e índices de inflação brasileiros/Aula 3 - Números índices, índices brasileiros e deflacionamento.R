### Aula 3 - Numeros indices, indices de inflacao brasileiros e deflacionamento ###

# Instalar pacotes
install.packages("IndexNumR")
install.packages("tideverse")
install.packages("readxl")
install.packages("dplyr")
install.packages("ggplot2")

# Carregar pacotes
library(IndexNumR)
library(readxl)
library(tidyverse)
library(dplyr)
library(ggplot2)

# Evitar notacao cientifica
options(scipen = 100)

### indices de precos ###
df <- data.frame(
  Produto = c("Arroz", "Feijao", "Duzia Ovo", "Macarrao", "Cafe"),
  Preco_atual = c(6, 4, 15, 4, 15),
  Quantidade_atual = c(3, 3, 3, 3, 2),
  Preco_base = c(4, 3, 20, 2, 12),
  Quantidade_base = c(5, 4, 2, 2, 1))

# O indice de Laspeyres para um conjunto de mercadorias, em um periodo t, e a 
# media ponderada dos precos relativos dessas mercadorias, utilizando, como 
# fatores de ponderacao, os valores monetarios das quantidades de cada mercadoria
# vendida no periodo base (Hoffmann, 2006).

# indice de Laspeyres
laspeyres <- function(preco_base, preco_atual, quantidade_base) {
  numerador <- sum(preco_atual * quantidade_base)
  denominador <- sum(preco_base * quantidade_base)
  indice_laspeyres <- numerador / denominador
  return(indice_laspeyres)
}

lasp <- laspeyres(df$Preco_base, df$Preco_atual, df$Quantidade_base)

# O indice de precos de Paasche para o periodo t pode ser interpretado
# como uma media ponderada dos precos relativos, utilizando-se como fatores de
# ponderadacao os valores monetarios das quantidades vendidas no periodo t, 
# considerando os precos relativos do periodo base.

# indice de Paasche
paasche <- function(preco_base, preco_atual, quantidade_atual) {
  numerador <- sum(preco_atual * quantidade_atual)
  denominador <- sum(preco_base * quantidade_atual)
  indice_paasche <- numerador / denominador
  return(indice_paasche)
}

paas <- paasche(df$Preco_base, df$Preco_atual, df$Quantidade_atual)

# indice de Fischer
fisher <- function(indice_laspeyres, indice_paasche) {
  indice_fisher <- sqrt(indice_laspeyres * indice_paasche)
  return(indice_fisher)
}

fis <- fisher(lasp,paas)

# Calculando os indices com o pacote IndexNumR

# Carregando uma nova base de dados com 12 periodos de tempo
head(CES_sigma_2)
View(CES_sigma_2)

# Gerando uma lista dos calculos que queremos e visto em aula
methods <- c("laspeyres","paasche","fisher", "marshalledgeworth")
prices <- lapply(methods, 
                 function(x) {priceIndex(CES_sigma_2,
                                         pvar = "prices", 
                                         qvar = "quantities", 
                                         pervar = "time", 
                                         prodID = "prodID", 
                                         indexMethod = x, 
                                         output = "chained")})

as.data.frame(prices, col.names = methods)

### indices de qauntidades ###
# indice de Laspeyres de Quantidades
laspeyres_quantidade <- function(quantidade_base, quantidade_atual, preco_base) {
  numerador <- sum(quantidade_atual * preco_base)
  denominador <- sum(quantidade_base * preco_base)
  indice_laspeyres_quantidade <- numerador / denominador
  return(indice_laspeyres_quantidade)
}

lasp_qtd <- laspeyres_quantidade(df$Quantidade_base, df$Quantidade_atual, df$Preco_base)

# Atividade: Faca outras funcoes para os indices de quantidades de paasche e fisher

### indices de Inflacao Brasileiros ###

# https://www.ibge.gov.br/estatisticas/economicas/precos-e-custos/9256-indice-nacional-de-precos-ao-consumidor-amplo.html?=&t=downloads
# https://sidra.ibge.gov.br/pesquisa/snipc/ipca/tabelas

# Selecionar a pasta
setwd(choose.dir())

# indice de Precos ao Consumidor Amplo (IPCA)
ipca <- read_xls("ipca.xls")
plot(ipca$Anos, ipca$IPCA, type = "l", col = "blue", xlab = "IPCA", ylab = "Anos", main = "IPCA (1996-2022")

# indice Nacional de Precos ao Consumidor (INPC)
inpc <- read_xls("inpc.xls")
plot(inpc$Anos, inpc$INPC, type = "l", col = "red", xlab = "IPNC", ylab = "Anos", main = "INPC (1996-2022")

# Juntando os dois graficos
ggplot(data = ipca, aes(x = ipca$Anos)) +
  geom_line(aes(y = ipca$IPCA, color = "IPCA"), linetype = "solid", size = 1) +
  geom_line(aes(y = inpc$INPC, color = "INPC"), linetype = "dashed", size = 1) +
  labs(x = "Anos", y = "% Inflacao", title = "indices de Inflacao: IPCA x INPC") +
  scale_color_manual(values = c("INPC" = "blue", "IPCA" = "red")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.title = element_blank())

# Usando o pacote sidrar

install.packages("sidrar")
library(sidrar)

# O numero 1737 e o da tabela do ipca no Sidra
# Podemos consultar as https://sidra.ibge.gov.br/

?search_sidra # Encontra as series com determinada palavra chave
?info_sidra   # Oferece informacoes sobre uma serie especifica
?get_sidra    # Baixa a(s) serie(s) de interesse

search_sidra(c("ipca"))
info_sidra(1737, wb=T)

### Deflacionar a serie ###
# Baseado em: https://rpubs.com/rafaeldeacypreste/deflacionamento_R

ipca14_23 <- get_sidra(1737,
                       variable = 63,
                       period = "201312-202306",
                       format = 2)

# Colocar o acento no "Mes" 
ipca14_23_mensal <- ipca14_23[,c("Mês","Valor")] # Selecionando as colunas
ipca14_23_mensal$indice <- 100 # Cria coluna de indices

for (i in 1:(length(ipca14_23_mensal$indice)-1)) {
  ipca14_23_mensal[i+1,"indice"] <- (ipca14_23_mensal[i+1,"Valor"]/100+1)*
    ipca14_23_mensal[i,"indice"]
}

# Cria a coluna do deflator
ipca14_23_mensal$precos_jun_2023 <- c(NA,rep(1,(length(ipca14_23_mensal$indice)-1)))

# Precos ao termo de junho de 2023
for (i in 0:(length(ipca14_23_mensal$indice)-2)) {
  ipca14_23_mensal[length(ipca14_23_mensal$indice)-i,"precos_jun_2023"] <- ipca14_23_mensal[length(ipca14_23_mensal$indice),"indice"]/
    ipca14_23_mensal[length(ipca14_23_mensal$indice)-i-1,"indice"]
}

# Deflacionando uma serie
install.packages("rbcb")
library(rbcb) # Base de dados do Banco Central do Brasil (BCB)

pib <- get_series(code = c(pib_m = 4380),  
                    start_date = "2014-01-01",
                    end_date = "2023-06-30")

# Retirando os dados de dezembro de 2013
deflatores <- ipca14_23_mensal$precos_jun_2023[2:length(ipca14_23_mensal$indice)]

# Deflaciona os precos e organiza para plotagem
pib_mensal <- pib %>% 
  mutate(pib_def = pib_m*deflatores) %>% 
  pivot_longer(cols = pib_m:pib_def,
               names_to = "Tipo",
               values_to = "Saldo")  

# Grafico comparando as series 
ggplot(data = pib_mensal,aes(x = date,y = Saldo/1000, color =Tipo))+
  geom_line(size = 1.25)+
  labs(x=NULL,
       y = NULL,
       color = NULL,
       title = "Produto Interno Bruto - Mensal (bilhoes R$)",
       caption = "Fonte: Banco Central do Brasil. Reais de 31/06/2023.")+
  scale_color_manual(values = c("#D55E00","#009E73"),
                     labels = c("Real","Nominal"))+
  scale_x_date(date_labels = "%m/%Y",
               date_breaks = "4 months")+
  theme(axis.text.y = element_text(angle = 0))

### FIM ###