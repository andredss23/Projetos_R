### Aula 1 - Introducao ao R ###

### Download do Software R Studio ###
# https://posit.co/download/rstudio-desktop
# Tambem temos o R mais "cru", mas nao vamos explorar ele.

###	Apresentacao do R ###
# O R e um software livre criado por estatisticos em 1995, sua implementacao
# foi feita a partir da linguagem S da Bell Labs. O R e um software gratuito
# e poderoso para realizar analises estatisticas e economicas, tendo uma grande
# e ativa comunidade de apoio espalhados pelo mundo todo.

### Sempre se lembre: A SUA DuVIDA Ja FOI A DuVIDA DE OUTRA PESSOA, PESQUISE! ###
# Pesquise suas duvidas no https://stackoverflow.com/, ChatGPT, Bard, entre outros
# sites e inteligencias artificias. 

###	Por que usar o R? ###
# Software potente para analise de dados, estatisticas, econometricas, etc.
# Codigo livre, ou seja, de gratis para usar e colaborar!
# Popular mundialmente, com uma comunidade que se ajuda muito.
# util para sua carreira profissional, seja academica ou no setor publico ou privado!

### Interface do R ###
# 4 telas principais: R script, R console, R environment e Graphic output.

### Comandos basicos ###
# Para chamar uma acao devemos pressionar "Ctrl+Enter".

### R como calculadora ###
4*4  # Multiplicacao
3+3  # Soma
4-2  # Subtracao
15/3 # Divisao
4**2 # Potencia

### R e uma linguagem voltada a objetos ###
# Os sinais "<-" e "=" atribuem valor a um objeto e guarda esse valor.
x <- c(1,2,3)
x

# Nos temos a "#" para inserir comentario no codigo, ou seja, sao linhas que 
# nao serao lidas.

### Comandos basicos ###
# getwd() para saber em qual de trabalho voce esta puxando os dados.
getwd()
# setwd() para alterar a pasta de trabalho - Atencao a "\" e "/".
setwd("") # Entre o parenteses coloque o caminho da sua pasta


### Atencao aos caracteres especiais! ###
# Nao podemos inserir caracteres especiais (c ou letras com acentos) no nome das
# variaveis. Alem disso, nos dados sempre e recomendado remover esses caracteres
# ou os espacos, pois isso pode dificultar a analise dos dados.

### Funcao Help ###
# otima ferramenta para entender como funciona uma funcao, seus parâmetros e
# detalhes. Atencao a como e a organizacao para chamar as funcões.

# paste() para unir objetos.
paste("Hello", "World")

?paste # para usar so inserir o ?"Nome do pacote"

### Pacotes do R ###
# Os pacotes sao basicamente para organizar funcões e padronizar funcões extras 
# que serao distribuidas para a comunidade do R. Existem pacotes nativos do R e
# outros pacotes que podemos instalar e carregar. Alem disso, podemos desenvolver 
# pacotes de funcões e carregar em nossa propria maquina sem subir na rede que e 
# o CRAN (Comprehensive R Archive Network). Atualmente existem mais de 19 mil 
# pacotes disponiveis para diversas aplicacões.

# A versao basica do R vem com 8 ou mais pacotes instalados.
subset(as.data.frame(installed.packages()),Priority
       %in%("base"), select=c(Package, Priority))
# Para instalar um pacote temos que usar a funcao install.packages("nome do pacote").
# Uma vez instalado, nao precisa instalar novamente na mesma maquina.
install.packages("readxl")
# Carregar um pacote usamos a library() ou require()
library(readxl)
require(readxl)

### Classificacao dos objetos) ###
nome <- c("Renato", "Augusto", "Thamires", "Lele")
renda <- c(2000, 2500, 3000, 3000)
#CHR: "character" = caracteres/texto
class(nome)
#NUM: "numeric" = numeros reais (incluso decimais)
class(renda)
#INT: "integer" = numeros inteiros
renda <- as.integer(renda) # Transformei o numeric em integer
class(renda)
#LOG: "logical" = logica Verdadeiro/Falso
#ORD: "ordered" = texto ordenado

# As categorias character e ordered sao exemplos de variaveis qualitativas,
# enquanto as numeric e  integer sao exemplos de variaveis quantitativas.

### Criar e manipular um data frame ### 
df <- data.frame(nome,renda)
class(df)
df[1] # ver coluna 1
df[,2] # ver coluna 2
df$nome # "$" e uma forma de chamar coluna em especifico
renda_3k <- subset(df, renda == 3000) # uma das varias formas de filtrar

### Atlas do Desenvolvimento Humano no Brasil ###
# http://www.atlasbrasil.org.br/

### Exemplo de Analise de Dados ###

# Carregar
library(readxl)
# Carregar o pacote, temos 3 jeitos: 
# 1) Definir primeiro o diretório e depois chamar o arquivo
setwd("") # Entre o parenteses coloque o caminho da sua pasta
df1 <- read_xlsx("Gini 2012 - 2021.xlsx")
# 2) Escrever todo o caminho da pasta e do cógido, exemplo: df1 <- read_xlsx("C:/Users/andre/OneDrive/Área de Trabalho/Mestrado/PED/Aulas André/Aula 1 - Introdução ao R/dados")
df1 <- read_xlsx(".../Gini 2012 - 2021.xlsx") # Escrever o caminho no lugar do ...
# 3) Definir pelo setwd(choose.dir()), onde irá abrir uma janela para escolher uma pasta
df1 <- setwd(choose.dir())
getwd() # Ver se está correta
df1 <- read_xlsx("Gini 2012 - 2021.xlsx")


# Estrutura dos dados
# Cross Section
cs <- subset(df1, Anos == "2021")
View(cs)

# Series de tempo
st <- df1[,1:2]
View(st)

# Dados em painel
View(df1)
df1

# Algumas funcões
View(df1)
dim(df1)
head(df1)
names(df1)

# Grafico simples

plot(df1$Anos, df1$Brasil, type = "l", col = "blue", xlab = "Anos", 
     ylab = "indice de Gini", main = "indice de Gini - Brasil (2012-2021)") # linhas
plot(df1$Anos, df1$Brasil, type = "p", col = "blue", xlab = "Anos", 
     ylab = "indice de Gini", main = "indice de Gini - Brasil (2012-2021)")# pontos
plot(df1$Anos, df1$Brasil, type = "b", col = "blue", xlab = "Anos", 
     ylab = "indice de Gini", main = "indice de Gini - Brasil (2012-2021)")# pontos+linhas
?plot # para mais detalhes

barplot(df1$Brasil, names.arg = df1$Anos, xlab="Anos", ylab="indice Gini", 
        ylim=c(0,0.6),  main = "indice de Gini - Brasil (2012-2021)") # colunas
# e um exemplo ruim de grafico para mostrar que temos que nos atentar ao que o
# grafico comunica.

# Graficos simples com ggplot2
install.packages("ggplot2") # O pacote ggplot2 e otimo para criar graficos.
# https://github.com/rstudio/cheatsheets/blob/main/data-visualization.pdf
# https://r-graph-gallery.com/ggplot2-package.html
library(ggplot2)

ggplot(data = df1, aes(x = df1$Anos, y = df1$Brasil)) +
  theme_classic() +
  geom_line(color = "blue") +
  theme(plot.title = element_text(hjust = 0.5))

# Adicionando um titulo
ggplot(data = df1, aes(x = df1$Anos, y = df1$Brasil)) +
  theme_classic() +
  geom_line(color = "blue") +
  labs(x = "Anos", y = "indice de Gini", title = "indice de Gini Brasil (2012-2021)") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data = df1, aes(x = df1$Anos)) +
  geom_line(aes(y = df1$`Rio de Janeiro`, color = "RJ"), linetype = "solid", size = 1) +
  geom_line(aes(y = df1$`Sao Paulo`, color = "SP"), linetype = "dashed", size = 1) +
  labs(x = "Anos", y = "indice de Gini", title = "indice de Gini: RJ x SP") +
  scale_color_manual(values = c("RJ" = "blue", "SP" = "red")) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.title = element_blank())

### Limpar os dados ###
rm(list=ls())

### FIM ###