### Aula 2 - Analise exploratoria no R ###

# Instalar pacotes
install.packages("ggplot2") # Pacote de visualizacao de dados e graficos
install.packages("corrplot")# Cria graficos de matriz de correlacao
install.packages("lmtest") # Para criar modelos de regressao linear

# Carregar pacotes
library(ggplot2)
library(corrplot)
library (lmtest)

# Evitar notacao cientifica
options(scipen = 100)

# Carregar base de dados
setwd(choose.dir())
df <- read.csv('salaries.csv',sep = ";")
# Baseado em: https://www.kaggle.com/datasets/lorenzovzquez/data-jobs-salaries/

### Analise basica dos dados ###
# Etapa essencial para analise de dados - "Se entra lixo, sai lixo"

str(df)
dim(df)
head(df)
View(df)
class(df)

# Devemos sempre verificar a quantidade de nulos (NA's).So podemos calcular 
# estatisticas quando as variaveis nao possuirem valores nulos.
sum(is.na(df)) #sum(is.na(df$salary))

# Maximo e minimo
max(df$salary) # Maximo
min(df$salary) # Minimo

### Medidas de tendencia central ###
# Media aritmetica
mean(df$salary)
media_atm <- (sum(df$salary)/length(df$salary)) # Outra forma
media_atm

# Outras medias, vamos usar outros dados a fins didaticos
peso <- c(0.2,  0.1,  0.6,  0.4)
x <- c(40,50,30,20)

# Media ponderada
weighted.mean(x, peso) 
media_pon <- sum(x*peso)/sum(peso)

# Media geometrica
media_geo <- (prod(x)^(1/length(x)))

# Media harmonica
media_har <- (length(x)/sum((1/x)))

### Mediana, quartis e moda
# Mediana
median(df$salary)

# Quartis
q1 <- quantile(df$salary, 0.25) # Primeiro quartil
q2 <- quantile(df$salary, 0.5) # Segundo quartil ou mediana
q3 <- quantile(df$salary, 0.75) # Terceiro quartil

# Moda
# Criar funcao de moda
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]}

getmode(df$salary)
getmode(df$job_title) # Podemos usar para ver as profissoes que aparecem mais

# Podemos verificar com a funcao table uma lista das principais repeticoes
sort(table(df$job_title), decreasing = TRUE)
lista <- as.data.frame(sort(table(df$job_title), decreasing = TRUE)) # Transformar em uma lista

# A funcao summary() resume muitas dessas estatisticas
summary(df$salary)
summary(df) # para todas as variaveis

hist(df$salary, main = "Histograma Salario", ylab = "Frequencia", 
     xlab = "Salarios")

### Medidas de dispersao ###
# Amplitude total - podemos ver os maximos e minimos
range(df$salary)

# Desvios em relacao a media
desvios <- (df$salary - mean(df$salary))
media_desvio <- sum(desvios)/length(desvios)
# A soma dos desvios em relacao a media nao e uma medida util, pois e praticamente zero

# Variancia
variancia <- (sum((desvios)^2)/(length(desvios)-1))
var(df$salary) # variancia amostral

# Variancia Populacional
varPop <- function(x){
  n <- length(x) # calcula o tamanho da amostra com a funcao lenght()
  var(x)*(n-1)/n # calcula a variancia amostral e multiplica por (n-1)/n
}
varPop(df$salary)

# Desvio Padrao
sqrt(variancia)# raiz da variancia e o desvio padrao
sd(df$salary)

# Intervalo interquartil (IQR) - pouco afertada por mudancas
IQR = (q3- q1) # terceiro quartil menos o primeiro quartil

# Z-score
df$z_score <- scale(df$salary) # inseri o z-score como uma coluna

### Graficos Boxplot - Eles sao uma otima forma para observar os dados ###

boxplot(df$salary, main = "Boxplot Salarios", ylab = "Valores")

### Medidas de associacao entre duas variaveis ###

# Correlacao entre duas variaveis
cor(df$salary, df$age)
cov(df$salary, df$age)

# Grafico de dispersao
plot(df$salary~df$age, 
     ylab="Salarios", xlab="Idade",
     col="blue", las=1)

# Matriz de correlacao
df1 <- df[,c("salary", "age", "company_size")]
matriz_correlacao = cor(df1)
corrplot(matriz_correlacao, type="upper", tl.col = 'black' )

# Regressao linear
reg <- lm (df$salary~df$age)
summary (reg)
plot (df$salary, df$age, xlab = 'Salarios', ylab = 'Idade')

# Usando o ggplot2
(grafico=ggplot(df, 
                aes(x=salary, y=age)) + 
    geom_point()+
    geom_smooth(method="lm")
  )

### FIM ###