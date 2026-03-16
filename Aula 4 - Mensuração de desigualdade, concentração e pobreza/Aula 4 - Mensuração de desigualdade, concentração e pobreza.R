### Aula 4 - Mensuracao de desigualdade, concentracao e pobreza ###]

# Carregar pacotes
library(ggplot2)
library(readxl)

# Evitar notacao cientifica
options(scipen = 100)

### Medidas de Desigualdade ###
# install.packages("ineq") # Pacote para calcular medidas de desigualdade
library(ineq)

# Data Frame da Renda Media da Regiao Metropolina de Campinas
rmc_df <- data.frame(
  Municipio = c("Campinas", "Americana", "Indaiatuba", "Sumare", "Hortolandia",
                "Valinhos", "Jaguariuna", "Paulinia", "Vinhedo", "Nova Odessa"),
  Renda_Media = c(5500, 4800, 3000, 2500, 2000, 6000, 5400, 5900, 6300, 3100))


install.packages()
# Curva de Lorenz

clorenz <- Lc(c(7, 4, 2, 3, 5)) # Salarios de uma empresa em mil
plot(clorenz, main = "Curva de Lorenz Salarios", col = 4)

clorenz <- Lc(c(50, 4, 2, 3, 5)) # Salarios de uma empresa em mil (mais desigual)
plot(clorenz, main = "Curva de Lorenz Salarios", col = 4)

clorenz <- Lc(c(4, 4, 5, 3, 5)) # Salarios de uma empresa em mil (menos desigual)
plot(clorenz, main = "Curva de Lorenz Salarios", col = 4)

rmc_clorenz <- Lc(rmc_df$Renda_Media)
plot(rmc_clorenz, main = "Curva de Lorenz - Renda media - Regiao Metropolina Campinas")
text(0.5, 0.45, 'A', cex = 0.8) # area de desigualdade
text(0.75, 0.2, 'B', cex = 0.8) # 

# indice de Gini - Varia de 0 a 1, ou seja, quanto mais próximo de 1, mais desigual.

# Gini = A/(A+B) = A/0.5 = 2A
G <- Gini(rmc_df$Renda_Media)
text(0.2, 0.9, 'G = 0.19', cex = 1.5)

# indice de Theil - A medida corresponde ao desvio logaritmico medio dos rendimentos
t <- Theil(rmc_df$Renda_Media)

# Selecionando pasta
setwd(choose.dir())

gini_theil <- read_xlsx("Gini e Theil-L 2012 - 2021.xlsx")

ggplot(data = gini_theil, aes(x = gini_theil$Anos)) +
  geom_line(aes(y = gini_theil$`BR-Gini`, color = "indice de Gini"), linetype = "solid", size = 1) +
  geom_line(aes(y = gini_theil$`BR-Theil`, color = "indice de Theil"), linetype = "dashed", size = 1) +
  labs(x = "Anos", y = "indices", title = "indice de Gini x Theil: Brasil (2012-2021)") +
  scale_color_manual(values = c("indice de Gini" = "blue", "indice de Theil" = "red")) +
  theme(plot.title = element_text(hjust = 0.5, size = 18)) +
  theme(legend.title = element_blank())

###	Medidas de Concentracao ###

clubes_df <- data.frame(
  Clube = c("Corinthians", "Flamengo", "Sao Paulo", "Palmeiras", "Gremio",
            "Santos", "Internacional", "Cruzeiro", "Fluminense", "Vasco da Gama"),
  Porcentagem_Vendas_Camisa = c(20, 25, 10, 10, 7, 5, 4, 6, 5, 8))

# indice de Herfindahl-Hirschman - Quanto mais concentrado maior o indice

?conc
conc(clubes_df$Porcentagem_Vendas_Camisa, type = "Herfindahl")
# Ou pode usar: Herfindahl(clubes_df$Porcentagem_Vendas_Camisa)

### Medidas de Pobreza ###
View(rmc_df)

# indice de Pobreza de Sen
pov(rmc_df$Renda_Media, 3500, type = "Sen")

# indice de Pobreza FGT (Foster, Greer e Thorbecke)
pov(rmc_df$Renda_Media, 3000, type = "SST")

pobreza <- read_xlsx("Pobreza Brasil (2012 - 2021).xlsx")
ggplot(data = pobreza, aes(x = pobreza$Anos)) +
  geom_line(aes(y = pobreza$`Ext. Pobres`, color = "Extremamente Pobres"), linetype = "solid", size = 1) +
  geom_line(aes(y = pobreza$Pobres, color = "Pobres"), linetype = "solid", size = 1) +
  geom_line(aes(y = pobreza$`Vul. Pobres`, color = "Vulneraveis à pobreza"), linetype = "solid", size = 1) +
  labs(x = "Anos", y = "Porcentagem (%)", title = "% Pobres, Extremamente Pobres e Vulneraveis a Pobreza") +
  scale_color_manual(values = c("Extremamente Pobres" = "blue", "Pobres" = "red", "Vulneraveis à pobreza" = "black")) +
  theme(plot.title = element_text(hjust = 0.5, size = 18)) +
  theme(legend.title = element_blank())

### FIM ###