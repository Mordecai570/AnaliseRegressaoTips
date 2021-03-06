# Primeiro baixamos todas as bibliotecas
install.packages("ggplot2")
install.packages("ggthemes")
install.packages("dplyr")
install.packages("reshape2")
install.packages("knitr")
install.packages("kableExtra")
install.packages("psych")
install.packages("GGally")
install.packages("car")

# Depois chamamos as bibliotecas necess�rias para o programa
library(ggplot2)
library(ggthemes)
library(dplyr)
library(reshape2)
library(knitr)
library(kableExtra)
library(psych)
library(GGally)
library(car)
df <- tips

# Arrumamos os dados para melhor trabalharmos com eles 
df$sex <- gsub('Female', 'Feminino', df$sex)
df$sex <- gsub('Male', 'Masculino', df$sex)
df$smoker <- gsub('Yes', 'Sim', df$smoker)
df$smoker <- gsub('No', 'N�o', df$smoker)
df$day <- gsub('Sun', 'Domingo', df$day)
df$day <- gsub('Sat', 'S�bado', df$day)
df$day <- gsub('Thur', 'Quinta', df$day)
df$day <- gsub('Fri', 'Sexta', df$day)
df$time <- gsub('Lunch', 'Almo�o', df$time)
df$time <- gsub('Dinner', 'Janta', df$time)


#Criando a vari�vel Raz�o
cbind(df, "rate")
df$rate = df$tip/df$total_bill


# Cria��o da tabela 1, em rela��o ao g�neros dos participantes
t1 <- df %>% select("Total da Conta" = total_bill, "Gorjeta" = tip, "Tamanho do Grupo" = size) %>% describe() %>% select("M�dia" = mean, "Desvio Padr�o" = sd, "Valor M�nimo" = min, "Valor M�ximo" = max)
t1 <- round(t1,digits=2)
knitr::kable(list(t1), booktabs = TRUE, valign = 't',caption = "M�dias e Desvios Padr�es das Vari�veis Num�ricas") %>% kable_styling(latex_options = "HOLD_position")

# Cria��o da tabelas 2, em rela��o ao g�neros dos clientes e se fumam
Sim <- c(33,60,93)
Nao <- c(54,97,151)
Total <- c(87,157,244)
Sex <- c("Feminino", "Masculino", "Total")

table2 <- data.frame(Sex,Sim,Nao,Total)
colnames(table2) <- c("G�nero/Fumante","N�o", "Sim", "Total")

knitr::kable(list(table2), booktabs = TRUE, valign = 't',caption = "G�nero do Pagante e se o Grupo � Fumante") %>% kable_styling(latex_options = "HOLD_position")

#Cria��o da tabela 3, em rela��o a quantidade de mesas atendidas ao longo do dia pelos dias da semana
Quinta <- c(61,1,62)
Sexta <- c(7, 12, 19)
Sabado <- c(0, 87, 87)
Domingo <- c(0,76,76)
Total <- c(68,176,244)
Horario <- c("Almo�o", "Janta", "Total")

table3 <- data.frame(Horario,Quinta,Sexta,Sabado,Domingo,Total)
colnames(table3) <- c("Per�odo/Dia","Quinta", "Sexta", "S�bado", "Domingo", "Total")

knitr::kable(list(table3), booktabs = TRUE, valign = 't',caption = "Quantidade de mesas atendidas ao longo do dia pelos dias da semana") %>% kable_styling(latex_options = "HOLD_position")

#Grafico de Correla��o de Pearson
plot_corr <- df %>% select("Total bill" = total_bill,"Tips" = tip, "Size" = size)
ggpairs(plot_corr) +
  ggtitle("Correla��o Entre as Vari�veis Quantitativas") + 
  theme_tufte()

#Gr�ficos explorat�rios 1
plot_sex <- ggplot(df, aes(sex, tip), add=TRUE) + 
  geom_boxplot() +
  xlab("G�nero dos Participantes") +
  ylab("Gorjeta") +
  labs(caption = "Gr�fico 1") + 
  theme_tufte()

plot1 <- ggplot(df, aes(x = total_bill, y = tip)) +
  geom_point(aes(group = sex, colour = sex)) +
  xlab("Total da Conta") +
  ylab("Gorjeta") +
  labs(caption = "Gr�fico 2") + 
  theme_tufte()


plot_time <- ggplot(df, aes(time, tip), add=TRUE) + 
  geom_boxplot() +
  xlab("Per�odo") +
  ylab("Gorjeta") +
  labs(caption = "Gr�fico 3") + 
  theme_tufte()

plot2 <- ggplot(df, aes(x = total_bill, y = tip)) +
  geom_point(aes(group = time, colour = time)) +
  xlab("Total da Conta") +
  ylab("Gorjeta") +
  labs(caption = "Gr�fico 4") + 
  theme_tufte()


plot_scatter <- grid.arrange(plot_sex, plot1, plot_time, plot2, nrow = 2)

#Gr�ficos explorat�rios 2
plot_dia <- ggplot(df, aes(day, tip), add=TRUE) + 
  geom_boxplot() +
  xlab("Dia da Semana") +
  ylab("Gorjeta") +
  labs(caption = "Gr�fico 5") + 
  theme_tufte()

plot3 <- ggplot(df, aes(x = total_bill, y = tip)) +
  geom_point(aes(group = day, colour = day)) +
  xlab("Total da Conta") +
  ylab("Gorjeta") +
  labs(caption = "Gr�fico 6") + 
  theme_tufte()

plot4 <- ggplot(df, aes(total_bill, rate), add=TRUE) + 
  geom_point(aes(group = day, colour = day)) +
  geom_abline(slope = 0, intercept = 0.15) +
  xlab("Total da Conta") +
  ylab("Gorjeta") +
  labs(caption = "Gr�fico 7") + 
  theme_tufte()

plot5 <- ggplot(df, aes(x=tip)) + 
  geom_histogram(col="red", fill="blue", alpha = .2) + 
  labs(x="Gorjeta", y="Frequ�ncia", caption="Gr�fico 8")

plot_scatter_2 <- grid.arrange(plot_dia, plot3, plot4, plot5, nrow = 2)

df <- tips

#Cria��o do modelo e tabela dos coeficientes
modelo <- lm(tip ~ total_bill + sex + smoker + day + time + size, df)
names(modelo$coefficients) <- c('Intercepto','Total da Conta','G�nero','Fumante','S�bado', 'Domingo','Quinta','Per�odo','Tamanho do Grupo')
kable(coef(modelo), booktabs = TRUE, valign = 't',caption = "Coeficientes do Modelo") %>% kable_styling(latex_options = "HOLD_position")

#Teste Anova dos coeficientes
a <- summary(modelo)
tabela_a <- data.frame(c("Total Conta", "G�nero", "Fumante", "S�bado","Domingo","Quinta", "Per�odo Almo�o", "Tamanho do Grupo"),c("<2e-16","0.8190","0.5561","0.6953","0.9369","0.6804","0.8783","0.0500"),c("Sucesso","Fracasso","Fracasso","Fracasso","Fracasso","Fracasso","Fracasso","Sucesso"))
colnames(tabela_a) <- c("Teste ANOVA","p-valor","Resultado")
kable(tabela_a, booktabs = TRUE, valign = 't') %>% kable_styling(latex_options = "HOLD_position")

#Teste de Shapiro-Wilks e Breusch-Pagan para o modelo refinado
modelo2 <- lm(tip ~ total_bill + size, df)

shapiro_m2 <- shapiro.test(residuals(modelo2))
tabela_shapiro_1 <- data.frame(c("Modelo"), c(shapiro_m2$p.value), c("Fracasso"))
colnames(tabela_shapiro_1) <- c("Teste de Shapiro-Wilks","p-valor","Resultado")
kable(tabela_shapiro_1, booktabs = TRUE, valign = 't') %>% kable_styling(latex_options = "HOLD_position")

breusch_m2 <- ncvTest(modelo2)
tabela_breusch_1 <- data.frame(c("Modelo"), c("< 2.22e-16"), c("Fracasso"))
colnames(tabela_breusch_1) <- c("Teste de Breusch-Pagan","p-valor","Resultado")
kable(tabela_breusch_1, booktabs = TRUE, valign = 't') %>% kable_styling(latex_options = "HOLD_position")

#Transformando a vari�vel e remodelando
lambda <- powerTransform(modelo)
tips2 <- bcPower(df$tip, lambda$lambda)
modelo3 <- lm(tips2 ~ total_bill + sex + smoker + day + time + size, df)
c <- summary(modelo3)

#Teste Anova dos coeficientes do novo modelo
tabela_c <- data.frame(c("Total Conta", "G�nero", "Fumante", "S�bado","Domingo","Quinta", "Per�odo Almo�o", "Tamanho do Grupo"),c("<2e-16","0.6912","0.6903","0.5020","0.9467","0.6784","0.9318","0.0426"),c("Sucesso","Fracasso","Fracasso","Fracasso","Fracasso","Fracasso","Fracasso","Sucesso"))
colnames(tabela_c) <- c("Teste ANOVA","p-valor","Resultado")
kable(tabela_c, booktabs = TRUE, valign = 't') %>% kable_styling(latex_options = "HOLD_position")

#Teste de Shapiro-Wilks e Breusch-Pagan para o novo modelo refinado
modelo4 <- lm(tips2 ~ total_bill + size, df)

breusch_m3 <- ncvTest(modelo4)

tabela_breusch_2 <- data.frame(c("Modelo"), c(breusch_m3$p), c("Sucesso"))
colnames(tabela_breusch_2) <- c("Teste de Breusch-Pagan","p-valor","Resultado")
kable(tabela_breusch_2, booktabs = TRUE, valign = 't') %>% kable_styling(latex_options = "HOLD_position")

shapiro_m3 <- shapiro.test(residuals(modelo4))

tabela_shapiro_2 <- data.frame(c("Modelo"), c(shapiro_m3$p.value), c("Sucesso"))
colnames(tabela_shapiro_2) <- c("Teste de Shapiro-Wilks","p-valor","Resultado")
kable(tabela_shapiro_2, booktabs = TRUE, valign = 't') %>% kable_styling(latex_options = "HOLD_position")

#Gr�fico residual 
residualPlot(modelo4)