#----------Kaggle - Análise de Adimissões nas Universidades EUA----------------
# Abrindo o arquivo para as análises
library(readxl)
admission_dataframe <- read.csv("C:/Users/rborges/../CodeLab/Datalab/Dataset - Graduation Program/Admission_Predict.csv")
View(admission_dataframe)

#Estatistícas Descritivas
summary(admission_dataframe)

# Variável Y = Chance de Admissão, Quais variáveis importam para criarmos um modelo preditivo?
library(PerformanceAnalytics)
chart.Correlation(admission_dataframe)

#Remover Serial No.
admission_dataframe <- admission_dataframe[,-1]
attach(admission_dataframe)
# Primeiro ajuste
# Montar um ajuste linear
ajuste_admission_1 <- lm(Chance.of.Admit ~ GRE.Score + TOEFL.Score + University.Rating 
                         + SOP + LOR + CGPA + factor(Research))

summary(ajuste_admission_1)

# Remover University Rating e SOP

#------CHECAR MULTICOLINEARIDADE------
library(car)
vif(ajuste_admission_1)

#Remover CGPA por multicolinearidade

#-------USAR SETP WISE----

library(MASS)
stepAIC(ajuste_admission_1, direction = "both")

# Manter GRE SCORE TOEFL SCORE LOR CGPA e factor(Research)

ajuste_admission_2 = lm(Chance.of.Admit ~ GRE.Score + TOEFL.Score + LOR + 
     CGPA + factor(Research))
summary(ajuste_admission_2)
vif(ajuste_admission_2)
par(mfrow = c(2,2))
plot(ajuste_admission_2)
plot(Chance.of.Admit ~ CGPA)
plot(Chance.of.Admit ~ TOEFL.Score)
plot(Chance.of.Admit ~ LOR)

# Modelo preditivo fica sendo 
# y_hat = -1.29 + 0.0017 * GRE Score + 0.0030 
#         * TOEFL Score + 0.022 * LOR Score + 0.121 * CGPA + 0.02457 * Research

# Falta verificar em uma tabela a assertividade do modelo acima com os dados reais.
