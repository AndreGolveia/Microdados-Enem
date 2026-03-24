library(ggplot2)
library(readr)
library(summarytools)
#install.packages("summarytools")
install.packages("questionr")
library(questionr)

main <- read_csv("train.csv", locale = locale(decimal_mark = ",", 
                                              grouping_mark = ".", encoding = "WINDOWS-1252"))
View(main)

data(main)


# Sumarios ----------------------------------------------------------------


summary(main$NU_NOTA_REDACAO)
summary(main$NU_IDADE)
summary(main$SG_UF_RESIDENCIA)
summary(main$TP_ST_CONCLUSAO)


# Usando summarytools -----------------------------------------------------

idade <- main$NU_IDADE

range(idade)

breaks = seq(13, 67, by=5)

idade.cut = cut(idade, breaks, right=FALSE)

freq(idade.cut)

freq(main$NU_IDADE)

#----nota redação-------

redacao <- main$NU_NOTA_REDACAO

breaks2 = seq(0, 1000, by = 100)

redacao.cut = cut(redacao, breaks2, right=FALSE)

freq(redacao.cut)

#----------------

freq(main$NU_IDADE)

freq(main$SG_UF_RESIDENCIA)

freq(main$TP_ST_CONCLUSAO)

freq(main$NU_NOTA_REDACAO)

freq(main$TP_SEXO)

freq(main$TP_ENSINO)

freq(main$Q006)

# Usando questionr --------------------------------------------------------

questionr::freq(main$NU_IDADE, cum = TRUE, total = TRUE)
