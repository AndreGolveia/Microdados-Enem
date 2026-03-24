library(ggplot2)
library(readr)

test <- read_csv("test.csv", locale = locale(decimal_mark = ",", 
                                             grouping_mark = ".", encoding = "WINDOWS-1252"))
View(test)

main <- read_csv("train.csv", locale = locale(decimal_mark = ",", 
                                             grouping_mark = ".", encoding = "WINDOWS-1252"))
View(main)

# Idade X Nota ------------------------------------------------------------

g1 <- ggplot(data = main, mapping = aes(x = NU_IDADE, y = NU_NOTA_REDACAO)) +
        geom_point() +
        labs(title = "Idade X Nota da redação",
                 x = "Idade",
                 y = "Nota da redação")
            
g1

ggsave("idadeXnota.png", width = 6, height = 6, plot=g1)


# Boxplot -----------------------------------------------------------------


g2 <- ggplot(data = main, mapping = aes(x = NU_IDADE, y = NU_NOTA_REDACAO, group = NU_IDADE)) +
        geom_boxplot(fill = "orange") +
        labs(title = "Idade X Nota da redação",
             x = "Idade",
             y = "Nota da redação") 

g2

ggsave("boxplot-idadeXnota.png", width = 14, height = 6, plot=g2)

gradient <- ggplot(data = main, mapping = aes(x = NU_IDADE, y = NU_NOTA_REDACAO, group = NU_IDADE, fill = NU_IDADE)) +
              geom_boxplot() +
              labs(title = "Idade X Nota da redação",
                   x = "Idade",
                   y = "Nota da redação") +
              scale_fill_gradient(name = "Idade", low = "yellow", high = "red", na.value = NA)

gradient


# Nota por UF -------------------------------------------------------------

g3 <- ggplot(data = main, mapping = aes(x = SG_UF_RESIDENCIA, y = NU_NOTA_REDACAO)) +
        geom_point() +
        labs(title = "UF X Nota da redação",
             x = "Unidades federativas",
             y = "Nota da redação")

g3

g4 <- ggplot(data = main, mapping = aes(x = SG_UF_RESIDENCIA, y = NU_NOTA_REDACAO, group = SG_UF_RESIDENCIA, fill = SG_UF_RESIDENCIA)) +
        geom_boxplot(fill = "orange") +
        labs(title = "UF X Nota da redação",
             x = "Unidades federativas",
             y = "Nota da redação") 

g4

ggsave("boxplot-UFXNota da redação.png", width = 14, height = 6, plot=g4)


?ggplot
?aes
?scale_fill_brewer
?geom_boxplot
?scale_x_continuous
?rescale
?factor
vignette("ggplot2-specs")


# Conclusão do Ensino Medio -----------------------------------------------

sca <- c("Concluío \n o Ensino Médio", "Está cursando\n e concluira em 2016", "Está cursando\n e concluira após 2016", "Não concluíu\n e não está cursando")

g5 <- ggplot(data = main, mapping = aes(x = TP_ST_CONCLUSAO, y = NU_NOTA_REDACAO, group = TP_ST_CONCLUSAO, fill = TP_ST_CONCLUSAO)) +
        geom_boxplot(fill = "orange") +
        labs(title = "Conclusão do ensino médio X Nota da redação",
             x = "Conclusão do ensino médio",
             y = "Nota da redação") + 
        scale_x_continuous(breaks = c(1,2,3,4), labels = sca) +
        stat_summary( fun ="median", geom = "point")

g5

#g5$stats

b1 <- boxplot(main$NU_NOTA_REDACAO ~ main$TP_ST_CONCLUSAO, 
              col= "orange" , 
              ylab="Nota da redação" , xlab="Conclusão do ensino médio")

b1

ggsave("boxplot-conclusãoXnota.png", width = 8, height = 6, plot=g5)


# Teste sexo --------------------------------------------------------------

tps <- ggplot(data = main, mapping = aes(x = TP_SEXO, y = NU_NOTA_REDACAO, group = TP_SEXO, fill = TP_SEXO)) +
        geom_boxplot(fill = "orange") +
        labs(title = "Feminino/Masculino X Nota da redação",
             x = "Sexo",
             y = "Nota da redação") 

tps

ggsave("boxplot-sexo.png", width = 8, height = 6, plot=tps)


# Tipo de ensino ----------------------------------------------------------

g6 <- ggplot(data = main, mapping = aes(x = TP_ENSINO, y = NU_NOTA_REDACAO, group = TP_ENSINO, fill = TP_ENSINO)) +
        geom_boxplot(fill = "orange") +
        labs(title = "Tipo de ensino X Nota da redação",
             x = "Ensino",
             y = "Nota da redação") +
        scale_x_continuous(breaks = c(1,2,3), labels = c("Ensino Regular",
                                                         "Educação Especial - \nModalidade Substitutiva",
                                                         "Educação de \nJovens e Adultos"))
        
g6


ggsave("boxplot-ensino.png", width = 8, height = 6, plot=g6)


# Renda -------------------------------------------------------------------

g7 <- ggplot(data = main, mapping = aes(x = Q006, y = NU_NOTA_REDACAO, group = Q006, fill = Q006)) +
        geom_boxplot(fill = "orange") +
        labs(title = "Tipo de renda mensal X Nota da redação",
             x = "Valor da Renda",
             y = "Nota da redação") +
        scale_x_discrete(breaks = c("A",'B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q'), 
                         labels = c("Nenhuma renda.",
                                    "At é R$ 880,00.",
                                    "R$ 880,01 - R$ 1.320,00.",
                                    "R$ 1.320,01 - R$ 1.760,00.",
                                    "R$ 1.760,01 - R$ 2.200,00.",
                                    "R$ 2.200,01 - R$ 2.640,00.",
                                    "R$ 2.640,01 - R$ 3.520,00.",
                                    "R$ 3.520,01 - R$ 4.400,00.",
                                    "R$ 4.400,01 - R$ 5.280,00.",
                                    "R$ 5.280,01 - R$ 6.160,00.",
                                    "R$ 6.160,01 - R$ 7.040,00.",
                                    "R$ 7.040,01 - R$ 7.920,00.",
                                    "R$ 7.920,01 - R$ 8.800,00.",
                                    "R$ 8.800,01 - R$ 10.560,00.",
                                    "R$ 10.560,01 - R$ 13.200,00.",
                                    "R$ 13.200,01 - R$ 17.600,00.",
                                    "Mais de R$ 17.600,00." )) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))


g7

ggsave("boxplot-renda-xaxis.png", width = 14, height = 6, plot=g7)

g8 <- ggplot(data = main, mapping = aes(y = Q006, x = NU_NOTA_REDACAO, group = Q006, fill = Q006)) +
  geom_boxplot(fill = "orange") +
  labs(title = "Tipo de renda mensal X Nota da redação",
       x = "Valor da Renda",
       y = "Nota da redação") +
  scale_y_discrete(breaks = c("A",'B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q'), 
                   labels = c("Nenhuma renda.",
                              "At é R$ 880,00.",
                              "R$ 880,01 - R$ 1.320,00.",
                              "R$ 1.320,01 - R$ 1.760,00.",
                              "R$ 1.760,01 - R$ 2.200,00.",
                              "R$ 2.200,01 - R$ 2.640,00.",
                              "R$ 2.640,01 - R$ 3.520,00.",
                              "R$ 3.520,01 - R$ 4.400,00.",
                              "R$ 4.400,01 - R$ 5.280,00.",
                              "R$ 5.280,01 - R$ 6.160,00.",
                              "R$ 6.160,01 - R$ 7.040,00.",
                              "R$ 7.040,01 - R$ 7.920,00.",
                              "R$ 7.920,01 - R$ 8.800,00.",
                              "R$ 8.800,01 - R$ 10.560,00.",
                              "R$ 10.560,01 - R$ 13.200,00.",
                              "R$ 13.200,01 - R$ 17.600,00.",
                              "Mais de R$ 17.600,00." )) 


g8

ggsave("boxplot-renda-yaxis.png", width = 14, height = 8, plot=g8)


# Conlusão do curso -------------------------------------------------------

g9 <- ggplot(data = main, mapping = aes(x = TP_ANO_CONCLUIU, 
                                        y = NU_NOTA_REDACAO, 
                                        group = TP_ANO_CONCLUIU, 
                                        fill = TP_ANO_CONCLUIU)) +
        geom_boxplot(fill = "orange") +
        labs(title = "Conclusão do ensino médio X Nota da redação",
             x = "Conclusão do ensino médio",
             y = "Nota da redação") +
        scale_x_continuous(n.breaks = 11,
                           breaks = c(0,1,2,3,4,5,6,7,8,9,10), 
                           labels = c("Não informado",
                                    "2015",
                                    "2014",
                                    "2013",
                                    "2012",
                                    "2011",
                                    "2010",
                                    "2009",
                                    "2008",
                                    "2007",
                                    "Anterior a 2007")) +
       theme(axis.text.x = element_text(angle = 45, hjust = 1))

g9 

ggsave("boxplot-conclusãoXnota.png", width = 12, height = 6, plot=g9)


# Tipo de escola ----------------------------------------------------------

g10 <- ggplot(data = main, mapping = aes(x = TP_ENSINO, 
                                         y = NU_NOTA_REDACAO, 
                                         group = TP_ENSINO, 
                                         fill = TP_ENSINO)) +
        geom_boxplot(fill = "orange") +
        labs(title = "Tipo de escola X Nota da redação",
             x = "Tipo de escola",
             y = "Nota da redação") +
        scale_x_continuous(breaks = c(1,2,3,4), labels = c("Não Respondeu",
                                                           "Pública",
                                                           "Privada",
                                                           "Exterior"))

g10


ggsave("boxplot-TipoEscola.png", width = 6, height = 6, plot=g10)


# Treineiro ---------------------------------------------------------------

g11 <- ggplot(data = main, mapping = aes(x = IN_TREINEIRO, 
                                         y = NU_NOTA_REDACAO, 
                                         group = IN_TREINEIRO, 
                                         fill = IN_TREINEIROO)) +
        geom_boxplot(fill = "orange") +
        labs(title = "Treineiro X Nota da redação",
             subtitle = "Indica se o inscrito fez a prova com intuito\n de apenas treinar seus conhecimentos",
             x = "Treineiro",
             y = "Nota da redação") +
        scale_x_continuous(breaks = c(0,1), labels = c("Não","Sim"))

g11

b2 <- boxplot(main$NU_NOTA_REDACAO ~ main$IN_TREINEIRO, 
              col= "orange" , 
              ylab="Nota da redação" , xlab="TTreineiro")

b2

ggsave("boxplot-Treineiro.png", width = 6, height = 6, plot=g11)


# Localização -------------------------------------------------------------

g12 <- ggplot(data = main, mapping = aes(x = TP_LOCALIZACAO_ESC, 
                                         y = NU_NOTA_REDACAO, 
                                         group = TP_LOCALIZACAO_ESC, 
                                         fill = TP_LOCALIZACAO_ESC)) +
        geom_boxplot(fill = "orange") +
        labs(title = "Localização(Escola) X Nota da redação",
             x = "Localizção",
             y = "Nota da redação") +
        scale_x_continuous(breaks = c(1,2), labels = c("Urbana","Rural"))

g12


ggsave("boxplot-LocalizacaoEscola.png", width = 6, height = 6, plot=g12)


# Teste histograma --------------------------------------------------------

nota <- ggplot(data = main, mapping = aes(x = NU_NOTA_REDACAO )) +
        geom_histogram(bins=30, color="black", fill="lightblue") +
        labs(title = "Nota da redação",
             x = "Nota da redação",
             y = "Contagem")

nota


ggsave("NotaDaProva.png", width = 6, height = 6, plot=nota)

math <- ggplot(data = main, mapping = aes(x = NU_NOTA_MT )) +
        geom_histogram(bins=30, color="black", fill="lightblue") +
        labs(title = "Nota Matematica",
             x = "Nota",
             y = "Contagem")

math


ggsave("NotaMatematica.png", width = 6, height = 6, plot=math)

allNota <- ggplot(data = main) +
            geom_freqpoly(mapping = aes(x = NU_NOTA_MT, colour = "Matematica"), bins = 10, size=1.2) +
            geom_freqpoly(mapping = aes(x = NU_NOTA_CH, colour = "Ciências Humanas" ), bins = 10, size=1.2) +
            geom_freqpoly(mapping = aes(x = NU_NOTA_CN, colour = "Ciências da Natureza" ), bins = 10, size=1.2) +
            geom_freqpoly(mapping = aes(x = NU_NOTA_LC, colour = "Linguagens e Códigos" ), bins = 10, size=1.2) +
            geom_freqpoly(mapping = aes(x = NU_NOTA_REDACAO*10, colour = "Redação" ), bins = 10, size=1.2) +
            labs(title = "Nota Geral",
                 x = "Nota",
                 y = "Contagem de pessoas",
                 colour = "Tipo de prova") +
            scale_x_continuous(n.breaks = 11, breaks = c(0,1000,2000,3000,4000,5000,6000,7000,8000,9000,10000)) 
          
allNota
?geom_histogram
?labs

ggsave("NotaGeral.png", width = 10, height = 6, plot=allNota)


tpescola <- ggplot(data = main, mapping = aes(x =TP_ESCOLA )) +
          geom_histogram(bins=30, color="black", fill="lightblue") +
          labs(title = "Tipo de ensino",
               x = "Tipo de escola",
               y = "Contagem") +
          scale_x_continuous(breaks = c(1,2,3,4), labels = c("Não Respondeu",
                                                     "Pública",
                                                     "Privada",
                                                     "Exterior"))

tpescola

ggsave("TipoDeEscola.png", width = 6, height = 6, plot=tpescola)


# Lingua estrangeira ------------------------------------------------------

g13 <- ggplot(data = main, mapping = aes(x = TP_LINGUA, 
                                         y = NU_NOTA_REDACAO, 
                                         group = TP_LINGUA, 
                                         fill = TP_LINGUA)) +
  geom_boxplot(fill = "orange") +
  labs(title = "Língua estrangeira X Nota da redação",
       x = "Escolha da língua",
       y = "Nota da redação") +
  scale_x_continuous(breaks = c(0,1), labels = c("Inglês","Espanhol"))

g13

ggsave("LinguaEstrangeira.png", width = 6, height = 6, plot=g13)

# Competências ------------------------------------------------------------

g14 <- ggplot(data = main, mapping = aes(x = NU_NOTA_COMP1, 
                                         y = NU_NOTA_LC,
                                         fill = NU_NOTA_COMP1,
                                         group = NU_NOTA_COMP1)) +
          geom_boxplot(fill = "orange") +
          labs(title = "Nota da prova de linguagens e códigos X Nota da competência 1\n na redação",
               y = "Nota em línguagens e códigos",
               x = "Nota da redação")

g14

ggsave("boxplot-LinguagensXcompetencia.png", width = 6, height = 6, plot=g14)

g15 <- ggplot(data = main, mapping = aes(x = NU_NOTA_COMP3, 
                                         y = NU_NOTA_MT,
                                         fill = NU_NOTA_COMP3,
                                         group = NU_NOTA_COMP3)) +
          geom_boxplot(fill = "orange") +
          labs(title = "Nota da prova de Matemática X Nota da competência 3 na redação",
               y = "Nota em Matemática",
               x = "Nota da redação")

g15

ggsave("boxplot-MatematicaXcompetencia.png", width = 6, height = 6, plot=g15)

g16 <- ggplot(data = main, mapping = aes(x = NU_NOTA_COMP5, 
                                         y = NU_NOTA_CH,
                                         fill = NU_NOTA_COMP5,
                                         group = NU_NOTA_COMP5)) +
  geom_boxplot(fill = "orange") +
  labs(title = "Nota da prova de ciências humanas X Nota da competência 5\n na redação",
       y = "Nota em Ciências humanas",
       x = "Nota da redação")

g16

ggsave("boxplot-CienciasHumanasXcompetencia.png", width = 6, height = 6, plot=g16)


# competencia 5 x idade-------------------------------------------------------------------------


g17 <- ggplot(data = main, mapping = aes(x = NU_IDADE, y = NU_NOTA_COMP5, group = NU_IDADE, fill = Nu_IDADE)) +
  geom_boxplot(fill = "orange") +
  labs(title = "Idade X Nota da competência 5\n da redação",
       x = "Idade",
       y = "Nota da competência") 

g17

ggsave("boxplot-idadeXcompetencia.png", width = 14, height = 6, plot=g17)



# Redação X linguagens ----------------------------------------------------

tabelaLC <- ggplot(data = main, mapping = aes(x = NU_NOTA_REDACAO, y = NU_NOTA_LC, group = NU_NOTA_REDACAO, fill = NU_NOTA_REDACAO)) +
  geom_boxplot(fill="orange") +
  labs(title = "Nota de Linguagens X Nota da redação",
       x = "Nota de Redação",
       y = "Nota de Linguagens")

tabelaLC

?geom_boxplot

ggsave("boxplot-redacaoXlinguagens.png", width = 14, height = 6, plot=tabelaLC)
