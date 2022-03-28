setwd("/home/gustavo/Projeto de Dados/Análise_R/Seguro_Saude")
getwd()

library(tidyverse)
library(dplyr)
library(esquisse)
library(ggplot2)

seguro <- read.csv("seguro_saude.csv")

View(seguro)

## paste("R$", format(seguro$valor_seguro_saude, decimal.mark = ",", big.mark = ".", nsmall = 2))

View(seguro)

View(is.na(seguro))

## 1 - Qual o gasto total da operadora?

total_gasto <- sum(seguro$valor_seguro_saude)

total_gasto <- paste("R$", format(total_gasto, decimal.mark = ",", big.mark = ".", nsmall = 2))

total_gasto

## 2 - Qual a idade média dos usuários da operadora? 

media_operadora <- mean(seguro$idade, round(2))
media_operadora

## 3 - Qual o gasto médio por região? 

Gasto_media <- seguro %>% group_by(regiao) %>% summarise(Media = mean(valor_seguro_saude)) 

Gasto_media <- Gasto_media[(-1),]

Gasto_media$Media <- paste("R$", format(Gasto_media$Media, decimal.mark = ",", big.mark = ".", nsmall = 2))

View(Gasto_media)

## 4. Qual faixa etária possui maior gasto com seguro saúde por região?

## Adicionando uma nova coluna
seguro["Faixa_Etária"] <- c("18 a 30 anos", "31 a 49 anos", "50 a 64 anos")

## Agrupamento 
Gasto_regiao_idade <- seguro %>% group_by(regiao, Faixa_Etária) %>% summarise(Media = mean(valor_seguro_saude)) 

## Removendo linhas
Gasto_regiao_idade <- Gasto_regiao_idade[(-1),]

view(Gasto_regiao_idade)

## Conversão para moeda
Gasto_regiao_idade$Media <- paste("R$", format(Gasto_regiao_idade$Media, decimal.mark = ",", 
                                               big.mark = ".", nsmall = 2))
view(Gasto_regiao_idade)

## Ordenação de Valores
Gasto_regiao_idade %>% 
  arrange(Media)

View(Gasto_regiao_idade)

## 7. O aumento da idade influencia no imc?

## Gráfico de Dipersão com Linha de Tedência
seguro %>% 
  
  ggplot(aes(x=idade, y=imc)) + 
  labs(title = "Aumento de Influência do IMC por Idade ? Sim") +
  geom_point() + geom_smooth(method = "lm") 

## 8. Quem tem maior gasto, homens ou mulheres?

Gasto_sexo <- seguro %>% group_by(sexo) %>% summarise(Gasto = sum(valor_seguro_saude)) 

Gasto_sexo <- Gasto_sexo[(-1),]

Gasto_sexo$Gasto <- paste("R$", format(Gasto_sexo$Gasto, decimal.mark = ",", big.mark = ".", nsmall = 2))

View(Gasto_sexo)

## Gráfico de Barras 

data <-data.frame(
  
  Sexo <- Gasto_sexo$sexo, 
  Valor_Gasto <- Gasto_sexo$Gasto
)

ggplot(data, aes(x=Sexo, y=Valor_Gasto)) + 
  labs(title = "Valor Gasto de Seguro Saúde por Sexo") +
  geom_bar(stat = "identity") + coord_flip()

## 9. Se o usuário for mulher, o imc é acima ou abaixo da média?

Sexo_imc <- seguro %>% group_by(sexo) %>% summarise(Media = median(seguro$imc))

Sexo_imc <- Sexo_imc[(-1),]

View(Sexo_imc)

data <- data.frame(
  
  Sexo <- Sexo_imc$sexo, 
  Imc <- Sexo_imc$Media
)

ggplot(data, aes(x=Sexo, y=Imc)) + 
  labs(title = "Média de Sexo por Imc, Acima do Peso") + 
  geom_bar(stat = "identity") + coord_flip()

## 10. Se for homem, com mais de 50 anos e da região Sudeste, o gasto é
## maior ou menor que a média de gastos da região

Sexo_idade_regiao <- seguro %>% group_by(regiao, sexo, Faixa_Etária) %>% summarise(Media = mean(valor_seguro_saude))

Sexo_idade_regiao <- Sexo_idade_regiao[(-1),]

Sexo_idade_regiao$Media <- paste("R$", format(Sexo_idade_regiao$Media, decimal.mark = ",", big.mark = ".", nsmall = 2))

View(Sexo_idade_regiao)

## 1 Gráfico 
Regiao <- c(rep("nordeste", 3), rep("norte"), rep("sudeste"), rep("sul"))
Faixa_Etaria <- rep(c("18 a 30 anos", "31 a 49 anos", "50 a 64 anos"),4)
Media_seguro <-Sexo_idade_regiao$Media

data <- data.frame(Regiao, Faixa_Etaria, Media_seguro)

ggplot(data, aes(fill = Faixa_Etaria, y=Media_seguro, x=Regiao)) + 
  labs(title = "Média de Sexo por Imc, Acima do Peso") + 
  geom_bar(position = "dodge", stat = "identity")

## 2 Gráfico 
esquisser(Sexo_idade_regiao)

library(ggplot2)

ggplot(Sexo_idade_regiao) +
  aes(x = regiao, y = Media, fill = Faixa_Etária) +
  geom_tile(size = 1.2) +
  scale_fill_hue(direction = 1) +
  labs(title = "Valor Gasto por Sexo, Faixa etária e Região", subtitle = "Valor Médio Gasto pelos Homens com mais de 50 anos") +
  theme_dark() +
  facet_wrap(vars(sexo))

