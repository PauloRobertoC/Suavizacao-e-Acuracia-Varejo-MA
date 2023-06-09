### series temporais - suaviza��o e acur�cia
### previs�o do consumo do varejo no Maranh�o
### autor: Paulo Roberto Carneiro

library(BETS)
# Pegando as s�ries a partir do site do Banco Central do Brasil
# �ndice de volume de vendas no varejo Total do Maranh�o
# mensal a partir de jan/2000 at� fev/2020 
# 242 observa��es mensais
varejoma <- BETSget(1463) 
print(varejoma)
class(varejoma)
dput(varejoma)  # op��o para ter os dados como na structure abaixo
View(varejoma)

### primeiras an�lises 

#Inicialmente olharei as estat�sticas descritivas da s�rie. 
#Em seguida farei um plot b�sico da s�rie e o plot pelo pacote dygraphs, 
#�til para ver os pontos de picos e momentos espec�ficos.

# estatisticas basicas
summary(varejoma)

# pelo pacote dygraph d� mais op��es
library(dygraphs)
dygraph(varejoma)

### Suaviza��o (ou alisamento) Exponencial Simples (SES) ou ainda, �Holt 1 par�metro�
#O forecast ser� constante para todas as observa��es futuras!!!
library(forecast)

x <- varejoma
# simple exponential smoothing (SES)

x.ses <- ses(x, h = 12, level = c(95))
x.ses  # exibe o forecast por holt 1 parametro

summary(x.ses)  # fornece o relatorio da estimacao

plot(x.ses, col = 1, main = "�ndice de volume de vendas no varejo Total 
     do Maranh�o", 
     sub = "modelo SES")

### Suaviza��o Holt-Winters - 3 par�metros
# Holt-Winters

x.hwm <- hw(x, seasonal = "multiplicative")  # multiplicativo
plot(x, ylab = "Indice 2011=100", main = "�ndice de volume de vendas no varejo Total do Maranh�o", 
     type = "o", xlab = "M�s/Ano")
lines(fitted(x.ses), col = "red", lty = 2)
lines(x.ses$mean, type = "o", col = "red")
lines(x.hwm$mean, type = "o", col = "green")
legend("topleft", lty = 1, pch = 1, col = 1:3, c("original", "Holt Simples", 
                                                 "Holt Winters' Multiplicativo"))

summary(x.hwm) #dados da previs�o (simula��o/ teste)

### Avalia��o da simula��o

#Alguns crit�rios para avalia��o da previs�o podem ser citados: 
#Raiz do Erro de Simula��o Quadr�tico M�dio; Raiz do Erro Quadr�tico M�dio Percentual; 
#Erro de Simula��o M�dio; Erro Percentual M�dio; Erro Absoluto M�dio; 
#Erro Absoluto Percentual M�dio; 
#Raiz do Erro de Simula��o Ex-post Quadr�tico M�dio; 
#Coeficiente de desigualdade de Theil (U).

### Extraindo amostras de teste

#A fun��o window auxiliar� na extra��o de uma parte de uma s�rie temporal, 
#e cria��o dos subsets de treino e teste no forecast. 
#Na fun��o window especificamos o in�cio (start) (e/ou fim: end) de modo a definir o subset. 
#O treino � onde ser� obtido o modelo estimado e o teste � onde verificaremos o acerto, ou acur�cia, do modelo. 
#Identificado o melhor modelo, faz-se ent�o o forecast fora da amostra. 
#A amostra de teste � em geral 20% da amostra existente.

library(fpp2)

# farei as janelas de teste e treino, estimarei os modelos para a amostra
# treino modelo 1: cons.ses modelo 2: cons.holttrend.EXP modelo 3:
# cons.holttrend.DAMPm modelo 4: conshw2

# amostra treino at� julho 2015 (187 meses)
cons <- window(varejoma, start = c(2000, 1), end = c(2015, 7))

# amostra teste (47 meses at� junho 2019)
teste <- window(varejoma, start = c(2015, 8))

#Vamos checar se separamos corretamente:
autoplot(varejoma) + autolayer(cons, series = "Training") + autolayer(teste, 
                                                                      series = "Test")

# estimar modelos
cons.ses <- ses(cons, h = 60, level = c(95))  # forecast at� jul/2020
cons.holttrend.EXP <- holt(cons, h = 60, level = c(95), exponential = TRUE)
cons.holttrend.DAMPm <- holt(cons, h = 60, level = c(95), exponential = TRUE, 
                             damped = TRUE)

conshw2 <- hw(cons, h = 60, seasonal = "multiplicative")  # multiplicativo

autoplot(varejoma) + forecast::autolayer(cons.ses, series = "SES", PI = FALSE) + 
  forecast::autolayer(cons.holttrend.EXP, series = "Holt Expon. Trend", PI = FALSE) + 
  forecast::autolayer(cons.holttrend.DAMPm, series = "Holt Trend Damped mult", 
                      PI = FALSE) + forecast::autolayer(conshw2, series = "Holt-Winters mult", 
                                                        PI = FALSE) + xlab("Ano") + ylab("Consumo") + ggtitle("Forecasts para consumo do varejo MA") + 
  guides(colour = guide_legend(title = "Forecast"))

summary(cons.ses)

summary(cons.holttrend.EXP)

summary(cons.holttrend.DAMPm)

summary(conshw2)

#Acur�cia
#As previs�es acima foram feitas com a amostra treino at� julho/2015, 
#mas temos dados at� fev/2020. Faremos a acur�cia para este per�odo. 
#A fun��o accuracy nos fornece v�rias medidas de acur�cia, 
#e para a sMAPE precisaremos do pacote Metrics e a fun��o smape

library(knitr)
teste <- window(varejoma, start = c(2015, 8))
kable(forecast::accuracy(cons.ses, teste), caption = "SES")

#resultado
#|             |        ME|     RMSE|      MAE|       MPE|     MAPE|     MASE|      ACF1| Theil's U|
|:------------|---------:|--------:|--------:|---------:|--------:|--------:|---------:|---------:|
  |Training set |  1.596318| 8.244631| 4.991772|  1.385804| 8.455203| 0.918377| 0.0070999|        NA|
  |Test set     | -1.650645| 9.817709| 7.455899| -2.778010| 7.921218| 1.371722| 0.2932037| 0.9298144|

kable(forecast::accuracy(cons.holttrend.EXP, teste), caption = "Holt-Trend Exponencial")

#Holt-Trend Amortecida Multiplicativo
kable(forecast::accuracy(cons.holttrend.DAMPm, teste), caption = "Holt-Trend Amortecida Multiplicativo")


#Holt-Winters Multiplicativo
kable(forecast::accuracy(conshw2, teste), caption = "Holt-Winters Multiplicativo")

#Para o pacote Metrics, observar que ele tem fun��es de mesmo nome que o pacote forecast, 
#e portanto, deves especificar exatamente qual o pacote e cuidar para alterar a ordem das s�ries (actual x predicted). 
#No pacote Metrics existe uma diferen�a da ordem de 102 comparando com os resultados do pacote forecast!

library(Metrics)
teste <- window(varejoma, start = c(2015, 8))
smape(teste, cons.ses$mean)

smape(teste, cons.holttrend.EXP$mean)

smape(teste, cons.holttrend.DAMPm$mean)

smape(teste, conshw2$mean)

Metrics::mape(teste, cons.ses$mean)

Metrics::mape(teste, cons.holttrend.EXP$mean)

Metrics::mape(teste, cons.holttrend.DAMPm$mean)

Metrics::mape(teste, conshw2$mean)

#Coment�rio: O modelo estimado utilizando a amostra de treino perde qualidade em virtude da forte queda ap�s 2015, 
#que a amostra n�o consegue perceber. Isso � ocasionado em quebras na s�rie, 
#de modo que o forecast ficou prejudicado pela escolha da amostra treino. 
#Observa-se isso no item 5, quando o forecast foi realizado sem delimitar a amostra, 
#realizando a previs�o com todo o conjunto de informa��es.

library(ggplot2)
autoplot(varejoma) + forecast::autolayer(x.hwm, series = "Holt-Winters mult", 
                                         PI = FALSE) + xlab("Ano") + ylab("Consumo - Indice 2011=100") + ggtitle("Forecasts para �ndice de volume de vendas no varejo Total 
          do Maranh�o") + 
  guides(colour = guide_legend(title = "Forecast"))

#Outra op��o pelo pacote stats e fun��o HoltWinters:

hw <- stats::HoltWinters(varejoma)
predicted <- predict(hw, n.ahead = 24, prediction.interval = TRUE)

library(dygraphs)
dygraph(predicted, main = "Previs�o do Varejo do Maranh�o") %>% dyAxis("x", drawGrid = TRUE) %>% 
  dySeries(c("lwr", "fit", "upr"), label = "Varejo") %>% dyOptions(colors = RColorBrewer::brewer.pal(3, 
                                                                                                     "Set1"))

# combine the time series actual and forcasted values
combined <- cbind(predicted, actual = varejoma)

# plot the different values as different series
dygraph(combined, main = "Previs�o do Varejo do Maranh�o - 24 meses") %>% dyAxis("x", 
                                                                           drawGrid = TRUE) %>% dySeries("actual", label = "Observado") %>% dySeries(paste0("predicted.", 
                                                                                                                                                            c("lwr", "fit", "upr")), label = "Previsto") %>% dyOptions(colors = RColorBrewer::brewer.pal(3, 
                                                                                                                                                                                                                                                         "Set1"), drawPoints = TRUE, pointSize = 2) %>% dyRangeSelector() %>% dyHighlight(highlightCircleSize = 4, 
                                                                                                                                                                                                                                                                                                                                          highlightSeriesBackgroundAlpha = 0.5, hideOnMouseOut = TRUE) %>% dyEvent("2018-01-01", 
                                                                                                                                                                                                                                                                                                                                                                                                                   "2018", labelLoc = "bottom") %>% dyEvent("2019-01-01", "2019", labelLoc = "bottom") %>% 
  dyLegend(show = "follow")




























































































































































































































