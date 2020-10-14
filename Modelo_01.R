# Seta diretório de trabalho
setwd("C:/Users/Carlo/OneDrive/PMMT")

# Lista arquivos
nomes_arquivos <- list.files(path="C:/Users/Carlo/Downloads/Nova pasta/", pattern="t*.csv")

# Combina arquivos
dados <- do.call("rbind",lapply(nomes_arquivos,
                                FUN=function(arquivo){ read.csv(arquivo,
                                                                encoding="UTF-8", 
                                                                header=TRUE)}))
# Renomeia a primeira coluna.
colnames(dados)[1] <- 'bo'

#Salva arquivo.
write.csv(dados, 'dados209-2020p.csv')

# Cria fator para os municípios.
dados$Municipio.Fato <- factor(dados$Municipio.Fato)

# Converte campo de data do fato.
# install.packages('lubridate')
# library(lubridate)

dados$Data <- as.Date(parse_date_time(dados[["Data.Fato"]], '%Y-%m-%d'))

dados$mes_data <- format(dados$Data, "%Y-%m")
 

# Agrega.
n_municipio_dados <- aggregate(bo ~ Municipio.Fato + Natureza.Ocorrencia + mes_data, 
                               data=dados, 
                               FUN=length)

n_municipio_dados$mes_data <- as.Date(parse_date_time(n_municipio_dados[["mes_data"]], 
                                                             '%Y-%m')
                                     )
# Salva arquivo.
write.csv(n_municipio_dados, 'dados_n_209-2020p.csv')

# Seleciona município e natureza.
municipio <- 'CUIABA'
natureza <- 'ROUBO'

n_municipio <-
  n_municipio_dados[n_municipio_dados$Municipio.Fato == municipio &
                    n_municipio_dados$Natureza.Ocorrencia == natureza, ]

# install.packages('tidyr')
library(tidyr)

# Cria serie numerica fechada para verificar ausentes.
aux <- data.frame(seq.Date(
  min(n_municipio$mes_data),
  max(n_municipio$mes_data),
  by = 'month'
))
colnames(aux) <- 'mes_data'



n_municipio <-
  merge(x = aux,
        y = n_municipio,
        by = 'mes_data',
        all.x = TRUE)

n_municipio$Municipio.Fato <- municipio
n_municipio$Natureza.Ocorrencia <- natureza
n_municipio[is.na(n_municipio$bo), 'bo'] <- 0

ts_n_municipio <- ts(n_municipio[, 'bo'],
                     start = c(year(n_municipio[1, 'mes_data']),
                               month(n_municipio[1, 'mes_data'])),
                     frequency = 12)

plot(ts_n_municipio)

# install.packages('forecast')
library(forecast)

l <- BoxCox.lambda(ts_n_municipio)
ajuste <- auto.arima(ts_n_municipio, lambda = l)
f <- forecast(ajuste, h = 5)

plot(f)

