library(tidyverse)
library(lubridate)
library(tsibble)
library(fable)
library(broom)

#Lectura de datos y cambio de nombre a formato más deseable
data <- read_csv2("table.csv")
dates_vector <- as.character(dmy(names(data)[c(-1,-2)]))
names(data)[c(-1,-2)] = dates_vector

#Construyendo DF con 75 SKU en tidy data
agrupado <- data[1:75,] %>% 
  replace(., is.na(.), 0) %>% 
  gather(date, valor, -codigo, -descripcion) %>%
  mutate(date = ymd(date)) %>% 
  arrange(codigo) %>% 
  write_csv("agrupado.csv")

#Identificando productos con mayor demanda
agrupado %>% group_by(codigo) %>% 
  summarize(demanda_anual = sum(valor, na.rm = T)) %>%
  arrange(desc(demanda_anual))

#Construyendo modelo lineal con tendencia y estacionalidad para los 75 SKU seleccionados

agrupado_tsibble <- agrupado %>%
  mutate(date = yearmonth(date)) %>% 
  as_tsibble(key = id(codigo), index = date)

champion_fitted <-  agrupado_tsibble %>%  
  model(
    "Global con indicadoras" = TSLM(valor ~ trend() + season()),
    Arima = ARIMA(valor),
    "Suavizamientos exponenciales" = ETS(box_cox(valor, 0.3))
  )

#Enviando a .csv los resultados de ajuste del modelo
champion_ajuste <- champion_fitted %>% 
  augment(model) %>% 
  write_csv("champion_ajuste.csv")

#Enviando a .csv los indicadores de ajuste
champion_indicadores <- champion_fitted %>% 
  glance(model) %>% 
  write_csv("champion_indicadores.csv")

#enviando a .csv las predicciones a 12 meses
champion_predicciones <- champion_fitted %>% 
  forecast(h = 12) %>% 
  write_csv("champion_predicciones.csv")


#Método autoplot para SKU específico
champion_predicciones %>% 
  filter(codigo == 18209) %>% 
autoplot(filter(agrupado_tsibble, year(date) >= 2016),level = NULL)

#Autoplot de serie + predicciones dependientes únicamente de la página
library(ggforce)
champion_predicciones %>% 
  autoplot(filter(agrupado_tsibble, year(date) >= 2016),level = NULL) +
  facet_wrap_paginate(~codigo, ncol = 2, nrow = 2, scales = "free_y", page = 3) +
  theme_bw() +
  theme(legend.position = "right", axis.text.x = element_text(angle = 45, hjust = 0.8)) 

#Creando versiones en factor de agrupado
agrupado_factor <- agrupado_tsibble %>% 
  mutate(codigo = as.factor(codigo))

#Creando una gráfica que contiene todas las series en un frame 3x5
ggplot(agrupado_factor, aes(x = date, y = valor)) +
geom_line() +
  facet_wrap_paginate(~codigo, ncol = 3, nrow = 5, scales = "free_y", page = 5)