library(foreign)
library(readxl)
library(tseries)
library(dplyr)
library(ggplot2)
##########################################################
#        Datos históricos del Indice de Precios Al Consumidor
#########################################################
IPC<- read_excel("Datos/Índice de Precios al Consumidor.xlsx")
# Convertir la fecha a año
IPC <- IPC %>%
  mutate(Año = lubridate::year(Fecha))

# Calcular el promedio por año para el indicador Variación Interanual que es 
## la inflación
Inflacion<- IPC %>%
  filter(Indicador == "Variación interanual") %>%
  group_by(Año) %>%
  summarise(Tasa_Inflación =round(mean(Valor),2))

#########################################
# Grafico para ver el comportamiento de la tasa de inflación 
##########################
# Graficar el IPC a lo largo de los años
ggplot(Inflacion, aes(x = Año, y = Tasa_Inflación)) +
  geom_line(color = "#87CEFA") +
  geom_point(color = "#EE9A00", size = 1) +
  geom_text(aes(label = round(Tasa_Inflación,2)), vjust = -0.5, hjust = -0.5, color = "black", size = 2.5)+
  labs(title = "Gráfico de la tasa de inflación en Honduras periodo 1992-2023",
       x = "Año", y = "Tasa de Inflación") +
  theme_minimal()+
  theme(
    plot.title = element_text(family = "Arial", hjust = 0.5),
    plot.background = element_rect(color = "black", size = 1, fill = "white"),
    panel.background = element_rect(fill = "white"),
    panel.grid = element_blank()
  )+
  scale_x_continuous(breaks = seq(min(Inflacion$Año), max(Inflacion$Año), by = 5))

