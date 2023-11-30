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

##########

library(vars)

# Crear una serie temporal con los datos de IPC por año
ipc_ts <- ts(Inflacion$Tasa_Inflación, start = min(Inflacion$Año), frequency = 1)

# Ajustar un modelo VAR con dos rezagos (lags)
modelo_var <- arima(ipc_ts, order = c(1, 0, 0))

# Hacer una predicción para los próximos 5 periodos
prediccion_var <- predict(modelo_var, n.ahead = 5)

# Mostrar los resultados de la predicción
print(prediccion_var)

# Graficar la serie temporal histórica junto con la predicción
plot(ipc_ts, xlim = c(min(time(ipc_ts)), max(time(ipc_ts)) + 5), ylim = c(min(ipc_ts), max(ipc_ts) + 1),
     xlab = "Año", ylab = "IPC Promedio", main = "Predicción con modelo ARIMA")
lines(prediccion_var$pred, col = "red")  # Agregar la línea de la predicción
legend("topleft", legend = c("Histórico", "Predicción"), col = c("black", "red"), lty = 1)
