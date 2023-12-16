library(foreign)
library(readxl)
library(tseries)
library(dplyr)
library(ggplot2)
library(scales) 
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
Grafico_infla<-ggplot(Inflacion, aes(x = Año, y = Tasa_Inflación)) +
  geom_line(color = "#87CEFA") +
  geom_point(color = "#EE9A00", size = 1) +
  geom_text(aes(label = round(Tasa_Inflación,2)), vjust = -0.5, hjust = -0.5, color = "black", size = 2.5)+
  labs(title = "",
       x = "Año", y = "Tasa de Inflación") +
  theme_minimal()+
  theme(
    plot.title = element_text(family = "Arial", hjust = 0.5),
    plot.background = element_rect(color = "black", size = 1, fill = "white"),
    panel.background = element_rect(fill = "white"),
    panel.grid = element_blank()
  )+
  scale_x_continuous(breaks = seq(min(Inflacion$Año), max(Inflacion$Año), by = 5))


ggsave("Grafico_infla.png", plot = Grafico_infla, width = 8, height = 4.5, dpi = 300)
#####################################################
### PIB
#####################################################
PIB <- read_excel("Datos/PIB.xlsx")
# Graficar el IPC a lo largo de los años
ggplot(PIB, aes(x = Año, y = Valor)) +
  geom_line(color = "#87CEFA") +
  geom_point(color = "#EE9A00", size = 1) +
  #geom_text(aes(label = round(Valor,2)), vjust = -0.5, hjust = -0.5, color = "black", size = 2.5)+
  labs(title = "Gráfico de la tasa de inflación en Honduras periodo 1992-2023",
       x = "Año", y = "Tasa de Inflación") +
  theme_minimal()+
  theme(
    plot.title = element_text(family = "Arial", hjust = 0.5),
    plot.background = element_rect(color = "black", size = 1, fill = "white"),
    panel.background = element_rect(fill = "white"),
    panel.grid = element_blank()
  )+
  scale_x_continuous(breaks = seq(min(PIB$Año), max(PIB$Año), by = 10))
plot(PIB)

# Suponiendo que 'Valor' está en millones, si no, ajusta la conversión
PIB$Valor_millones <- PIB$Valor / 1000000
ggplot(PIB, aes(x = Año, y = Valor_millones)) +
  geom_line(color = "#87CEFA") +
  geom_point(color = "#EE9A00", size = 1) +
  labs(title = "Gráfico del comportamiento del PIB en Honduras periodo 1992-2023",
       x = "Año", y = "Tasa de Inflación (Mil Millones $)") +
  scale_y_continuous(labels = label_comma()) +  # Formatear los números en el eje Y
  theme_minimal() +
  theme(
    plot.title = element_text(family = "Arial", hjust = 0.5),
    plot.background = element_rect(color = "black", size = 1, fill = "white"),
    panel.background = element_rect(fill = "white"),
    panel.grid = element_blank()
  ) +
  scale_x_continuous(breaks = seq(min(PIB$Año), max(PIB$Año), by = 10))
PIB$Valor_millones <- PIB$Valor / 1000000

# Filtrar los datos desde 1992
PIB_filtrado <- PIB %>% filter(Año >= 1992)

Grafico_Pib<-ggplot(PIB_filtrado, aes(x = Año, y = Valor_millones)) +
  geom_line(color = "#87CEFA") +
  geom_point(color = "#EE9A00", size = 1) +
  labs(x = "Año", y = "Tasa de Inflación (Mil Millones $)") +
  scale_y_continuous(labels = label_comma()) +
  theme_minimal() +
  theme(
    plot.title = element_text(family = "Arial", hjust = 0.5),
    plot.background = element_rect(color = "black", size = 1, fill = "white"),
    panel.background = element_rect(fill = "white"),
    panel.grid = element_blank()
  ) +
  scale_x_continuous(breaks = seq(min(PIB_filtrado$Año), max(PIB_filtrado$Año), by = 5))
ggsave("Grafico_Pib.png", plot = Grafico_Pib, width = 8, height = 4.5, dpi = 300)


PIB2 <- read_excel("Datos/PIB.xlsx", sheet = "Hoja2")

# Graficar el IPC a lo largo de los años
Grafico_Pib2<-ggplot(PIB2, aes(x = Año, y = Variacion)) +
  geom_line(color = "#87CEFA") +
  geom_point(color = "#EE9A00", size = 1) +
  geom_text(aes(label = round(Variacion,2)), vjust = -0.5, hjust = -0.5, color = "black", size = 2.5)+
  labs(title = "",
       x = "Año", y = "Variación del PIB") +
  theme_minimal()+
  theme(
    plot.title = element_text(family = "Arial", hjust = 0.5),
    plot.background = element_rect(color = "black", size = 1, fill = "white"),
    panel.background = element_rect(fill = "white"),
    panel.grid = element_blank()
  )+
  scale_x_continuous(breaks = seq(min(PIB2$Año), max(PIB2$Año), by = 5))
ggsave("Grafico_Pib2.png", plot = Grafico_Pib2, width = 8, height = 4.5, dpi = 300)
