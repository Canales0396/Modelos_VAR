library(foreign)
library(readxl)
library(tseries)
##########################################################
#        Datos históricos de la inflación
#########################################################
TInflacion <- read_excel("Modelos_VAR/Datos/Inflacion.xlsx",sheet = "TInflación")
plot(TInflacion)
