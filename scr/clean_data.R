###############################################################################
############################## CLEAN DATA #####################################
###############################################################################

################### Se realiza limpieza del environment ########################

rm(list = ls())

######### Se cargan los paquetes considerados necesarios para el script ########

require(pacman)

p_load(rio,
       tidyverse,
       skimr,
       caret,
       readxl,
       rvest,
       stargazer,
       mice)

library(tidyverse)
library(rio)
library(openxlsx)
library(ggplot2)
library(dplyr)
library(tidyr)
library (mice)
library(rvest)
library (skimr)
library(tibble)

############################## Se asigna el directorio #########################

base_filtrada <- read.csv("~/MeCA - Gestión de Proyectos de Investigación y Ciencia Abierta/Semana 7/taller4-GPI/data_raw/base_filtrada.csv")


######## Imputación de NA en la variable de Salario

install.packages("mice")
library(mice)

columns <- colnames(base_filtrada)
base_filtrada2 <- mice(base_filtrada[,names(base_filtrada) %in% columns],m = 1,
                       maxit = 1, method = "norm.nob",seed = 2018,print=F)

base_filtrada2 <- mice::complete(base_filtrada2)
par(mfrow=c(1,1))

# Guardar el gráfico como PNG
png("C:/Users/user/Documents/MeCA - Gestión de Proyectos de Investigación y Ciencia Abierta/Semana 7/taller4-GPI/results/salario_por_hora.png", width = 800, height = 600)

# Generar el gráfico
plot(density(base_filtrada$y_total_m_ha,na.rm = T),col=2,main="Salario Por Hora")
lines(density(base_filtrada2$y_total_m_ha),col=3)

# Cerrar el dispositivo gráfico y guardar el archivo
dev.off()



