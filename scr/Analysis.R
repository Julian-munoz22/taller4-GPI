
########################## Estadísticas descriptivas ###########################

# Se pasa la base a tibble para generar un reporte de estadísticas descriptivas 

library(tibble)

base_tibble<- as_tibble(base_filtrada2)
write.csv(base_filtrada2, "C:/Users/user/Documents/MeCA - Gestión de Proyectos de Investigación y Ciencia Abierta/Semana 7/taller4-GPI/clean_data.csv", row.names = FALSE)

## generamos un reporte de las descriptivas

descriptivas <- skim(base_tibble)

## se exporta el reporte de estadísticas

install.packages('openxlsx')
library(openxlsx)

write.csv(descriptivas, "C:/Users/user/Documents/MeCA - Gestión de Proyectos de Investigación y Ciencia Abierta/Semana 7/taller4-GPI/descriptivas.csv", row.names = FALSE)

##### Gráficos para interpretación

# Histograma edad

edad <- ggplot(data = base_filtrada,
               mapping = aes(x = age))  + 
  geom_histogram(aes(y =after_stat(density)),
                 bins = 5,
                 position = 'identity',
                 color="black", fill="gray" ) +
  stat_function(fun = dnorm, xlim = c(min(base_filtrada$age),max(base_filtrada$age)), colour="darkgreen", linewidth=1,
                args = list(mean = mean(base_filtrada$age), 
                            sd = sd(base_filtrada$age))) + 
  labs(title = 'Distribución de edad',
       x = 'Edad',
       y = 'Frecuencia') + 
  theme_bw()
edad

ggsave("C:/Users/user/Documents/MeCA - Gestión de Proyectos de Investigación y Ciencia Abierta/Semana 7/taller4-GPI/results/distribucion_edad.png", plot = edad, width = 8, height = 6)

##Histograma ingreso

Ingreso <- ggplot(data = base_filtrada,
                  mapping = aes(x = y_total_m_ha))  + 
  geom_histogram(aes(y =after_stat(density)),
                 bins = 5,
                 position = 'identity',
                 color="black", fill="gray" ) +
  stat_function(fun = dnorm, xlim = c(min(base_filtrada$y_total_m_ha),max(base_filtrada$y_total_m_ha)), colour="darkgreen", linewidth=1,
                args = list(mean = mean(base_filtrada$y_total_m_ha), 
                            sd = sd(base_filtrada$y_total_m_ha))) + 
  labs(title = 'Distribución del ingreso',
       x = 'Ingreso',
       y = 'Frecuencia') + 
  theme_bw()

Ingreso

ggsave("C:/Users/user/Documents/MeCA - Gestión de Proyectos de Investigación y Ciencia Abierta/Semana 7/taller4-GPI/results/distribucion_edad.png", plot = Ingreso, width = 8, height = 6)


### relación ingreso edad

ing_edad <- ggplot(data=base_filtrada)+ 
  geom_smooth(mapping = aes(x =age, y =y_total_m_ha))+
  scale_y_continuous (labels=function(n) {format(n, scientific = FALSE)})+
  labs(x="Edad", y="Ingreso")

ggsave("C:/Users/user/Documents/MeCA - Gestión de Proyectos de Investigación y Ciencia Abierta/Semana 7/taller4-GPI/results/ingreso_edad.png", plot = ing_edad, width = 8, height = 6)

mapa <- ggplot(data=base_filtrada)+ 
  geom_point(mapping = aes(x =age, y =y_total_m_ha))+
  scale_y_continuous (labels=function(n) {format(n, scientific = FALSE)})+
  labs(x="Edad", y="Ingreso")

ggsave("C:/Users/user/Documents/MeCA - Gestión de Proyectos de Investigación y Ciencia Abierta/Semana 7/taller4-GPI/results/disp_ing_edad.png", plot = mapa, width = 8, height = 6)

### relación ingreso estrato

#Grafico de dispesión con línea de tendencia

png("C:/Users/user/Documents/MeCA - Gestión de Proyectos de Investigación y Ciencia Abierta/Semana 7/taller4-GPI/results/Est_ing.png", width = 8, height = 6, units = "in", res = 300)

scatter.smooth(base_filtrada$estrato1, base_filtrada$y_total_m_ha, span = 2/3, degree = 1,
               family = "symmetric",
               xlab = "Estrato", ylab = "Ingreso",
               evaluation = 50, col = "green")  
title("Relación entre Estrato e Ingreso")

dev.off()

# gráfico de cajas y bigotes

png("C:/Users/user/Documents/MeCA - Gestión de Proyectos de Investigación y Ciencia Abierta/Semana 7/taller4-GPI/results/bigote.png", width = 8, height = 6, units = "in", res = 300)

boxplot(base_filtrada$y_total_m_ha ~ base_filtrada$estrato1, 
        xlab = "Estrato", ylab = "Ingreso",
        main = "Ingreso vs Estrato")

dev.off()

<<<<<<< HEAD
######################### CAMBIO DIFERENTE ###################################
=======
################################################################################
################## NUEVO TITUTLO PARA MEJORAR LA VISUALIZACIÓN #################
################################################################################

>>>>>>> mejorar-visualizacion
## Nuevo modelo



library(randomForest)
library(ggplot2)
library(caret)
library(gridExtra)
library(reshape2)

# 📌 Generar datos sintéticos
set.seed(123)
n <- 200  # Número de observaciones
p <- 8    # Variables macroeconómicas predictoras

# Simulación de variables predictoras (factores macroeconómicos)
set.seed(123)
n <- 200  # Número de observaciones
p <- 8 
X <- as.data.frame(matrix(rnorm(n * p, mean = 100, sd = 20), nrow = n, ncol = p))
colnames(X) <- c("Tasa_Interes", "M3", "Desempleo", "Exportaciones", "Importaciones", "Gasto_Publico", "Inversion_Extranjera", "Tipo_Cambio")

# Variables objetivo (Inflación, TRM, PIB, MSCI COLCAP)
Y <- data.frame(
  Inflacion = 0.5 * X$Tasa_Interes - 0.3 * X$Desempleo + rnorm(n, 0, 2),
  TRM = 1.2 * X$Tipo_Cambio + 0.4 * X$M3 + rnorm(n, 0, 5),
  PIB = 0.7 * X$Inversion_Extranjera + 0.6 * X$Gasto_Publico + rnorm(n, 0, 10),
  MSCI_COLCAP = 0.9 * X$Exportaciones - 0.5 * X$Importaciones + rnorm(n, 0, 15)
)

# 📌 Combinar datos
datos <- cbind(X, Y)

# 📌 Separar en entrenamiento (80%) y prueba (20%)
set.seed(123)
trainIndex <- createDataPartition(datos$Inflacion, p = 0.8, list = FALSE)
trainData <- datos[trainIndex, ]
testData  <- datos[-trainIndex, ]

# 🌲 Entrenar modelos Random Forest para cada variable económica
set.seed(123)
modelo_rf_inflacion <- randomForest(Inflacion ~ ., data = trainData, ntree = 200, mtry = 3, importance = TRUE)
modelo_rf_trm <- randomForest(TRM ~ ., data = trainData, ntree = 200, mtry = 3, importance = TRUE)
modelo_rf_pib <- randomForest(PIB ~ ., data = trainData, ntree = 200, mtry = 3, importance = TRUE)
modelo_rf_msi <- randomForest(MSCI_COLCAP ~ ., data = trainData, ntree = 200, mtry = 3, importance = TRUE)

# 📌 Realizar predicciones
pred_inflacion <- predict(modelo_rf_inflacion, newdata = testData)
pred_trm <- predict(modelo_rf_trm, newdata = testData)
pred_pib <- predict(modelo_rf_pib, newdata = testData)
pred_msi <- predict(modelo_rf_msi, newdata = testData)

# 📊 Calcular errores
rmse <- function(real, pred) sqrt(mean((real - pred)^2))
errores <- data.frame(
  Variable = c("Inflacion", "TRM", "PIB", "MSCI_COLCAP"),
  RMSE = c(rmse(testData$Inflacion, pred_inflacion),
           rmse(testData$TRM, pred_trm),
           rmse(testData$PIB, pred_pib),
           rmse(testData$MSCI_COLCAP, pred_msi))
)

# 🔍 Importancia de variables

vars_comunes <- Reduce(intersect, list(
  rownames(importance(modelo_rf_inflacion)),
  rownames(importance(modelo_rf_trm)),
  rownames(importance(modelo_rf_pib)),
  rownames(importance(modelo_rf_msi))
))

importancia <- data.frame(
  Variable = rep(vars_comunes, 4),
  Importancia = c(importance(modelo_rf_inflacion)[vars_comunes, 1],
                  importance(modelo_rf_trm)[vars_comunes, 1],
                  importance(modelo_rf_pib)[vars_comunes, 1],
                  importance(modelo_rf_msi)[vars_comunes, 1]),
  Modelo = rep(c("Inflación", "TRM", "PIB", "MSCI_COLCAP"), each = length(vars_comunes))
)

# 📊 Crear gráficos agrupados

# 1️⃣ Importancia de Variables
p1 <- ggplot(importancia, aes(x = reorder(Variable, Importancia), y = Importancia, fill = Modelo)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  labs(title = "Importancia de Variables", x = "Variables", y = "Importancia") +
  theme_minimal()

# 2️⃣ Comparación Real vs Predicho
predicciones <- data.frame(
  Real = c(testData$Inflacion, testData$TRM, testData$PIB, testData$MSCI_COLCAP),
  Predicho = c(pred_inflacion, pred_trm, pred_pib, pred_msi),
  Variable = rep(c("Inflación", "TRM", "PIB", "MSCI COLCAP"), each = nrow(testData))
)

p2 <- ggplot(predicciones, aes(x = Real, y = Predicho, color = Variable)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Comparación de Predicciones", x = "Real", y = "Predicho") +
  theme_minimal()

# 3️⃣ Distribución de errores
p3 <- ggplot(errores, aes(x = Variable, y = RMSE, fill = Variable)) +
  geom_bar(stat = "identity") +
  labs(title = "Error de Predicción (RMSE)", x = "Variable", y = "Error RMSE") +
  theme_minimal()

# 4️⃣ Comparación de valores reales por variable
datos_long <- melt(testData[, c("Inflacion", "TRM", "PIB", "MSCI_COLCAP")])
p4 <- ggplot(datos_long, aes(x = value, fill = variable)) +
  geom_histogram(alpha = 0.6, bins = 20, position = "identity") +
  labs(title = "Distribución de Variables Económicas", x = "Valor", y = "Frecuencia") +
  theme_minimal()

# 📌 Mostrar los gráficos agrupados
grid.arrange(p1, p2, p3, p4, ncol = 2)