
########################## Estadísticas descriptivas ###########################

# Se pasa la base a tibble para generar un reporte de estadísticas descriptivas 

library(tibble)

base_tibble<- as_tibble(base_filtrada2)
write.csv(base_filtrada2, "C:/Users/user/Documents/taller3/data_clean/clean_data.csv", row.names = FALSE)

## generamos un reporte de las descriptivas

descriptivas <- skim(base_tibble)

## se exporta el reporte de estadísticas

install.packages('openxlsx')
library(openxlsx)

write.csv(descriptivas, "C:/Users/user/Documents/taller3/results/descriptivas.csv", row.names = FALSE)

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

ggsave("C:/Users/user/Documents/taller3/results/distribucion_edad.png", plot = edad, width = 8, height = 6)

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

ggsave("C:/Users/user/Documents/taller3/results/dist_ingreso.png", plot = Ingreso, width = 8, height = 6)


### relación ingreso edad

ing_edad <- ggplot(data=base_filtrada)+ 
  geom_smooth(mapping = aes(x =age, y =y_total_m_ha))+
  scale_y_continuous (labels=function(n) {format(n, scientific = FALSE)})+
  labs(x="Edad", y="Ingreso")

ggsave("C:/Users/user/Documents/taller3/results/ingreso_edad.png", plot = ing_edad, width = 8, height = 6)

mapa <- ggplot(data=base_filtrada)+ 
  geom_point(mapping = aes(x =age, y =y_total_m_ha))+
  scale_y_continuous (labels=function(n) {format(n, scientific = FALSE)})+
  labs(x="Edad", y="Ingreso")

ggsave("C:/Users/user/Documents/taller3/results/disp_ing_edad.png", plot = mapa, width = 8, height = 6)

### relación ingreso estrato

#Grafico de dispesión con línea de tendencia

png("C:/Users/user/Documents/taller3/results/Est_ing.png", width = 8, height = 6, units = "in", res = 300)

scatter.smooth(base_filtrada$estrato1, base_filtrada$y_total_m_ha, span = 2/3, degree = 1,
               family = "symmetric",
               xlab = "Estrato", ylab = "Ingreso",
               evaluation = 50, col = "green")  
title("Relación entre Estrato e Ingreso")

dev.off()

# gráfico de cajas y bigotes

png("C:/Users/user/Documents/taller3/results/bigote.png", width = 8, height = 6, units = "in", res = 300)

boxplot(base_filtrada$y_total_m_ha ~ base_filtrada$estrato1, 
        xlab = "Estrato", ylab = "Ingreso",
        main = "Ingreso vs Estrato")

dev.off()
