# Cargar librerías
library(tidyverse)

# Variable: número de individuos observados en 23 parcelas del árbol Chorisia integrifolia
dt_bosque <- data.frame(
  num_arboles = c(2, 3, 3, 4, 5, 5, 3, 4, 2, 3, 6, 7, 3 , 6, 2, 3, 3, 6, 5, 1, 2, 3, 3))

# Tabulación de frecuencias (número de veces que se observó cada cantidad de árboles)
dt_bosque %>% 
  count(num_arboles)

# pak::pak("summarytools")  
library(summarytools)
dfSummary(dt_bosque)

library(summarytools)
summarytools::descr(dt_bosque)

# pak::pak("Hmisc")
library(Hmisc)
Hmisc::describe(dt_bosque)

pak::pak("skimr")
library(skimr)
skim(dt_bosque)

# pak::pak("psych")
library(psych)
psych::describe(dt_bosque)

# Calcular la probabilidad para cada número de árboles
pmf <- prop.table(tabla_frecuencias)

# Convertir a data frame para facilitar la visualización
pmf_df <- as.data.frame(pmf)
colnames(pmf_df) <- c("Num_Arboles", "Probabilidad")

# Gráfico de la función de masa de probabilidad
ggplot(pmf_df, aes(x = Num_Arboles, y = Probabilidad)) +
  geom_bar(stat = "identity", fill = "forestgreen", color = "black") +
  labs(title = "Función de masa de probabilidad: Número de árboles en parcelas",
       x = "Número de árboles",
       y = "Probabilidad") +
  theme_minimal()
