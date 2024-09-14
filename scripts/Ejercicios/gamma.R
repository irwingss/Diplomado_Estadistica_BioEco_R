# Ejemplo de tiempos de recuperación observados
tiempos_observados <- c(20.5, 25.3, 23.1, 18.9, 27.6, 30.4, 22.7, 24.9)
range(tiempos_observados)

# Ajuste de la distribución Gamma
library(MASS)
ajuste_gamma <- fitdistr(tiempos_observados, "gamma")

# Estimación de los parámetros α y β
alpha <- ajuste_gamma$estimate["shape"]
beta <- ajuste_gamma$estimate["rate"]

# Simulación de 1000 tiempos de recuperación
set.seed(123)  # Para reproducibilidad
tiempos_recuperacion <- rgamma(1000, shape = alpha, rate = beta)

# Visualización de la distribución de tiempos de recuperación
hist(tiempos_recuperacion, breaks = 30, col = "lightblue", border = "black",
     main = "Distribución de tiempos de recuperación del ecosistema",
     xlab = "Tiempo de recuperación (años)", ylab = "Frecuencia")


# Parámetros de la distribución Gamma
alpha <- 5    # Parámetro de forma
beta <- 0.2   # Parámetro de tasa

# Simulación de 1000 tiempos de recuperación
set.seed(123)  # Para reproducibilidad
tiempos_recuperacion <- rgamma(1000, shape = alpha, rate = beta)

# Visualización de la distribución de tiempos de recuperación
hist(tiempos_recuperacion, breaks = 30, col = "lightblue", border = "black",
     main = "Distribución de tiempos de recuperación del ecosistema",
     xlab = "Tiempo de recuperación (años)", ylab = "Frecuencia")

# Cálculo del tiempo medio de recuperación esperado
tiempo_medio <- mean(tiempos_recuperacion)
tiempo_medio
