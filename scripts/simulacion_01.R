# Iniciar el cronómetro
start_time <- Sys.time()

# Definamos una población
poblacion <- rnorm(1000, mean = 250, sd = 1)

# Cantidad de replicas
B <- 200

# Cantidad de muestras que serán promediadas en cada réplica
rep <- 100

# Objeto para almacenar los 1000 promedios de los muestreos
simulacion <- c()

# Simulación
for (i in 1:B) {
  
  muestra <- c()
  
  for (j in 1:rep) {
    m <- sample(poblacion, size = i)
    muestra <- c(muestra, m)
  }
  
  simulacion <- c(simulacion, mean(muestra))
}


# Crear la base de datos de graficación
base_final <- data.frame(N = 1:B,
                         Prom = simulacion)

# Obtener promedio y desviación estandar 
# de los resultados
pr <- mean(poblacion)
sd <- sd(poblacion)

# Calcular el error estándar
ME <- 1.69 * sd/sqrt(200*100)
ME

# Gráfico de la simulación
base_final %>% 
  ggplot(aes(x=N, y=Prom))+
  geom_line(color="red")+
  geom_hline(yintercept = pr)+
  geom_hline(yintercept = (pr-ME), lty=2)+
  geom_hline(yintercept = (pr+ME), lty=2) 

# Finalizar el cronómetro
end_time <- Sys.time()

# Calcular el tiempo transcurrido
time_taken <- end_time - start_time
print(paste("El tiempo total de ejecución fue:", time_taken))