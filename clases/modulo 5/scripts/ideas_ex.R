v1 <- c(12, 15, 15, 17, 18, 21, 21, 21, 23)
median(v1)
quantile(v1)

# Cuantiles
cuantiles <- quantile(v1)
cuantiles

# Rango intercuantil (IQR)
IQR <- cuantiles[4] - cuantiles[2]
IQR

# Calcular el Umbral para Outliers: 
umbral <- 1.5*IQR
umbral

# Q0 (ceja inferior)
q0 <- cuantiles[2] - umbral
q0

# Q4 (ceja superior)
q4 <- cuantiles[4] + umbral
q4

# Outliers
outliers <- v1[v1 < q0 | v1 > q4]
outliers

boxplot.stats(v1)$out



dpois(5, lambda = 3)
1- pnorm(350, mean = 300, sd = 25)

dgamma(x = 1.5, shape = 2, rate = 3)
dgamma(x = 3, shape = 2, rate = 3)
dgamma(x = 3, shape = 3, rate = 2)
dgamma(x = 2, shape = 3, rate = 3)
rgamma(n = 2, shape = 2, rate = 3)