# Cargar las librerías necesarias
library(ggplot2)
library(dplyr)

# Definir los valores de p (probabilidades de extinción)
p_values <- seq(0.01, 0.9, by = 0.01)

# Crear una secuencia de valores para el número de especies
S <- seq(0, 10, by = 1)

# Generar un datos frame que contenga las combinaciones de S y p
datos <- expand.grid(S = S, P_funcionamiento = p_values)

# Calcular la probabilidad de funcionamiento del ecosistema
datos <- datos %>%
  # mutate(P_funcionamiento = 1 - p^S)
  mutate(p = (1 - P_funcionamiento)^(1 / S))
  

# Crear el gráfico de contornos con relleno
titulo <- c("Relación entre la probabilidad de funcionamiento del ecosistema (P)  y el número de especies.")

ppp <-ggplot(datos, aes(x = S, y = P_funcionamiento, fill = p)) +
  geom_tile() +
  geom_contour(aes(z = p), color = "white",
               lwd=0.5) +
  labs(title = "",
       x = "Número de especies",
       y = "Probabilidad de funcionamiento del ecosistema (P) ",
       fill = "Probabilidades\nde extinción (p)") +
  theme_classic() +
  theme(axis.title = element_text(size = 20, face = 2),    
        axis.text = element_text(size = 20),     
        legend.title = element_text(size = 20, face = 2),
        legend.text = element_text(size = 20),   
        plot.title = element_text(size = 20, hjust = 0.5))+
  scale_x_continuous(breaks = scales::pretty_breaks(5),
                     expand=c(0,0))+
  scale_y_continuous(expand=c(0,0))+
  scale_fill_viridis_c(option = "D", direction = 1)  #
ppp
# pak::pak("plotly")
library(plotly)
ggplotly(ppp, tooltip = "text") %>%
  layout(
    hoverlabel = list(font = list(size = 35)),  
    hovermode = 'closest'
  ) %>%
  style(hovertemplate = 'N° Especies: %{x}<br>Prob. Func.: %{y}<br>Prob. Ext.: %{z}<extra></extra>')


ppp
