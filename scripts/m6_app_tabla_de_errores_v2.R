# Paquetes necesarios
library(shiny)
library(ggplot2)
library(ggridges)
library(plotly)

# Interfaz de usuario (UI)
ui <- fluidPage(
  tags$head(tags$style(HTML("
    .irs--shiny .irs-grid-text {
    bottom: 5px;
    font-size: 16px;
    }
    .irs--shiny .irs-min, .irs--shiny .irs-max {
    font-size: 16px;
    }
    .sidebar { 
    height: 100vh; 
    position: -webkit-sticky;   
    position: sticky; 
    top: 20px; /* Ajusta la distancia desde el top */
  }
  "))),
  
  titlePanel("Tabla de Errores"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("n", "Tamaño de la muestra (n):", min = 10, max = 1000, value = 100, step = 10),
      sliderInput("delta", "Diferencia de medias (Δ):", min = 0.1, max = 2, value = 0.3, step = 0.1),
      sliderInput("sigma", "Desviación estándar (σ):", min = 0.1, max = 2, value = 1, step = 0.1),
      sliderInput("alpha", "Nivel de significancia (α):", min = 0.01, max = 0.2, value = 0.05, step = 0.01),
      style = "font-size: 22px; position: -webkit-sticky; position: sticky; top: 20px;"
    ),
    
    mainPanel(
      plotOutput("grafico_poder", height = "700px"),
      plotlyOutput("grafico_dispersion", height = "400px")  # plotlyOutput para el gráfico interactivo
    )
  ),
)

# Lógica del servidor (Server)
server <- function(input, output) {
  
  # Gráfico general de poder
  output$grafico_poder <- renderPlot({
    n <- input$n
    delta <- input$delta
    sigma <- input$sigma
    alpha <- input$alpha
    
    # Calcular el error estándar y el parámetro de no centralidad
    se <- sigma / sqrt(n)
    ncp <- delta / se
    
    # Valor crítico
    z_alpha <- qnorm(1 - alpha)
    
    # Generar valores z
    z_min <- min(-4, ncp - 4)
    z_max <- max(4, ncp + 4)
    z <- seq(z_min, z_max, length.out = 1000)
    
    # Densidades bajo H0 y H1
    h0_density <- dnorm(z, mean = 0, sd = 1)
    h1_density <- dnorm(z, mean = ncp, sd = 1)
    
    # Crear data frames
    df_h0 <- data.frame(
      z = z,
      density = h0_density,
      hypothesis = "H0",
      region = ifelse(z >= z_alpha, "Sig. o Error Tipo I (α)", "Confianza (1-α)")
    )
    
    df_h1 <- data.frame(
      z = z,
      density = h1_density,
      hypothesis = "H1",
      region = ifelse(z >= z_alpha, "Poder (1-β)", "Error Tipo II (β)")
    )
    
    # Combinar data frames
    df <- rbind(df_h0, df_h1)
    
    # Asignar colores
    df$fill_color <- factor(df$region, levels = c("Sig. o Error Tipo I (α)", "Confianza (1-α)", "Error Tipo II (β)", "Poder (1-β)"))
    
    # Definir colores
    fill_colors <- c(
      "Sig. o Error Tipo I (α)" = "#ff9c96",
      "Confianza (1-α)" = "#a0ff78",
      "Error Tipo II (β)" = "#ff0d00",
      "Poder (1-β)" = "#2fa100"
    )
    
    # Cambiar orden factor
    df$hypothesis <- factor(df$hypothesis, levels = c("H1", "H0"))
    
    # Generar el gráfico
    ggplot(df, aes(x = z, y = hypothesis, height = density, fill = fill_color)) +
      geom_ridgeline(alpha = 0.8, color = "black", size = 0.3) +
      scale_fill_manual(values = fill_colors) +
      geom_vline(xintercept = z_alpha, linetype = "dashed", color = "black") +
      labs(x = "", y = "", fill = "") +
      theme_minimal() +
      theme(legend.position = "bottom",
            axis.text = element_text(size = 20),
            legend.text = element_text(size = 15)) +
      guides(fill = guide_legend(nrow = 2))
  })
  
  # Gráfico de dispersión comparando H0 vs Ha
  output$grafico_dispersion <- renderPlotly({
    n <- input$n
    delta <- input$delta
    sigma <- input$sigma
    alpha <- input$alpha
    
    # Calcular el error estándar y el parámetro de no centralidad
    se <- sigma / sqrt(n)
    ncp <- delta / se
    
    # Valor crítico
    z_alpha <- qnorm(1 - alpha)
    
    # Generar puntos aleatorios para las distribuciones
    set.seed(123)
    z_h0 <- rnorm(n, mean = 0, sd = 1)
    z_h1 <- rnorm(n, mean = ncp, sd = 1)
    
    # Crear data frame para el gráfico
    df_dispersion <- data.frame(
      z_h0 = z_h0,
      z_h1 = z_h1
    )
    
    # Generar el gráfico de dispersión
    plot2 <- ggplot(df_dispersion, aes(x = z_h0, y = z_h1)) +
      geom_point(color = "#0013fe", size = 3, alpha = 0.7) +
      geom_vline(xintercept = z_alpha, linetype = "dashed", color = "black") +
      geom_hline(yintercept = z_alpha, linetype = "dashed", color = "black") +
      labs(x = "H0", y = "Ha") +
      theme_minimal() +
      theme(axis.text.y = element_blank(),  # Oculta los valores del eje Y
            axis.ticks.y = element_blank(), # Oculta las marcas del eje Y
            axis.text = element_text(size = 15)) 
    
    # Convertir el gráfico a plotly
    ggplotly(plot2) %>%
      layout(
        autosize = TRUE,  # Ajuste automático del tamaño
        yaxis = list(showticklabels = FALSE),  # Oculta las etiquetas del eje Y
        margin = list(l = 0, r = 0, b = 50, t = 50)  # Ajuste de márgenes
      )
  })
}

# Ejecutar la aplicación
shinyApp(ui = ui, server = server)
