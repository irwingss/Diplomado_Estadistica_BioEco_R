# Paquetes necesarios
library(shiny)
library(ggplot2)
library(ggridges)

# Interfaz de usuario (UI)
ui <- fluidPage(
  tags$head(tags$style(HTML("
  .irs--flat .irs-single, 
  .irs--flat .irs-min, 
  .irs--flat .irs-max, 
  .irs--flat .irs-from, 
  .irs--flat .irs-to, 
  .irs--flat .irs-grid-text {
    font-size: 16px !important;
  }
"))),
  
  titlePanel("Tabla de Errores"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("n", "Tamaño de la muestra (n):", min = 10, max = 1000, value = 100, step = 10),
      sliderInput("delta", "Diferencia de medias (Δ):", min = 0.1, max = 2, value = 0.5, step = 0.1),
      sliderInput("sigma", "Desviación estándar (σ):", min = 0.1, max = 2, value = 1, step = 0.1),
      sliderInput("alpha", "Nivel de significancia (α):", min = 0.01, max = 0.2, value = 0.05, step = 0.01),
      style = "font-size: 22px;"
    ),
    
    mainPanel(
      plotOutput("grafico_poder", height = "700px")
    ),
  ),
)

# Lógica del servidor (Server)
server <- function(input, output) {
  
  # Gráfico general
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
      theme_minimal()+
      theme(legend.position = "bottom",
            axis.text = element_text(size = 20),
            legend.text = element_text(size=15))+
      guides(fill = guide_legend(nrow = 2))
  })
}

# Ejecutar la aplicación
shinyApp(ui = ui, server = server)