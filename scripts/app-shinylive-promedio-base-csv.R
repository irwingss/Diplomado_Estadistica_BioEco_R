library(shiny)
library(bslib)

# Define UI for app that draws a histogram ----
ui <- page_sidebar(
  sidebar = sidebar(open = "open",
                    fileInput("file", "Subir archivo CSV"),
                    uiOutput("column_selector"), # Selector de columna dinámico
                    numericInput("n", "Sample count", 100),
                    checkboxInput("pause", "Pause", FALSE)
  ),
  plotOutput("plot", width=900)
)

server <- function(input, output, session) {
  
  # Reactivo para leer el archivo CSV
  data_file <- reactive({
    req(input$file)
    read.csv(input$file$datapath)
  })
  
  # Crear UI dinámico para el selector de columnas
  output$column_selector <- renderUI({
    req(data_file())
    selectInput("selected_column", "Seleccione una columna", 
                choices = names(data_file()), selected = names(data_file())[1])
  })
  
  # Reactivo para extraer la columna seleccionada
  selected_data <- reactive({
    req(input$selected_column)
    column_data <- data_file()[[input$selected_column]]
    
    # Filtra solo los valores numéricos
    column_data <- column_data[is.numeric(column_data)]
    
    # Asegura que no hay NA y retorna los datos
    na.omit(column_data)
  })
  
  # Reactivo para generar los datos simulados basados en la columna seleccionada
  data_simulated <- reactive({
    req(selected_data())
    
    if (!isTRUE(input$pause)) {
      invalidateLater(1000)
    }
    
    # Simular la distribución normal basada en la media y desviación estándar de la columna
    rnorm(input$n, mean = mean(selected_data()), sd = sd(selected_data()))
  })
  
  # Renderiza el histograma basado en la simulación
  output$plot <- renderPlot({
    req(data_simulated(), selected_data())
    
    # Establece límites dinámicos del eje X basados en los valores reales de la columna
    x_min <- min(selected_data(), na.rm = TRUE)
    x_max <- max(selected_data(), na.rm = TRUE)
    
    # Calcula la media de la columna seleccionada (poblacional)
    poblational_mean <- mean(selected_data(), na.rm = TRUE)
    
    hist(data_simulated(),
         breaks = 40,
         xlim = c(x_min, x_max),  # Actualiza los límites del eje X
         ylim = c(0, 1),
         lty = "blank",
         xlab = "value",
         freq = FALSE,
         main = ""
    )
    
    x <- seq(from = x_min, to = x_max, length.out = 500)
    y <- dnorm(x, mean = mean(selected_data()), sd = sd(selected_data()))
    lines(x, y, lwd=1.5)
    
    lwd <- 5
    # Línea roja para el promedio poblacional
    abline(v=poblational_mean, col="red", lwd=lwd, lty=2)  # Media poblacional
    abline(v=mean(data_simulated()), col="blue", lwd=lwd, lty=1)  # Media de la muestra simulada
    
    legend(legend = c("Normal", "Poblational mean", "Sample mean"),
           col = c("black", "red", "blue"),
           lty = c(1, 2, 1),
           lwd = c(1, lwd, lwd),
           x = x_max - (x_max - x_min) * 0.25,
           y = 0.7,
           cex = 0.7,
    )
  }, res=125)
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)
