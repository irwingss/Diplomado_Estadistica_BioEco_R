library(shiny)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Comparación de Ecosistemas"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Sube tus datos (CSV o XLSX)"),
      selectInput("variable", "Selecciona una variable numérica",
                  choices = c("pH", "Oxígeno disuelto", "BMWP"))
    ),
    mainPanel(
      h3("Estadísticas Descriptivas"),
      tableOutput("statsTable"),
      h3("Visualización de Datos"),
      plotOutput("plot")
    )
  )
)

server <- function(input, output) {
  # Carga de datos
  data <- reactive({
    req(input$file)
    ext <- tools::file_ext(input$file$name)
    if (ext == "csv") {
      read.csv(input$file$datapath)
    } else if (ext == "xlsx") {
      readxl::read_excel(input$file$datapath)
    }
  })
  
  # Estadísticas descriptivas
  output$statsTable <- renderTable({
    req(input$variable)
    variable_data <- data()[[input$variable]]
    stats <- data.frame(
      Media = mean(variable_data, na.rm = TRUE),
      Mediana = median(variable_data, na.rm = TRUE),
      Varianza = var(variable_data, na.rm = TRUE),
      `Desviación Estándar` = sd(variable_data, na.rm = TRUE)
    )
    return(stats)
  })
  
  # Gráfico
  output$plot <- renderPlot({
    req(input$variable)
    ggplot(data(), aes_string(x = input$variable)) +
      geom_histogram(binwidth = 0.5, fill = "blue", color = "black") +
      theme_minimal() +
      labs(title = paste("Histograma de", input$variable))
  })
}

shinyApp(ui, server)
