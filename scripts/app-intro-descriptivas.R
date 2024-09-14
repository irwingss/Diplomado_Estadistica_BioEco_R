#| standalone: true
#| viewerHeight: 600

library(shiny)
library(readxl)  # Para leer archivos .xlsx
library(datasets)  # Acceso a las bases de datos incluidas en R

# Define el UI
ui <- fluidPage(
  
  # Título de la app
  titlePanel("Introducción a la Estadística Descriptiva"),
  
  # Sidebar para cargar datos y seleccionar una variable
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Sube tu archivo CSV o XLSX",
                accept = c(".csv", ".xlsx")),
      uiOutput("varSelect"),
      selectInput("defaultDataset", "O selecciona un dataset por defecto",
                  choices = c("mtcars", "iris", "PlantGrowth"),
                  selected = "mtcars")
    ),
    
    # Muestra las estadísticas descriptivas
    mainPanel(
      h3("Estadísticas Descriptivas"),
      tableOutput("summaryStats")
    )
  )
)

# Define el server
server <- function(input, output, session) {
  
  # Función reactiva para cargar los datos
  data <- reactive({
    # Si el usuario carga un archivo
    if (!is.null(input$file)) {
      ext <- tools::file_ext(input$file$name)
      if (ext == "csv") {
        return(read.csv(input$file$datapath))
      } else if (ext == "xlsx") {
        return(read_excel(input$file$datapath))
      }
    }
    # Si no se carga un archivo, usa el dataset por defecto seleccionado
    switch(input$defaultDataset,
           "mtcars" = mtcars,
           "iris" = iris,
           "PlantGrowth" = PlantGrowth)
  })
  
  # Genera un UI dinámico para seleccionar una variable numérica
  output$varSelect <- renderUI({
    req(data())
    selectInput("variable", "Selecciona una variable numérica",
                choices = names(data()))
  })
  
  # Calcula las estadísticas descriptivas
  output$summaryStats <- renderTable({
    req(input$variable)
    var_data <- data()[[input$variable]]
    if(is.numeric(var_data)) {
      stats <- data.frame(
        Estadística = c("Media", "Mediana", "Varianza", "Desviación Estándar"),
        Valor = c(mean(var_data, na.rm = TRUE),
                  median(var_data, na.rm = TRUE),
                  var(var_data, na.rm = TRUE),
                  sd(var_data, na.rm = TRUE))
      )
      stats
    } else {
      data.frame(Estadística = "Error", Valor = "La variable seleccionada no es numérica")
    }
  })
}

# Ejecuta la aplicación
shinyApp(ui, server)
