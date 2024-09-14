#| echo: false
#| standalone: true
#| viewerHeight: 800

library(shiny)
library(ggplot2)

# Generar la población
set.seed(123)
population_size <- 500
population <- data.frame(
  id = 1:population_size,
  x = runif(population_size, 0, 100),
  y = runif(population_size, 0, 100)
)

ui <- fluidPage(
  titlePanel("Visualización de Métodos de Muestreo"),
  sidebarLayout(
    sidebarPanel(
      selectInput("sampling_method", "Selecciona el Método de Muestreo:",
                  choices = c("Muestreo Simple",
                              "Muestreo Estratificado",
                              "Muestreo por Conglomerados",
                              "Muestreo Sistemático")),
      numericInput("sample_size", "Tamaño de la Muestra:", value = 50, min = 1, max = population_size),
      conditionalPanel(
        condition = "input.sampling_method == 'Muestreo Estratificado'",
        numericInput("num_strata", "Número de Estratos:", value = 5, min = 1)
      ),
      conditionalPanel(
        condition = "input.sampling_method == 'Muestreo por Conglomerados'",
        numericInput("num_clusters", "Número de Conglomerados:", value = 5, min = 1)
      ),
      actionButton("sample_button", "Tomar Muestra")
    ),
    mainPanel(
      plotOutput("population_plot")
    )
  )
)

server <- function(input, output) {
  
  sampled_data <- eventReactive(input$sample_button, {
    method <- input$sampling_method
    n <- input$sample_size
    pop <- population  # Copia de la población
    
    if (method == "Muestreo Simple") {
      sample_indices <- sample(1:nrow(pop), n)
      sample <- pop[sample_indices, ]
      
    } else if (method == "Muestreo Estratificado") {
      num_strata <- input$num_strata
      pop$stratum <- cut(pop$y, breaks = num_strata, labels = FALSE)
      sample <- do.call(rbind, lapply(split(pop, pop$stratum), function(stratum_data) {
        stratum_n <- ceiling(n / num_strata)
        sample_n <- min(stratum_n, nrow(stratum_data))
        stratum_sample <- stratum_data[sample(1:nrow(stratum_data), sample_n), ]
        return(stratum_sample)
      }))
      
    } else if (method == "Muestreo por Conglomerados") {
      num_clusters <- input$num_clusters
      pop$cluster <- cut(pop$x, breaks = num_clusters, labels = FALSE)
      clusters_selected <- sample(unique(pop$cluster), size = ceiling(num_clusters / 2))
      sample <- subset(pop, cluster %in% clusters_selected)
      if (nrow(sample) > n) {
        sample <- sample(sample$id, n)
        sample <- pop[pop$id %in% sample, ]
      }
      
    } else if (method == "Muestreo Sistemático") {
      k <- floor(nrow(pop) / n)
      start <- sample(1:k, 1)
      sample_indices <- seq(start, by = k, length.out = n)
      sample_indices <- sample_indices[sample_indices <= nrow(pop)]
      sample <- pop[sample_indices, ]
    }
    return(list(sample = sample, pop = pop))
  })
  
  output$population_plot <- renderPlot({
    req(sampled_data())
    sample <- sampled_data()$sample
    pop <- sampled_data()$pop
    method <- input$sampling_method
    
    p <- ggplot(pop, aes(x = x, y = y)) +
      geom_point(color = "grey", alpha = 0.5) +
      geom_point(data = sample, color = "red", size = 2) +
      theme_minimal() +
      labs(title = paste("Método de Muestreo:", method),
           x = "X", y = "Y")
    
    if (method == "Muestreo Estratificado") {
      num_strata <- input$num_strata
      p <- p + geom_hline(yintercept = seq(0, 100, length.out = num_strata + 1), color = "blue", linetype = "dashed")
    } else if (method == "Muestreo por Conglomerados") {
      num_clusters <- input$num_clusters
      p <- p + geom_vline(xintercept = seq(0, 100, length.out = num_clusters + 1), color = "green", linetype = "dashed")
    } else if (method == "Muestreo Sistemático") {
      k <- floor(nrow(pop) / input$sample_size)
      p <- p + annotate("text", x = 50, y = 105, label = paste("k =", k), size = 5)
    }
    print(p)
  })
}

shinyApp(ui = ui, server = server)
