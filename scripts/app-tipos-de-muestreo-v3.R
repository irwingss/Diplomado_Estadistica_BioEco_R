#| echo: false
#| standalone: true
#| viewerHeight: 800

library(shiny)
library(ggplot2)
library(dplyr)

# Generar la cuadrícula
grid_size <- 10  # Tamaño de la cuadrícula (10x10)
cells <- expand.grid(x = 1:grid_size, y = 1:grid_size)
cells$id <- 1:nrow(cells)

ui <- fluidPage(
  titlePanel("Visualización de Métodos de Muestreo en una Cuadrícula"),
  sidebarLayout(
    sidebarPanel(
      selectInput("sampling_method", "Selecciona el Método de Muestreo:",
                  choices = c("Muestreo Simple",
                              "Muestreo Estratificado",
                              "Muestreo por Conglomerados",
                              "Muestreo Sistemático")),
      numericInput("sample_size", "Tamaño de la Muestra:", value = 20, min = 1, max = nrow(cells)),
      conditionalPanel(
        condition = "input.sampling_method == 'Muestreo Estratificado'",
        numericInput("n_strata", "Número de Estratos:", value = 2, min = 2, max=2)
      ),
      conditionalPanel(
        condition = "input.sampling_method == 'Muestreo por Conglomerados'",
        numericInput("cluster_rows", "Filas por Conglomerado:", value = 2, min = 1),
        numericInput("cluster_cols", "Columnas por Conglomerado:", value = 2, min = 1)
      ),
      conditionalPanel(
        condition = "input.sampling_method == 'Muestreo Sistemático'",
        numericInput("k_value", "Valor de k (intervalo de muestreo):", value = 5, min = 1)
      ),
      actionButton("sample_button", "Tomar Muestra")
    ),
    mainPanel(
      plotOutput("grid_plot", height = "600px")
    )
  )
)

server <- function(input, output) {
  
  sampled_data <- eventReactive(input$sample_button, {
    method <- input$sampling_method
    n <- input$sample_size
    grid <- cells  # Copia de la cuadrícula
    grid$selected <- FALSE
    
    if (method == "Muestreo Simple") {
      sample_ids <- sample(grid$id, n)
      grid$selected[grid$id %in% sample_ids] <- TRUE
      
    } else if (method == "Muestreo Estratificado") {
      n_strata <- input$n_strata
      grid$stratum <- cut(grid$id, breaks = n_strata, labels = FALSE)
      strata <- unique(grid$stratum)
      samples_per_stratum <- n %/% length(strata)
      extra_samples <- n %% length(strata)
      for (s in strata) {
        stratum_cells <- grid %>% filter(stratum == s)
        stratum_n <- samples_per_stratum + ifelse(s <= extra_samples, 1, 0)
        sample_ids <- sample(stratum_cells$id, min(stratum_n, nrow(stratum_cells)))
        grid$selected[grid$id %in% sample_ids] <- TRUE
      }
      
    } else if (method == "Muestreo por Conglomerados") {
      cluster_rows <- input$cluster_rows
      cluster_cols <- input$cluster_cols
      num_clusters_row <- grid_size %/% cluster_rows
      num_clusters_col <- grid_size %/% cluster_cols
      grid$cluster_row <- ((grid$y - 1) %/% cluster_rows) + 1
      grid$cluster_col <- ((grid$x - 1) %/% cluster_cols) + 1
      grid$cluster_id <- paste0(grid$cluster_row, "-", grid$cluster_col)
      clusters <- unique(grid$cluster_id)
      clusters_selected <- sample(clusters, size = ceiling(length(clusters) * n / nrow(grid)))
      grid$selected[grid$cluster_id %in% clusters_selected] <- TRUE
      
    } else if (method == "Muestreo Sistemático") {
      k <- input$k_value
      start <- sample(1:k, 1)
      grid <- grid %>% arrange(y, x)
      sample_indices <- seq(start, by = k, length.out = n)
      sample_indices <- sample_indices[sample_indices <= nrow(grid)]
      grid$selected[sample_indices] <- TRUE
    }
    return(grid)
  })
  
  output$grid_plot <- renderPlot({
    req(sampled_data())
    grid <- sampled_data()
    method <- input$sampling_method
    
    p <- ggplot(grid, aes(x = x, y = y)) +
      geom_tile(aes(fill = selected), color = "black") +
      scale_fill_manual(values = c("FALSE" = "white", "TRUE" = "red"), guide = FALSE) +
      theme_minimal() +
      coord_fixed() +
      labs(title = paste("Método de Muestreo:", method),
           x = "", y = "") +
      theme(axis.text = element_blank(),
            axis.ticks = element_blank())
    
    # Añadir numeración
    p <- p + geom_text(aes(label = id, color = selected), 
                       size = 8) +
      scale_color_manual(values = c("FALSE" = "transparent", "TRUE" = "white"), guide = FALSE) 
    
    if (method == "Muestreo Estratificado") {
      p <- p + facet_wrap(~ stratum) +
        labs(subtitle = paste("Número de Estratos:", input$n_strata))
      
    } else if (method == "Muestreo por Conglomerados") {
      cluster_rows <- input$cluster_rows
      cluster_cols <- input$cluster_cols
      p <- p +
        geom_hline(yintercept = seq(0.5, grid_size + 0.5, by = cluster_rows), color = "blue", linetype = "dashed", lwd = 1) +
        geom_vline(xintercept = seq(0.5, grid_size + 0.5, by = cluster_cols), color = "blue", linetype = "dashed", lwd = 1) +
        labs(subtitle = paste("Conglomerados de", cluster_rows, "x", cluster_cols, "celdas"))
      
    } else if (method == "Muestreo Sistemático") {
      k <- input$k_value
      p <- p + labs(subtitle = paste("Valor de k =", k))
    }
    
    print(p)
  })
}

shinyApp(ui = ui, server = server)
