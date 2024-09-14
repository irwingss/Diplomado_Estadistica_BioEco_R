library(shiny)
library(ggplot2)

# Definición de la interfaz de usuario
ui <- fluidPage(
  titlePanel("App de Probabilidad Interactiva"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("dist", "Selecciona la Distribución:",
                  choices = c("Binomial", "Poisson", "Geométrica", "Hipergeométrica", 
                              "Normal", "Exponencial", "Gamma", "Beta", 
                              "Cauchy", "Uniforme", "Log-Normal", "Chi-cuadrado",
                              "t-Student", "F-Snedecor")),
      
      # Paneles condicionales para cada distribución
      conditionalPanel(
        condition = "input.dist == 'Binomial'",
        numericInput("size_binom", "Número de Ensayos (n):", value = 10, min = 1),
        numericInput("prob_binom", "Probabilidad de Éxito (p):", value = 0.5, min = 0, max = 1)
      ),
      conditionalPanel(
        condition = "input.dist == 'Poisson'",
        numericInput("lambda_pois", "Parámetro Lambda (λ):", value = 5, min = 0)
      ),
      conditionalPanel(
        condition = "input.dist == 'Geométrica'",
        numericInput("prob_geom", "Probabilidad de Éxito (p):", value = 0.5, min = 0, max = 1)
      ),
      conditionalPanel(
        condition = "input.dist == 'Hipergeométrica'",
        numericInput("m_hyper", "Número de Éxitos en la Población (K):", value = 7, min = 1),
        numericInput("n_hyper", "Número de Fracasos en la Población (N-K):", value = 13, min = 1),
        numericInput("k_hyper", "Tamaño de la Muestra (n):", value = 10, min = 1)
      ),
      conditionalPanel(
        condition = "input.dist == 'Normal'",
        numericInput("mean_norm", "Media (μ):", value = 0),
        numericInput("sd_norm", "Desviación Estándar (σ):", value = 1, min = 0)
      ),
      conditionalPanel(
        condition = "input.dist == 'Exponencial'",
        numericInput("rate_exp", "Tasa (λ):", value = 1, min = 0)
      ),
      conditionalPanel(
        condition = "input.dist == 'Gamma'",
        numericInput("shape_gamma", "Forma (α):", value = 2, min = 0),
        numericInput("rate_gamma", "Tasa (β):", value = 1, min = 0)
      ),
      conditionalPanel(
        condition = "input.dist == 'Beta'",
        numericInput("shape1_beta", "Parámetro Alpha (α):", value = 2, min = 0),
        numericInput("shape2_beta", "Parámetro Beta (β):", value = 5, min = 0)
      ),
      # Botón para simular
      actionButton("simulate", "Simular"),
      
      # Texto descriptivo de la distribución
      h3("Descripción de la distribución seleccionada"),
      uiOutput("description")  # Aquí se mostrará el texto regular
    ),
    
    mainPanel(
      plotOutput("distPlot"),
      verbatimTextOutput("summary")
    )
  )
)

# Definición de la lógica del servidor
server <- function(input, output) {
  
  # Función para generar la descripción de la distribución
  output$description <- renderUI({
    if (input$dist == "Binomial") {
      withMathJax(HTML("<p><strong>La distribución binomial:</strong> Modela el número de éxitos en <em>n</em> ensayos independientes con una probabilidad <em>p</em> fija.</p>
          <p>Fórmula: $$P(X = k) = \\binom{n}{k} p^k (1-p)^{n-k}$$</p>
          <p><strong>Ejemplo 1:</strong> Supervivencia de plántulas en una población, donde <em>p</em> es la probabilidad de supervivencia de cada plántula.</p>
          <p><strong>Ejemplo 2:</strong> Número de especies con una característica genética particular en una muestra de individuos.</p>"))
    } else if (input$dist == "Poisson") {
      withMathJax(HTML("<p><strong>La distribución de Poisson:</strong> Describe el número de eventos que ocurren en un intervalo de tiempo o espacio dado, con una tasa constante <em>λ</em>.</p>
          <p>Fórmula: $$P(X = k) = \\frac{\\lambda^k e^{-\\lambda}}{k!}$$</p>
          <p><strong>Ejemplo 1:</strong> Número de individuos de una especie rara en una parcela de terreno.</p>
          <p><strong>Ejemplo 2:</strong> Número de llamadas a una estación de monitoreo ambiental en un día.</p>"))
    } else if (input$dist == "Geométrica") {
      withMathJax(HTML("<p><strong>La distribución geométrica:</strong> Modela el número de ensayos hasta obtener el primer éxito, donde <em>p</em> es la probabilidad de éxito en cada ensayo.</p>
          <p>Fórmula: $$P(X = k) = (1-p)^{k-1} p$$</p>
          <p><strong>Ejemplo 1:</strong> Número de intentos hasta encontrar la primera especie rara en una expedición.</p>
          <p><strong>Ejemplo 2:</strong> Número de observaciones de depredadores hasta ver una captura exitosa.</p>"))
    } else if (input$dist == "Hipergeométrica") {
      withMathJax(HTML("<p><strong>La distribución hipergeométrica:</strong> Modela el número de éxitos en una muestra sin reemplazo de una población finita.</p>
          <p>Fórmula: $$P(X = k) = \\frac{\\binom{K}{k} \\binom{N-K}{n-k}}{\\binom{N}{n}}$$</p>
          <p><strong>Ejemplo 1:</strong> Número de plantas con flor en una muestra tomada de un ecosistema.</p>
          <p><strong>Ejemplo 2:</strong> Número de especies invasoras detectadas en un área protegida tras muestrear sin reemplazo.</p>"))
    } else if (input$dist == "Normal") {
      withMathJax(HTML("<p><strong>La distribución normal:</strong> Describe variables continuas cuya distribución se agrupa alrededor de una media <em>μ</em>, con una desviación estándar <em>σ</em>.</p>
          <p>Fórmula: $$f(x) = \\frac{1}{\\sigma\\sqrt{2\\pi}} e^{-\\frac{(x-\\mu)^2}{2\\sigma^2}}$$</p>
          <p><strong>Ejemplo 1:</strong> Distribución de alturas de los árboles en un bosque.</p>
          <p><strong>Ejemplo 2:</strong> Peso de los individuos de una especie animal en una población natural.</p>"))
    }
    else if (input$dist == "Gamma") {
      withMathJax(HTML("<p><strong>La distribución Gamma:</strong> Modela tiempos de espera de un evento en un proceso en el que suceden múltiples eventos de manera continua. Utiliza dos parámetros: <em>α</em> (forma) y <em>β</em> (tasa).</p>
          <p>Fórmula: $$f(x; \\alpha, \\beta) = \\frac{\\beta^{\\alpha}}{\\Gamma(\\alpha)} x^{\\alpha-1} e^{-\\beta x}$$</p>
          <p><strong>Ejemplo 1:</strong> Tiempo hasta que un ecosistema se recupere después de un disturbio.</p>
          <p><strong>Ejemplo 2:</strong> Modelar el tiempo que tarda un cultivo en alcanzar su madurez.</p>"))
    } else if (input$dist == "Exponencial") {
      withMathJax(HTML("<p><strong>La distribución Exponencial:</strong> Modela el tiempo entre eventos en un proceso de Poisson con tasa constante. Se describe con el parámetro <em>λ</em>.</p>
          <p>Fórmula: $$f(x; \\lambda) = \\lambda e^{-\\lambda x}$$</p>
          <p><strong>Ejemplo 1:</strong> Tiempo hasta la próxima aparición de un incendio en un ecosistema.</p>
          <p><strong>Ejemplo 2:</strong> Tiempo de espera entre eventos de depredación en un sistema ecológico.</p>"))
    } else if (input$dist == "Beta") {
      withMathJax(HTML("<p><strong>La distribución Beta:</strong> Modela variables continuas entre 0 y 1, a menudo utilizada en la modelización de proporciones. Se describe con los parámetros <em>α</em> (shape1) y <em>β</em> (shape2).</p>
          <p>Fórmula: $$f(x; \\alpha, \\beta) = \\frac{x^{\\alpha-1}(1-x)^{\\beta-1}}{B(\\alpha, \\beta)}$$</p>
          <p><strong>Ejemplo 1:</strong> Proporción de la población que sobrevive en diferentes condiciones ambientales.</p>
          <p><strong>Ejemplo 2:</strong> Fracción del área de un ecosistema afectada por una perturbación.</p>"))
    }
  })
  
  data <- eventReactive(input$simulate, {
    if (input$dist == "Binomial") {
      rbinom(1000, Csize = input$size_binom, prob = input$prob_binom)
    } else if (input$dist == "Poisson") {
      rpois(1000, lambda = input$lambda_pois)
    } else if (input$dist == "Geométrica") {
      rgeom(1000, prob = input$prob_geom)
    } else if (input$dist == "Hipergeométrica") {
      rhyper(1000, m = input$m_hyper, n = input$n_hyper, k = input$k_hyper)
    } else if (input$dist == "Normal") {
      rnorm(1000, mean = input$mean_norm, sd = input$sd_norm)
    } else if (input$dist == "Exponencial") {
      rexp(1000, rate = input$rate_exp)
    } else if (input$dist == "Gamma") {
      rgamma(1000, shape = input$shape_gamma, rate = input$rate_gamma)
    } else if (input$dist == "Beta") {
      rbeta(1000, shape1 = input$shape1_beta, shape2 = input$shape2_beta)
    }
  })
  
  output$distPlot <- renderPlot({
    req(data())
    dist_data <- data()
    ggplot(data.frame(x = dist_data), aes(x = x)) +
      geom_histogram(binwidth = 1, color = "black", fill = "lightblue") +
      theme_minimal() +
      labs(title = paste("Distribución", input$dist),
           x = "Valor", y = "Frecuencia")
  })
  
  output$summary <- renderPrint({
    summary(data())
  })
}

# Ejecutar la aplicación Shiny
shinyApp(ui = ui, server = server)
