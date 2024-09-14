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
        numericInput("m_hyper", "Número de Éxitos en la Población (m):", value = 7, min = 1),
        numericInput("n_hyper", "Número de Fracasos en la Población (n):", value = 13, min = 1),
        numericInput("k_hyper", "Tamaño de la Muestra (k):", value = 10, min = 1)
      ),
      conditionalPanel(
        condition = "input.dist == 'Normal'",
        numericInput("mean_norm", "Media (μ):", value = 0),
        numericInput("sd_norm", "Desviación Estándar (σ):", value = 1, min = 0)
      ),
      conditionalPanel(
        condition = "input.dist == 'Exponencial'",
        numericInput("rate_exp", "Tasa (rate):", value = 1, min = 0)
      ),
      conditionalPanel(
        condition = "input.dist == 'Gamma'",
        numericInput("shape_gamma", "Forma (shape):", value = 2, min = 0),
        numericInput("rate_gamma", "Tasa (rate):", value = 1, min = 0)
      ),
      conditionalPanel(
        condition = "input.dist == 'Beta'",
        numericInput("shape1_beta", "Parámetro Alpha (shape1):", value = 2, min = 0),
        numericInput("shape2_beta", "Parámetro Beta (shape2):", value = 5, min = 0)
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
      withMathJax(HTML("<p><strong>La distribución Gamma:</strong> Modela tiempos de espera hasta el k-ésimo evento en un proceso de Poisson. Utiliza dos parámetros: <em>shape</em> y <em>rate</em>.</p>
                      <p>Fórmula: $$f(x; \\alpha, \\beta) = \\frac{\\beta^{\\alpha}}{\\Gamma(\\alpha)} x^{\\alpha-1} e^{-\\beta x}$$</p>
                      <p><strong>Ejemplo 1:</strong> Tiempo hasta que un ecosistema se recupere después de un disturbio.</p>
                      <p><strong>Ejemplo 2:</strong> Duración de vida de una especie bajo condiciones ambientales fluctuantes.</p>"))
    } else if (input$dist == "Exponencial") {
      withMathJax(HTML("<p><strong>La distribución Exponencial:</strong> Modela el tiempo entre eventos en un proceso de Poisson con tasa constante. Se describe con el parámetro <em>rate</em>.</p>
                      <p>Fórmula: $$f(x; \\lambda) = \\lambda e^{-\\lambda x}$$</p>
                      <p><strong>Ejemplo 1:</strong> Tiempo hasta la próxima aparición de un incendio en un ecosistema.</p>
                      <p><strong>Ejemplo 2:</strong> Tiempo de espera entre eventos de depredación en un sistema ecológico.</p>"))
    } else if (input$dist == "Beta") {
      withMathJax(HTML("<p><strong>La distribución Beta:</strong> Modela variables continuas entre 0 y 1, a menudo utilizada en la modelización de proporciones. Se describe con los parámetros <em>shape1</em> y <em>shape2</em>.</p>
                      <p>Fórmula: $$f(x; \\alpha, \\beta) = \\frac{x^{\\alpha-1}(1-x)^{\\beta-1}}{B(\\alpha, \\beta)}$$</p>
                      <p><strong>Ejemplo 1:</strong> Proporción de la población que sobrevive en diferentes condiciones ambientales.</p>
                      <p><strong>Ejemplo 2:</strong> Fracción del área de un ecosistema afectada por una perturbación.</p>"))
    } else if (input$dist == "Cauchy") {
      withMathJax(HTML("<p><strong>La distribución Cauchy:</strong> Tiene forma de campana pero con colas más pesadas que la normal. Es útil para modelar fenómenos con variaciones extremas.</p>
                      <p>Fórmula: $$f(x; x_0, \\gamma) = \\frac{1}{\\pi \\gamma \\left[1 + \\left(\\frac{x - x_0}{\\gamma}\\right)^2\\right]}$$</p>
                      <p><strong>Ejemplo 1:</strong> Fluctuaciones extremas en las concentraciones de nutrientes en un lago.</p>
                      <p><strong>Ejemplo 2:</strong> Variabilidad en las tasas de crecimiento de una especie bajo condiciones adversas.</p>"))
    } else if (input$dist == "Uniforme") {
      withMathJax(HTML("<p><strong>La distribución Uniforme:</strong> Describe variables que tienen la misma probabilidad de tomar cualquier valor dentro de un intervalo definido. Se describe con los parámetros <em>min</em> y <em>max</em>.</p>
                      <p>Fórmula: $$f(x; a, b) = \\frac{1}{b-a}, \\quad a \\leq x \\leq b$$</p>
                      <p><strong>Ejemplo 1:</strong> Distribución de especies en un hábitat homogéneo.</p>
                      <p><strong>Ejemplo 2:</strong> Longitud de organismos distribuidos de manera uniforme en una población.</p>"))
    } else if (input$dist == "Log-Normal") {
      withMathJax(HTML("<p><strong>La distribución Log-Normal:</strong> Describe una variable cuyo logaritmo está distribuido normalmente. Es útil para modelar datos sesgados a la derecha.</p>
                      <p>Fórmula: $$f(x; \\mu, \\sigma) = \\frac{1}{x \\sigma \\sqrt{2\\pi}} e^{-\\frac{(\\ln x - \\mu)^2}{2\\sigma^2}}$$</p>
                      <p><strong>Ejemplo 1:</strong> Distribución del tamaño corporal en una población de organismos.</p>
                      <p><strong>Ejemplo 2:</strong> Concentraciones de contaminantes en el agua de un ecosistema afectado.</p>"))
    } else if (input$dist == "Chi-cuadrado") {
      withMathJax(HTML("<p><strong>La distribución Chi-cuadrado:</strong> Describe la suma de los cuadrados de variables normales estandarizadas. Se utiliza en pruebas de hipótesis y en el análisis de varianza.</p>
                      <p>Fórmula: $$f(x; k) = \\frac{1}{2^{k/2}\\Gamma(k/2)} x^{k/2-1} e^{-x/2}$$</p>
                      <p><strong>Ejemplo 1:</strong> Análisis de la variabilidad de la biomasa en parcelas de un ecosistema.</p>
                      <p><strong>Ejemplo 2:</strong> Comparación de la dispersión de especies entre hábitats diferentes.</p>"))
    } else if (input$dist == "t-Student") {
      withMathJax(HTML("<p><strong>La distribución t-Student:</strong> Se utiliza cuando se estima la media de una población normalmente distribuida con un tamaño de muestra pequeño. Tiene colas más gruesas que la normal.</p>
                      <p>Fórmula: $$f(t; \\, \\nu) = \\frac{\\Gamma\\left(\\frac{\\nu+1}{2}\\right)}{\\sqrt{\\nu\\pi} \\Gamma\\left(\\frac{\\nu}{2}\\right)} \\left(1 + \\frac{t^2}{\\nu}\\right)^{-\\frac{\\nu+1}{2}}$$</p>
                      <p><strong>Ejemplo 1:</strong> Comparación de medias de altura entre dos poblaciones de plantas.</p>
                      <p><strong>Ejemplo 2:</strong> Análisis del efecto de diferentes tratamientos en la tasa de crecimiento de una especie.</p>"))
    } else if (input$dist == "F-Snedecor") {
      withMathJax(HTML("<p><strong>La distribución F-Snedecor:</strong> Se utiliza en la prueba de hipótesis de igualdad de varianzas entre dos poblaciones. Está relacionada con el análisis de varianza (ANOVA).</p>
                      <p>Fórmula: $$f(x; d_1, d_2) = \\frac{\\left(\\frac{d_1}{d_2}\\right)^{d_1/2} x^{(d_1/2)-1}}{B(d_1/2, d_2/2) \\left(1 + \\frac{d_1}{d_2} x\\right)^{(d_1 + d_2)/2}}$$</p>
                      <p><strong>Ejemplo 1:</strong> Comparación de la variabilidad en la productividad de dos ecosistemas.</p>
                      <p><strong>Ejemplo 2:</strong> Análisis de varianza de las tasas de crecimiento de especies en distintas condiciones ambientales.</p>"))
    }
})
  
  
  data <- eventReactive(input$simulate, {
    if (input$dist == "Binomial") {
      rbinom(1000, size = input$size_binom, prob = input$prob_binom)
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
    }
    # Añadir otras distribuciones...
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
