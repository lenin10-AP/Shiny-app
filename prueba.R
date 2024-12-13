library(shiny)
library(ggplot2)

# Definir la interfaz de usuario
ui <- fluidPage(
  titlePanel("Descenso del Gradiente", windowTitle = "Gradient Descent"),
  
  # Aplicar CSS personalizado para hacer la interfaz más bonita
  tags$head(
    tags$style(HTML("
                    body {
                    background-color: #f4f4f9;
                    font-family: 'Arial', sans-serif;
                    }
                    .container-fluid {
                    padding: 30px;
                    }
                    .well {
                    background-color: #e0e0e0;
                    border-radius: 10px;
                    }
                    .btn-primary {
                    background-color: #4CAF50;
                    border-color: #4CAF50;
                    }
                    .btn-primary:hover {
                    background-color: #45a049;
                    }
                    h2 {
                    color: #333;
                    }
                    .plot-container {
                    margin-top: 20px;
                    }
                    .table {
                    margin-top: 20px;
                    border-radius: 10px;
                    background-color: #ffffff;
                    }
                    "))
    ),
  
  # Layout de la interfaz
  fluidRow(
    column(4, 
           wellPanel(
             h2("Parametros del Modelo"),
             numericInput("alpha", "Tasa de Aprendizaje (α):", value = 0.1, step = 0.01, min = 0.01),
             numericInput("x0", "Valor Inicial (x₀):", value = 0, step = 0.1),
             numericInput("iterations", "Numero de Iteraciones:", value = 10, min = 1, step = 1),
             actionButton("run", "Ejecutar Descenso", class = "btn-primary")
           )
    ),
    column(8, 
           div(class = "plot-container",
               plotOutput("gradientPlot", height = "500px")
           ),
           tableOutput("results")
    )
  )
    )

# Logica del servidor
server <- function(input, output) {
  
  # Funcion del descenso del gradiente
  gradient_descent <- reactive({
    req(input$run) # Esperar a que el boton sea presionado
    
    alpha <- input$alpha
    x <- input$x0
    iterations <- input$iterations
    results <- data.frame(Iteracion = 0, x = x, f_x = (x - 3)^2)
    
    for (i in 1:iterations) {
      grad <- 2 * (x - 3) # Derivada de f(x)
      x <- x - alpha * grad
      results <- rbind(results, data.frame(Iteracion = i, x = x, f_x = (x - 3)^2))
    }
    results
  })
  
  # Mostrar grafico con ggplot2 para hacerlo mas bonito
  output$gradientPlot <- renderPlot({
    results <- gradient_descent()
    ggplot(results, aes(x = Iteracion, y = f_x)) +
      geom_point(color = "#1E90FF", size = 3) +
      geom_line(color = "#1E90FF") +
      theme_minimal() +
      theme(
        text = element_text(family = "Arial"),
        plot.title = element_text(size = 18, face = "bold"),
        axis.title = element_text(size = 14)
      ) +
      labs(title = "Descenso del Gradiente", x = "Iteracion", y = "f(x)") +
      geom_hline(yintercept = 0, color = "red", linetype = "dashed")  # Linea horizontal en el minimo
  })
  
  # Mostrar tabla de resultados con formato atractivo
  output$results <- renderTable({
    gradient_descent()
  }, striped = TRUE, bordered = TRUE, hover = TRUE)
}

# Ejecutar la aplicacion
shinyApp(ui = ui, server = server)