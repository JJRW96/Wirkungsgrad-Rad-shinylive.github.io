library(shiny)
library(plotly)

# UI
ui <- fluidPage(
  titlePanel("Lineare Modellfunktion"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("slope", "Steigung", min = -2, max = 2, value = 1.0, step = 0.01),
      numericInput("slope_input", "Steigung eingeben", value = 1.0, step = 0.01),
      sliderInput("intercept", "Achsenabschnitt", min = -5, max = 5, value = 0.0, step = 0.01),
      numericInput("intercept_input", "Achsenabschnitt eingeben", value = 0.0, step = 0.01),
      actionButton("toggle_data", "Beispieldaten einfügen"),
      actionButton("fit_model", "Fit: Slope only")
    ),
    mainPanel(
      plotlyOutput("plot")
    )
  )
)

# Server
server <- function(input, output, session) {
  show_data <- reactiveVal(FALSE)
  fit_params <- reactiveVal(list(slope = NULL, r_squared_fitted = NULL))
  
  observeEvent(input$toggle_data, {
    show_data(!show_data())
  })
  
  observeEvent(input$fit_model, {
    if (show_data()) {
      example_points <- data.frame(x = c(1, 3, 5, 8, 10), y = c(0.4, 1, 2, 3, 3.5))
      
      fit <- lm(y ~ 0 + x, data = example_points)  # Fit only slope
      slope <- round(coef(fit)["x"], 2)
      
      fitted_values <- predict(fit, example_points)
      ss_res <- sum((example_points$y - fitted_values)^2)
      ss_tot <- sum((example_points$y - mean(example_points$y))^2)
      r_squared_fitted <- 1 - (ss_res / ss_tot)
      
      fit_params(list(slope = slope, r_squared_fitted = r_squared_fitted))
      
      # Update the numeric input to the fitted slope
      updateNumericInput(session, "slope_input", value = unname(slope))
    }
  })
  
  observeEvent(input$slope_input, {
    updateSliderInput(session, "slope", value = input$slope_input)
  })
  
  observeEvent(input$intercept_input, {
    updateSliderInput(session, "intercept", value = input$intercept_input)
  })
  
  output$plot <- renderPlotly({
    slope <- input$slope
    intercept <- input$intercept
    x_values <- seq(0, 10, by = 0.1)
    y_values <- slope * x_values + intercept
    
    # Beispielpunkte
    example_points <- data.frame(x = c(1, 3, 5, 8, 10), y = c(0.4, 1, 2, 3, 3.5))
    
    # Calculate R² for current slider values
    fitted_values_manual <- slope * example_points$x + intercept
    ss_res_manual <- sum((example_points$y - fitted_values_manual)^2)
    ss_tot_manual <- sum((example_points$y - mean(example_points$y))^2)
    r_squared_manual <- 1 - (ss_res_manual / ss_tot_manual)
    
    p <- plot_ly() %>%
      add_trace(x = ~x_values, y = ~y_values, type = 'scatter', mode = 'lines',
                name = 'Lineare Funktion', line = list(color = '#EF6F6A')) %>%
      layout(title = "Lineare Modellfunktion",
             xaxis = list(title = "x"),
             yaxis = list(title = "y"))
    
    if (show_data()) {
      p <- p %>%
        add_trace(data = example_points, x = ~x, y = ~y, type = 'scatter', mode = 'markers',
                  name = 'Beispieldaten', marker = list(color = 'rgba(38, 131, 198, 0.9)', size = 8))
      
      if (!is.null(fit_params()$slope)) {
        fit_slope <- fit_params()$slope
        r_squared_fitted <- fit_params()$r_squared_fitted
        
        p <- p %>%
          layout(annotations = list(
            list(
              x = 8, y = max(y_values) * 0.9,
              text = sprintf("Fitted Slope: %.2f", fit_slope),
              showarrow = FALSE,
              xanchor = 'left',
              yanchor = 'bottom',
              font = list(size = 12)
            ),
            list(
              x = 8, y = max(y_values) * 0.8,
              text = sprintf("R² Fitted: %.3f", r_squared_fitted),
              showarrow = FALSE,
              xanchor = 'left',
              yanchor = 'bottom',
              font = list(size = 12)
            ),
            list(
              x = 8, y = max(y_values) * 0.7,
              text = sprintf("R²: %.3f", r_squared_manual),
              showarrow = FALSE,
              xanchor = 'left',
              yanchor = 'bottom',
              font = list(size = 12)
            )
          ))
      }
    }
    
    p
  })
}

# App ausführen
shinyApp(ui = ui, server = server)