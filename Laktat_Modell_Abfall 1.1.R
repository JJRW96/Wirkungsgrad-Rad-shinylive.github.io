library(shiny)
library(plotly)
library(minpack.lm)

# Define data
t_s <- c(23, 127, 261, 422.5, 549.8, 664.5, 795.8, 1254.9)
BLC_t <- c(10.04, 7.49, 5.18, 3.9, 2.67, 2.18, 1.64, 0.98)

# New model function: A * exp(-k * (t - delay)) + BLC0
laktat_model_function <- function(t, A, k, BLC0, delay) {
  A * exp(-k * (t - delay)) + BLC0
}

# User interface (UI)
ui <- fluidPage(
  titlePanel("Exponentielles Laktat-Abbaumodell"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      sliderInput("A", "A [mmol/l]", min = 0.0, max = 15.0, value = 8.0, step = 0.1),
      sliderInput("k", "k [s^-1]", min = 0.0001, max = 0.02, value = 0.005, step = 0.0001),
      sliderInput("BLC0", "BLC0 [mmol/l]", min = 0.0, max = 5.0, value = 1.0, step = 0.01),
      sliderInput("delay", "Delay [s]", min = 0, max = 3600, value = 0, step = 0.1),
      actionButton("set_delay", "Delay auf BLC_1"),
      br(), 
      actionButton("optimize", "Anpassen: nlsLM"),
      fileInput("file_upload", "CSV-Datei hochladen", accept = ".csv")
    ),
    mainPanel(
      plotlyOutput("plot")
    )
  )
)

# Server logic
server <- function(input, output, session) {
  # Reactive value for uploaded data
  uploaded_data <- reactiveVal(NULL)
  
  # Observer for CSV upload
  observeEvent(input$file_upload, {
    req(input$file_upload)
    df <- read.csv(input$file_upload$datapath)
    
    if ("t_s" %in% names(df) && "BLC_t" %in% names(df)) {
      uploaded_data(df)
    } else {
      showModal(modalDialog(
        title = "Ungültige CSV",
        "Die CSV-Datei muss 't_s' und 'BLC_t' als Spaltennamen enthalten.",
        easyClose = TRUE,
        footer = NULL
      ))
    }
  })
  
  # Observer for setting delay to first time point
  observeEvent(input$set_delay, {
    data <- if (is.null(uploaded_data())) {
      data.frame(t_s = t_s, BLC_t = BLC_t)
    } else {
      uploaded_data()
    }
    updateSliderInput(session, "delay", value = min(data$t_s))
  })
  
  observeEvent(input$optimize, {
    data <- if (is.null(uploaded_data())) {
      data.frame(t_s = t_s, BLC_t = BLC_t)
    } else {
      uploaded_data()
    }
    
    delay <- input$delay
    
    # Filter data points after the delay
    data_filtered <- data[data$t_s > delay, ]
    
    if (nrow(data_filtered) < 3) {
      showNotification("Nicht genügend Datenpunkte nach dem Delay für den Fitting-Prozess.", type = "error")
      return()
    }
    
    best_fit <- NULL
    best_rss <- Inf
    
    withProgress(message = 'Fitting in progress', value = 0, {
      for (i in 1:100) {
        # Randomized starting values
        A_start <- runif(1, 0, 15)
        k_start <- runif(1, 0.0001, 0.02)
        BLC0_start <- runif(1, 0, 5)
        
        start_values <- list(A = A_start, k = k_start, BLC0 = BLC0_start)
        
        model_equation <- BLC_t ~ A * exp(-k * (t_s - delay)) + BLC0
        
        fit <- try(nlsLM(model_equation,
                         data = data_filtered,
                         start = start_values,
                         lower = c(A = 0, k = 0.0001, BLC0 = 0),
                         upper = c(A = 15, k = 0.02, BLC0 = 5),
                         control = nls.control(maxiter = 1024)), silent = TRUE)
        
        if (!inherits(fit, "try-error")) {
          rss <- sum(residuals(fit)^2)
          if (rss < best_rss) {
            best_rss <- rss
            best_fit <- fit
          }
        }
        
        incProgress(1/100, detail = paste("Iteration", i))
      }
    })
    
    if (!is.null(best_fit)) {
      params <- as.list(coef(best_fit))
      updateSliderInput(session, "A", value = params$A)
      updateSliderInput(session, "k", value = params$k)
      updateSliderInput(session, "BLC0", value = params$BLC0)
      
      showNotification("Fitting completed successfully!", type = "message")
    } else {
      showModal(modalDialog(
        title = "nlsLM fehlgeschlagen",
        "Die nicht-lineare Regression mit dem Levenberg-Marquardt-Algorithmus konnte nicht erfolgreich durchgeführt werden.",
        easyClose = TRUE,
        footer = NULL
      ))
    }
  })
  
  output$plot <- renderPlotly({
    A <- input$A
    k <- input$k
    BLC0 <- input$BLC0
    delay <- input$delay
    
    data <- if (is.null(uploaded_data())) {
      data.frame(t_s = t_s, BLC_t = BLC_t)
    } else {
      uploaded_data()
    }
    
    max_x <- max(data$t_s) * 1.66
    min_x <- max(delay - 360, 0)
    
    t_s_modell <- seq(min_x, max_x, length.out = 1000)
    BLC_t_modell <- laktat_model_function(t_s_modell, A, k, BLC0, delay)
    
    max_y <- max(max(BLC_t_modell) * 1.1, max(data$BLC_t) * 1.1)
    
    # Create equation text
    eq_text <- sprintf("BLC(t) = %.2f * e<sup>-%.4f*(t-%.2f)</sup> + %.3f", A, k, delay, BLC0)
    
    # Calculate R²
    data_filtered <- data[data$t_s > delay, ]
    BLC_t_modell_at_data <- laktat_model_function(data_filtered$t_s, A, k, BLC0, delay)
    ss_res <- sum((data_filtered$BLC_t - BLC_t_modell_at_data)^2)
    ss_tot <- sum((data_filtered$BLC_t - mean(data_filtered$BLC_t))^2)
    r_squared <- 1 - (ss_res / ss_tot)
    
    # Plotly plot
    p <- plot_ly() %>%
      add_trace(x = t_s_modell[t_s_modell >= delay], y = BLC_t_modell[t_s_modell >= delay], 
                type = 'scatter', mode = 'lines', 
                name = 'Modellfunktion', line = list(color = '#EF6F6A')) %>%
      add_trace(x = t_s_modell[t_s_modell < delay], y = BLC_t_modell[t_s_modell < delay], 
                type = 'scatter', mode = 'lines', 
                name = 'Modellfunktion (vor Delay)', line = list(color = '#EF6F6A', dash = 'dash')) %>%
      add_trace(x = data$t_s, y = data$BLC_t, type = 'scatter', mode = 'markers', 
                name = 'Gemessene BLC-Werte', marker = list(color = '#42BA97', size = 10, symbol = 'x')) %>%
      add_segments(x = delay, xend = delay, y = 0, yend = max_y, 
                   line = list(color = 'darkgrey', width = 1, dash = 'dash'), 
                   showlegend = FALSE, opacity = 0.75) %>%
      layout(title = "Exponentielles Laktat-Abbaumodell",
             margin = list(t = 40),
             xaxis = list(title = "Zeit [s]", range = c(min_x, max_x)),
             yaxis = list(title = "Laktat [mmol/l]", range = c(0, max_y)),
             annotations = list(
               list(
                 x = max_x * 0.5, 
                 y = max_y * 0.95,
                 text = eq_text,
                 showarrow = FALSE,
                 xanchor = 'left',
                 yanchor = 'bottom',
                 font = list(
                   size = 12,
                   color = "black"
                 )
               ),
               list(
                 x = max_x * 0.5,
                 y = max_y * 0.90,
                 text = sprintf("R²: %.4f", r_squared),
                 showarrow = FALSE,
                 xanchor = 'left',
                 yanchor = 'bottom',
                 font = list(
                   size = 12,
                   color = "black"
                 )
               )
             ))
    
    p
  })
}

# Start the app
shinyApp(ui = ui, server = server)