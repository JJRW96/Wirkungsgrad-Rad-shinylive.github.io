library(shiny)
library(plotly)
library(minpack.lm)

# Definiere Daten
t_min <- c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
BLC_t <- c(2.5, 4.5, 7.0, 9.0, 10.0, 10.5, 10.0, 9.5, 8.5, 7.5, 6.5, 5.5, 4.5)

# Normalisiere t_min, wenn der kleinste Wert größer als 0.0 ist
if (min(t_min) > 0.0) {
  t_min <- t_min - min(t_min)
}

# 3-Parameter Bi-exponential Modellfunktion für Laktatwerte (in Minuten)
laktat_model_function <- function(t, A, k1, k2, BLC0) {
  (A * k1 / (k2 - k1)) * (exp(-k1 * t) - exp(-k2 * t)) + BLC0
}

# Benutzeroberfläche (UI)
ui <- fluidPage(
  titlePanel("3-Parameter Bi-exponential Laktat-Modellfunktion"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      sliderInput("A", "A [mmol/l]", min = 3.0, max = 30.0, value = 11.0, step = 0.1),
      sliderInput("k1", "k1 [min^-1]", min = 0.1, max = 0.8, value = 0.55, step = 0.01),
      sliderInput("k2", "k2 [min^-1]", min = 0.001, max = 1.00, value = 0.03, step = 0.001),
      sliderInput("BLC0", "BLC0 [mmol/l]", min = 0.1, max = 4.0, value = 1.5, step = 0.1),
      actionButton("optimize", "Anpassen: Erweitertes Fitting"),
      fileInput("file_upload", "CSV-Datei hochladen", accept = ".csv")
    ),
    mainPanel(
      plotlyOutput("plot")
    )
  )
)

# Server-Logik
server <- function(input, output, session) {
  
  # Reaktiver Wert für hochgeladene Daten
  uploaded_data <- reactiveVal(NULL)
  
  # Beobachter für CSV-Upload
  observeEvent(input$file_upload, {
    req(input$file_upload)
    df <- read.csv(input$file_upload$datapath)
    
    if ("t_min" %in% names(df) && "BLC_t" %in% names(df)) {
      if (df$t_min[1] != 0.0) {
        df$t_min <- df$t_min - df$t_min[1]
      }
      
      uploaded_data(df)
      
      # Aktualisiere den BLC0 Slider mit dem ersten BLC_t Wert der neuen Daten
      updateSliderInput(session, "BLC0", value = df$BLC_t[1])
    } else {
      showModal(modalDialog(
        title = "Ungültige CSV",
        "Die CSV-Datei muss 't_min' und 'BLC_t' als Spaltennamen enthalten.",
        easyClose = TRUE,
        footer = NULL
      ))
    }
  })
  
  observeEvent(input$optimize, {
    num_attempts <- 100  # Feste Anzahl von Versuchen
    
    data <- if (is.null(uploaded_data())) {
      data.frame(t_min = t_min, BLC_t = BLC_t)
    } else {
      uploaded_data()
    }
    
    # Setze BLC0 auf den ersten BLC_t Wert vor dem Fitting-Prozess
    updateSliderInput(session, "BLC0", value = data$BLC_t[1])
    
    best_fit <- NULL
    best_rss <- Inf
    
    withProgress(message = 'Führe Fitting durch...', value = 0, {
      for (i in 1:num_attempts) {
        # Zufällige Startparameter innerhalb der definierten Grenzen
        start_values <- list(
          A = runif(1, 3.0, 30.0),
          k1 = runif(1, 0.1, 0.8),
          k2 = runif(1, 0.001, 1.00),
          BLC0 = runif(1, 0.1, 4.0)
        )
        
        model_equation <- BLC_t ~ (A * k1 / (k2 - k1)) * (exp(-k1 * t_min) - exp(-k2 * t_min)) + BLC0
        
        fit <- try(nlsLM(model_equation,
                         data = data,
                         start = start_values,
                         lower = c(A = 3.0, k1 = 0.1, k2 = 0.001, BLC0 = 0.1),
                         upper = c(A = 30.0, k1 = 0.8, k2 = 1.00, BLC0 = 4.0),
                         control = nls.lm.control(maxiter = 1024, ftol = 1e-8, ptol = 1e-8)), 
                   silent = TRUE)
        
        if (!inherits(fit, "try-error")) {
          rss <- sum(residuals(fit)^2)
          if (rss < best_rss) {
            best_fit <- fit
            best_rss <- rss
          }
        }
        
        incProgress(1/num_attempts, detail = paste("Versuch", i, "von", num_attempts))
      }
    })
    
    if (!is.null(best_fit)) {
      params <- as.list(coef(best_fit))
      updateSliderInput(session, "A", value = params$A)
      updateSliderInput(session, "k1", value = params$k1)
      updateSliderInput(session, "k2", value = params$k2)
      updateSliderInput(session, "BLC0", value = params$BLC0)
      
      showNotification(
        paste("Fitting erfolgreich abgeschlossen mit", num_attempts, "Versuchen."),
        type = "message"
      )
    } else {
      showNotification(
        "Fitting fehlgeschlagen. Bitte versuchen Sie es erneut oder passen Sie die Startparameter an.",
        type = "error"
      )
    }
  })
  
  output$plot <- renderPlotly({
    A <- input$A
    k1 <- input$k1
    k2 <- input$k2
    BLC0 <- input$BLC0
    
    data <- if (is.null(uploaded_data())) {
      data.frame(t_min = t_min, BLC_t = BLC_t)
    } else {
      uploaded_data()
    }
    
    max_x <- max(data$t_min) * 1.66
    
    t_min_modell <- seq(0, max_x, length.out = 1000)
    BLC_t_modell <- laktat_model_function(t_min_modell, A, k1, k2, BLC0)
    
    max_y <- max(max(BLC_t_modell) * 1.1, max(data$BLC_t) * 1.1)
    
    # Erstelle Gleichungstext
    eq_text <- sprintf("BLC(t) = (%.2f * %.2f / (%.2f - %.2f)) * (e<sup>-%.2f*t</sup> - e<sup>-%.2f*t</sup>) + %.2f", 
                       A, k1, k2, k1, k1, k2, BLC0)
    
    # Berechne R² und RMSE
    BLC_t_modell_at_data <- laktat_model_function(data$t_min, A, k1, k2, BLC0)
    ss_res <- sum((data$BLC_t - BLC_t_modell_at_data)^2)
    ss_tot <- sum((data$BLC_t - mean(data$BLC_t))^2)
    r_squared <- 1 - (ss_res / ss_tot)
    rmse <- sqrt(mean((data$BLC_t - BLC_t_modell_at_data)^2))
    
    # Plotly-Plot
    p <- plot_ly() %>%
      add_trace(x = t_min_modell, y = BLC_t_modell, type = 'scatter', mode = 'lines', 
                name = 'Modellfunktion', line = list(color = '#EF6F6A')) %>%
      add_trace(x = data$t_min, y = data$BLC_t, type = 'scatter', mode = 'markers', 
                name = 'Gemessene BLC-Werte', marker = list(color = '#42BA97', size = 10, symbol = 'x')) %>%
      layout(title = "3-Parameter Bi-exponential Laktat-Modellfunktion",
             xaxis = list(title = "Zeit [min]", range = c(0, max_x)),
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
                 text = sprintf("R²: %.4f, RMSE: %.4f", r_squared, rmse),
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

# Starte die App
shinyApp(ui = ui, server = server)