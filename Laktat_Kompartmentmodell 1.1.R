library(shiny)
library(plotly)
library(minpack.lm)

# Definiere Daten
t_min <- c(0.4, 3.3, 6.5, 8.4, 10.2, 12.5, 14.9, 16.7, 19.3)
BLC_t <- c(0.82, 4.53, 6.97, 7.52, 7.73, 7.28, 6.37, 5.41, 4.71)

# Normalisiere t_min, wenn der kleinste Wert größer als 0.0 ist
if (min(t_min) > 0.0) {
  t_min <- t_min - min(t_min)
}

# Freund et al. Kompartmentmodell-Funktion
freund_model <- function(t, A1, A2, gamma1, gamma2, La0) {
  La0 + A1 * (1 - exp(-gamma1 * t)) + A2 * (1 - exp(-gamma2 * t))
}

# Berechne tmax und Lamax
calculate_tmax_lamax <- function(A1, A2, gamma1, gamma2, La0) {
  f <- function(t) freund_model(t, A1, A2, gamma1, gamma2, La0)
  tmax <- optimize(f, c(0, 100), maximum = TRUE)$maximum
  Lamax <- f(tmax)
  list(tmax = tmax, Lamax = Lamax)
}

# Benutzeroberfläche (UI)
ui <- fluidPage(
  titlePanel("Kompartmentmodell für Laktat-Modellierung"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      sliderInput("A1", "A1 [mmol/l]", min = 0.0, max = 100.0, value = 20.0, step = 0.1),
      sliderInput("A2", "A2 [mmol/l]", min = -100.0, max = 0.0, value = -20.0, step = 0.1),
      sliderInput("gamma1", "γ1 [min^-1]", min = 0.05, max = 2.0, value = 0.1, step = 0.001),
      sliderInput("gamma2", "γ2 [min^-1]", min = 0.01, max = 0.5, value = 0.05, step = 0.001),
      sliderInput("La0", "La0 [mmol/l]", min = 0.0, max = 5.0, value = 1.0, step = 0.01),
      actionButton("optimize", "Anpassen: nlsLM"),
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
    data <- if (is.null(uploaded_data())) {
      data.frame(t_min = t_min, BLC_t = BLC_t)
    } else {
      uploaded_data()
    }
    
    # Setze La0 Grenzen basierend auf dem ersten BLC_t Wert
    La0_lower <- max(0, data$BLC_t[1] * 0.9)
    La0_upper <- min(5, data$BLC_t[1] * 1.1)
    
    best_fit <- NULL
    best_rss <- Inf
    
    withProgress(message = 'Fitting in progress', value = 0, {
      for (i in 1:100) {
        # Randomisierte Startwerte
        A1_start <- runif(1, 0, 100)
        A2_start <- runif(1, -100, 0)
        gamma1_start <- runif(1, 0.05, 2)
        gamma2_start <- runif(1, 0.01, 0.5)
        La0_start <- runif(1, La0_lower, La0_upper)
        
        start_values <- list(A1 = A1_start, A2 = A2_start, gamma1 = gamma1_start, gamma2 = gamma2_start, La0 = La0_start)
        
        model_equation <- BLC_t ~ La0 + A1 * (1 - exp(-gamma1 * t_min)) + A2 * (1 - exp(-gamma2 * t_min))
        
        fit <- try(nlsLM(model_equation,
                         data = data,
                         start = start_values,
                         lower = c(A1 = 0.0, A2 = -100.0, gamma1 = 0.05, gamma2 = 0.01, La0 = La0_lower),
                         upper = c(A1 = 100.0, A2 = 0.0, gamma1 = 2.0, gamma2 = 0.5, La0 = La0_upper),
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
      updateSliderInput(session, "A1", value = params$A1)
      updateSliderInput(session, "A2", value = params$A2)
      updateSliderInput(session, "gamma1", value = params$gamma1)
      updateSliderInput(session, "gamma2", value = params$gamma2)
      updateSliderInput(session, "La0", value = params$La0)
      
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
    A1 <- input$A1
    A2 <- input$A2
    gamma1 <- input$gamma1
    gamma2 <- input$gamma2
    La0 <- input$La0
    
    data <- if (is.null(uploaded_data())) {
      data.frame(t_min = t_min, BLC_t = BLC_t)
    } else {
      uploaded_data()
    }
    
    max_x <- max(data$t_min) * 1.66
    
    t_min_modell <- seq(0, max_x, length.out = 1000)
    BLC_t_modell <- freund_model(t_min_modell, A1, A2, gamma1, gamma2, La0)
    
    max_y <- max(max(BLC_t_modell) * 1.2, max(data$BLC_t) * 1.2)
    
    # Berechne tmax und Lamax
    results <- calculate_tmax_lamax(A1, A2, gamma1, gamma2, La0)
    tmax <- results$tmax
    Lamax <- results$Lamax
    
    # Erstelle Gleichungstext
    eq_text <- sprintf("[La]<sub>b(t)</sub> = %.2f + %.1f * (1 - e<sup>-%.3f*t</sup>) + %.1f * (1 - e<sup>-%.3f*t</sup>)", 
                       La0, A1, gamma1, A2, gamma2)
    
    # Berechne R²
    BLC_t_modell_at_data <- freund_model(data$t_min, A1, A2, gamma1, gamma2, La0)
    ss_res <- sum((data$BLC_t - BLC_t_modell_at_data)^2)
    ss_tot <- sum((data$BLC_t - mean(data$BLC_t))^2)
    r_squared <- 1 - (ss_res / ss_tot)
    
    # Plotly-Plot
    p <- plot_ly() %>%
      add_trace(x = t_min_modell, y = BLC_t_modell, type = 'scatter', mode = 'lines', 
                name = 'Modellgleichung', line = list(color = '#EF5350')) %>%
      add_trace(x = data$t_min, y = data$BLC_t, type = 'scatter', mode = 'markers', 
                name = 'Gemessene BLC-Werte', marker = list(color = "#42BA97", size = 9, symbol = 'x')) %>%
      add_segments(x = tmax, xend = tmax, y = 0, yend = max_y, 
                   line = list(color = 'darkgrey', width = 1, dash = 'dash'), 
                   showlegend = FALSE, opacity = 0.5) %>%
      add_segments(x = 0, xend = max_x, y = Lamax, yend = Lamax, 
                   line = list(color = 'darkgrey', width = 1, dash = 'dash'), 
                   showlegend = FALSE, opacity = 0.5) %>%
      layout(title = "Freund et al. Kompartmentmodell für Laktat-Modellierung",
             margin = list(t = 40),
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
                 text = sprintf("R²: %.4f", r_squared),
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
                 y = max_y * 0.85,
                 text = sprintf("BLC<sub>max</sub>: %.2f mmol/l, TBLC<sub>max</sub>: %.2f min", Lamax, tmax),
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