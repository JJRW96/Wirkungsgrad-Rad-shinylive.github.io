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

# Bateman-Funktion für Laktatwerte (in Minuten) mit BLC0
bateman_function <- function(t, a, k1, k2, BLC0) {
  BLC0 + (a * k1 / (k2 - k1)) * (exp(-k1 * t) - exp(-k2 * t))
}

# Invasions- und Eliminationsfunktionen
invasion_function <- function(t, a, k1, BLC0) {
  BLC0 + a * (1 - exp(-k1 * t))
}

elimination_function <- function(t, a, k2, BLC0) {
  BLC0 + a * exp(-k2 * t)
}

# Benutzeroberfläche (UI)
ui <- fluidPage(
  titlePanel("Bateman-Funktion für Laktat-Modellierung"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      sliderInput("a", "a [mmol/l]", min = 0.0, max = 30.0, value = 15.0, step = 0.01),
      sliderInput("k1", "k1 [min^-1]", min = 0.1, max = 3.5, value = 0.5, step = 0.001),
      sliderInput("k2", "k2 [min^-1]", min = 0.020, max = 0.200, value = 0.10, step = 0.001),
      sliderInput("BLC0", "BLC0 [mmol/l]", min = 0.0, max = 5.0, value = 1.0, step = 0.01),
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
    
    # Setze BLC0 Grenzen basierend auf dem ersten BLC_t Wert
    BLC0_lower <- max(0, data$BLC_t[1] * 0.9)
    BLC0_upper <- min(5, data$BLC_t[1] * 1.1)
    
    best_fit <- NULL
    best_rss <- Inf
    
    withProgress(message = 'Fitting in progress', value = 0, {
      for (i in 1:100) {
        # Randomisierte Startwerte
        a_start <- runif(1, 0, 30)
        k1_start <- runif(1, 0.1, 3.5)
        k2_start <- runif(1, 0.020, 0.200)
        BLC0_start <- runif(1, BLC0_lower, BLC0_upper)
        
        start_values <- list(a = a_start, k1 = k1_start, k2 = k2_start, BLC0 = BLC0_start)
        
        model_equation <- BLC_t ~ BLC0 + (a * k1 / (k2 - k1)) * (exp(-k1 * t_min) - exp(-k2 * t_min))
        
        fit <- try(nlsLM(model_equation,
                         data = data,
                         start = start_values,
                         lower = c(a = 0.0, k1 = 0.1, k2 = 0.020, BLC0 = BLC0_lower),
                         upper = c(a = 30.0, k1 = 3.5, k2 = 0.200, BLC0 = BLC0_upper),
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
      updateSliderInput(session, "a", value = params$a)
      updateSliderInput(session, "k1", value = params$k1)
      updateSliderInput(session, "k2", value = params$k2)
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
    a <- input$a
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
    BLC_t_modell <- bateman_function(t_min_modell, a, k1, k2, BLC0)
    invasion_modell <- invasion_function(t_min_modell, a, k1, BLC0)
    elimination_modell <- elimination_function(t_min_modell, a, k2, BLC0)
    
    max_y <- max(max(BLC_t_modell) * 1.2, max(data$BLC_t) * 1.2, max(invasion_modell) * 1.2, max(elimination_modell) * 1.2)
    
    # Berechne tmax und Lamax
    tmax <- log(k1/k2) / (k1 - k2)
    Lamax <- bateman_function(tmax, a, k1, k2, BLC0)
    
    # Erstelle Gleichungstext
    eq_text <- sprintf("[La]<sub>b(t)</sub> = %.2f + (%.2f * %.3f / (%.3f - %.3f)) * (e<sup>-%.3f*t</sup> - e<sup>-%.3f*t</sup>)", 
                       BLC0, a, k1, k2, k1, k1, k2)
    
    # Berechne R²
    BLC_t_modell_at_data <- bateman_function(data$t_min, a, k1, k2, BLC0)
    ss_res <- sum((data$BLC_t - BLC_t_modell_at_data)^2)
    ss_tot <- sum((data$BLC_t - mean(data$BLC_t))^2)
    r_squared <- 1 - (ss_res / ss_tot)
    
    # Plotly-Plot
    p <- plot_ly() %>%
      add_trace(x = t_min_modell, y = BLC_t_modell, type = 'scatter', mode = 'lines', 
                name = 'Bateman-Funktion', line = list(color = '#EF5350')) %>%
      add_trace(x = t_min_modell, y = invasion_modell, type = 'scatter', mode = 'lines', 
                name = 'Invasion', line = list(color = "#9C85C0", width = 2, dash = '8 5'),
                opacity = 0.65) %>%
      add_trace(x = t_min_modell, y = elimination_modell, type = 'scatter', mode = 'lines', 
                name = 'Elimination', line = list(color = '#2683C6', width = 2, dash = '8 5'),
                opacity = 0.65) %>%
      add_trace(x = data$t_min, y = data$BLC_t, type = 'scatter', mode = 'markers', 
                name = 'Gemessene BLC-Werte', marker = list(color = "#42BA97", size = 9, symbol = 'x')) %>%
      add_segments(x = tmax, xend = tmax, y = 0, yend = max_y, 
                   line = list(color = 'darkgrey', width = 1, dash = 'dash'), 
                   showlegend = FALSE, opacity = 0.5) %>%
      add_segments(x = 0, xend = max_x, y = Lamax, yend = Lamax, 
                   line = list(color = 'darkgrey', width = 1, dash = 'dash'), 
                   showlegend = FALSE, opacity = 0.5) %>%
      layout(title = "Bateman-Funktion für Laktat-Modellierung",
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