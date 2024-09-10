library(shiny)
library(plotly)
library(minpack.lm)

# Definiere Daten
t_s <- c(0.0, 127.5, 248.8, 378.7, 523.2, 631.9, 765.7, 1014.2, 1193.6, 1402.3, 2001.9, 2600.1, 3200.1, 3800.1) 
BLC_t <- c(2.07, 6.46, 9.54, 9.49, 8.51, 7.26, 7.01, 5.97, 4.84, 3.69, 2.03, 1.3, 0.8, 0.5)

# Normalisiere t_s, wenn der kleinste Wert größer als 0.0 ist
if (min(t_s) > 0.0) {
  t_s <- t_s - min(t_s) # Verschiebe Zeitpunkte, sodass der erste bei 0 liegt
}

# 4-Parameter Modellfunktion für Laktatwerte (in Sekunden)
laktat_model_function <- function(t, BLC_0, A1, gamma1, A2, gamma2) {
  BLC_0 + A1 * (1 - exp(-gamma1 * t)) + A2 * (1 - exp(-gamma2 * t)) # Berechne Laktatwerte
}

# Benutzeroberfläche (UI)
ui <- fluidPage(
  titlePanel("4-Parameter Laktat-Modellfunktion"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      sliderInput("BLC_0", "BLC_0 [mmol/l]", min = 0.5, max = 10.0, value = BLC_t[1], step = 0.1), # Slider für BLC_0
      sliderInput("A1", "A1 [mmol/l]", min = 2.0, max = 20, value = 9.5, step = 0.1),
      sliderInput("gamma1", "γ1 [s^-1]", min = 0.0005, max = 0.02, value = 0.009, step = 0.0001),
      sliderInput("A2", "A2 [mmol/l]", min = -10000, max = 0, value = -20, step = 1),
      sliderInput("gamma2", "γ2 [s^-1]", min = 0.000001, max = 0.01, value = 0.00025, step = 0.000001),
      actionButton("optimize", "Anpassen: nlsLM"), # Button zum Starten der Optimierung
      fileInput("file_upload", "CSV-Datei hochladen", accept = ".csv") # Neuer Button für CSV-Upload
    ),
    mainPanel(
      plotlyOutput("plot") # Ausgabe des Plots
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
    
    if ("t_s" %in% names(df) && "BLC_t" %in% names(df)) {
      if (df$t_s[1] != 0.0) {
        df$t_s <- df$t_s - df$t_s[1]
      }
      
      uploaded_data(df)
      
      # Aktualisiere den BLC_0 Slider mit dem ersten BLC_t Wert der neuen Daten
      updateSliderInput(session, "BLC_0", value = df$BLC_t[1])
    } else {
      showModal(modalDialog(
        title = "Ungültige CSV",
        "Die CSV-Datei muss 't_s' und 'BLC_t' als Spaltennamen enthalten.",
        easyClose = TRUE,
        footer = NULL
      ))
    }
  })
  
  observeEvent(input$optimize, {
    data <- if (is.null(uploaded_data())) {
      data.frame(t_s = t_s, BLC_t = BLC_t)
    } else {
      uploaded_data()
    }
    
    # Setze BLC_0 auf den ersten BLC_t Wert vor dem Fitting-Prozess
    updateSliderInput(session, "BLC_0", value = data$BLC_t[1])
    
    start_values <- list(A1 = input$A1, gamma1 = input$gamma1, 
                         A2 = input$A2, gamma2 = input$gamma2) # Startwerte für Optimierung
    
    model_equation <- BLC_t ~ BLC_t[1] + A1 * (1 - exp(-gamma1 * t_s)) + 
      A2 * (1 - exp(-gamma2 * t_s)) # Modellgleichung
    
    fit <- try(nlsLM(model_equation,
                     data = data,
                     start = start_values,
                     lower = c(A1 = 2.0, gamma1 = 0.0005, A2 = -10000, gamma2 = 0.000001),
                     upper = c(A1 = 20, gamma1 = 0.02, A2 = 0, gamma2 = 0.001),
                     control = nls.control(maxiter = 1024)), silent = TRUE) # Durchführung der Optimierung
    
    if (!inherits(fit, "try-error")) {
      params <- as.list(coef(fit)) # Extrahiere optimierte Parameter
      updateSliderInput(session, "A1", value = params$A1) # Aktualisiere UI-Elemente
      updateSliderInput(session, "gamma1", value = params$gamma1)
      updateSliderInput(session, "A2", value = params$A2)
      updateSliderInput(session, "gamma2", value = params$gamma2)
    } else {
      showModal(modalDialog( # Zeige Fehlermeldung bei fehlgeschlagener Optimierung
        title = "nlsLM fehlgeschlagen",
        "Die nicht-lineare Regression mit dem Levenberg-Marquardt-Algorithmus konnte nicht erfolgreich durchgeführt werden.",
        easyClose = TRUE,
        footer = NULL
      ))
    }
  })
  
  output$plot <- renderPlotly({
    BLC_0 <- input$BLC_0  # Verwende den Slider-Wert für BLC_0
    A1 <- input$A1
    gamma1 <- input$gamma1
    A2 <- input$A2
    gamma2 <- input$gamma2
    
    data <- if (is.null(uploaded_data())) {
      data.frame(t_s = t_s, BLC_t = BLC_t)
    } else {
      uploaded_data()
    }
    
    max_x <- max(data$t_s) * 1.66 # Bestimme maximalen x-Wert für Plot
    
    t_s_modell <- seq(0, max_x, length.out = 1000)
    BLC_t_modell <- laktat_model_function(t_s_modell, BLC_0, A1, gamma1, A2, gamma2) # Berechne Modellwerte
    
    max_y <- max(max(BLC_t_modell) * 1.1, max(data$BLC_t) * 1.1)
    
    # Erstelle Gleichungstext
    eq_text <- sprintf("BLK(t) = %.2f + %.2f · (1-e<sup>-%.4f·t</sup>) + %.2f · (1-e<sup>-%.6f·t</sup>)", 
                       BLC_0, A1, gamma1, A2, gamma2)
    
    # Berechne R²
    BLC_t_modell_at_data <- laktat_model_function(data$t_s, BLC_0, A1, gamma1, A2, gamma2) # Modellwerte an Datenpunkten
    ss_res <- sum((data$BLC_t - BLC_t_modell_at_data)^2) # Summe der Fehlerquadrate
    ss_tot <- sum((data$BLC_t - mean(data$BLC_t))^2) # Gesamtvarianz
    r_squared <- 1 - (ss_res / ss_tot) # Berechne R²
    
    # Plotly-Plot
    p <- plot_ly() %>%
      add_trace(x = t_s_modell, y = BLC_t_modell, type = 'scatter', mode = 'lines', 
                name = 'Modellfunktion', line = list(color = '#EF6F6A')) %>%
      add_trace(x = data$t_s, y = data$BLC_t, type = 'scatter', mode = 'markers', 
                name = 'Gemessene BLC-Werte', marker = list(color = '#42BA97', size = 10, symbol = 'x')) %>%
      layout(title = "4-Parameter Laktat-Modellfunktion",
             xaxis = list(title = "Zeit [s]", range = c(0, max_x)),
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
                 text = sprintf("R²: %.2f", r_squared),
                 showarrow = FALSE,
                 xanchor = 'left',
                 yanchor = 'bottom',
                 font = list(
                   size = 12,
                   color = "black"
                 )
               )
             ))
    
    p # Gebe den Plot zurück
  })
}

# Starte die App
shinyApp(ui = ui, server = server)