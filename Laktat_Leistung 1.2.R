library(shiny)
library(plotly)

# UI
ui <- fluidPage(
  titlePanel("Laktat-Leistungs-Modell"),
  sidebarLayout(
    sidebarPanel(
      numericInput("num_rows", "Anzahl der Datenpunkte", 4, min = 1, max = 20),
      uiOutput("input_fields"),
      width = 2  
    ),
    mainPanel(
      width = 9,
      plotlyOutput("plot"),
      uiOutput("instructions")
    )
  )
)

# Server
server <- function(input, output, session) {
  # Initialwerte für die Leistung und ΔBLC
  initial_values <- list(
    list(Leistung = 100, DeltaBLC = 0.3),
    list(Leistung = 200, DeltaBLC = 1.0),
    list(Leistung = 300, DeltaBLC = 2.8),
    list(Leistung = 400, DeltaBLC = 8.0)
  )
  
  # Dynamisch Input-Felder erzeugen
  output$input_fields <- renderUI({
    num_rows <- input$num_rows
    
    lapply(1:num_rows, function(i) {
      initial_value <- if (i <= length(initial_values)) initial_values[[i]] else list(Leistung = 0, DeltaBLC = 0)
      tagList(
        numericInput(paste0("leistung_", i), paste("Leistung", i), value = initial_value$Leistung, step = 10),
        numericInput(paste0("deltaBLC_", i), paste("ΔBLC", i), value = initial_value$DeltaBLC, step = 0.1)
      )
    })
  })
  
  # Reaktive Datenframe basierend auf Eingabewerten
  input_data <- reactive({
    num_rows <- input$num_rows
    if (is.null(num_rows)) return(NULL)
    
    data <- data.frame(Leistung = numeric(num_rows), ΔBLC = numeric(num_rows))
    
    for (i in 1:num_rows) {
      leistung_val <- input[[paste0("leistung_", i)]]
      deltaBLC_val <- input[[paste0("deltaBLC_", i)]]
      
      if (is.null(leistung_val) || is.null(deltaBLC_val)) {
        data$Leistung[i] <- if (i <= length(initial_values)) initial_values[[i]]$Leistung else 0
        data$ΔBLC[i] <- if (i <= length(initial_values)) initial_values[[i]]$DeltaBLC else 0
      } else {
        data$Leistung[i] <- leistung_val
        data$ΔBLC[i] <- deltaBLC_val
      }
    }
    
    data
  })
  
  # Initiale Werte setzen
  observe({
    for (i in 1:length(initial_values)) {
      updateNumericInput(session, paste0("leistung_", i), value = initial_values[[i]]$Leistung)
      updateNumericInput(session, paste0("deltaBLC_", i), value = initial_values[[i]]$DeltaBLC)
    }
  })
  
  # Modell anpassen und Plot aktualisieren
  output$plot <- renderPlotly({
    df_proband <- input_data()
    
    if (is.null(df_proband)) return(NULL)
    
    # Anpassung des Exponentialmodells
    model <- nls(ΔBLC ~ a * exp(b * Leistung), data = df_proband, start = list(a = 1, b = 0.01))
    coef_exponential_model <- coef(model)
    
    # Vorhersagen und Sequenz für den Plot
    x_seq <- seq(min(df_proband$Leistung), max(df_proband$Leistung), length.out = 100)
    y_pred <- predict(model, newdata = data.frame(Leistung = x_seq))
    
    # Plotly-Diagramm
    plot_ly() %>%
      add_trace(data = df_proband, x = ~Leistung, y = ~ΔBLC, type = 'scatter', mode = 'markers',
                name = "ΔBLC",
                marker = list(color = "#2683C6", size = 7.0)) %>%
      add_trace(x = x_seq, y = y_pred, type = 'scatter', mode = 'lines', 
                name = 'Laktat-Modellfunktion', 
                line = list(color = '#62A39F', width = 3)) %>%
      add_annotations(text = sprintf("ΔBLC = %.5f \u00B7 e<sup>%.5f \u00B7 Leistung</sup>", 
                                     coef_exponential_model['a'], coef_exponential_model['b']),
                      x = min(df_proband$Leistung), y = max(y_pred), showarrow = FALSE,
                      xanchor = 'left', yanchor = 'bottom',
                      font = list(family = "Arial, sans-serif", size = 12, color = "black")) %>%
      layout(
        title = "Laktat-Leistungs-Modell",
        margin = list(t = 40),
        xaxis = list(title = "Leistung [Watt]"),
        yaxis = list(title = "ΔBLC [mmol \u00B7 l<sup>-1</sup>]")
      )
  })
  output$instructions <- renderUI({
    HTML(
      "<div style='margin-top: 20px; padding: 10px; background-color: #f0f0f0; border: 1px solid #ddd; border-radius: 5px; width: fit-content;'>
        <h4 style='color: #333;'><strong>Anleitung - Modellanpassung:</strong></h4>
        <ol style='color: #555; list-style-position: outside; padding-left: 20px;'>
          <li>Anzahl der Datenpunkte bestimmen.</li>
          <li>Für jeden Datenpunkt die Leistung (in Watt) und den entsprechenden ΔBLC-Wert eingeben.</li>
          <li>Das exponentielle Modell wird automatisch an die eingegebenen Daten angepasst.</li>
        </ol>
      </div>"
    )
  })
}

# App ausführen
shinyApp(ui = ui, server = server)