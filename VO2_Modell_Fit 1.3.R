library(shiny)
library(plotly)
library(minpack.lm)

# Beispieldaten
t_data <- c(0, 2.8, 5.3, 8.6, 10.4, 15.2, 19.7, 22.5, 24.5, 26.2, 28.2, 31.6, 33.5, 35.3, 37, 38.5, 40.3, 41.9, 43.5, 45.2, 46.8, 48.4, 50, 51.5, 53.3, 54.8, 56.5, 57.9, 59.5, 61.1, 62.9, 64.3, 65.8, 67.7, 69.1, 70.7, 72.4, 73.9, 75.7, 77.2, 78.9, 80.5, 82.7, 84.1, 85.5, 87, 88.6, 90.2, 91.5, 92.9, 94.3, 95.8, 97.6, 99.1, 100.5, 102.1, 103.5, 105.3, 106.6, 108.1, 109.6, 111.4, 112.8, 114.3, 115.8, 117.7, 119.1, 120.8, 122.3, 124.2, 125.7, 127.2, 128.8, 131.1, 132.5, 133.9, 135.4, 136.9, 138.4, 140, 141.3, 143, 144.5, 146.1, 147.6, 149, 150.5, 152.3, 153.9, 155.6, 156.9, 158.6, 160.1, 162.4, 163.7, 165.1)
VO2_data <- c(0.465, 0.34, 0.466, 0.503, 0.452, 0.687, 0.846, 0.963, 0.936, 1.125, 1.238, 1.541, 1.395, 1.685, 1.689, 1.622, 1.917, 1.732, 1.835, 1.801, 1.664, 1.744, 1.891, 1.85, 1.858, 1.89, 2.108, 1.962, 1.992, 1.972, 2.026, 1.868, 1.949, 2.033, 1.887, 1.856, 2.112, 2.117, 2.129, 1.994, 2.03, 1.925, 1.916, 1.949, 1.956, 2.119, 1.894, 2.034, 1.766, 1.994, 1.985, 2.068, 1.796, 2.172, 2.09, 2.017, 2.152, 2.238, 2.027, 2.214, 2.159, 2.098, 1.963, 2.063, 2.292, 2.21, 1.84, 2.102, 2.1, 2.086, 2.008, 2.048, 1.978, 1.87, 2.16, 1.977, 2.335, 2.053, 2.084, 2.056, 2.005, 2.161, 2.088, 2.161, 2.078, 2.038, 2.117, 2.148, 2.043, 2.342, 2.142, 2.306, 2.043, 1.881, 1.918, 2.252)

# UI
ui <- fluidPage(
  titlePanel("EPOC-Modellfunktion"),
  fluidRow(
    column(3,
     style = "height: 90vh; overflow-y: auto;",
     
      tags$h4(tags$strong("Modellparameter:")),
      sliderInput("VO2", "V̇O2 Amplitude", min = 0.0, max = 7.0, value = 1.7, step = 0.001),
      sliderInput("Tau", "Tau", min = 5, max = 360, value = 23, step = 1.0),
      sliderInput("VO2_Start", "V̇O2 Start", min = 0.0, max = 4.0, value = 0.4, step = 0.001),
      sliderInput("VO2_Ruhe", "V̇O2 Ruhe", min = 0, max = 1, value = 0.3, step = 0.001),
      sliderInput("time_delay", "Zeitverzögerung", min = 0, max = 300, value = 11, step = 1),
      conditionalPanel(
        condition = "output.showFitSlider",
        sliderInput("nlsLM_end", "nlsLM Ende", min = 10, max = max(t_data), value = max(t_data), step = 1)
      ),
      actionButton("toggle_data", "Beispieldaten einfügen"),
      br(), br(),
      fileInput("file_upload", "CSV-Datei hochladen", accept = ".csv"),
      tags$h4(tags$strong("Modelanpassung:")),
      actionButton("optimize", "nlsLM - Fit"),
      br(), br(),
      tags$h4(tags$strong("Berechnung - Ruhesauerstoffaufnahme:")),
      radioButtons("geschlecht", "Geschlecht:", choices = c("Männlich", "Weiblich")),
      sliderInput("koerpermasse", "Körpermasse [kg]:", min = 40, max = 150, value = 55),
      sliderInput("koerperlaenge", "Körperlänge [cm]:", min = 140, max = 220, value = 160),
      sliderInput("alter", "Alter [Jahre]:", min = 18, max = 100, value = 24),
      sliderInput("rq", "RQ:", min = 0.7, max = 1.0, value = 0.77, step = 0.01),
      actionButton("berechne_vo2_ruhe", "VO2 Ruhe berechnen")
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
  show_data <- reactiveVal(FALSE)
  uploaded_data <- reactiveVal(NULL)
  
  observeEvent(input$toggle_data, {
    show_data(!show_data())
    max_t <- max(t_data)
    updateSliderInput(session, "nlsLM_end", min = input$time_delay, max = max_t, value = max_t)
  })
  
  observeEvent(input$file_upload, {
    req(input$file_upload)
    df <- read.csv(input$file_upload$datapath)
    if ("t_s" %in% names(df) && "VO2_t" %in% names(df)) {
      uploaded_data(df)
      show_data(TRUE)
      max_t <- max(df$t_s)
      updateSliderInput(session, "nlsLM_end", min = input$time_delay, max = max_t, value = max_t)
    } else {
      showModal(modalDialog(
        title = "Invalid CSV",
        "Die csv-Datei muss 't_s' und 'VO2_t' als Spaltennamen beinhalten.",
        easyClose = TRUE,
        footer = NULL
      ))
    }
  })
  
  # Neue Funktionen zur Berechnung des Grundumsatzes und RMR
  berechne_grundumsatz <- function(geschlecht, masse, laenge, alter) {
    if (geschlecht == "Männlich") {
      return(66.5 + (13.75 * masse) + (5.003 * laenge) - (6.775 * alter))
    } else {
      return(655.1 + (9.563 * masse) + (1.850 * laenge) - (4.676 * alter))
    }
  }
  
  berechne_rmr <- function(grundumsatz, rq, geschlecht) {
    ka <- 19.946  # Annahme für RQ = 0.77, passen Sie dies an, wenn nötig
    faktor <- if(geschlecht == "Männlich") 1.287 else 1.278
    return((grundumsatz / (24 * 60 * ka)) * 4.1868 * faktor)
  }
  
  # Reaktion auf den Klick des "VO2 Ruhe berechnen" Buttons
  observeEvent(input$berechne_vo2_ruhe, {
    grundumsatz <- berechne_grundumsatz(
      input$geschlecht,
      input$koerpermasse,
      input$koerperlaenge,
      input$alter
    )
    
    rmr <- berechne_rmr(grundumsatz, input$rq, input$geschlecht)
    
    # Aktualisieren Sie den VO2_Ruhe Slider mit dem berechneten Wert
    updateSliderInput(session, "VO2_Ruhe", value = round(rmr, 3))
    
    # Zeigen Sie eine Benachrichtigung mit dem berechneten Wert an
    showNotification(paste("Berechnete Ruhesauerstoffaufnahme:", round(rmr, 3), "l/min"), type = "message")
  })
  
  
  
  observe({
    if (input$VO2_Start < input$VO2_Ruhe) {
      updateSliderInput(session, "VO2_Start", value = input$VO2_Ruhe)
    }
  })
  
  output$showFitSlider <- reactive({
    show_data()
  })
  outputOptions(output, "showFitSlider", suspendWhenHidden = FALSE)
  
  observeEvent(input$optimize, {
    if (show_data()) {
      Beispieldaten <- if (is.null(uploaded_data())) {
        data.frame(t_s = t_data, VO2_t = VO2_data)
      } else {
        uploaded_data()
      }
      
      t_delay <- isolate(input$time_delay)
      nlsLM_end <- isolate(input$nlsLM_end)
      
      # Finde den nächstgelegenen Zeitpunkt zu t_delay
      closest_index <- which.min(abs(Beispieldaten$t_s - t_delay))
      
      # Setze VO2_Start auf den gerundeten Wert des nächsten Punktes
      VO2_Start <- round(Beispieldaten$VO2_t[closest_index], 3)
      
      updateSliderInput(session, "VO2_Start", value = VO2_Start)
      
      data_subset <- Beispieldaten[Beispieldaten$t_s >= t_delay & Beispieldaten$t_s <= nlsLM_end, ]
      
      start_values <- list(VO2 = isolate(input$VO2), Tau = isolate(input$Tau))
      
      fit <- try(nlsLM(VO2_t ~ VO2 * (1 - exp(-(t_s - t_delay) / Tau)) + VO2_Start,
                       data = data_subset,
                       start = start_values,
                       lower = c(VO2 = 0.5, Tau = 5),
                       upper = c(VO2 = 7.0, Tau = 360),
                       control = nls.control(maxiter = 1024)), silent = TRUE)
      
      if (!inherits(fit, "try-error")) {
        params <- as.list(coef(fit))
        updateSliderInput(session, "VO2", value = params$VO2)
        updateSliderInput(session, "Tau", value = params$Tau)
      } else {
        showModal(modalDialog(
          title = "nlsLM fehlgeschlagen",
          "Die nicht-lineare Regression mittels Levenberg-Marquardt-Algorithmus konnte nicht erfolgreich durchgeführt werden.",
          easyClose = TRUE,
          footer = NULL
        ))
      }
    }
  })
  
  output$plot <- renderPlotly({
    VO2 <- input$VO2
    Tau <- input$Tau
    VO2_Start <- input$VO2_Start
    t_delay <- input$time_delay
    VO2_Ruhe <- input$VO2_Ruhe
    nlsLM_end <- input$nlsLM_end
    
    model_function <- function(t_s, VO2, Tau, VO2_Start, t_delay) {
      VO2 * (1 - exp(-(t_s - t_delay) / Tau)) + VO2_Start
    }
    
    Beispieldaten <- if (show_data() && !is.null(uploaded_data())) {
      uploaded_data()
    } else {
      data.frame(t_s = t_data, VO2_t = VO2_data)
    }
    
    tau4 <- 4 * Tau
    max_t <- max(Beispieldaten$t_s, na.rm = TRUE)
    max_x <- max(1.05 * tau4 + t_delay, max_t * 1.05)
    t_s <- seq(0, max_x * 1.20, by = 1)
    model_values <- ifelse(t_s >= t_delay, model_function(t_s, VO2, Tau, VO2_Start, t_delay), NA)
    VO2_SS_gross <- model_function(tau4 + t_delay, VO2, Tau, VO2_Start, t_delay)
    VO2_SS_net <- VO2_SS_gross - VO2_Ruhe
    max_y <- max(1.05 * VO2_SS_gross, max(Beispieldaten$VO2_t, na.rm = TRUE) * 1.05)
    
    eq_text <- sprintf("V̇O₂ = %.3f * (1 - e<sup>-(t - %.2f) / %.2f</sup>) + %.3f",
                       VO2, t_delay, Tau, VO2_Start)
    VO2_SS_gross_text <- sprintf("V̇O₂ <sub>SS,gross</sub>: %.3f", VO2_SS_gross)
    VO2_SS_net_text <- sprintf("V̇O₂ <sub>SS,net</sub>: %.3f", VO2_SS_net)
    t_halb <- Tau * log(2)
    
    shapes <- list(
      list(
        type = "line", x0 = t_delay, x1 = t_delay, y0 = 0, y1 = max_y * 1.2,
        line = list(color = "gray", width = 1, dash = "dash")
      ),
      list(
        type = "line", x0 = Tau + t_delay, x1 = Tau + t_delay, y0 = 0, y1 = max_y * 1.2,
        line = list(color = "gray", width = 1, dash = "dash")
      ),
      list(
        type = "line", x0 = tau4 + t_delay, x1 = tau4 + t_delay, y0 = 0, y1 = max_y * 1.2,
        line = list(color = "gray", width = 1, dash = "dash")
      ),
      if (show_data()) {
        list(
          type = "line", x0 = nlsLM_end, x1 = nlsLM_end, y0 = 0, y1 = max_y * 1.2,
          line = list(color = "gray", width = 1, dash = "dash")
        )
      },
      list(
        type = "line", x0 = 0, x1 = max_x, y0 = VO2_Ruhe, y1 = VO2_Ruhe,
        line = list(color = '#1CADE4', width = 1, dash = "solid")
      )
    )
    
    p <- plot_ly() %>%
      add_trace(x = ~t_s, y = ~model_values, type = 'scatter', mode = 'lines',
                name = 'Modellfunktion', line = list(color = '#EF6F6A')) %>%
      layout(title = "V̇O<sub>2</sub>-Modellfunktion",
             margin = list(t = 40),
             xaxis = list(title = "t [s]", range = c(0, max_x)),
             yaxis = list(title = "V̇O<sub>2</sub> [l·min<sup>-1</sup>]", tickformat = ".3f"),
             shapes = shapes,
             annotations = list(
               list(
                 x = max_x * 0.70,
                 y = max_y * 0.6,
                 text = eq_text,
                 showarrow = FALSE,
                 xanchor = 'left',
                 yanchor = 'bottom',
                 font = list(
                   family = "Arial, sans-serif",
                   size = 12,
                   color = "black"
                 )
               ),
               list(
                 x = max_x * 0.70,
                 y = max_y * 0.5,
                 text = VO2_SS_gross_text,
                 showarrow = FALSE,
                 xanchor = 'left',
                 yanchor = 'bottom',
                 font = list(
                   family = "Arial, sans-serif",
                   size = 12,
                   color = "black"
                 )
               ),
               list(
                 x = max_x * 0.70,
                 y = max_y * 0.4,
                 text = VO2_SS_net_text,
                 showarrow = FALSE,
                 xanchor = 'left',
                 yanchor = 'bottom',
                 font = list(
                   family = "Arial, sans-serif",
                   size = 12,
                   color = "black"
                 )
               ),
               list(
                 x = max_x * 0.70,
                 y = max_y * 0.3,
                 text = paste("T<sub>1/2</sub>:", round(t_halb, 1)),
                 showarrow = FALSE,
                 xanchor = 'left',
                 yanchor = 'bottom',
                 font = list(
                   family = "Arial, sans-serif",
                   size = 12,
                   color = "black"
                 )
               ),
               list(
                 x = t_delay, y = max_y * 0.9, text = sprintf("t<sub>delay</sub>: %.1f", t_delay), showarrow = FALSE, xanchor = "left", yanchor = "bottom",
                 textangle = -90, font = list(size = 11)
               ),
               list(
                 x = Tau + t_delay, y = max_y * 0.9, text = sprintf("tau: %.1f", Tau), showarrow = FALSE, xanchor = "left", yanchor = "bottom",
                 textangle = -90, font = list(size = 11)
               ),
               list(
                 x = tau4 + t_delay, y = max_y * 0.9, text = sprintf("4tau: %.1f", tau4), showarrow = FALSE, xanchor = "left", yanchor = "bottom",
                 textangle = -90, font = list(size = 11)
               ),
               if (show_data()) {
                 list(
                   x = nlsLM_end, y = max_y * 0.9, text = sprintf("nlsLM Ende: %.1f", nlsLM_end), showarrow = FALSE, xanchor = "left", yanchor = "bottom",
                   textangle = -90, font = list(size = 11)
                 )
               }
             )) %>%
      add_trace(x = ~t_s, y = ~rep(VO2_Ruhe, length(t_s)), type = 'scatter', mode = 'lines',
                name = 'V̇O<sub>2, Ruhe</sub>', line = list(color = '#1CADE4'))
    
    if (show_data()) {
      model_values_at_data <- model_function(Beispieldaten$t_s, VO2, Tau, VO2_Start, t_delay)
      
      data_subset <- Beispieldaten[Beispieldaten$t_s >= t_delay & Beispieldaten$t_s <= input$nlsLM_end, ]
      
      ss_res <- sum((data_subset$VO2_t - model_values_at_data[Beispieldaten$t_s >= t_delay & Beispieldaten$t_s <= input$nlsLM_end])^2, na.rm = TRUE)
      ss_tot <- sum((data_subset$VO2_t - mean(data_subset$VO2_t, na.rm = TRUE))^2, na.rm = TRUE)
      r_squared <- 1 - (ss_res / ss_tot)
      
      p <- p %>%
        add_trace(data = Beispieldaten, x = ~t_s, y = ~VO2_t, type = 'scatter', mode = 'markers+lines',
                  name = 'V̇O<sub>2</sub>', 
                  marker = list(color = 'rgba(38, 131, 198, 0.9)', size = 5.0),
                  line = list(color = 'rgba(38, 131, 198, 1.0)', width = 0.65, dash = '4 4')) %>%
        layout(annotations = list(
          list(
            x = max_x * 0.70,
            y = max_y * 0.2,
            text = sprintf("R²: %.3f", r_squared),
            showarrow = FALSE,
            xanchor = 'left',
            yanchor = 'bottom',
            font = list(
              family = "Arial, sans-serif",
              size = 12,
              color = "black"
            )
          )
        ))
    }
    
    p
  })
  output$instructions <- renderUI({
    HTML(
      "<div style='margin-top: 20px; padding: 10px; background-color: #f0f0f0; border: 1px solid #ddd; border-radius: 5px; width: fit-content;'>
        <h4 style='color: #333;'><strong>Anleitung - Modellanpassung:</strong></h4>
        <ol style='color: #555; list-style-position: outside; padding-left: 20px;'>
          <li>Beispiel VO2-Daten oder eigene VO2-Daten als CSV-Datei einfügen.</li>
          <li>Die Zeitverzögerung festlegen, um den Startpunkt der Modellanpassung zu bestimmen, und bei Bedarf das Ende der Anpassung anpassen.</li>
          <li>Modellanpassung 'Fit: nlsLM' durchführen.</li>
          <li>Alternativ: Manuelle Modellanpassung der Modellparameter mit den Schiebereglern.</li>
        </ol>
        <div style='margin-top: 20px;'></div>
        <pre style='background-color: #f8f8f8; padding: 10px; border: 1px solid #ddd; border-radius: 5px; width: fit-content;'>
VO2-Daten können als CSV-Datei im folgenden Format hochgeladen werden:
t_s,VO2_t
0.0,0.479
1.0,0.459
2.3,0.488
…
        </pre>
      </div>"
    )
  })
}

# App ausführen
shinyApp(ui = ui, server = server)
