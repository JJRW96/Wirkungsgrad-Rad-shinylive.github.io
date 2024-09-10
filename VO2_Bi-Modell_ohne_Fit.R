library(shiny)
library(plotly)
library(minpack.lm)

# Example data
t_data <- c(0, 2.8, 5.3, 8.6, 10.4, 15.2, 19.7, 22.5, 24.5, 26.2, 28.2, 31.6, 33.5, 35.3, 37, 38.5, 40.3, 41.9, 43.5, 45.2, 46.8, 48.4, 50, 51.5, 53.3, 54.8, 56.5, 57.9, 59.5, 61.1, 62.9, 64.3, 65.8, 67.7, 69.1, 70.7, 72.4, 73.9, 75.7, 77.2, 78.9, 80.5, 82.7, 84.1, 85.5, 87, 88.6, 90.2, 91.5, 92.9, 94.3, 95.8, 97.6, 99.1, 100.5, 102.1, 103.5, 105.3, 106.6, 108.1, 109.6, 111.4, 112.8, 114.3, 115.8, 117.7, 119.1, 120.8, 122.3, 124.2, 125.7, 127.2, 128.8, 131.1, 132.5, 133.9, 135.4, 136.9, 138.4, 140, 141.3, 143, 144.5, 146.1, 147.6, 149, 150.5, 152.3, 153.9, 155.6, 156.9, 158.6, 160.1, 162.4, 163.7, 165.1)
VO2_data <- c(0.465, 0.34, 0.466, 0.503, 0.452, 0.687, 0.846, 0.963, 0.936, 1.125, 1.238, 1.541, 1.395, 1.685, 1.689, 1.622, 1.917, 1.732, 1.835, 1.801, 1.664, 1.744, 1.891, 1.85, 1.858, 1.89, 2.108, 1.962, 1.992, 1.972, 2.026, 1.868, 1.949, 2.033, 1.887, 1.856, 2.112, 2.117, 2.129, 1.994, 2.03, 1.925, 1.916, 1.949, 1.956, 2.119, 1.894, 2.034, 1.766, 1.994, 1.985, 2.068, 1.796, 2.172, 2.09, 2.017, 2.152, 2.238, 2.027, 2.214, 2.159, 2.098, 1.963, 2.063, 2.292, 2.21, 1.84, 2.102, 2.1, 2.086, 2.008, 2.048, 1.978, 1.87, 2.16, 1.977, 2.335, 2.053, 2.084, 2.056, 2.005, 2.161, 2.088, 2.161, 2.078, 2.038, 2.117, 2.148, 2.043, 2.342, 2.142, 2.306, 2.043, 1.881, 1.918, 2.252)

# UI
ui <- fluidPage(
  titlePanel("Biexponentielle V̇O2-Modellfunktion"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      sliderInput("VO2", "V̇O2_fast", min = 0.0, max = 7.0, value = 1.7, step = 0.1),
      sliderInput("tau_fast", "Tau_fast", min = 5, max = 360, value = 23, step = 1.0),
      sliderInput("VO2_Start", "V̇O2_Start", min = 0.0, max = 4.0, value = 0.4, step = 0.1),
      sliderInput("VO2_Ruhe", "V̇O2_Ruhe", min = 0, max = 1, value = 0.4, step = 0.1),
      sliderInput("time_delay", "Zeitverzögerung_fast", min = 0, max = 300, value = 10, step = 1),
      sliderInput("VO2_slow", "V̇O2_slow", min = 0.0, max = 7.0, value = 0.2, step = 0.1),
      sliderInput("tau_slow", "Tau_slow", min = 30, max = 600, value = 120, step = 1.0),
      sliderInput("time_delay_slow", "Zeitverzögerung_slow", min = 0, max = 600, value = 160, step = 1),
      actionButton("toggle_data", "Beispieldaten einfügen"),
      fileInput("file_upload", "CSV-Datei hochladen", accept = ".csv")
    ),
    mainPanel(
      width = 9,
      plotlyOutput("plot")
    )
  )
)

# Server
server <- function(input, output, session) {
  show_data <- reactiveVal(FALSE)
  uploaded_data <- reactiveVal(NULL)
  
  observeEvent(input$toggle_data, {
    show_data(!show_data())
  })
  
  observeEvent(input$file_upload, {
    req(input$file_upload)
    df <- read.csv(input$file_upload$datapath)
    if ("t_s" %in% names(df) && "VO2_t" %in% names(df)) {
      uploaded_data(df)
      show_data(TRUE)
    } else {
      showModal(modalDialog(
        title = "Invalid CSV",
        "Die csv-Datei muss 't_s' und 'VO2_t' als Spaltennamen beinhalten.",
        easyClose = TRUE,
        footer = NULL
      ))
    }
  })
  
  observe({
    if (input$VO2_Start < input$VO2_Ruhe) {
      updateSliderInput(session, "VO2_Start", value = input$VO2_Ruhe)
    }
  })
  
  output$plot <- renderPlotly({
    VO2 <- input$VO2
    tau_fast <- input$tau_fast
    VO2_slow <- input$VO2_slow
    tau_slow <- input$tau_slow
    VO2_Start <- input$VO2_Start
    t_delay <- input$time_delay
    t_delay_slow <- input$time_delay_slow
    VO2_Ruhe <- input$VO2_Ruhe
    
    model_function <- function(t_s, VO2, tau_fast, VO2_slow, tau_slow, VO2_Start, t_delay, t_delay_slow) {
      VO2 * (1 - exp(-(t_s - t_delay) / tau_fast)) + 
        VO2_slow * (1 - exp(-(t_s - t_delay_slow) / tau_slow)) + 
        VO2_Start
    }
    
    model_fast <- function(t_s, VO2, tau_fast, t_delay) {
      VO2 * (1 - exp(-(t_s - t_delay) / tau_fast))
    }
    
    model_slow <- function(t_s, VO2_slow, tau_slow, t_delay_slow) {
      pmax(VO2_slow * (1 - exp(-(t_s - t_delay_slow) / tau_slow)), 0)
    }
    
    Beispieldaten <- if (show_data() && !is.null(uploaded_data())) {
      uploaded_data()
    } else {
      data.frame(t_s = t_data, VO2_t = VO2_data)
    }
    
    max_t <- max(Beispieldaten$t_s, na.rm = TRUE)
    max_x <- max_t * 1.05
    t_s <- seq(0, max_x * 1.20, by = 1)
    
    model_values <- ifelse(t_s >= t_delay, model_function(t_s, VO2, tau_fast, VO2_slow, tau_slow, VO2_Start, t_delay, t_delay_slow), NA)
    fast_values <- ifelse(t_s >= t_delay, model_fast(t_s, VO2, tau_fast, t_delay), NA)
    slow_values <- ifelse(t_s >= t_delay_slow, model_slow(t_s, VO2_slow, tau_slow, t_delay_slow), NA)
    
    max_y <- max(Beispieldaten$VO2_t, na.rm = TRUE) * 1.05
    
    eq_text <- sprintf("V̇O₂ = %.2f * (1 - e<sup>-(t - %.2f) / %.2f</sup>) + %.2f * (1 - e<sup>-(t - %.2f) / %.2f</sup>) + %.2f",
                       VO2, t_delay, tau_fast, VO2_slow, t_delay_slow, tau_slow, VO2_Start)
    t_halb <- tau_fast * log(2)
    
    shapes <- list(
      list(
        type = "line", x0 = t_delay, x1 = t_delay, y0 = 0, y1 = max_y * 1.2,
        line = list(color = "gray", width = 1, dash = "dash")
      ),
      list(
        type = "line", x0 = t_delay_slow, x1 = t_delay_slow, y0 = 0, y1 = max_y * 1.2,
        line = list(color = "gray", width = 1, dash = "dash")
      )
    )
    
    p <- plot_ly() %>%
      add_trace(x = ~t_s, y = ~model_values, type = 'scatter', mode = 'lines',
                name = 'V̇O2-Modellfunktion', line = list(color = '#EF6F6A')) %>%
      add_trace(x = ~t_s, y = ~fast_values, type = 'scatter', mode = 'lines', 
                name = 'V̇O<sub>2</sub><sub>fast</sub>', line = list(color = '#42BA97')) %>%
      add_trace(x = ~t_s, y = ~slow_values, type = 'scatter', mode = 'lines', 
                name = 'V̇O<sub>2</sub><sub>slow</sub>', line = list(color = '#BB7693')) %>%
      layout(title = "Biexponentielle V̇O<sub>2</sub>-Modellfunktion",
             margin = list(t = 40),
             xaxis = list(title = "t [s]"),
             yaxis = list(title = "V̇O<sub>2</sub> [l·min<sup>-1</sup>]", tickformat = ".1f"),
             shapes = shapes,
             annotations = list(
               list(
                 x = max_x * 0.20,
                 y = max_y * 0.3,
                 text = eq_text,
                 showarrow = FALSE,
                 xanchor = 'left',
                 yanchor = 'bottom',
                 font = list(
                   family = "Arial, sans-serif",
                   size = 10,
                   color = "black"
                 )
               ),
               list(
                 x = max_x * 0.2,
                 y = max_y * 0.2,
                 text = paste("T<sub>1/2</sub>:", round(t_halb, 1)),
                 showarrow = FALSE,
                 xanchor = 'left',
                 yanchor = 'bottom',
                 font = list(
                   family = "Arial, sans-serif",
                   size = 10,
                   color = "black"
                 )
               ),
               list(
                 x = t_delay, y = max_y * 0.9, text = sprintf("t<sub>delay</sub>: %.1f", t_delay), showarrow = FALSE, xanchor = "left", yanchor = "bottom",
                 textangle = -90, font = list(size = 11)
               ),
               list(
                 x = t_delay_slow, y = max_y * 0.9, text = sprintf("t<sub>delay_slow</sub>: %.1f", t_delay_slow), showarrow = FALSE, xanchor = "left", yanchor = "bottom",
                 textangle = -90, font = list(size = 11)
               )
             )) %>%
      add_trace(x = ~t_s, y = ~rep(VO2_Ruhe, length(t_s)), type = 'scatter', mode = 'lines',
                name = 'V̇O<sub>2, Ruhe</sub>', line = list(color = '#1CADE4'))
    
    if (show_data()) {
      p <- p %>%
        add_trace(data = Beispieldaten, x = ~t_s, y = ~VO2_t, type = 'scatter', mode = 'markers+lines',
                  name = 'V̇O<sub>2</sub>', 
                  marker = list(color = 'rgba(38, 131, 198, 0.9)', size = 5.0),
                  line = list(color = 'rgba(38, 131, 198, 1.0)', width = 0.65, dash = '4 4'))
    }
    
    p
  })
}

# App ausführen
shinyApp(ui = ui, server = server)