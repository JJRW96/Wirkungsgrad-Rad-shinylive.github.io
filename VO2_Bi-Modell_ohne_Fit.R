library(shiny)
library(plotly)
library(minpack.lm)

# Example data
t_data <- c(0.0, 2.7, 5.3, 8.3, 11.5, 13.6, 15.4, 17.1, 19.8, 21.1, 22.3, 23.5, 24.9, 27.5, 30.9, 32.4, 35.0, 38.3, 39.7, 41.6, 43.7, 45.3, 46.8, 49.8, 52.0, 53.6, 55.5, 57.5, 59.0, 60.7, 62.4, 64.4, 66.6, 68.1, 69.8, 71.4, 73.3, 75.0, 76.7, 78.4, 80.0, 82.2, 83.9, 85.4, 87.3, 88.8, 90.6, 92.0, 93.6, 94.8, 96.3, 97.9, 99.5, 101.1, 102.7, 104.3, 106.0, 107.7, 109.3, 110.9, 112.5, 114.8, 116.4, 118.1, 119.7, 121.3, 122.8, 124.3, 125.9, 127.4, 128.9, 130.6, 132.1, 133.6, 135.6, 137.1, 138.6, 140.1, 141.6, 143.1, 144.4, 145.8, 147.2, 148.4, 149.9, 151.6, 153.1, 154.7, 156.1, 157.4, 158.7, 160.5, 161.9, 163.3, 164.7, 166.2, 167.6, 169.1, 172.6, 173.9, 175.4, 176.7, 178.1, 179.6, 181.0, 182.4, 183.8, 185.3, 186.7, 188.2, 189.6, 191.0, 192.4, 193.8, 195.2, 196.6, 198.0, 199.3, 200.7, 202.0, 203.3, 204.7, 206.1, 207.5, 208.8, 210.2, 211.6, 213.0, 214.3, 215.7, 217.1, 218.4, 219.8, 221.1, 222.4, 223.7, 225.0, 226.3, 227.7, 229.0, 230.5, 231.9, 233.1, 234.4)
VO2_data <- c(0.823, 0.898, 0.966, 1.122, 1.281, 1.313, 1.463, 1.537, 1.602, 1.595, 1.598, 1.674, 1.923, 2.168, 2.439, 2.670, 2.797, 2.712, 2.688, 2.968, 3.228, 3.371, 3.673, 3.938, 4.076, 3.959, 3.886, 3.880, 3.928, 3.962, 4.173, 4.381, 4.268, 4.208, 4.234, 4.132, 4.109, 4.192, 4.304, 4.493, 4.352, 4.349, 4.332, 4.346, 4.345, 4.373, 4.320, 4.286, 4.353, 4.346, 4.287, 4.322, 4.250, 4.332, 4.257, 4.161, 4.120, 4.144, 4.115, 4.158, 4.186, 4.206, 4.220, 4.259, 4.291, 4.279, 4.295, 4.339, 4.371, 4.425, 4.448, 4.474, 4.517, 4.536, 4.544, 4.528, 4.432, 4.371, 4.367, 4.430, 4.492, 4.535, 4.510, 4.433, 4.399, 4.305, 4.308, 4.304, 4.366, 4.434, 4.501, 4.535, 4.512, 4.463, 4.509, 4.529, 4.586, 4.624, 4.740, 4.769, 4.788, 4.794, 4.824, 4.830, 4.888, 4.887, 4.915, 4.924, 4.955, 4.965, 4.982, 4.992, 5.016, 5.024, 5.033, 5.066, 5.081, 5.053, 5.035, 4.997, 5.018, 5.038, 5.035, 5.039, 4.970, 4.970, 4.973, 4.964, 4.945, 4.953, 4.974, 4.984, 4.997, 4.995, 4.946, 4.997, 5.014, 5.092, 5.072, 5.111, 5.088, 5.065, 5.061, 5.038)
# UI
ui <- fluidPage(
  titlePanel("Biexponentielle V̇O2-Modellfunktion"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      sliderInput("VO2", "V̇O2_fast", min = 0.0, max = 7.0, value = 3.7, step = 0.1),
      sliderInput("tau_fast", "Tau_fast", min = 5, max = 360, value = 45, step = 1.0),
      sliderInput("VO2_Start", "V̇O2_Start", min = 0.0, max = 4.0, value = 0.8, step = 0.1),
      sliderInput("VO2_Ruhe", "V̇O2_Ruhe", min = 0, max = 1, value = 0.4, step = 0.1),
      sliderInput("time_delay", "Zeitverzögerung_fast", min = 0, max = 300, value = 0, step = 1),
      sliderInput("VO2_slow", "V̇O2_slow", min = 0.0, max = 7.0, value = 0.5, step = 0.1),
      sliderInput("tau_slow", "Tau_slow", min = 30, max = 600, value = 110, step = 1.0),
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
      VO2 * (1 - exp(-(pmax(t_s - t_delay, 0)) / tau_fast)) + 
        pmax(VO2_slow * (1 - exp(-(pmax(t_s - t_delay_slow, 0)) / tau_slow)), 0) + 
        VO2_Start
    }
    
    model_fast <- function(t_s, VO2, tau_fast, t_delay) {
      VO2 * (1 - exp(-(pmax(t_s - t_delay, 0)) / tau_fast))
    }
    
    model_slow <- function(t_s, VO2_slow, tau_slow, t_delay_slow) {
      pmax(VO2_slow * (1 - exp(-(pmax(t_s - t_delay_slow, 0)) / tau_slow)), 0)
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