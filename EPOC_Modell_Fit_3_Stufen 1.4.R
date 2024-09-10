library(shiny)
library(plotly)
library(minpack.lm)
library(dplyr)
library(shinyjs)

# Beispieldaten
t_data <- c(0, 1.2, 2.3, 3.6, 4.7, 5.9, 7, 8.1, 9.3, 10.5, 11.6, 12.8, 14, 15.3, 16.6, 18.2, 19.4, 20.7, 22.7, 23.9, 25.3, 26.7, 28.1, 29.6, 31, 32.5, 34.1, 35.5, 36.9, 38.3, 39.7, 41.2, 42.6, 44.4, 45.7, 47.9, 49.2, 50.4, 51.8, 53.7, 55, 56.3, 58, 59.5, 62.1, 63.8, 65.3, 66.8, 68.1, 69.6, 71, 72.4, 74, 75.4, 77.8, 79.2, 80.5, 81.9, 83.3, 84.8, 86.2, 87.6, 88.9, 90.3, 92, 93.5, 94.9, 96.2, 97.6, 98.8, 100.3, 101.8, 103.4, 104.7, 107.9, 109.4, 111, 112.6, 114.1, 115.5, 117.2, 118.9, 120.6, 122, 123.3, 124.8, 127.8, 129.6, 131, 132.6, 134.1, 135.5, 137, 138.4, 139.7, 140.9, 142.2, 143.6, 145, 146.2, 147.2, 148.6, 149.8, 150.9, 152.3, 153.9, 155.5, 156.8, 158.2, 160.5, 162, 164.6, 167.4, 170.8, 172.5, 174.3, 175.8, 177.6, 179.2, 180.6, 182.1, 185.5, 187, 188.5, 190, 191.6, 193.3, 195, 196.6, 198.4, 200, 201.8, 203.4, 205, 207.1, 208.9, 210.5, 212, 214, 215.6, 217.5, 219.4, 221.6, 223.2, 225.1, 226.6, 227.8, 229.1, 230.1, 231.4, 233.3, 234.9, 236.5, 238.1, 239.6, 241.3, 242.7, 245.1, 246.6, 248.6, 250.9, 253, 254.4, 256.1, 257.4, 260.3, 263.4, 265, 266.5, 268.1, 269.8, 271.7, 273.2, 274.8, 276.6, 278.3, 280.6, 282.3, 285.9, 287.6, 289.2, 290.7, 292.4, 294.3, 296, 297.6, 299.4, 301, 302.5, 304.1, 306.8, 308.6, 310.4, 312.8, 314.8, 316.9, 318.8, 320.9, 322.7, 324.4, 327.6, 330.5, 332.3, 334.1, 336.1, 338, 339.7, 341.4, 342.9, 344.5, 346.1, 347.8, 349.2, 350.7, 352.5, 354.3, 355.5, 357, 358.3, 359.8, 360.7, 361.7, 363.6, 365.3, 366.9, 368.4, 370, 371.6, 373, 376.6, 378.3, 382.2, 383.8, 385.4, 386.8, 388.6, 390.2, 391.8, 393.4, 394.9, 396.5, 398.1, 399.9, 401.6, 403.3, 405.9, 407.6, 409.6, 410.7, 412.2, 414.6, 415.7, 417.2, 418.4, 420, 421.7, 423.5, 425.2, 427, 429.1, 432.8, 434.5, 437.9, 439.7, 441.3, 442.9, 444.4, 445.6, 447.4, 448.7, 451.2, 453, 454.6, 456, 457.3, 458.6, 459.7, 461.3, 462.9, 463.7, 467.7, 469, 470.8, 471.9, 473.2, 473.9, 475, 476.4, 480.2, 481, 484.7, 486.1, 487.5, 492.3, 494, 495.9, 499.4, 501, 502.7, 504.4, 506.3, 508.3, 509.7, 510.7, 512.4, 514.5, 516.3, 518, 519.7, 521.6, 523.3, 524.9, 526.3, 528, 529.6, 531.3, 533.1, 534.8, 536.6, 538.4, 541.6, 543.4, 545, 546.9, 548.8, 550.6, 552.6, 554.3, 556.1, 557.9, 559.6, 561.5, 563.2, 564.8, 566.3, 567.8, 569, 570.5, 572.3, 573.9, 575.6, 577.5, 579.2, 581.9, 583.3, 585.5, 587.2, 589.2, 591.8, 594.2, 596, 600.2, 602.2, 604.2, 606, 609.8, 611.8, 613.7, 615.7, 617.5, 619.6, 621.3, 623.1, 624.7, 626.5, 628.3, 630, 631.7, 633.6, 635.5, 637.6, 639.6, 641.7, 643.4, 645.3, 647.1, 649, 650.7, 652.4, 654.2, 656.1, 658, 659.8, 661.5, 663.3, 665, 666.9, 669.1, 670.8, 672.6, 675.6, 677.4, 679.6, 681.6, 684.2, 685.9, 688, 690.5, 692.4, 694.9, 697.5, 699.4, 701.6, 703.7, 706.1, 708.1, 710, 712.1, 715.6, 717.7, 719.8, 722.1, 724.2, 726.4, 728.6, 730.5, 734.6, 736.8, 738.7, 740.5, 742.2, 744, 745.8, 747.6, 749.5, 751.8, 753.7, 755.8, 757.9, 759.8, 761.9, 763.9, 766.2, 768.5, 770.6, 773.1, 775.3, 777.3, 779.6, 781.5, 783.5, 785.4, 787.3, 789.3, 792.5, 795, 797.2, 799.2, 801, 802.9, 804.9, 806.5, 808.2, 810.3, 812.2, 814.3, 818.3, 820.3, 823.4, 826.8, 829.5, 831.8, 834, 836.7, 839, 841.1, 843.4, 845.6, 847.7, 850.1, 852.4, 854.5, 856.8, 859.1, 861.8, 864.1, 866, 868.1, 870.5, 873.1, 875.6, 878.1, 881.2, 884.6, 889.2, 891.6, 894.1, 896.7, 899.3)
VO2_data <- c(3.624, 3.734, 3.714, 3.494, 3.8, 3.803, 3.736, 3.534, 3.644, 3.583, 3.606, 3.459, 3.203, 3.376, 3.272, 3.392, 3.555, 3.283, 2.984, 3.042, 3.161, 3.212, 3.075, 3.163, 3.067, 3.181, 2.952, 2.929, 3.116, 2.828, 2.805, 2.768, 2.768, 2.576, 2.186, 2.014, 2.914, 2.692, 2.659, 2.304, 2.197, 2.347, 2.17, 2.038, 1.463, 2.492, 2.307, 2.083, 1.997, 2.002, 1.974, 1.847, 1.737, 1.693, 1.576, 1.375, 1.588, 1.701, 1.75, 1.632, 1.725, 1.606, 1.655, 1.665, 1.408, 1.638, 1.592, 1.61, 1.537, 1.441, 1.548, 1.564, 1.69, 1.395, 0.969, 1.977, 1.927, 1.602, 1.589, 1.498, 1.512, 1.413, 1.448, 1.536, 1.434, 1.473, 1.369, 1.019, 1.182, 1.44, 1.548, 1.329, 1.112, 1.398, 1.317, 1.429, 1.356, 1.509, 1.427, 0.864, 1.102, 1.12, 1.943, 1.636, 1.943, 1.738, 1.5, 1.01, 0.498, 1.974, 1.618, 1.571, 1.441, 0.833, 1.378, 1.557, 1.543, 1.305, 1.27, 1.379, 1.227, 1.29, 1.206, 1.329, 1.433, 1.253, 1.072, 1.218, 1.419, 1.51, 1.309, 1.117, 1.218, 1.259, 1.603, 0.856, 1.175, 1.473, 1.393, 1.405, 0.982, 0.815, 0.929, 1.033, 1.636, 1.382, 1.295, 1.278, 0.789, 1.35, 0.963, 0.582, 1.24, 1.013, 0.727, 1.126, 1.348, 0.504, 0.941, 1.153, 1.336, 1.391, 2.357, 1.859, 1.196, 1.536, 0.834, 1.612, 1.631, 1.752, 1.375, 1.349, 1.47, 1.33, 1.396, 1.467, 1.491, 1.47, 0.746, 1.638, 1.664, 1.339, 1.133, 1.256, 1.094, 1.199, 1.078, 1.404, 1.289, 1.199, 1.491, 0.719, 0.904, 0.565, 0.814, 0.922, 1.465, 0.963, 1.657, 1.801, 0.844, 0.82, 1.202, 1.173, 1.227, 1.037, 1.376, 0.964, 1.809, 1.75, 1.497, 1.215, 1.36, 1.371, 0.872, 1.017, 1.67, 1.273, 1.337, 1.267, 0.584, 1.227, 1.164, 1.206, 1.283, 1.121, 1.108, 1.167, 0.965, 0.439, 1.376, 1.381, 1.128, 0.828, 1.135, 0.87, 1.16, 1.195, 1.454, 1.139, 0.988, 1.143, 0.937, 1.17, 1.175, 0.704, 1.218, 1.274, 0.859, 1.019, 1.189, 1.556, 1.75, 1.227, 1.227, 1.262, 1.31, 1.378, 1.157, 1.138, 1.248, 0.776, 0.702, 1.252, 1.308, 1.476, 1.277, 0.88, 1.174, 1.135, 1.058, 0.609, 0.724, 1.632, 1.352, 1.866, 1.632, 0.791, 1.142, 0.748, 0.649, 1.224, 1.028, 1.042, 1.185, 1.207, 0.912, 1.722, 0.656, 1.147, 0.852, 2.311, 2.267, 0.825, 1.665, 1.219, 1.532, 1.203, 1.145, 0.982, 1.059, 1.314, 0.678, 0.736, 1.565, 1.006, 0.791, 1.166, 1.134, 0.982, 1.168, 0.985, 1.127, 1.23, 0.969, 1.171, 0.9, 1.159, 1.115, 1.368, 0.55, 0.911, 1.329, 1.333, 1.288, 1.026, 1.142, 0.938, 1.214, 1.182, 1.09, 0.731, 0.806, 1.052, 1.378, 1.204, 1.103, 0.856, 1.27, 1.18, 1.222, 1.206, 1.204, 1.237, 1.111, 1.037, 1.271, 1.031, 0.907, 1.238, 0.968, 0.688, 1.158, 1.115, 1.299, 1.372, 0.663, 0.703, 1.158, 1.331, 1.247, 1.122, 1.318, 1.442, 0.828, 1.057, 1.158, 1.416, 0.966, 1.152, 1.008, 1.056, 1.495, 1.023, 1.087, 1.116, 1.169, 1.318, 1.287, 1.189, 1.068, 0.756, 1.136, 1.221, 1.203, 1.037, 1.107, 1.314, 0.933, 1.095, 0.609, 1.18, 0.911, 1.296, 1.121, 1.087, 1.259, 0.871, 1.144, 1.326, 0.644, 1.299, 0.739, 0.937, 0.719, 1.28, 1.073, 0.931, 0.644, 1.127, 1.172, 1.099, 1.225, 1.105, 0.986, 1.27, 1.496, 0.855, 0.8, 1.091, 1.233, 1.216, 1.103, 1.067, 1.073, 0.776, 1.03, 0.947, 1.354, 0.899, 0.989, 0.951, 0.973, 0.955, 0.96, 0.934, 0.709, 1.235, 1.477, 1.246, 0.885, 0.776, 1.12, 1.094, 1.186, 0.696, 0.687, 1.099, 1.179, 1.212, 0.898, 0.925, 1.242, 0.924, 0.99, 1.399, 0.742, 1.048, 1.424, 0.902, 0.466, 0.908, 1.02, 0.914, 1.021, 1.009, 1.544, 1.343, 1.144, 1.225, 0.949, 1.013, 1.137, 1.05, 0.531, 0.696, 0.953, 1.116, 1.033, 0.797, 0.982, 0.799, 0.889, 0.62, 1.302, 0.76, 0.702, 0.583, 0.671)

# Modellfunktion
model_function <- function(t_s, A, TauA, B, TauB, C, t_delay) {
  A * exp(-(t_s - t_delay) / TauA) + B * exp(-(t_s - t_delay) / TauB) + C
}

# UI
ui <- fluidPage(
  useShinyjs(),
  titlePanel("EPOC-Modellfunktion"),
  fluidRow(
    column(3,
      style = "height: 90vh; overflow-y: auto;",
      tags$h4(tags$strong("Modellparameter:")),
      sliderInput("A", "A", min = 0.0, max = 6, value = 2.2, step = 0.01),
      sliderInput("TauA", "TauA", min = 5, max = 90, value = 35, step = 0.10),
      sliderInput("B", "B", min = 0.0, max = 5, value = 0.8, step = 0.01),
      sliderInput("TauB", "TauB", min = 0.0, max = 1800, value = 180, step = 0.10),
      sliderInput("C", "C", min = 0.0, max = 3.0, value = 0.90, step = 0.01),
      sliderInput("O2_Store", "O2-Speicher [l]", min = 0, max = 1, value = 0.4, step = 0.01),
      sliderInput("t_delay", "Zeitverzögerung [s]", min = 0, max = 300, value = 0, step = 1),
      sliderInput("VO2_Ruhe", "VO2 Ruhe [l · min^-1]", min = 0.0, max = 1.0, value = 0.3, step = 0.01),
      br(), br(),
      actionButton("toggle_data", "Beispieldaten anzeigen"),
      br(), br(),
      fileInput("file_upload", "CSV-Datei hochladen", accept = ".csv"),
      tags$h4(tags$strong("Modelanpassung:")),
      actionButton("fit_all", "nlsLM - Fit"),
      h4("Schrittweise:"),
      actionButton("fit_tau", "1. Fit: Tau"), br(),
      actionButton("fit_slow", "2. Fit: EPOC Slow"), br(),
      actionButton("fit_full", "3. Fit: EPOC Fast"),
      br(), br(),
      actionButton("toggle_view", "Ruhe_sim anzeigen"),
      br(), br(),
      tags$h4(tags$strong("Berechnung - Ruhesauerstoffaufnahme:")),
      radioButtons("geschlecht", "Geschlecht:", choices = c("Männlich", "Weiblich")),
      sliderInput("koerpermasse", "Körpermasse [kg]:", min = 40, max = 150, value = 75),
      sliderInput("koerperlaenge", "Körperlänge [cm]:", min = 140, max = 220, value = 180),
      sliderInput("alter", "Alter [Jahre]:", min = 18, max = 100, value = 25),
      sliderInput("rq", "RQ:", min = 0.7, max = 1.0, value = 0.77, step = 0.01),
      actionButton("berechne_vo2_ruhe", "VO2 Ruhe berechnen")
    ),
    column(8,
           fluidRow(
             column(12, plotlyOutput("plot"))
           ),
           fluidRow(
             column(12, htmlOutput("instructions"))
           )
    )
  )
)


server <- function(input, output, session) {
  show_data <- reactiveVal(FALSE)
  uploaded_data <- reactiveVal(NULL)
  tau_estimate <- reactiveVal(NULL)
  slow_estimates <- reactiveVal(NULL)
  show_full_view <- reactiveVal(FALSE)
  max_ruhe_t_s <- reactiveVal(NULL)
  ruhe_sim <- reactiveVal(NULL)
  
  observeEvent(input$toggle_data, {
    show_data(!show_data())
  })
  
  observeEvent(input$file_upload, {
    req(input$file_upload)
    df <- read.csv(input$file_upload$datapath)
    
    if ("t_s" %in% names(df) && "VO2_t" %in% names(df)) {
      if (df$t_s[1] != 0.0) {
        df$t_s <- df$t_s - df$t_s[1]
      }
      
      uploaded_data(df)
      show_data(TRUE)
    } else {
      showModal(modalDialog(
        title = "Invalid CSV",
        "Die CSV-Datei muss 't_s' und 'VO2_t' als Spaltennamen beinhalten.",
        easyClose = TRUE,
        footer = NULL
      ))
    }
  })
  
  # Funktion zur Berechnung des Grundumsatzes
  berechne_grundumsatz <- function(geschlecht, masse, laenge, alter) {
    if (geschlecht == "Männlich") {
      return(66.5 + (13.75 * masse) + (5.003 * laenge) - (6.775 * alter))
    } else {
      return(655.1 + (9.563 * masse) + (1.850 * laenge) - (4.676 * alter))
    }
  }
  
  # Funktion zur Berechnung des RMR
  berechne_rmr <- function(grundumsatz, rq, geschlecht) {
    ka <- 19.946  # Annahme für RQ = 0.77, passen Sie dies an, wenn nötig
    faktor <- if(geschlecht == "Männlich") 1.287 else 1.278
    return((grundumsatz / (24 * 60 * ka)) * 4.1868 * faktor)
  }
  
  # Reaktion auf den Klick des "VO2,Ruhe berechnen" Buttons
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
  
  observeEvent(input$fit_tau, {
    if (show_data()) {
      Beispieldaten <- if (is.null(uploaded_data())) {
        data.frame(t_s = t_data, VO2_t = VO2_data)
      } else {
        uploaded_data()
      }
      
      t_delay <- isolate(input$t_delay)
      C <- isolate(input$VO2_Ruhe)
      
      # Datenfilterung und Zeitverschiebung
      filtered_data <- Beispieldaten %>%
        filter(t_s >= t_delay) %>%
        mutate(t_s = t_s - t_delay)
      
      model_tau <- nlsLM(VO2_t ~ x * exp(-t_s/Tau) + C, 
                         data = filtered_data,
                         start = list(x = max(filtered_data$VO2_t), Tau = 45, C = C),
                         lower = c(x = 0, Tau = 10, C = 0),
                         upper = c(x = Inf, Tau = 600, C = Inf),
                         control = nls.lm.control(maxiter = 1024, ftol = 1e-10, ptol = 1e-10))
      
      tau_estimate(round(as.list(coef(model_tau))$Tau, 1))
      
      updateSliderInput(session, "TauA", value = tau_estimate())
      updateSliderInput(session, "A", value = as.list(coef(model_tau))$x)
      updateSliderInput(session, "C", value = as.list(coef(model_tau))$C)
      
      # Set B and TauB sliders to 0
      updateSliderInput(session, "B", value = 0)
      updateSliderInput(session, "TauB", value = 0)
    }
  })

  
  observeEvent(input$fit_slow, {
    req(tau_estimate())
    if (show_data()) {
      Beispieldaten <- if (is.null(uploaded_data())) {
        data.frame(t_s = t_data, VO2_t = VO2_data)
      } else {
        uploaded_data()
      }
      
      VO2_Ruhe <- isolate(input$VO2_Ruhe)
      t_delay <- isolate(input$t_delay)
      max_VO2 <- max(VO2_data) * 1.1
      
      tau2 <- 2 * tau_estimate()
      tau8 <- 8 * tau_estimate()
      
      simulierte_ruhewerte <- seq(1800, 2400, by = 1)
      ruhe_sim_data <- data.frame(
        t_s = simulierte_ruhewerte,
        VO2_t = rep(VO2_Ruhe, length(simulierte_ruhewerte))
      )
      
      ruhe_sim(ruhe_sim_data)
      max_ruhe_t_s(max(ruhe_sim_data$t_s))
      
      Beispieldaten_extended <- rbind(Beispieldaten, ruhe_sim_data)
      
      # Datenfilterung und Zeitverschiebung
      filtered_data <- Beispieldaten_extended %>% 
        filter(t_s >= t_delay) %>%
        mutate(t_s = t_s - t_delay) %>%
        filter((t_s >= tau2 & t_s <= tau8) | t_s >= 1800)
      
      model_slow <- nlsLM(VO2_t ~ B * exp(-t_s/TauB) + VO2_Ruhe,
                          data = filtered_data,
                          start = list(B = 0.5, TauB = 540),
                          lower = c(B = 0.2, TauB = 180),
                          upper = c(B = (max_VO2 * 0.5), TauB = 900),
                          control = nls.lm.control(maxiter = 1024, ftol = 1e-10, ptol = 1e-10))
      
      slow_estimates_list <- list(
        B = as.numeric(coef(model_slow)["B"]),
        TauB = round(as.numeric(coef(model_slow)["TauB"]), 1),
        C = VO2_Ruhe
      )
      
      slow_estimates(slow_estimates_list)
      
      updateSliderInput(session, "B", value = slow_estimates_list$B)
      updateSliderInput(session, "TauB", value = slow_estimates_list$TauB)
      updateSliderInput(session, "C", value = VO2_Ruhe)
    }
  })
  
  observeEvent(input$fit_full, {
    req(tau_estimate(), slow_estimates())
    if (show_data()) {
      Beispieldaten <- if (is.null(uploaded_data())) {
        data.frame(t_s = t_data, VO2_t = VO2_data)
      } else {
        uploaded_data()
      }
      
      slow_est <- slow_estimates()
      t_delay <- isolate(input$t_delay)
      
      if (is.null(tau_estimate()) || is.null(slow_est)) {
        showNotification("Einige Schätzwerte fehlen. Bitte führen Sie Schritt 1 und 2 erneut aus.", type = "error")
        return()
      }
      
      start_params <- list(
        A = isolate(input$A),
        TauA = tau_estimate()
      )
      
      # Definiere eine Funktion mit festen Werten für B, TauB und C
      model_func <- function(t_s, A, TauA) {
        A * exp(-t_s/TauA) + slow_est$B * exp(-t_s/slow_est$TauB) + slow_est$C
      }
      
      # Berechne 2tau
      tau2 <- 2 * tau_estimate()
      
      # Datenfilterung und Zeitverschiebung
      Beispieldaten_gefiltert <- Beispieldaten %>%
        filter(t_s >= t_delay) %>%
        mutate(t_s = t_s - t_delay) %>%
        filter(t_s < 1800)  # Entferne simulierte Ruhewerte
      
      # Erstelle Gewichte basierend auf t_s
      weights <- ifelse(Beispieldaten_gefiltert$t_s <= tau2, 1, 1)
      
      tryCatch({
        model_full <- nlsLM(VO2_t ~ model_func(t_s, A, TauA),
                            data = Beispieldaten_gefiltert,
                            start = list(A = 2.5, TauA = 42),
                            lower = c(A = 1.0, TauA = 15),
                            upper = c(A = 4.0, TauA = 90),
                            weights = weights,
                            control = nls.lm.control(maxiter = 1024, ftol = 1e-10, ptol = 1e-10))
        
        full_estimates <- list(
          A = as.numeric(coef(model_full)["A"]),
          TauA = round(as.numeric(coef(model_full)["TauA"]), 1)
        )
        
        updateSliderInput(session, "A", value = full_estimates$A)
        updateSliderInput(session, "TauA", value = full_estimates$TauA)
        
        showNotification("Fitting erfolgreich abgeschlossen", type = "message")
      }, error = function(e) {
        showNotification(paste("Fehler beim Fitting:", e$message), type = "error")
      })
    }
  })
  
  observeEvent(input$toggle_view, {
    show_full_view(!show_full_view())
  })
  
  observeEvent(input$fit_all, {
    if (show_data()) {
      # Trigger fit_tau
      shinyjs::click("fit_tau")
      
      # Warte kurz, bevor der nächste Fit ausgeführt wird
      Sys.sleep(0.5)
      
      # Trigger fit_slow
      shinyjs::click("fit_slow")
      
      # Warte kurz, bevor der nächste Fit ausgeführt wird
      Sys.sleep(0.5)
      
      # Trigger fit_full
      shinyjs::click("fit_full")
      
      showNotification("Alle Fits wurden nacheinander durchgeführt.", type = "message")
    } else {
      showNotification("Bitte fügen Sie zuerst Daten ein.", type = "warning")
    }
  })
  
  output$plot <- renderPlotly({
    A <- input$A
    TauA <- input$TauA
    B <- input$B
    TauB <- input$TauB
    C <- input$C
    O2_Store <- input$O2_Store
    t_delay <- input$t_delay
    VO2_Ruhe <- input$VO2_Ruhe
    
    # Bestimmen des x-Achsen-Bereichs
    x_range <- if (show_full_view()) {
      c(0, max_ruhe_t_s())
    } else {
      c(0, 600)
    }
    
    max_x <- max(max_ruhe_t_s(), 600 + 0.5 * TauB, max(t_data))
    
    VO2_data <- if (show_data()) {
      if (is.null(uploaded_data())) {
        VO2_data
      } else {
        uploaded_data()$VO2_t
      }
    } else {
      VO2_data
    }
    
    max_y <- max((A + B + C) * 1.1, max(VO2_data) * 1.1)
    
    t_s <- seq(0, max_x, by = 1)
    
    model_values <- A * exp(-t_s / TauA) + B * exp(-t_s / TauB) + C
    model_fast <- A * exp(-t_s / TauA)
    model_slow <- B * exp(-t_s / TauB)
    model_ruhe <- rep(C, length(t_s))
    
    model_fast_func <- function(t_s) A * exp(-t_s / TauA)
    integrated_model_fast <- integrate(model_fast_func, lower = 0, upper = max(t_s))
    
    VO2_fast <- integrated_model_fast$value / 60
    CE_max <- 21.1307796
    WPCR <- VO2_fast * CE_max
    WPCR_corrected <- ifelse(VO2_fast < O2_Store, 0, (VO2_fast * CE_max) - (O2_Store * CE_max))
    
    cumulative_area <- cumsum(A * exp(-t_s / TauA)) * diff(t_s)[1]
    O2_Store_index <- which.min(abs(cumulative_area - O2_Store * 60))
    O2_Store_x <- t_s[O2_Store_index]
    
    eq_text <- sprintf("V̇O₂ = %.2f * e<sup>-t / %.1f</sup> + %.2f * e<sup>-t / %.1f</sup> + %.2f", 
                       A, TauA, B, TauB, C)
    
    p <- plot_ly() %>%
      add_trace(x = ~t_s, y = ~model_values, type = 'scatter', mode = 'lines', 
                name = 'Modellfunktion', line = list(color = '#EF6F6A')) %>%
      add_trace(x = ~t_s, y = ~model_fast, type = 'scatter', mode = 'lines', 
                name = 'EPOC<sub>fast</sub>', line = list(color = '#42BA97')) %>%
      add_trace(x = ~t_s, y = ~model_slow, type = 'scatter', mode = 'lines', 
                name = 'EPOC<sub>slow</sub>', line = list(color = '#BB7693')) %>%
      add_trace(x = ~t_s, y = ~model_ruhe, type = 'scatter', mode = 'lines', 
                name = 'C', line = list(color = '#1CADE4')) %>%
      add_ribbons(x = ~t_s, ymin = 0, ymax = ~model_fast, 
                  name = 'EPOC<sub>fast,Integriert</sub>', fillcolor = 'rgba(66,186,151,0.5)', 
                  line = list(color = "rgba(0,0,0,0)")) %>%
      add_ribbons(x = ~t_s, ymin = 0, ymax = ~model_fast, 
                  data = data.frame(t_s = t_s[1:O2_Store_index], model_fast = model_fast[1:O2_Store_index]),
                  fillcolor = 'rgba(0,131,143,0.3)', line = list(color = "rgba(0,0,0,0)", dash = "dash"), 
                  name = 'O<sub>2</sub>-Speicher') %>%
      add_trace(x = c(O2_Store_x, O2_Store_x), y = c(0, max(model_fast[O2_Store_index])),
                type = 'scatter', mode = 'lines', 
                line = list(color = "#00838F", width = 1, dash = "dash"),
                name = 'O2 Store Linie', showlegend = FALSE) %>%
      layout(title = "EPOC-Modellfunktion",
             margin = list(t = 40),
             xaxis = list(title = "Zeit [s]", range = x_range, autorange = FALSE),
             yaxis = list(title = "V̇O<sub>2</sub> [l·min<sup>-1</sup>]", range = c(0, max_y)),
             shapes = list(
               list(
                 type = "line",
                 x0 = 2 * TauA,
                 x1 = 2 * TauA,
                 y0 = 0,
                 y1 = max_y,
                 line = list(color = "gray", width = 0.5, dash = "dash")
               ),
               list(
                 type = "line",
                 x0 = 8 * TauA,
                 x1 = 8 * TauA,
                 y0 = 0,
                 y1 = max_y,
                 line = list(color = "gray", width = 0.5, dash = "dash")
               )
             ),
             annotations = list(
               list(
                 x = 2 * TauA, y = max_y * 0.80, text = sprintf("2tau: %.1f", 2 * TauA), showarrow = FALSE, xanchor = "left", yanchor = "bottom",
                 textangle = -90, font = list(size = 11, color = "darkgrey")
               ),
               list(
                 x = 8 * TauA, y = max_y * 0.80, text = sprintf("8tau: %.1f", 8 * TauA), showarrow = FALSE, xanchor = "left", yanchor = "bottom",
                 textangle = -90, font = list(size = 11, color = "darkgrey")
               ),
               list(
                 x = max_x * 0.10, 
                 y = max_y * 0.85,
                 text = paste("W<sub>PCR</sub>:", round(WPCR, 2), "kJ"),
                 showarrow = FALSE,
                 xanchor = 'left',
                 yanchor = 'bottom',
                 font = list(
                   size = 12,
                   color = "black"
                 )
               ),
               list(
                 x = max_x * 0.10, 
                 y = max_y * 0.75,
                 text = paste("W<sub>PCR, korrigiert</sub>:", round(WPCR_corrected, 2), "kJ"),
                 showarrow = FALSE,
                 xanchor = 'left',
                 yanchor = 'bottom',
                 font = list(
                   size = 12,
                   color = "black"
                 )
               ),
               list(
                 x = max_x * 0.10, 
                 y = max_y * 0.95,
                 text = eq_text,
                 showarrow = FALSE,
                 xanchor = 'left',
                 yanchor = 'bottom',
                 font = list(
                   size = 12,
                   color = "black"
                 )
               )
             ))
    
    if (show_data()) {
      Beispieldaten <- if (is.null(uploaded_data())) {
        data.frame(t_s = t_data, VO2_t = VO2_data)
      } else {
        uploaded_data()
      }
      
      # Datenfilterung und Zeitverschiebung
      Beispieldaten <- Beispieldaten %>%
        filter(t_s >= t_delay) %>%
        mutate(t_s = t_s - t_delay) %>%
        filter(t_s < 1800)  # Entferne simulierte Ruhewerte
      
      model_values_at_data <- A * exp(-Beispieldaten$t_s / TauA) + B * exp(-Beispieldaten$t_s / TauB) + C
      
      data_subset <- Beispieldaten
      
      ss_res <- sum((data_subset$VO2_t - model_values_at_data)^2, na.rm = TRUE)
      ss_tot <- sum((data_subset$VO2_t - mean(data_subset$VO2_t, na.rm = TRUE))^2, na.rm = TRUE)
      r_squared <- 1 - (ss_res / ss_tot)
      
      p <- p %>%
        add_trace(data = Beispieldaten, x = ~t_s, y = ~VO2_t, type = 'scatter', mode = 'markers+lines',
                  name = 'V̇O<sub>2</sub>', 
                  marker = list(color = 'rgba(38, 131, 198, 0.9)', size = 5.0),
                  line = list(color = 'rgba(38, 131, 198, 1.0)', width = 0.65, dash = '4 4'))
      
      if (show_full_view()) {
        ruhe_sim <- data.frame(
          t_s = seq(1800, max_ruhe_t_s(), by = 1),
          VO2_t = rep(VO2_Ruhe, max_ruhe_t_s() - 1799)
        )
        
        p <- p %>%
          add_trace(data = ruhe_sim, x = ~t_s, y = ~VO2_t, type = 'scatter', mode = 'markers+lines',
                    name = 'Sim. Ruhewerte', 
                    marker = list(color = '#1CADE4', size = 3.0),
                    line = list(color = '#1CADE4', width = 0.5, dash = '4 4'))
      }
      
      p <- p %>%
        layout(annotations = list(
          list(
            x = max_x * 0.10,
            y = max_y * 0.65,
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
      "<div style='margin-top: 20px; padding: 10px; background-color: #f0f0f0; border: 1px solid #ddd; border-radius: 5px;'>
    <h4 style='color: #333;'><strong>Anleitung - Modellanpassung:</strong></h4>
    <ol style='color: #555;'>
      <li>Beispiel VO2-Daten oder eigene VO2-Daten als CSV-Datei einfügen.</li>
      <li>Ruhesauerstoffaufnahme (VO2 Ruhe) manuell eingeben oder nach Angabe der nötigen Parameter (Geschlecht, Körpermasse, Alter, RQ) berechnen lassen.</li>
      <li>O2-Speicher manuell festlegen oder auf 0 setzen, falls dieser in der Berechnung nicht beachtet werden soll.</li>
      <li>Zeitverzögerung festlegen, um Startpunkt der Modellanpassung festzulegen.</li>
      <li>Komplette Modellanpassung in 3 Schritten über 'nlsLM - Fit' oder schrittweise durchführen:</li>
      <ul>
        <li>'1. Fit: Tau' Bestimmung der Zeitkonstante tau mit einfacher Exponentialfunktion.</li>
        <li>'2. Fit: EPOC Slow' Modellanpassung der langsamen EPOC-Komponente mit simulierten Ruhewerten.
          <ul>
            <li>'Ruhe_sim anzeigen' wechselt zwischen der normalen und der erweiterten Ansicht mit der Ruhesauerstoffaufnahme.</li>
          </ul>
        </li>
        <li>'3. Fit: EPOC Fast' Modellanpassung der schnellen EPOC-Komponente.</li>
      </ul>
      <li>Alternativ: Manuelle Modellanpassung der Modellparameter mit den Schiebereglern.</li>
    </ol>
    </div>"
    )
  })
}

shinyApp(ui = ui, server = server)
