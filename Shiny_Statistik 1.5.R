library(shiny)
library(dplyr)
library(tidyr)
library(plotly)
library(htmltools)
library(afex)
library(emmeans)
library(reactable)
library(stats)

# Beispieldaten (Sie sollten diese durch Ihre tatsächlichen Daten ersetzen)
Bedingungen_data <- data.frame(
  Proband = factor(rep(c(1, 6, 10, 13, 14, 15, 17, 18, 19), each = 6)),
  Bedingung = rep(c("stehen", "sitzen", "sitzen", "stehen", "sitzen", "stehen"), 9),
  Intensität = rep(c("leicht", "leicht", "moderat", "moderat", "schwer", "schwer"), 9),
  WirkPhysio = c(0.236, 0.246, 0.242, 0.229, 0.232, 0.239, 0.235, 0.241, 0.248, 0.239, 0.240, 0.243, 0.253, 0.268, 0.246, 0.243, 0.237, 0.239, 0.246, 0.260, 0.237, 0.256, 0.245, 0.236, 0.256, 0.230, 0.251, 0.238, 0.236, 0.257, 0.229, 0.250, 0.239, 0.226, 0.230, 0.224, 0.267, 0.254, 0.247, 0.250, 0.239, 0.241, 0.253, 0.256, 0.262, 0.257, 0.240, 0.255, 0.236, 0.269, 0.260, 0.237, 0.243, 0.259),
  WAerob_kJ = c(364.789, 341.630, 380.176, 397.986, 434.292, 433.605, 324.974, 328.244, 338.783, 345.676, 364.681, 257.548, 373.643, 359.054, 449.591, 437.034, 472.355, 502.399, 358.838, 352.868, 394.753, 386.920, 414.584, 422.975, 336.737, 327.041, 374.941, 376.055, 407.569, 413.170, 301.002, 268.617, 293.783, 323.273, 338.096, 356.148, 301.109, 280.951, 322.983, 336.779, 374.988, 386.658, 266.533, 251.790, 278.169, 296.280, 322.403, 294.594, 228.956, 211.733, 242.183, 251.874, 269.031, 263.981),
  WPCR_kJ = c(24.990, 32.078, 29.980, 41.372, 32.638, 31.531, 36.043, 30.172, 32.921, 37.818, 39.501, 28.280, 26.361, 43.004, 44.844, 47.735, 57.011, 41.969, 33.885, 39.927, 47.648, 43.936, 53.680, 50.565, 29.216, 27.530, 35.164, 28.986, 47.232, 36.751, 26.606, 20.363, 30.392, 26.097, 32.816, 35.505, 26.571, 18.819, 26.123, 31.549, 24.554, 32.396, 18.144, 19.103, 15.287, 14.856, 29.950, 33.898, 19.655, 11.905, 16.782, 20.699, 25.292, 21.067),
  WBLC_kJ = c(10.331, 11.611, 15.658, 12.815, 20.331, 7.660, 10.576, 10.736, 15.179, 19.020, 24.804, 17.400, 6.874, 5.614, 18.402, 10.604, 15.802, 23.444, 7.487, 10.041, 9.859, 9.539, 15.336, 13.647, 6.432, 8.576, 8.576, 11.804, 7.468, 17.874, 6.891, 7.499, 11.084, 12.980, 17.018, 18.007, 6.655, 10.143, 11.106, 8.317, 17.902, 13.236, 6.610, 6.086, 10.863, 13.388, 20.296, 15.275, 4.766, 4.184, 7.303, 10.460, 13.388, 11.525),
  WTOT_kJ = c(400.111, 385.318, 425.813, 452.173, 487.261, 472.796, 371.593, 369.153, 386.883, 402.514, 428.986, 303.228, 406.879, 407.672, 512.836, 495.373, 545.169, 567.812, 400.210, 402.837, 452.260, 440.395, 483.600, 487.187, 372.385, 363.147, 418.681, 416.845, 462.268, 467.795, 334.499, 296.479, 335.259, 362.350, 387.929, 409.659, 334.335, 309.913, 360.213, 376.645, 417.444, 432.290, 291.287, 276.979, 304.319, 324.524, 372.649, 343.767, 253.376, 227.822, 266.267, 283.032, 307.712, 296.572),
  P_Tot = c(315.088, 315.356, 343.250, 345.622, 376.014, 377.269, 291.341, 296.009, 319.564, 320.346, 342.797, 334.697, 342.780, 364.604, 419.800, 400.468, 431.054, 452.464, 328.689, 348.476, 357.036, 375.551, 395.659, 383.032, 317.382, 278.361, 350.890, 330.104, 363.619, 400.224, 255.678, 247.500, 267.481, 272.461, 297.603, 306.493, 297.678, 262.465, 297.034, 313.722, 332.282, 347.560, 245.195, 236.218, 265.627, 278.280, 297.955, 291.810, 199.167, 204.304, 230.540, 223.281, 248.864, 255.967),
  P_mean = structure(c(286.197, 279.856, 308.058, 316.423, 340.826, 347.288, 277.240, 271.455, 304.467, 298.231, 325.413, 309.695, 323.151, 316.844, 371.964, 380.656, 411.775, 405.479, 300.612, 294.276, 330.975, 323.263, 347.878, 356.524, 264.043, 264.046, 296.596, 312.821, 338.375, 340.656, 222.299, 217.725, 237.150, 238.511, 268.300, 270.268, 259.610, 246.985, 281.341, 276.153, 316.925, 309.070, 220.908, 221.312, 250.936, 254.072, 274.367, 276.738, 175.467, 173.182, 199.048, 199.115, 224.703, 224.227),
                     names = paste0(rep(c("01", "06", "10", "13", "14", "15", "17", "18", "19"), each = 6), "_", rep(1:6, 9), "_", rep(c("leicht", "leicht", "moderat", "moderat", "schwer", "schwer"), 9), "_", rep(c("stehen", "sitzen", "sitzen", "stehen", "sitzen", "stehen"), 9), "_final.xlsm")),
  P_Int = c(28.891, 35.500, 35.191, 29.199, 35.188, 29.981, 14.101, 24.553, 15.096, 22.115, 17.384, 25.002, 19.629, 47.760, 47.836, 19.812, 19.280, 46.985, 28.078, 54.200, 26.061, 52.288, 47.782, 26.508, 53.339, 14.315, 54.294, 17.283, 25.244, 59.568, 33.379, 29.774, 30.332, 33.951, 29.303, 36.225, 38.068, 15.480, 15.693, 37.570, 15.358, 38.490, 24.288, 14.906, 14.691, 24.208, 23.588, 15.072, 23.700, 31.122, 31.492, 24.167, 24.161, 31.740),
  nD = c(76.656, 86.401, 86.152, 76.970, 86.149, 77.755, 59.670, 79.341, 59.012, 78.632, 58.578, 74.838, 64.403, 93.431, 93.480, 64.651, 63.923, 92.926, 76.179, 100.726, 76.497, 100.707, 101.494, 76.290, 94.922, 67.303, 95.691, 67.756, 68.909, 97.233, 82.316, 85.115, 84.987, 82.412, 85.664, 82.543, 88.353, 59.284, 59.646, 87.968, 59.075, 88.675, 86.174, 65.943, 65.511, 86.081, 85.349, 66.273, 75.210, 87.711, 88.053, 75.796, 75.789, 88.282),
  Efficiency = structure(c(66.420, 80.670, 85.600, 72.950, 87.650, 78.670, 80.040, 86.410, 86.780, 88.630, 88.240, 92.410, 73.990, 73.770, 81.110, 85.390, 91.790, 84.470, 72.050, 72.340, 79.170, 76.950, 79.780, 85.170, 66.520, 69.930, 72.410, 82.580, 87.010, 79.530, 65.430, 72.540, 77.020, 70.470, 80.770, 78.760, 64.470, 62.050, 68.400, 71.350, 77.440, 75.100, 81.990, 82.220, 91.470, 89.760, 92.560, 97.110, 50.580, 59.590, 65.120, 56.310, 63.190, 72.060),
                         names = paste0(rep(c("01", "06", "10", "13", "14", "15", "17", "18", "19"), each = 6), "_", rep(1:6, 9), "_", rep(c("leicht", "leicht", "moderat", "moderat", "schwer", "schwer"), 9), "_", rep(c("stehen", "sitzen", "sitzen", "stehen", "sitzen", "stehen"), 9), "_final.xlsm")),
  Pedal_Smoothness = structure(c(39.710, 50.818, 52.443, 40.206, 53.679, 41.045, 47.915, 51.534, 49.217, 51.139, 48.034, 52.822, 47.930, 51.948, 54.061, 50.656, 52.634, 54.273, 43.084, 49.064, 44.663, 50.294, 51.290, 46.464, 48.329, 44.259, 52.672, 47.444, 48.247, 57.974, 42.430, 50.291, 52.357, 42.713, 52.269, 44.794, 46.357, 38.358, 39.662, 49.311, 42.188, 51.480, 54.435, 49.538, 51.844, 57.505, 57.678, 52.618, 32.376, 42.067, 44.898, 34.164, 36.544, 47.313),
                               names = paste0(rep(c("01", "06", "10", "13", "14", "15", "17", "18", "19"), each = 6), "_", rep(1:6, 9), "_", rep(c("leicht", "leicht", "moderat", "moderat", "schwer", "schwer"), 9), "_", rep(c("stehen", "sitzen", "sitzen", "stehen", "sitzen", "stehen"), 9), "_final.xlsm")),
  P_max = structure(c(720.714, 550.698, 587.417, 787.012, 634.932, 846.119, 578.610, 526.754, 618.616, 583.178, 677.468, 586.295, 674.211, 609.931, 688.050, 751.453, 782.343, 747.115, 697.739, 599.784, 741.044, 642.751, 678.261, 767.314, 546.343, 596.592, 563.099, 659.349, 701.336, 587.603, 523.918, 432.933, 452.948, 558.406, 513.310, 603.362, 560.027, 643.901, 709.354, 560.028, 751.219, 600.372, 405.819, 446.750, 484.018, 441.824, 475.692, 525.937, 541.967, 411.679, 443.334, 582.826, 614.878, 473.924),
                    names = paste0(rep(c("01", "06", "10", "13", "14", "15", "17", "18", "19"), each = 6), "_", rep(1:6, 9), "_", rep(c("leicht", "leicht", "moderat", "moderat", "schwer", "schwer"), 9), "_", rep(c("stehen", "sitzen", "sitzen", "stehen", "sitzen", "stehen"), 9), "_final.xlsm")),
  HR_percent = c(87.370, 85.070, 88.400, 90.930, 93.120, 93.780, 88.050, 88.160, 92.210, 91.970, 95.390, 93.580, 84.410, 84.650, 88.640, 89.980, 91.120, 94.040, 89.190, 89.260, 92.250, 92.320, 93.980, 94.900, 86.650, 87.880, 89.950, 91.140, 93.960, 94.270, 90.900, 86.550, 87.590, 91.620, 91.130, 95.120, 86.770, 87.490, 90.800, 89.740, 93.350, 94.180, 80.640, 81.030, 84.870, 88.210, 91.540, 92.470, 91.920, 88.360, 90.080, 92.490, 93.580, 93.310),
  VO2_percent = c(71.270, 66.600, 72.840, 76.050, 83.970, 83.100, 73.170, 73.360, 75.130, 76.760, 82.170, 80.020, 65.960, 64.110, 76.980, 75.530, 81.190, 87.700, 77.060, 75.210, 82.910, 81.140, 86.200, 88.050, 69.580, 67.180, 76.090, 77.010, 83.240, 82.990, 68.970, 61.940, 67.210, 72.610, 75.890, 79.960, 59.980, 55.950, 63.850, 65.630, 72.700, 75.690, 74.640, 71.070, 77.240, 82.440, 89.640, 82.880, 64.160, 59.830, 66.380, 68.750, 72.980, 72.620),
  ΔBLC = c(2.144, 2.410, 3.250, 2.660, 4.220, 1.590, 2.285, 2.320, 3.280, 4.110, 5.360, 3.760, 1.322, 1.080, 3.540, 2.040, 3.040, 4.510, 1.640, 2.200, 2.160, 2.090, 3.360, 2.990, 1.335, 1.780, 1.780, 2.450, 1.550, 3.710, 1.672, 1.820, 2.690, 3.150, 4.130, 4.370, 1.312, 2.000, 2.190, 1.640, 3.530, 2.610, 2.172, 2.000, 3.570, 4.400, 6.670, 5.020, 1.253, 1.100, 1.920, 2.750, 3.520, 3.030)
)

# UI Definition
ui <- fluidPage(
  titlePanel("Wirkungsgrad Analyse"),
  
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("selectedBedingung", "Bedingungen:",
                         unique(Bedingungen_data$Bedingung),
                         selected = c("sitzen", "stehen")),
      checkboxGroupInput("selectedIntensität", "Intensitäten:",
                         unique(Bedingungen_data$Intensität),
                         selected = unique(Bedingungen_data$Intensität)),
      checkboxGroupInput("selectedProband", "Probanden:",
                         unique(Bedingungen_data$Proband),
                         selected = unique(Bedingungen_data$Proband))
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Boxplot", plotlyOutput("boxplot")),
        tabPanel("Histogramme", uiOutput("histogramOutput")),
        tabPanel("ANOVA", uiOutput("anovaOutput"))
      )
    )
  )
)

# Server Logik
server <- function(input, output, session) {
  
  filtered_data <- reactive({
    data <- Bedingungen_data %>%
      filter(Proband %in% input$selectedProband)
    
    if (length(input$selectedBedingung) > 0) {
      data <- data %>% filter(Bedingung %in% input$selectedBedingung)
    }
    
    if (length(input$selectedIntensität) > 0) {
      data <- data %>% filter(Intensität %in% input$selectedIntensität)
    }
    
    # Gruppennamen-Generierung
    if (length(input$selectedBedingung) > 0 || length(input$selectedIntensität) > 0) {
      data$Gruppe <- paste(data$Intensität, data$Bedingung, sep = "_")
    } else {
      data$Gruppe <- "Alle"
    }
    
    return(data)
  })
  
  # Definiere color_map im Server-Bereich
  color_map <- reactive({
    c(
      "leicht_sitzen" = "#42BA97", "leicht_stehen" = "#62A39F",
      "moderat_sitzen" = "#1CADE4", "moderat_stehen" = "#2683C6",
      "schwer_sitzen" = "#EF5350", "schwer_stehen" = "#C8133B",
      "leicht" = "#42BA97", "moderat" = "#1CADE4", "schwer" = "#EF5350",
      "sitzen" = "#62A39F", "stehen" = "#2683C6",
      "Alle" = "#808080"  # Grau für die "Alle" Option
    )
  })
  
  # Wirkungsgrad Boxplot
  output$boxplot <- renderPlotly({
    data <- filtered_data()
    
    if (nrow(data) == 0) {
      return(plotly_empty() %>% 
               layout(title = "Keine Daten für die ausgewählten Kriterien verfügbar"))
    }
    
    color_map_values <- color_map()
    
    p2BxI <- plot_ly(data = data, 
                     x = ~Gruppe, 
                     y = ~WirkPhysio,
                     type = "box",
                     color = ~Gruppe,
                     colors = color_map_values[unique(data$Gruppe)],
                     opacity = 0.8, 
                     line = list(color = "black", width = 0.9),
                     boxpoints = "outliers",  
                     pointpos = 0,
                     marker = list(color = "black", size = 4),
                     boxmean = TRUE,  
                     hoverlabel = list(bgcolor = "#F5F5F5")
    ) %>%
      layout(title = 'Wirkungsgrad der Probanden',
             margin = list(t = 40),
             xaxis = list(title = htmltools::HTML('<b>Gruppe</b>'), 
                          showgrid = TRUE,  
                          gridcolor = "lightgray",  
                          gridwidth = 0.05),  
             yaxis = list(title = htmltools::HTML('<b>Wirkungsgrad</b>'), tickformat = ".3f",
                          showgrid = TRUE,  
                          gridcolor = "lightgray",  
                          gridwidth = 0.05)  
      )
    return(p2BxI)
  })
  
  # Neue Histogramme
  output$histogramOutput <- renderUI({
    data <- filtered_data()
    
    if (nrow(data) == 0) {
      return(p("Keine Daten für die ausgewählten Kriterien verfügbar"))
    }
    
    groups <- unique(data$Gruppe)
    n_groups <- length(groups)
    
    # Erstellen Sie Reihen mit je zwei Plots
    rows <- lapply(seq(1, n_groups, by = 2), function(i) {
      fluidRow(
        column(6, plotlyOutput(outputId = paste0("hist_", gsub(" ", "_", groups[i])), height = "400px")),
        if (i + 1 <= n_groups) column(6, plotlyOutput(outputId = paste0("hist_", gsub(" ", "_", groups[i + 1])), height = "400px")) else NULL
      )
    })
    
    # Erstellen Sie die Ausgabe-UI
    do.call(tagList, rows)
  })
  
  # Rendere die einzelnen Histogramme
  observe({
    data <- filtered_data()
    color_map_values <- color_map()
    
    # Berechne globale min und max Werte für die x-Achse
    x_min <- min(data$WirkPhysio, na.rm = TRUE)
    x_max <- max(data$WirkPhysio, na.rm = TRUE)
    
    for (group in unique(data$Gruppe)) {
      local({
        local_group <- group
        output_id <- paste0("hist_", gsub(" ", "_", local_group))
        
        output[[output_id]] <- renderPlotly({
          group_data <- data[data$Gruppe == local_group, ]
          
          # Bestimme die Farbe basierend auf der Gruppe
          group_color <- if(local_group %in% names(color_map_values)) {
            color_map_values[local_group]
          } else {
            # Wenn keine spezifische Farbe definiert ist, versuchen wir, eine passende Farbe zu finden
            intensity <- strsplit(local_group, "_")[[1]][1]
            condition <- strsplit(local_group, "_")[[1]][2]
            if (paste(intensity, condition, sep = "_") %in% names(color_map_values)) {
              color_map_values[paste(intensity, condition, sep = "_")]
            } else if (intensity %in% names(color_map_values)) {
              color_map_values[intensity]
            } else if (condition %in% names(color_map_values)) {
              color_map_values[condition]
            } else {
              "#999999"  # Fallback-Farbe
            }
          }
          
          # Führe Shapiro-Wilk-Test durch
          sw_test <- shapiro.test(group_data$WirkPhysio)
          sw_result <- paste("Shapiro-Wilk Test: W =", round(sw_test$statistic, 3), 
                             ", p =", format.pval(sw_test$p.value, digits = 3))
          
          # Interpretiere das Ergebnis des Shapiro-Wilk-Tests
          sw_interpretation <- if(sw_test$p.value > 0.05) {
            "Die Daten sind normalverteilt (p > 0.05)."
          } else {
            "Die Daten sind nicht normalverteilt (p ≤ 0.05)."
          }
          
          plot_ly(data = group_data, x = ~WirkPhysio, type = "histogram", 
                  marker = list(color = group_color),
                  opacity = 0.7, nbinsx = 30) %>%
            layout(title = paste('Verteilung des Wirkungsgrades -', local_group),
                   xaxis = list(title = "Wirkungsgrad", range = c(x_min, x_max)),
                   yaxis = list(title = "Häufigkeit"),
                   margin = list(l = 30, r = 30, b = 30, t = 60, pad = 4),
                   annotations = list(
                     list(x = 0.5, y = 1.08, xref = "paper", yref = "paper",
                          text = sw_result, showarrow = FALSE,
                          font = list(size = 10)),
                     list(x = 0.5, y = 1.02, xref = "paper", yref = "paper",
                          text = sw_interpretation, showarrow = FALSE,
                          font = list(size = 10))
                   ))
        })
      })
    }
  })
  
  # ANOVA und Post-hoc Test
  output$anovaOutput <- renderUI({
    # Hole die gefilterten Daten
    data <- filtered_data()
    
    # Überprüfe, ob mehr als eine Bedingung oder Intensität ausgewählt wurde
    # Dies bestimmt, ob eine ein- oder zweifaktorielle ANOVA durchgeführt wird
    bedingung_selected <- length(input$selectedBedingung) > 1
    intensität_selected <- length(input$selectedIntensität) > 1
    
    # Führe die entsprechende ANOVA basierend auf den ausgewählten Faktoren durch
    if (bedingung_selected && intensität_selected) {
      # Zweifaktorielle ANOVA, wenn beide Faktoren ausgewählt sind
      anova_result <- afex::aov_ez(
        id = "Proband",  # Identifikator für die Versuchspersonen
        dv = "WirkPhysio",  # Abhängige Variable
        data = data,  # Datensatz
        within = c("Bedingung", "Intensität"),  # Inner-Subjekt-Faktoren
        type = 3  # Typ-III-Quadratsummen
      )
      
      anova_title <- "Zweifaktorielle ANOVA mit Messwiederholung Ergebnisse:"
    } else if (bedingung_selected) {
      # Einfaktorielle ANOVA für Bedingung
      anova_result <- afex::aov_ez(
        id = "Proband",
        dv = "WirkPhysio",
        data = data,
        within = "Bedingung",
        type = 3
      )
      
      anova_title <- "Einfaktorielle ANOVA mit Messwiederholung für Bedingung:"
    } else if (intensität_selected) {
      # Einfaktorielle ANOVA für Intensität
      anova_result <- afex::aov_ez(
        id = "Proband",
        dv = "WirkPhysio",
        data = data,
        within = "Intensität",
        type = 3
      )
      
      anova_title <- "Einfaktorielle ANOVA mit Messwiederholung für Intensität:"
    } else {
      # Fehlermeldung, wenn keine ausreichenden Faktoren ausgewählt wurden
      return(p("Bitte wählen Sie mindestens zwei Stufen eines Faktors (Bedingung oder Intensität) aus."))
    }
    
    # Wandle die ANOVA-Ergebnisse in ein Dataframe um und formatiere sie
    anova_df <- as.data.frame(anova_result$anova_table)
    anova_df$Effect <- rownames(anova_df)  # Füge eine Spalte für die Effekte hinzu
    # Wähle nur die relevanten Spalten aus und ordne sie
    anova_df <- anova_df[, c("Effect", "num Df", "den Df", "MSE", "F", "Pr(>F)", "ges")]
    
    # Benenne "Intercept" in "Konstante" um für bessere Verständlichkeit
    anova_df$Effect <- ifelse(anova_df$Effect == "(Intercept)", "Konstante", anova_df$Effect)
    
    # Funktion zur Bestimmung der Signifikanzcodes
    get_sig_code <- function(p_value, effect) {
      if (effect == "Konstante") return("")
      if (p_value < 0.001) return("***")
      else if (p_value < 0.01) return("**")
      else if (p_value < 0.05) return("*")
      else if (p_value < 0.1) return(".")
      else return(" ")
    }
    
    # Runde die Werte für bessere Lesbarkeit und füge Signifikanzcodes hinzu
    anova_df$MSE <- round(anova_df$MSE, 6)
    anova_df$F <- round(anova_df$F, 3)
    anova_df$`Pr(>F)` <- round(anova_df$`Pr(>F)`, 5)
    anova_df$ges <- round(anova_df$ges, 3)
    anova_df$Signifikanz <- mapply(get_sig_code, anova_df$`Pr(>F)`, anova_df$Effect)
    
    # Ändere die Spaltenüberschriften auf Deutsch für bessere Verständlichkeit
    colnames(anova_df) <- c("Effekt", "df", "Residuen", "MSE", "F-Wert", "p-Wert", "η²", "Signifikanz")
    
    # Erstelle eine interaktive ANOVA-Tabelle mit reactable
    anova_table <- reactable(anova_df)
    
    # Definiere eine Legende für die Signifikanzcodes
    sig_codes <- "Signifikanzcodes: 0 = '***' | 0.001 = '**' | 0.01 = '*' | 0.05 = '.' | 0.1 = ' '"
    
    # Funktion zur Formatierung der Post-hoc-Tests
    format_posthoc <- function(posthoc, factor_name) {
      posthoc_df <- as.data.frame(posthoc)
      posthoc_df <- posthoc_df[, c("contrast", "p.value")]
      colnames(posthoc_df) <- c("Kontrast", "p_Wert")
      
      # Definiere get_sig_code innerhalb der format_posthoc-Funktion
      get_sig_code <- function(p_value) {
        if (p_value < 0.001) return("***")
        else if (p_value < 0.01) return("**")
        else if (p_value < 0.05) return("*")
        else if (p_value < 0.1) return(".")
        else return(" ")
      }
      
      posthoc_df <- posthoc_df %>%
        dplyr::mutate(
          `p-Wert` = sprintf("%.4f", round(p_Wert, 4)),
          Signifikanz = sapply(p_Wert, get_sig_code)
        ) %>%
        dplyr::select(Kontrast, `p-Wert`, Signifikanz)
      
      posthoc_df <- posthoc_df[order(as.numeric(posthoc_df$`p-Wert`)), ]
      
      return(posthoc_df)
    }
    
    # Führe Post-hoc-Tests durch
    posthoc_tables <- list()
    
    if (bedingung_selected && intensität_selected) {
      # Post-hoc für Kombination von Bedingung und Intensität
      posthoc_kombination <- emmeans(anova_result, ~Bedingung:Intensität) %>%
        pairs(adjust = "bonferroni")  # Paarweise Vergleiche mit Bonferroni-Korrektur
      posthoc_kombination_df <- format_posthoc(posthoc_kombination, "Kombination")
      posthoc_tables$kombination <- reactable(posthoc_kombination_df, defaultPageSize = 20, rownames = FALSE)
    } else if (bedingung_selected) {
      # Post-hoc für Bedingung
      posthoc_bedingung <- emmeans(anova_result, ~Bedingung) %>%
        pairs(adjust = "bonferroni")
      posthoc_bedingung_df <- format_posthoc(posthoc_bedingung, "Bedingung")
      posthoc_tables$bedingung <- reactable(posthoc_bedingung_df, defaultPageSize = 20, rownames = FALSE)
    } else if (intensität_selected) {
      # Post-hoc für Intensität
      posthoc_intensität <- emmeans(anova_result, ~Intensität) %>%
        pairs(adjust = "bonferroni")
      posthoc_intensität_df <- format_posthoc(posthoc_intensität, "Intensität")
      posthoc_tables$intensität <- reactable(posthoc_intensität_df, defaultPageSize = 20, rownames = FALSE)
    }
    
    # Stelle die Ausgabe zusammen
    output <- tagList(
      h4(anova_title),  # Überschrift
      div(style = "margin-top: 20px;"),  # Abstand
      anova_table,  # ANOVA-Tabelle
      div(style = "margin-top: 20px;"),  # Abstand
      tags$i(p(sig_codes))  # Legende für Signifikanzcodes
    )
    
    # Füge Post-hoc-Tabellen hinzu, falls vorhanden
    if (length(posthoc_tables) > 0) {
      for (factor_name in names(posthoc_tables)) {
        output <- tagList(
          output,
          div(style = "margin-top: 20px;"),  # Abstand
          h4(paste("Paarweise Vergleiche (Post-hoc) für", factor_name, ":")),  # Überschrift
          posthoc_tables[[factor_name]]  # Post-hoc-Tabelle
        )
      }
    }
    
    return(output)  # Gebe die gesamte UI-Ausgabe zurück
  })
}

# ShinyApp starten
shinyApp(ui = ui, server = server)