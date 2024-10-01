library(dplyr)
library(tidyverse)
library(plotly)
library(htmltools)
library(DT) 
library(shiny)
library(ggstatsplot)

# Laden des DataFrames aus der RDS-Datei
Bedingungen_data <- readRDS("C:/Users/johan/OneDrive/Desktop/SpoWi/WS 22,23/Masterarbeit - Wirkungsgrad/Daten/Probanden_Energieberechnung/xlsm/Bedingungen_data.rds")
Bedingungen_data_app <- Bedingungen_data %>%
  select(Proband, Bedingung, Intensität, WirkPhysio, `W*Aerob [kJ]`, `WPCR [kJ]`, `WBLC [kJ]`, `WTOT [kJ]`, P_Tot, P_mean, P_Int, nD, Efficiency, Pedal_Smoothness, P_max, HR_percent, VO2_percent, `ΔBLC`)
print(Bedingungen_data_app)

# UI (User Interface) Definition:
ui <- fluidPage(
  titlePanel("Gestapeltes Balkendiagramm für Energieumsatz"),
  sidebarLayout(
    sidebarPanel(
      width = 2,  # Etwas breitere Sidebar
      checkboxGroupInput("selectedBedingung", "Bedingungen:",
                         unique(Bedingungen_data$Bedingung),
                         selected = c("sitzen", "stehen")),
      checkboxGroupInput("selectedIntensität", "Intensitäten:",
                         unique(Bedingungen_data$Intensität),
                         selected = NULL),
      checkboxGroupInput("selectedProband", "Probanden:",
                         unique(Bedingungen_data$Proband),
                         selected = unique(Bedingungen_data$Proband))
    ),
    mainPanel(
      width = 10,  # Entsprechend angepasster Hauptbereich
      plotlyOutput("barplot", height = "calc(65vh - 150px)"),  # Etwa 75% der vorherigen Höhe
      DTOutput("summaryTable"),
      br(), br(),  # Fügt zwei Zeilenumbrüche für mehr Abstand hinzu
      plotOutput("ggbetweenstatsPlot", height = "800px"),  # Erhöht die Höhe der ANOVA-Abbildung
      br(), br(),  # Fügt zwei Zeilenumbrüche für mehr Abstand hinzu
      h4("ANOVA-Ergebnisse für WirkPhysio"),
      verbatimTextOutput("anovaResults"),
      br(), br()  # Fügt zwei Zeilenumbrüche am Ende für mehr Abstand hinzu
    )
  )
)

sort_with_leicht_sitzen_first <- function(x) {
  leicht_sitzen <- "leicht_sitzen"
  if (leicht_sitzen %in% x) {
    return(c(leicht_sitzen, setdiff(x, leicht_sitzen)))
  }
  return(x)
}

server <- function(input, output) {
  
  generatePlot <- reactive({
    # Datenfilterung je nach Auswahl
    filtered_data <- filter(Bedingungen_data, 
                            Proband %in% input$selectedProband)
    
    # Prüfen, ob Bedingungen oder Intensitäten ausgewählt wurden
    bedingungen_ausgewaehlt <- length(input$selectedBedingung) > 0
    intensitaeten_ausgewaehlt <- length(input$selectedIntensität) > 0
    
    if (bedingungen_ausgewaehlt) {
      filtered_data <- filtered_data %>% 
        filter(Bedingung %in% input$selectedBedingung)
    }
    
    if (intensitaeten_ausgewaehlt) {
      filtered_data <- filtered_data %>% 
        filter(Intensität %in% input$selectedIntensität)
    }
    
    # Gruppierung und Berechnung basierend auf der Auswahl
    if (bedingungen_ausgewaehlt && intensitaeten_ausgewaehlt) {
      group_vars <- c("Bedingung", "Intensität")
    } else if (bedingungen_ausgewaehlt) {
      group_vars <- "Bedingung"
    } else if (intensitaeten_ausgewaehlt) {
      group_vars <- "Intensität"
    } else {
      # Wenn weder Bedingungen noch Intensitäten ausgewählt sind
      return(NULL)
    }
    
    # Berechnen des durchschnittlichen Wirkungsgrads
    avg_efficiency_data <- filtered_data %>%
      group_by(across(all_of(group_vars))) %>%
      summarize(AvgWirkungsgrad = mean(WirkPhysio, na.rm = TRUE))
    
    # Berechnen der durchschnittlichen Leistung
    avg_power_data <- filtered_data %>%
      group_by(across(all_of(group_vars))) %>%
      summarize(AvgPTOT = mean(`P_Tot`, na.rm = TRUE)) 
    
    # Daten umwandeln und Durchschnittswerte berechnen
    avg_energy_data <- filtered_data %>%
      gather(Energiequelle, Wert, `W*Aerob [kJ]`, `WPCR [kJ]`, `WBLC [kJ]`) %>%
      group_by(across(all_of(c(group_vars, "Energiequelle")))) %>%
      summarize(AvgWert = mean(Wert, na.rm = TRUE),
                AvgWTOT = mean(`WTOT [kJ]`, na.rm = TRUE),
                Prozentsatz = (AvgWert / AvgWTOT) * 100)
    
    # X-Achsen-Variable festlegen
    if (length(group_vars) > 1) {
      x_var <- ~interaction(Intensität, Bedingung, sep = "_")
      x_title <- "Intensität_Bedingung"
    } else {
      x_var <- as.formula(paste0("~", group_vars))
      x_title <- group_vars
    }
    
    p <- plot_ly(data = avg_energy_data, 
                 x = x_var, 
                 y = ~AvgWert, 
                 color = ~Energiequelle, 
                 type = 'bar',
                 marker = list(line = list(color = 'black', width = 0.75)),
                 colors = c("#42BA97","#F4737A", "#1CADE4"),
                 text = ~sprintf("%.1f%%", Prozentsatz),
                 textposition = "auto",
                 textfont = list(color = "black")
    ) %>%
      layout(
        title = 'Durchschnittlicher Energieverbrauch der ausgewählten Probanden',
        xaxis = list(title = htmltools::HTML(paste0('<b>', x_title, '</b>')),
                     showgrid = TRUE,
                     gridcolor = "lightgray",
                     gridwidth = 0.05),
        yaxis = list(title = htmltools::HTML('<b>Durchschnittliche Energie [kJ]</b>'), tickformat = ",.0f",
                     showgrid = TRUE,
                     gridcolor = "lightgray",
                     gridwidth = 0.05),
        barmode = 'stack',
        bargap = 0.4
      )
    
    # Wirkungsgradbeschriftung 
    for(i in 1:nrow(avg_efficiency_data)) {
      x_val <- if (length(group_vars) > 1) {
        interaction(avg_efficiency_data$Intensität[i], avg_efficiency_data$Bedingung[i], sep = "_")
      } else {
        avg_efficiency_data[[group_vars]][i]
      }
      
      y_val <- sum(avg_energy_data$AvgWert[
        if (length(group_vars) > 1) {
          avg_energy_data$Intensität == avg_efficiency_data$Intensität[i] & 
            avg_energy_data$Bedingung == avg_efficiency_data$Bedingung[i]
        } else {
          avg_energy_data[[group_vars]] == avg_efficiency_data[[group_vars]][i]
        }
      ]) + 15
      
      p <- add_annotations(p,
                           x = x_val,
                           y = y_val,
                           text = sprintf("η = %.3f", avg_efficiency_data$AvgWirkungsgrad[i]),
                           showarrow = FALSE,
                           yshift = 25,
                           bgcolor = "white",
                           bordercolor = "black",
                           borderpad = 4
      )
    }
    
    # Durchschnittliche Leistung beschriften
    for(i in 1:nrow(avg_power_data)) {
      x_val <- if (length(group_vars) > 1) {
        interaction(avg_power_data$Intensität[i], avg_power_data$Bedingung[i], sep = "_")
      } else {
        avg_power_data[[group_vars]][i]
      }
      
      p <- add_annotations(p,
                           x = x_val,
                           y = 0,
                           text = sprintf("P<sub>TOT</sub> = %.1fW", avg_power_data$AvgPTOT[i]),
                           showarrow = FALSE,
                           yshift = 12,
                           xanchor = "center",
                           font = list(color = "black", size = 10)
      )
    }
    
    return(p)
  })
  
  output$barplot <- renderPlotly({
    generatePlot()
  })
  
  output$summaryTable <- renderDT({
    # Datenfilterung je nach Auswahl
    filtered_data <- filter(Bedingungen_data, 
                            Proband %in% input$selectedProband)
    
    # Prüfen, ob Bedingungen oder Intensitäten ausgewählt wurden
    bedingungen_ausgewaehlt <- length(input$selectedBedingung) > 0
    intensitaeten_ausgewaehlt <- length(input$selectedIntensität) > 0
    
    if (bedingungen_ausgewaehlt) {
      filtered_data <- filtered_data %>% 
        filter(Bedingung %in% input$selectedBedingung)
    }
    
    if (intensitaeten_ausgewaehlt) {
      filtered_data <- filtered_data %>% 
        filter(Intensität %in% input$selectedIntensität)
    }
    
    # Gruppierung basierend auf der Auswahl
    if (bedingungen_ausgewaehlt && intensitaeten_ausgewaehlt) {
      group_vars <- c("Bedingung", "Intensität")
    } else if (bedingungen_ausgewaehlt) {
      group_vars <- "Bedingung"
    } else if (intensitaeten_ausgewaehlt) {
      group_vars <- "Intensität"
    } else {
      # Wenn weder Bedingungen noch Intensitäten ausgewählt sind
      return(NULL)
    }
    
    # Daten umwandeln und Durchschnittswerte berechnen für die Energiequellen
    avg_energy_data <- filtered_data %>%
      group_by(across(all_of(group_vars))) %>%
      summarise(
        W_aerob = sprintf("%.1f kJ", round(mean(`W*Aerob [kJ]`, na.rm = TRUE), 1)),
        W_BLC = sprintf("%.1f kJ", round(mean(`WBLC [kJ]`, na.rm = TRUE), 1)),
        W_PCR = sprintf("%.1f kJ", round(mean(`WPCR [kJ]`, na.rm = TRUE), 1)),
        P_mean = sprintf("%.1f Watt", round(mean(P_mean, na.rm = TRUE), 1)),
        P_Int = sprintf("%.1f Watt", round(mean(P_Int, na.rm = TRUE), 1)),
        Drehzahl = sprintf("%.1f rpm", mean(nD, na.rm = TRUE)),
        Wirkungsgrad = sprintf("%.2f%%", 100 * mean(WirkPhysio, na.rm = TRUE)),
        Efficiency = sprintf("%.2f%%", mean(Efficiency, na.rm = TRUE)),
        `Pedal Smoothness` = sprintf("%.2f%%", mean(Pedal_Smoothness, na.rm = TRUE)),
        `P_max` = sprintf("%.1f Watt", mean(P_max, na.rm = TRUE)),
        `HR%` = sprintf("%.1f%%", mean(HR_percent, na.rm = TRUE)),
        `VO2%` = sprintf("%.1f%%", mean(VO2_percent, na.rm = TRUE)),
        `ΔBLC` = sprintf("%.1f mmol/L", mean(`ΔBLC`, na.rm = TRUE))
      ) %>% 
      ungroup()
    
    # Anzeigen der Zusammenfassung in der Tabelle
    datatable(avg_energy_data)
  })
  
  output$ggbetweenstatsPlot <- renderPlot({
    # Datenfilterung je nach Auswahl
    filtered_data <- filter(Bedingungen_data, 
                            Proband %in% input$selectedProband)
    
    # Prüfen, ob Bedingungen oder Intensitäten ausgewählt wurden
    bedingungen_ausgewaehlt <- length(input$selectedBedingung) > 0
    intensitaeten_ausgewaehlt <- length(input$selectedIntensität) > 0
    
    if (bedingungen_ausgewaehlt) {
      filtered_data <- filtered_data %>% 
        filter(Bedingung %in% input$selectedBedingung)
    }
    
    if (intensitaeten_ausgewaehlt) {
      filtered_data <- filtered_data %>% 
        filter(Intensität %in% input$selectedIntensität)
    }
    
    # Wenn weder Bedingungen noch Intensitäten ausgewählt sind
    if (!bedingungen_ausgewaehlt && !intensitaeten_ausgewaehlt) {
      return(NULL)
    }
    
    # Erstellen des Plots mit angepasster Textgröße
    if (bedingungen_ausgewaehlt && intensitaeten_ausgewaehlt) {
      filtered_data <- filtered_data %>%
        mutate(Intensität_Bedingung = interaction(Intensität, Bedingung, sep = "_"))
      
      plt <- ggbetweenstats(
        data = filtered_data,
        x = Intensität_Bedingung,
        y = WirkPhysio,
        type = "parametric",
        var.equal = TRUE,
        plot.type = "box",
        pairwise.comparisons = TRUE,
        pairwise.display = "all",
        p.adjust.method = "bonferroni",
        centrality.plotting = TRUE,
        bf.message = FALSE,
        title = "WirkPhysio nach Intensität und Bedingung",
        xlab = "Intensität_Bedingung",
        ylab = "WirkPhysio",
        ggtheme = ggplot2::theme_light() +
          theme(
            plot.title = element_text(size = 18, face = "bold"),
            axis.title.x = element_text(size = 16),
            axis.title.y = element_text(size = 16),
            axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
            axis.text.y = element_text(size = 12)
          )
      ) +
        ggplot2::theme(
          text = element_text(size = 14),  # Erhöht die Größe des gesamten Textes
          plot.subtitle = element_text(size = 16),  # Erhöht die Größe des Untertitels (einschließlich p-Werte)
          axis.title = element_text(size = 16),
          axis.text = element_text(size = 14)
        )
    } else if (bedingungen_ausgewaehlt) {
      plt <- ggbetweenstats(
        data = filtered_data,
        x = Bedingung,
        y = WirkPhysio,
        type = "parametric",
        var.equal = TRUE,
        plot.type = "box",
        pairwise.comparisons = TRUE,
        pairwise.display = "all",
        p.adjust.method = "bonferroni",
        centrality.plotting = TRUE,
        bf.message = FALSE,
        title = "WirkPhysio nach Bedingung",
        xlab = "Bedingung",
        ylab = "WirkPhysio",
        ggtheme = ggplot2::theme_light() +
          theme(
            plot.title = element_text(size = 16, face = "bold"),
            axis.title.x = element_text(size = 14),
            axis.title.y = element_text(size = 14),
            axis.text.x = element_text(size = 12),
            axis.text.y = element_text(size = 12)
          )
      )
    } else if (intensitaeten_ausgewaehlt) {
      plt <- ggbetweenstats(
        data = filtered_data,
        x = Intensität,
        y = WirkPhysio,
        type = "parametric",
        var.equal = TRUE,
        plot.type = "box",
        pairwise.comparisons = TRUE,
        pairwise.display = "all",
        p.adjust.method = "bonferroni",
        centrality.plotting = TRUE,
        bf.message = FALSE,
        title = "WirkPhysio nach Intensität",
        xlab = "Intensität",
        ylab = "WirkPhysio",
        ggtheme = ggplot2::theme_light() +
          theme(
            plot.title = element_text(size = 16, face = "bold"),
            axis.title.x = element_text(size = 14),
            axis.title.y = element_text(size = 14),
            axis.text.x = element_text(size = 12),
            axis.text.y = element_text(size = 12)
          )
      )
    }
    
    print(plt)
  })
  
  output$anovaResults <- renderPrint({
    # Datenfilterung je nach Auswahl
    filtered_data <- filter(Bedingungen_data, 
                            Proband %in% input$selectedProband)
    
    # Prüfen, ob Bedingungen oder Intensitäten ausgewählt wurden
    bedingungen_ausgewaehlt <- length(input$selectedBedingung) > 0
    intensitaeten_ausgewaehlt <- length(input$selectedIntensität) > 0
    
    if (bedingungen_ausgewaehlt) {
      filtered_data <- filtered_data %>% 
        filter(Bedingung %in% input$selectedBedingung)
    }
    
    if (intensitaeten_ausgewaehlt) {
      filtered_data <- filtered_data %>% 
        filter(Intensität %in% input$selectedIntensität)
    }
    
    # Wenn weder Bedingungen noch Intensitäten ausgewählt sind
    if (!bedingungen_ausgewaehlt && !intensitaeten_ausgewaehlt) {
      return("Bitte wählen Sie mindestens eine Bedingung oder Intensität aus.")
    }
    
    # Kombinierte Gruppe erstellen
    filtered_data <- filtered_data %>%
      mutate(Kombinierte_Gruppe = interaction(Bedingung, Intensität, sep = "_"))
    
    # ANOVA für WirkPhysio durchführen
    anova_model <- aov(WirkPhysio ~ Kombinierte_Gruppe, data = filtered_data)
    
    # ANOVA-Ergebnisse ausgeben
    cat("ANOVA-Ergebnisse für WirkPhysio:\n\n")
    print(summary(anova_model))
    
    # Zusätzliche statistische Werte
    cat("\nZusätzliche statistische Werte:\n")
    cat("Mittelwert WirkPhysio:", mean(filtered_data$WirkPhysio, na.rm = TRUE), "\n")
    cat("Standardabweichung WirkPhysio:", sd(filtered_data$WirkPhysio, na.rm = TRUE), "\n")
    cat("Median WirkPhysio:", median(filtered_data$WirkPhysio, na.rm = TRUE), "\n")
    cat("Minimum WirkPhysio:", min(filtered_data$WirkPhysio, na.rm = TRUE), "\n")
    cat("Maximum WirkPhysio:", max(filtered_data$WirkPhysio, na.rm = TRUE), "\n")
    
    # Post-hoc Test (Tukey HSD), wenn mindestens 3 Kombinationen vorhanden sind
    if (length(unique(filtered_data$Kombinierte_Gruppe)) >= 3) {
      cat("\nTukey HSD Post-hoc Test:\n")
      tukey_result <- TukeyHSD(anova_model)
      print(tukey_result)
    } else {
      cat("\nKein Post-hoc Test durchgeführt, da weniger als 3 Kombinationen ausgewählt wurden.\n")
    }
  })
}

# Starte ShinyApp
shinyApp(ui = ui, server = server)
