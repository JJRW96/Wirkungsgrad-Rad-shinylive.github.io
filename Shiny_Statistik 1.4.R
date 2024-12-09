library(shiny)
library(dplyr)
library(tidyr)
library(plotly)
library(htmltools)
library(afex)
library(emmeans)
library(reactable)

# Beispieldaten (Sie sollten diese durch Ihre tatsächlichen Daten ersetzen)
Bedingungen_data <- data.frame(
  Proband = factor(rep(c(1, 6, 10, 13, 14, 15, 17, 18, 19), each = 6)),
  Bedingung = rep(c("stehen", "sitzen", "sitzen", "stehen", "sitzen", "stehen"), 9),
  Intensität = rep(c("leicht", "leicht", "moderat", "moderat", "schwer", "schwer"), 9),
  WirkPhysio = c(0.236, 0.246, 0.242, 0.229, 0.232, 0.239, 0.235, 0.241, 0.248, 0.239, 0.240, 0.243, 0.253, 0.268, 0.246, 0.243, 0.237, 0.239, 0.246, 0.260, 0.237, 0.256, 0.245, 0.236, 0.256, 0.230, 0.251, 0.238, 0.236, 0.257, 0.229, 0.250, 0.239, 0.226, 0.230, 0.224, 0.267, 0.254, 0.247, 0.250, 0.239, 0.241, 0.253, 0.256, 0.262, 0.257, 0.240, 0.255, 0.236, 0.269, 0.260, 0.237, 0.243, 0.259)
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
      plotlyOutput("boxplot"),
      uiOutput("anovaOutput")
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
    
    if (length(input$selectedBedingung) > 0 && length(input$selectedIntensität) > 0) {
      data$Gruppe <- interaction(data$Intensität, data$Bedingung, sep = "_")
    } else if (length(input$selectedBedingung) > 0) {
      data$Gruppe <- data$Bedingung
    } else if (length(input$selectedIntensität) > 0) {
      data$Gruppe <- data$Intensität
    } else {
      data$Gruppe <- "Alle"
    }
    
    return(data)
  })
  
  # Wirkungsgrad Boxplot
  output$boxplot <- renderPlotly({
    data <- filtered_data()
    
    if (nrow(data) == 0) {
      return(plotly_empty() %>% 
               layout(title = "Keine Daten für die ausgewählten Kriterien verfügbar"))
    }
    
    color_map <- c(
      "leicht_sitzen" = "#42BA97", "leicht_stehen" = "#62A39F",
      "moderat_sitzen" = "#1CADE4", "moderat_stehen" = "#2683C6",
      "schwer_sitzen" = "#EF5350", "schwer_stehen" = "#C8133B",
      "leicht" = "#42BA97", "moderat" = "#1CADE4", "schwer" = "#EF5350",
      "sitzen" = "#62A39F", "stehen" = "#2683C6"
    )
    
    p2BxI <- plot_ly(data = data, 
                     x = ~Gruppe, 
                     y = ~WirkPhysio,
                     type = "box",
                     color = ~Gruppe,
                     colors = color_map[unique(data$Gruppe)],
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
  
  # ANOVA und Post-hoc Test
  output$anovaOutput <- renderUI({
    data <- filtered_data()
    
    bedingung_selected <- length(input$selectedBedingung) > 1
    intensität_selected <- length(input$selectedIntensität) > 1
    
    if (bedingung_selected && intensität_selected) {
      anova_result <- afex::aov_ez(
        id = "Proband",
        dv = "WirkPhysio",
        data = data,
        within = c("Bedingung", "Intensität"),
        type = 3
      )
      
      anova_title <- "Zweifaktorielle ANOVA mit Messwiederholung Ergebnisse:"
    } else if (bedingung_selected) {
      anova_result <- afex::aov_ez(
        id = "Proband",
        dv = "WirkPhysio",
        data = data,
        within = "Bedingung",
        type = 3
      )
      
      anova_title <- "Einfaktorielle ANOVA mit Messwiederholung für Bedingung:"
    } else if (intensität_selected) {
      anova_result <- afex::aov_ez(
        id = "Proband",
        dv = "WirkPhysio",
        data = data,
        within = "Intensität",
        type = 3
      )
      
      anova_title <- "Einfaktorielle ANOVA mit Messwiederholung für Intensität:"
    } else {
      return(p("Bitte wählen Sie mindestens zwei Stufen eines Faktors (Bedingung oder Intensität) aus."))
    }
    
    anova_df <- as.data.frame(anova_result$anova_table)
    anova_df$Effect <- rownames(anova_df)
    anova_df <- anova_df[, c("Effect", "num Df", "den Df", "MSE", "F", "Pr(>F)", "ges")]
    
    anova_df$Effect <- ifelse(anova_df$Effect == "(Intercept)", "Konstante", anova_df$Effect)
    
    get_sig_code <- function(p_value, effect) {
      if (effect == "Konstante") return("")
      if (p_value < 0.001) return("***")
      else if (p_value < 0.01) return("**")
      else if (p_value < 0.05) return("*")
      else if (p_value < 0.1) return(".")
      else return(" ")
    }
    
    anova_df$MSE <- round(anova_df$MSE, 6)
    anova_df$F <- round(anova_df$F, 3)
    anova_df$`Pr(>F)` <- round(anova_df$`Pr(>F)`, 5)
    anova_df$ges <- round(anova_df$ges, 3)
    anova_df$Signifikanz <- mapply(get_sig_code, anova_df$`Pr(>F)`, anova_df$Effect)
    
    colnames(anova_df) <- c("Effekt", "df", "Residuen", "MSE", "F-Wert", "p-Wert", "η²", "Signifikanz")
    
    anova_table <- reactable(anova_df)
    
    sig_codes <- "Signifikanzcodes: 0 = '***' | 0.001 = '**' | 0.01 = '*' | 0.05 = '.' | 0.1 = ' '"
    
    get_sig_code <- function(p_value) {
      if (p_value < 0.001) return("***")
      else if (p_value < 0.01) return("**")
      else if (p_value < 0.05) return("*")
      else if (p_value < 0.1) return(".")
      else return(" ")
    }
    
    format_posthoc <- function(posthoc, factor_name) {
      posthoc_df <- as.data.frame(posthoc)
      posthoc_df <- posthoc_df[, c("contrast", "p.value")]
      colnames(posthoc_df) <- c("Kontrast", "p_Wert")
      
      posthoc_df <- posthoc_df %>%
        dplyr::mutate(
          `p-Wert` = sprintf("%.4f", round(p_Wert, 4)),
          Signifikanz = sapply(p_Wert, get_sig_code)
        ) %>%
        dplyr::select(Kontrast, `p-Wert`, Signifikanz)
      
      posthoc_df <- posthoc_df[order(as.numeric(posthoc_df$`p-Wert`)), ]
      
      return(posthoc_df)
    }
    
    posthoc_tables <- list()
    
    if (bedingung_selected && intensität_selected) {
      posthoc_kombination <- emmeans(anova_result, ~Bedingung:Intensität) %>%
        pairs(adjust = "bonferroni")
      posthoc_kombination_df <- format_posthoc(posthoc_kombination, "Kombination")
      posthoc_tables$kombination <- reactable(posthoc_kombination_df, defaultPageSize = 20, rownames = FALSE)
    } else if (bedingung_selected) {
      posthoc_bedingung <- emmeans(anova_result, ~Bedingung) %>%
        pairs(adjust = "bonferroni")
      posthoc_bedingung_df <- format_posthoc(posthoc_bedingung, "Bedingung")
      posthoc_tables$bedingung <- reactable(posthoc_bedingung_df, defaultPageSize = 20, rownames = FALSE)
    } else if (intensität_selected) {
      posthoc_intensität <- emmeans(anova_result, ~Intensität) %>%
        pairs(adjust = "bonferroni")
      posthoc_intensität_df <- format_posthoc(posthoc_intensität, "Intensität")
      posthoc_tables$intensität <- reactable(posthoc_intensität_df, defaultPageSize = 20, rownames = FALSE)
    }
    
    output <- tagList(
      h4(anova_title),
      div(style = "margin-top: 20px;"),
      anova_table,
      div(style = "margin-top: 20px;"),
      tags$i(p(sig_codes))
    )
    
    if (length(posthoc_tables) > 0) {
      for (factor_name in names(posthoc_tables)) {
        output <- tagList(
          output,
          div(style = "margin-top: 20px;"),
          h4(paste("Paarweise Vergleiche (Post-hoc) für", factor_name, ":")),
          posthoc_tables[[factor_name]]
        )
      }
    }
    
    return(output)
  })
}

# ShinyApp starten
shinyApp(ui = ui, server = server)