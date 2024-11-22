library(shiny)
library(tidyverse)
library(HHSKwkl)
library(leaflet)
library(glue)
library(sf)
library(lubridate)
library(timetk)
library(plotly)
library(rlang)
library(bslib)



# source("R/data_online.R")
ws_grens <- sf::st_read("data/ws_grens.gpkg") %>% sf::st_transform(crs = 4326)

url_csv <- function(mp) paste0('<a href = "https://www.schielandendekrimpenerwaard.nl/kaart/waterkwaliteit/wkl_gegevens_op_kaart/meetgegevens/', mp, '.csv">Meetgegevens</a>')
url_pdf <- function(mp) paste0('<a href = "https://www.schielandendekrimpenerwaard.nl/kaart/waterkwaliteit/wkl_gegevens_op_kaart/grafieken/', mp, '.pdf">Grafieken</a>')

# Define UI 
ui <- page_sidebar(
  theme = bs_theme(version = 5,
                   primary = "#0079C2",
                   "navbar-bg" = "#0079C2"),
  
  title = div(
    class = "d-flex align-items-center",
    tags$img(src = "logo_website.png", height = "60px", class = "me-3"),
    span("Diagnostiek waterkwaliteit", style = "color: #0079C2; font-weight: bold")
  ),
  
  # Sidebar
  sidebar = sidebar(width = 350,
    selectInput("mp_sel", "Meetpunt", choices = c("S_0040")),
    selectInput("param_sel", "Parameter", choices = c("Chloride" = 1)),
    selectInput("param_group", "Parametergroep (optioneel)", multiple = TRUE,
                choices = list("Algemeen", "Bacteriologie", "Bestrijdingsmiddelen", "Blauwalgen", 
                               "Metalen opgelost", "Metalen totaal", "Organisch", "Zintuiglijk")),
    sliderInput("jaar_sel", "Jaren", 
                value = c(year(Sys.Date()) - 20, year(Sys.Date())), step = 1,
                min = 1967, max = year(Sys.Date()), sep = ""),
    checkboxInput("log_trans", "Logaritmische transformatie")
  ),
  
  # Main content
  navset_card_tab(
    nav_panel("Grafiek", 
              card(
                card_body(
                  plotOutput("grafiek_loc", height = "600px")
                )
              )
    ),
    nav_panel("Boxplots",
              card(
                card_body(
                  plotOutput("boxplots", height = "600px")
                )
              )
    ),
    nav_panel("Vergelijk jaren", 
              card(
                card_body(
                  plotOutput("grafiek_vgl", height = "600px")
                )
              )
    ),
    nav_panel("Histogram", 
              card(
                card_body(
                  plotOutput("histogram", height = "600px", width = "900px")
                )
              )
    ),
    nav_panel("Kaart", 
              card(
                card_body(
                  leafletOutput("kaart", height = "800px")
                )
              )
    ),
    nav_panel("STL", 
              card(
                card_body(
                  plotlyOutput("stl", width = "80%", height = "800px")
                )
              )
    ),
    nav_panel("Anomaly", 
              card(
                card_body(
                  plotlyOutput("anomaly", width = "100%", height = "700px")
                )
              )
    ),
    nav_panel("ACF", 
              card(
                card_body(
                  plotlyOutput("acf", width = "70%", height = "700px")
                )
              )
    ),
    nav_panel("Seasonal", 
              card(
                card_body(
                  plotlyOutput("seasonal", width = "70%", height = "700px")
                )
              )
    )
  )
)

# Server logic remains the same
server <- function(input, output) {
  
  meetpunten <- data_online("meetpunten.rds")
  parameters <- data_online("parameters.rds")
  fys_chem <- data_online("fys_chem.rds") %>% semi_join(filter(meetpunten, meetpunttypering %in% c(1, 2, 3, 5, 6, 12)))
  
  f_parnaam <- maak_opzoeker(parameters, parnr, parnaamlang)
  f_eenheid <- maak_opzoeker(parameters, parnr, eenheid)
  f_mpomsch <- maak_opzoeker(meetpunten, mp, mpomsch)
  
  vgl_jaren <- function(data, mp, parnr){
    
    if (max(data$waarde) < 2 * min(data$waarde)) scale_y_custom <- scale_y_continuous() else scale_y_custom <- scale_y_continuous(limits = c(0, NA), expand = expansion(c(0, 0.1)))
    
    max_jaar = max(year(data$datum))
    
    data %>% 
      mutate(jaar = year(datum),
             maand = month(datum, label = TRUE)) %>% 
      mutate(max_jaar = ifelse(jaar == max_jaar, max_jaar, "Andere jaren")) %>% 
      ggplot(aes(maand, waarde, group = jaar, colour = max_jaar, size = max_jaar)) + 
      geom_line() +
      scale_y_custom +
      scale_colour_manual(values = setNames(c(blauw, "grey70"), c(max_jaar, "Andere jaren"))) +
      scale_size_manual(values = c(2, 0.6)) + 
      labs(title = glue("Meetpunt: {mp}"),
           subtitle = glue("Parameter: {f_parnaam(parnr)}"),
           y = f_eenheid(parnr),
           caption = f_mpomsch(mp)) +
      hhskthema()
  }
  
  f_bins <- function(domain){ 
    bins <- 
      c(min(domain),
        domain %>% 
          quantile(c(0.01,0.05,0.1,0.3,0.7,0.9,0.95,0.99)) %>% 
          signif(digits = 2),
        max(domain)
      ) %>% 
      unique()
    
    if (length(bins)  <= 1) bins <- c(bins, bins + 0.0001)
    
    bins
  }
  
  fys_chem_mp <- reactive({fys_chem %>%  filter(mp == input$mp_sel)})
  
  fys_chem_sel <- reactive({
    fc_sel <- fys_chem_mp() %>% 
      filter(parnr == input$param_sel,
             year(datum) >= input$jaar_sel[1],
             year(datum) <= input$jaar_sel[2]
      )
    
    try(
      if (input$log_trans) fc_sel <- mutate(fc_sel, waarde = log10(waarde))
      )
    
    fc_sel
  })
  
  # Update meetpuntselectie
  observe({
    meetpunten <- fys_chem %>% pull(mp) %>% unique() %>% sort()
    updateSelectInput(inputId = "mp_sel", choices = meetpunten, selected = "S_0040")
  })
  
  # Update parameterselectie
  observe({

    parnrs <- fys_chem_mp() %>% pull(parnr) %>% unique() %>% sort()

    parnamen <- parameters %>% filter(parnr %in% parnrs)
    if (!is.null(input$param_group)) parnamen <- parnamen %>% filter(cluster %in% input$param_group)

    par_selected <- ifelse(input$param_sel %in% parnamen$parnr, input$param_sel, parnamen$parnr[1])

    parnamen_choices <-  parnamen %>% select(parnaamlang, parnr) %>% deframe()

    updateSelectInput(inputId = "param_sel", choices = parnamen_choices, selected = par_selected)
  })
  
  output$kaart <- renderLeaflet({
    meetpunten %>% 
      filter(mp == input$mp_sel) %>% 
      sf::st_as_sf(coords = c("x", "y"), crs = 28992) %>% 
      st_transform(crs = 4326) %>% 
      basiskaart() %>%
      addPolylines(data = ws_grens, color = "grey", opacity = 1, weight = 2) %>%
      addCircleMarkers(popup = ~glue("Meetpunt: {input$mp_sel}<br>Omschrijving: {f_mpomsch(input$mp_sel)}")) %>% 
      addPopups(popup = ~glue("Meetpunt: {input$mp_sel}<br>Omschrijving: {f_mpomsch(input$mp_sel)}")) %>% 
      leaflet.extras::addFullscreenControl()
  })
  
  output$histogram <- renderPlot({
    plot <-
      fys_chem_sel() %>%
      ggplot(aes(waarde)) +
      geom_histogram(fill = grijs) +
      scale_y_continuous(limits = c(0, NA), expand = expansion(c(0, 0.1))) +
      labs(title = glue("Histogram van {f_parnaam(input$param_sel)}"),
           subtitle = glue("Alle metingen van {input$datum_sel[1]} tot en met {input$datum_sel[2]}"),
           x = f_eenheid(input$param_sel),
           y = "aantal") +
      hhskthema()
    
    if (input$log_trans) {
      plot <-
        fys_chem_sel() %>%
        mutate(waarde = 10 ^ waarde) %>% 
        ggplot(aes(waarde)) +
        geom_histogram(fill = grijs) +
        scale_y_continuous(limits = c(0, NA), expand = expansion(c(0, 0.1))) +
        labs(title = glue("Histogram van {f_parnaam(input$param_sel)}"),
             subtitle = glue("Alle metingen van {input$datum_sel[1]} tot en met {input$datum_sel[2]}"),
             x = f_eenheid(input$param_sel),
             y = "aantal") +
        hhskthema() +
        scale_x_log10()
    }

    plot
  })
  
  output$grafiek_loc <- renderPlot({

    grafiek <- 
      fys_chem_sel() %>%
      grafiek_basis(mp = glue("{input$mp_sel}"),
                    mpomsch = f_mpomsch(input$mp_sel),
                    parnaam = f_parnaam(input$param_sel),
                    eenheid = f_eenheid(input$param_sel))
    
    if(input$log_trans) {
      grafiek <- 
        fys_chem_sel() %>%
        mutate(waarde = 10 ^ waarde) %>% 
        grafiek_basis(mp = glue("{input$mp_sel}"),
                      mpomsch = f_mpomsch(input$mp_sel),
                      parnaam = f_parnaam(input$param_sel),
                      eenheid = f_eenheid(input$param_sel)) 
        grafiek <- grafiek + scale_y_log10()
    }
    
    grafiek  

  })
  
  output$boxplots <- renderPlot({
    plot_data <- fys_chem_sel() %>%
      mutate(jaar = year(datum))
    
    if(input$log_trans) {
      plot_data <- plot_data %>%
        mutate(waarde = 10 ^ waarde)
      
      ggplot(plot_data, aes(x = factor(jaar), y = waarde)) +
        geom_boxplot(fill = blauw_l) +
        scale_y_log10() +
        labs(title = glue("Boxplots van {f_parnaam(input$param_sel)} per jaar"),
             subtitle = glue("Meetpunt: {input$mp_sel}"),
             x = "Jaar",
             y = f_eenheid(input$param_sel),
             caption = f_mpomsch(input$mp_sel)) +
        hhskthema() 
    } else {
      ggplot(plot_data, aes(x = factor(jaar), y = waarde)) +
        geom_boxplot(fill = blauw_l) +
        scale_y_continuous(limits = c(0, NA), expand = expansion(c(0, 0.1))) +
        labs(title = glue("Boxplots van {f_parnaam(input$param_sel)} per jaar"),
             subtitle = glue("Meetpunt: {input$mp_sel}"),
             x = "Jaar",
             y = f_eenheid(input$param_sel),
             caption = f_mpomsch(input$mp_sel)) +
        hhskthema() 
    }
  })
  
  output$grafiek_vgl <- renderPlot({
    if(input$log_trans) {
      grafiek <- 
      fys_chem_sel() %>% 
      mutate(waarde = 10 ^ waarde) %>% 
        vgl_jaren(mp = input$mp_sel,
                  parnr = input$param_sel)
      grafiek <- grafiek + scale_y_log10()
      
    } else {
    grafiek <- 
      fys_chem_sel() %>% 
      vgl_jaren(mp = input$mp_sel,
                parnr = input$param_sel)
    }
    
    grafiek
  })
  
  output$stl <- renderPlotly({
    fys_chem_sel() %>% 
      plot_stl_diagnostics(datum, waarde, .interactive = TRUE)
  })
  
  output$anomaly <- renderPlotly({
    fys_chem_sel() %>% 
      plot_anomaly_diagnostics(datum, waarde, .interactive = TRUE, .alpha = 0.04)
  })
  
  output$acf <- renderPlotly({
    fys_chem_sel() %>% 
      plot_acf_diagnostics(datum, waarde, .interactive = TRUE)
  })
  
  output$seasonal <- renderPlotly({
    fys_chem_sel() %>% 
      plot_seasonal_diagnostics(datum, waarde, .interactive = TRUE)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
