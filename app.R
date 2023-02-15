library(shiny)
library(tidyverse)
library(HHSKwkl)
library(leaflet)
library(glue)
library(sf)
library(lubridate)
library(timetk)

# options(OutDec = ",")

# source("R/data_online.R")
ws_grens <- sf::st_read("data/ws_grens.gpkg") %>% sf::st_transform(crs = 4326)

url_csv <- function(mp) paste0('<a href = "https://www.schielandendekrimpenerwaard.nl/kaart/waterkwaliteit/wkl_gegevens_op_kaart/meetgegevens/', mp, '.csv">Meetgegevens</a>')
url_pdf <- function(mp) paste0('<a href = "https://www.schielandendekrimpenerwaard.nl/kaart/waterkwaliteit/wkl_gegevens_op_kaart/grafieken/', mp, '.pdf">Grafieken</a>')



# Define UI 
ui <- fluidPage(
  # Application title
  
  titlePanel( 
    div(column(width = 3, tags$img(src = "logo_website.png", height = "60px")), 
        column(width = 9, h2("Actuele waterkwaliteit", style = "color: #0079C2; font-weight: bold"))),
    windowTitle = "HHSK - Actuele waterkwaliteit"
  ),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput("mp_sel", "Meetpunt", choices = c("S_0040")),
      selectInput("param_sel", "Parameter", choices = c("Chloride" = 1)),
      selectInput("param_group", "Parametergroep (optioneel)", multiple = TRUE,
                         choices = list("Algemeen", "Bacteriologie", "Bestrijdingsmiddelen", "Blauwalgen", 
                                        "Metalen opgelost", "Metalen totaal", "Organisch", "Zintuiglijk")),
      # dateRangeInput("datum_sel", "Periode", language = "nl", separator = "t/m", start = Sys.Date() - 31, format = "dd-mm-yyyy"),
      sliderInput("jaar_sel", "Periode", 
                  value = c(year(Sys.Date()) - 20, year(Sys.Date())), 
                            min = 1967, max = year(Sys.Date()), sep = ""),
      # selectInput("agg_fun", "Waardebewerkingsmethode", choices = c("Gemiddelde" = "mean",
      #                                                               "Laatste" = "first", #sorteervolgorde is achterstevoren
      #                                                              "Mediaan" = "median",
      #                                                              "Maximum" = "max",
      #                                                              "Minimum" = "min")),
      # h3("Toelichting"),
      # p("De kaart laat de meetpunten zien waar in de gekozen periode metingen van de gekozen parameter beschikbaar zijn.
      #   De kleuren geven de waarde aan volgens de gekozen bewerkingsmethode. De kleuren zijn niet lineair verdeeld,
      #   maar geven de volgende intervallen: 0% - 1% - 5% - 10% - 30% - 70% - 90% - 95% - 99% - 100%."),
      # p("De eerste grafiek toont de metingen (onbewerkt) vanaf 2010 van het meetpunt dat op de kaart wordt aangeklikt."),
      # p("De tweede grafiek is een histogram met alle metingen (onbewerkt) van de gekozen periode en parameters."),
      
      width = 3
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Grafieken", 
                 plotOutput("grafiek_loc", height = "500px"),
                 plotOutput("histogram", height = "350px", width = "600px"),
                 leafletOutput("kaart", height = "500px"),
                 ),
        tabPanel("Diagnostiek")
      ),
      
      
      width = 9
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  meetpunten <- data_online("meetpunten.rds")
  parameters <- data_online("parameters.rds")
  fys_chem <- data_online("fys_chem.rds") %>% semi_join(filter(meetpunten, meetpunttypering %in% c(1, 2, 3, 5, 12)))
  
  # meetpunten_leaflet <- meetpunten %>% 
  #   select(mp, x, y) %>% 
  #   sf::st_as_sf(coords = c("x", "y"), crs = 28992) %>% 
  #   add_lat_long() %>% 
  #   sf::st_transform(crs = 4326)
  
  f_parnaam <- maak_opzoeker(parameters, parnr, parnaamlang)
  f_eenheid <- maak_opzoeker(parameters, parnr, eenheid)
  f_mpomsch <- maak_opzoeker(meetpunten, mp, mpomsch)
  
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
    fys_chem_mp() %>% 
      filter(parnr == input$param_sel,
             year(datum) >= input$jaar_sel[1],
             year(datum) <= input$jaar_sel[2]
      )
  })
  
  # mp_sel <- reactive({
  #   if (!is.null(input$kaart_marker_click)){
  #     
  #     punt <- 
  #       st_sfc(
  #         st_point(c(input$kaart_marker_click[[4]], 
  #                    input$kaart_marker_click[[3]])), 
  #         crs = 4326)
  #     
  #     mp_op_kaart <- 
  #       meetpunten_leaflet %>% 
  #       inner_join(fys_chem_sel(), by = "mp")
  #     
  #     mp_sel <- 
  #       mp_op_kaart %>% 
  #       .[[st_nearest_feature(punt, mp_op_kaart), 1]]
  #     
  #   } else {
  #     mp_sel <- "XXXX"
  #   }
  #   mp_sel
  # })
  
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
  
  # observe({
  #   
  #   agg_fun <- eval(sym(input$agg_fun))
  #   
  #   data_leaflet <- meetpunten_leaflet %>% 
  #     inner_join(fys_chem_sel()) %>% 
  #     group_by(mp) %>% 
  #     arrange(desc(datum)) %>% 
  #     summarise(detectiegrens = ifelse(any(is.na(detectiegrens)), "", paste0(first(detectiegrens), " ")),
  #               # popup_tekst = glue_collapse(na.omit(glue("{datum} -- {detectiegrens} {waarde} {f_eenheid(input$param_sel)}")[1:12]), sep = "<br>"),
  #               popup_tekst = glue("<b>Meetpunt:</b> {first(mp)}<br><b>Parameter:</b> {f_parnaam(input$param_sel)}<br><br>{url_pdf(first(mp))}<br><br>{url_csv(first(mp))}"),
  #               waarde = agg_fun(waarde)) %>% 
  #     ungroup() %>% 
  #     arrange(waarde) %>% 
  #     mutate(label_tekst = glue("{detectiegrens} {format(signif(waarde, 3), decimal.mark = ',', nsmall = 0)} {f_eenheid(input$param_sel)}"))
  #   
  #   rev_switch <- !input$param_sel %in% c(10, 11, 12, 14)
  #   
  #   pal <- colorBin("RdBu", domain = NULL, bins = f_bins(data_leaflet$waarde), reverse = rev_switch)
  #   
  #   if(nrow(data_leaflet) > 0){
  #     
  #     leafletProxy("kaart", data = data_leaflet) %>% 
  #       leaflet::removeControl("legenda") %>% 
  #       leaflet::clearMarkers() %>% 
  #       addCircleMarkers(label = ~label_tekst, popup = ~popup_tekst,
  #                        fillColor = ~pal(waarde), color = "#555", fillOpacity = 1, opacity = 1, 
  #                        weight = 1, radius = 8, clusterId = "meetpunten") %>% 
  #       addLegend(pal = pal, values = ~waarde, opacity = 1, 
  #                 labFormat = labelFormat(suffix = glue(" {f_eenheid(input$param_sel)}")),
  #                 layerId = "legenda")
  #   } else {
  #     
  #     leafletProxy("kaart") %>% 
  #       leaflet::removeControl("legenda") %>% 
  #       leaflet::clearMarkers()
  #   }
  # })
  
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

    plot
  })
  
  output$grafiek_loc <- renderPlot({

    fys_chem_sel() %>%

      grafiek_basis(mp = glue("{input$mp_sel}"),
                    mpomsch = f_mpomsch(input$mp_sel),
                    parnaam = f_parnaam(input$param_sel),
                    eenheid = f_eenheid(input$param_sel))

  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
