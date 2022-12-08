library(shiny)
library(leaflet)
library(leaflet.extras)
library(sf)
library(DT)
library(tidyverse)

source("data-run.R", encoding = "utf-8")

ui <- fixedPage(
  
  titlePanel(title = div(h1("Welcome to DREAMS Sat", style="margin: 0;"), 
                         h4('Saturation calculation application', style="margin: 0;")), 
             windowTitle = "DREAMS Sat"),
  sidebarLayout(
    sidebarPanel(
      strong("Step 0"),
      br(),
      strong("Import Saved Parameters"),
      br(),
      strong("[returning teams]"),
      br(),
      br(),
      actionButton("import",
                   "Import save token?"),
      br(),
      br(),
      strong("Step 1"),
      selectInput("country",
                  "Select Country",
                  selected = "Botswana",
                  choices = c("Botswana",
                              #"Cote d'Ivoire",
                              #"Eswatini",
                              #"Haiti",
                              "Kenya",
                              "Lesotho",
                              #"Malawi",
                              #"Mozambique",
                              #"Namibia",
                              #"Rwanda",
                              #"South Africa",
                              #"South Sudan",
                              #"Tanzania",
                              #"Uganda",
                              #"Zambia",
                              "Zimbabwe")),
      strong("Step 2"),
      radioButtons("structure",
                   "Set Population Structure:",
                   c("Simplified (Even 20%)" = "basic20",
                     "Match National" = "matchNational",
                     "Custom" = "custom")
      ),
      conditionalPanel(
        condition = "input.structure == 'custom'",
        strong("Custom Structure step 2a"),
        p("Download blank population\nstructure worksheet"),
        actionButton("blankTemplateDownload",
                     "Download blank"),
        br(),
        br(),
        strong("Step 2b"),
        p("Open worksheet in Excel, fill out 'proportion' column and save"),
        strong("Step 2c"),
        p("Upload completed population\nstructure worksheet"),
        actionButton("completedTemplateUpload",
                     "Upload completed"),
        br(),
        br()),
      strong("Step 3"),
      checkboxGroupInput("checkGroup_eligibility", 
                         label = "Apply eligibility modifier to:", 
                         choices = ""),
      numericInput("eligibility",
                   "Set eligibility modifier:",
                   value = 0,
                   0,
                   100,
                   0.1),
      strong("Step 4"),
      checkboxGroupInput("checkGroup_catchment", 
                         label = "Apply DREAMS catchment to:", 
                         choices = ""),
      checkboxInput("catchment",
                    "Apply DREAMS catchment modifier (DEV ONLY):",
                    value = FALSE),
      conditionalPanel(
        condition = "input.catchment = TRUE",
        radioButtons("catchment_upordown",
                     "Adjust denominator(s) up or numerator(s) down",
                     c("Denominator up" = "dUp",
                       "Numerator down" = "nDown")
        )
      ),
      strong("Step 5"),
      checkboxGroupInput("checkGroup_doubleCount", 
                         label = "Apply double count modifier to:", 
                         choices = ""),
      checkboxInput("doublecount",
                    "Apply double count modifier (DEV ONLY):",
                    value = FALSE),
      numericInput("doublecount_num",
                   "Set double count modifier:",
                   value = 0,
                   0,
                   100,
                   0.1),
      strong("Step 6"),
      br(),
      actionButton("export_token",
                   "Save Token Export"),
      br(),
      actionButton("export_DataPack",
                   "DataPack Export"),
      br(),
      br(),
      br(),
      p(
        tags$a(
          href = "https://www.census.gov/data/software/osds.html",
          "Data Source Details"
        )
      ),
      p(
        tags$a(
          href = "https://www.census.gov/data/software/osds.html",
          "Open Source Dissemination System, U.S. Census Bureau"
        )
      ),
      p(
        tags$a(
          href = "https://github.com/samdupre/PoC/app.R",
          "Source code on GitHub"
        )
      )
    ),
    mainPanel(
      # tabsetPanel(type = "pills",
      #             tabPanel("OU/SNUs",
      #                      leafletOutput("country_map")),
      #             tabPanel("PSNUs",
      #                      leafletOutput("PSNU_map"))),
      # p("Estimates plots"),
      #p("Estimates tables"),
      #DT::dataTableOutput("structureTable_check"),
      # DT::dataTableOutput("agywTable_check"),
      strong("Country:"),
      textOutput("params_country"),
      strong("Pop Structure Type:"),
      textOutput("params_popStructureType"),
      strong("Catchment Flag:"),
      textOutput("params_catchmentModifierFlag"),
      strong("Enrollment Modifier Flag:"),
      textOutput("params_enrollmentModifierFlag"),
      strong("Double Grad Modifier Flag:"),
      textOutput("params_doubleGradModifierFlag"),
      DT::dataTableOutput("countryDataFiltered_check")
      # DT::dataTableOutput("paramsTable_check"),
      # actionButton("Stakeholder_Export",
      #              "Export stakeholder reports"),
      # actionButton("Analyst_Export",
      #              "Export analyst reports")
    )
  )
)


server <- function(input, output, session) {
  params <- reactiveValues(
    country = "Botswana",
    country_SF1 = botADM1.sf,
    country_SF2 = botADM2.sf, #Adapt to allow for Lesotho case
    popStructureType = "basic20",
    popStructure = structure_table,
    catchmentModifierFlag = FALSE,
    catchmentModifier = 1,
    enrollmentModifierFlag = 0,
    enrollmentModifier = 1,
    doubleGradModifierFlag = 0,
    doubleGradModifier = 1
  )
  
  districts <- reactive({
    req(input$country)
    
    if(input$country == "Botswana") {
      a <- c("CENTRAL", 
             "KGATLENG", 
             "KWENENG", 
             "NORTH EAST", 
             "SOUTH EAST", 
             "SOUTHERN")
    } else if (input$country == "Kenya") {
      a <- c("HOMA BAY", 
             "KIAMBU", 
             "KISUMU", 
             "MIGORI", 
             "MOMBASA", 
             "NAIROBI CITY", 
             "SIAYA")
    } else if (input$country == "Lesotho") {
      a <- c("BEREA", 
             "MAFETENG",
             "MASERU",
             "MOHALE'S HOEK")
    } else if (input$country == "Zimbabwe") {
      a <- c("BULAWAYO", 
             "MANICALAND", 
             "MASHONALAND CENTRAL", 
             "MATABELELAND NORTH", 
             "MATABELELAND SOUTH", 
             "MIDLANDS")
    }
  })
  
  observeEvent(input$country, {
    
    updateCheckboxGroupInput(session, 
                             "checkGroup_eligibility", 
                             choices = districts())
  })
  
  observeEvent(input$country, {
    
    updateCheckboxGroupInput(session, 
                             "checkGroup_catchment", 
                             choices = districts())
  })
  
  observeEvent(input$country, {
    
    updateCheckboxGroupInput(session, 
                             "checkGroup_doubleCount", 
                             choices = districts())
  })
  
  countryDataFiltered <- reactive({
    req(input$country)
    selected_data <- countryData %>%
      dplyr::filter(country == input$country)
  })
  
  output$country_map <- leaflet::renderLeaflet({
    
    leaflet() %>%
      addProviderTiles("cartoDB.DarkMatter",
                       layerId = "basetile",
                       options = providerTileOptions(minZoom = 6)) %>%
      setMapWidgetStyle(list(background = "white"))
  })
  
  # observeEvent(input$country_refresh, {
  #   leafletProxy("country_map",
  #                data = country_SF) %>%
  #     clearShapes() %>%
  #     addPolygons(color = "#444444",
  #                 weight = 1,
  #                 opacity = 1,
  #                 fillOpacity = 0.8,
  #                 fillColor = "#FF6663",
  #                 highlightOptions = highlightOptions(color = "white",
  #                                                     weight = 3,
  #                                                     bringToFront = TRUE)) %>%
  #     setView(lng = mean(st_bbox(country_SF)[c(1,3)]),
  #             lat = mean(st_bbox(country_SF)[c(2,4)]),
  #             zoom = 6) %>%
  #     setMapWidgetStyle(list(background = "white")) %>%
  #     addFullscreenControl()
  # })
  
  output$PSNU_map <- leaflet::renderLeaflet({
    
    leaflet() %>%
      addProviderTiles("cartoDB.DarkMatter",
                       layerId = "basetile",
                       options = providerTileOptions(minZoom = 6)) %>%
      setMapWidgetStyle(list(background = "white"))
    
  })
  
  observeEvent(input$country_refresh, {
    leafletProxy("PSNU_map",
                 data = PSNU_SF) %>%
      clearShapes() %>%
      addPolygons(color = "#444444",
                  weight = 1,
                  opacity = 1,
                  fillOpacity = 0.8,
                  fillColor = "#F4D06F",
                  highlightOptions = highlightOptions(color = "white",
                                                      weight = 3,
                                                      bringToFront = TRUE)) %>%
      setView(lng = mean(st_bbox(PSNU_SF)[c(1,3)]),
              lat = mean(st_bbox(PSNU_SF)[c(2,4)]),
              zoom = 6) %>%
      setMapWidgetStyle(list(background = "white")) %>%
      addFullscreenControl()
  })
  
  observeEvent(input$country, {
    params$country = input$country
  })
  
  observeEvent(input$catchment, {
    if (params$catchmentModifierFlag == FALSE) {
      params$catchmentModifierFlag == TRUE
    } else {
      params$catchmentModifierFlag == FALSE
    }
  })
  
  output$structureTable_check <- DT::renderDataTable({
    req(structure_table)
    
    datatable(structure_table,
              options = list(scrollx = TRUE)
    )
  })
  
  output$countryDataFiltered_check <- DT::renderDataTable({
    req(countryDataFiltered())
    
    datatable(countryDataFiltered(),
              options = list(scrollx = TRUE)
    )
  })
  
  # output$agywTable_check <- DT::renderDataTable({
  #   req(agyw_table)
  # })
  #### PARAMS CHECK PANEL
  output$params_country <- renderText(params$country)
  output$params_popStructureType <- renderText(params$popStructureType)
  output$params_catchmentModifierFlag <- renderText(params$catchmentModifierFlag)
  output$params_enrollmentModifierFlag <- renderText(params$enrollmentModifierFlag)
  output$params_doubleGradModifierFlag <- renderText(params$doubleGradModifierFlag)
  
  
}

shinyApp(ui, server)