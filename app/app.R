library(shiny)
library(leaflet)
library(leaflet.extras)
library(sf)
library(DT)
library(tidyverse)
library(shinyglide)

#source("data-run.R", encoding = "utf-8") #Gone?
#source("s3_read.R")
#source("components.R")
source("functions.R")
source("params.R")
source("data_load.R")

css <- "
.container-fluid {
  padding: 0 30px;
}
.shinyglide {
  border: 1px solid #888;
  box-shadow: 0px 0px 10px #888;
  padding: 1em;
}
"


ui <- fluidPage(#style = "max-width: 800px;",
  tags$head(
    tags$style(css)
  ),
  titlePanel(title = div(h1("Welcome to DREAMS Sat", style="margin: 0;"), 
                         h4('Saturation calculation application', style="margin: 0;")), 
             windowTitle = "DREAMS Sat"),
  fluidRow(
    column(12,
           h3(textOutput("params_country")))
  ),
  fluidRow(
    column(12,
           actionButton("useDefaultParameters",
                        "Use defaults"),
           actionButton("initializeSelection",
                        "initialize selection"))
  ),
  fluidRow(
    column(12,
           glide(
             height = "450px",
             shinyglide::screen(
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
                                       "Zimbabwe"))
             ),
             shinyglide::screen(
               strong("Step 2"),
               radioButtons("structure",
                            "Set Population Structure:",
                            c("Simplified (Even 20%)" = "Default",
                              "Match National" = "National",
                              "Custom" = "Custom")),
               conditionalPanel(
                 condition = "input.structure == 'custom'",
                 strong("Custom Structure step 2a"),
                 p("Download blank population\nstructure worksheet"),
                 downloadButton("blankTemplateDownloadStructure",
                                "Download blank template"),
                 br(),
                 br(),
                 strong("Step 2b"),
                 p("Open worksheet in Excel, fill out 'proportion' column and save"),
                 strong("Step 2c"),
                 p("Upload completed population\nstructure worksheet"),
                 actionButton("completedTemplateUploadStructure",
                              "Upload completed template"))
               ),
             shinyglide::screen(
               strong("Step 3"),
               checkboxGroupInput("checkGroup_catchment",
                                  label = "Apply DREAMS catchment to:",
                                  choices = ""),
               checkboxInput("catchment",
                             "Apply DREAMS catchment modifier (DEV ONLY):",
                             value = FALSE)#,
               # conditionalPanel(
               #   condition = "input.catchment == 1",
               #   radioButtons("catchment_upordown",
               #                "Adjust denominator(s) up or numerator(s) down",
               #                c("Denominator up" = "dUp",
               #                  "Numerator down" = "nDown"))
               # )
             ),
             shinyglide::screen(
               strong("Step 4"),
               checkboxGroupInput("checkGroup_eligibility", 
                                  label = "Apply eligibility modifier to:", 
                                  choices = ""),
               checkboxInput("eligibility",
                             "Apply eligibility modifier (DEV ONLY):",
                             value = FALSE),
               conditionalPanel(
                 condition = "input.eligibility",
                 strong("Custom Structure step 4a"),
                 p("Download blank population\nstructure worksheet"),
                 downloadButton("blankTemplateDownloadEligibility",
                                "Download blank template"),
                 br(),
                 br(),
                 strong("Step 4b"),
                 p("Open worksheet in Excel, fill out 'eligibility_modifier' column and save"),
                 strong("Step 4c"),
                 p("Upload completed population\nstructure worksheet"),
                 actionButton("completedTemplateUploadEligibility",
                              "Upload completed template"))
             ),
             shinyglide::screen(
               strong("Step 5"),
               checkboxGroupInput("checkGroup_doubleCount",
                                  label = "Apply double count modifier to:",
                                  choices = ""),
               checkboxInput("doublecount",
                             "Apply double count modifier (DEV ONLY):",
                             value = FALSE),
               conditionalPanel(
                 condition = "input.doublecount",
                 strong("Double count modifier step 5a"),
                 p("Download blank count\nmodifier worksheet"),
                 downloadButton("blankTemplateDownloadDoubleCount",
                                "Download blank template"),
                 br(),
                 br(),
                 strong("Step 5b"),
                 p("Open worksheet in Excel, fill out 'doublecount_modifier' column and save"),
                 strong("Step 5c"),
                 p("Upload completed count\nmodifier worksheet"),
                 actionButton("completedTemplateUploadDoubleCount",
                              "Upload completed template"))
              ),
             shinyglide::screen(
               strong("Step 6"),
               br(),
               actionButton("export_token",
                            "Save Token Export"),
               br(),
               actionButton("export_COP",
                            "COP Export"),
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
             )
             
          )
    )
  ),
  fluidRow(
    column(5,
           h3("DREAMS Districts"),
           leafletOutput("map_main")),
    column(7,
           fluidRow(
             h3("2022 Figures for COP Export"),
             strong("NOTE: Exports will only include displayed content. Set to display all desired content before exporting."),
             DT::dataTableOutput("stats_COP")
           ),
           fluidRow(
             p("Parameters here")
           ))
  ),
  fluidRow(
    column(12,
           h3("Result Analytics")
           )
  ),
  fluidRow(
    column(12,
           strong("Pop Structure Type:"),
           textOutput("params_popStructureType"),
           strong("Catchment Flag:"),
           textOutput("params_catchmentModifierFlag"),
           strong("Enrollment Modifier Flag:"),
           textOutput("params_enrollmentModifierFlag"),
           strong("Double Grad Modifier Flag:"),
           textOutput("params_doubleGradModifierFlag"),
           DT::dataTableOutput("workingDataPost_check"))
  )
)


server <- function(input, output, session) {
  
  params <- reactiveValues(
    country = "Botswana",
    country_SF1 = botADM1.sf,
    country_SF2 = botADM2.sf, #Adapt to allow for Lesotho case
    popStructureType = "Default",
    catchmentModifierFlag = FALSE,
    enrollmentModifierFlag = 0,
    doubleGradModifierFlag = 0
  )
  
  workingDataPre <- reactiveValues(
    data = NULL
  )
  
  workingDataPost <- reactiveValues(
    data = NULL
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
  
  countryDataFiltered <- eventReactive(input$country, {
    #req(input$country)
    selected_data <- countryDataJoined %>%
      dplyr::filter(country == input$country)
  })
  
  data_stats_COP <- eventReactive(input$initializeSelection, {

    selected_data <- workingDataPost$data %>%
      reduceToCOPExport()
  })
  

  
  
  # Maps ----
  
  output$map_main <- leaflet::renderLeaflet({
    a <- leaflet() %>%
      #addTiles() %>%
      setMapWidgetStyle(list(background = "white"))
  })
  
  observeEvent(input$country, {
    
    if (input$country == "Botswana") {
      selected_country <- botADM1.sf
    } else if (input$country == "Kenya") {
      selected_country <- kenADM1.sf
    } else if (input$country == "Lesotho") {
      selected_country <- lesADM1.sf
    } else if (input$country == "Zimbabwe") {
      selected_country <- zimADM1.sf
    }
    
    if (input$country %in% small_countries) {
      selected_zoom <- 7
    } else if (input$country %in% medium_countries) {
      selected_zoom <- 6
    } else if (input$country %in% large_countries) {
      selected_zoom <- 5
    }
    
    
    leafletProxy("map_main",
                 data = selected_country) %>%
      clearShapes() %>%
      addPolygons(color = "#444444",
                  weight = 1,
                  opacity = 1,
                  fillOpacity = 0.8,
                  fillColor = "#FF6663",
                  highlightOptions = highlightOptions(color = "white",
                                                      weight = 3,
                                                      bringToFront = TRUE)) %>%
      setView(lng = mean(st_bbox(selected_country)[c(1,3)]),
              lat = mean(st_bbox(selected_country)[c(2,4)]),
              zoom = selected_zoom) %>%
      setMapWidgetStyle(list(background = "white")) %>%
      addFullscreenControl()
  })

  # Update parameters ----
  observeEvent(input$country, {
    params$country = input$country
  })

  
  
  #### PARAMS CHECK PANEL
  output$params_country <- renderText(params$country)
  output$params_popStructureType <- renderText(params$popStructureType)
  output$params_catchmentModifierFlag <- renderText(params$catchmentModifierFlag)
  output$params_enrollmentModifierFlag <- renderText(params$enrollmentModifierFlag)
  output$params_doubleGradModifierFlag <- renderText(params$doubleGradModifierFlag)
  
  #### PROCESSING BUTTONS
  
  observeEvent(input$useDefaultParameters, {
    req(countryDataFiltered())
    
    workingDataPre$data <- attachParameters_5year(countryDataFiltered(),
                                               dataParameters_5Year) %>%
      reshapeWide() %>%
      attachParameters_1year(dataParameters_1Year)
    
    
    workingDataPost$data <- workingDataPre$data %>%
      deriveStatistics()
    
  })
  
  
  observeEvent(input$catchment, {
    
    if (input$catchment){
      params$catchmentModifierFlag <- TRUE
    } else {
      params$catchmentModifierFlag <- FALSE
    } 
    
  })
  
  observeEvent(input$structure, {
    
    if (input$structure == "Default"){
      params$popStructureType <- "Default"
    } else if (input$structure == "National") {
      params$popStructureType <- "National"
    } else if (input$structure == "Custom") {
      params$popStructureType <- "Custom"
    }
    
  })
  
  popCatchmentChoice <- reactive(
    
    if (params$catchmentModifierFlag == TRUE) {
      "Expanded"
    } 
    else {
      "DistrictOnly"
    }
    
  )
  
  popStructureChoice <- reactive(
    
    params$popStructureType
    
  )
  
  observeEvent(input$initializeSelection, {
    req(workingDataPost)

    workingDataPost$data <- workingDataPost$data %>%
      mutate(
        IsSelected = case_when(
          ((PopStructure == popStructureChoice()) & populationtx == popCatchmentChoice()) ~ as.character("Selected"),
          TRUE ~ as.character("Unselected")
        )
      )
  })
  
  output$workingDataPost_check <- DT::renderDataTable({
    req(workingDataPost)
    
    datatable(workingDataPost$data,
              options = list(scrollx = TRUE)
    )
  })
  
  # Render tables
  
  output$stats_COP <- DT::renderDataTable({
    req(data_stats_COP())
    
    datatable(data_stats_COP(),
              rownames = FALSE,
              selection = 'none',
              extensions = 'Buttons', 
              options = list(
                dom = 'Blfrtip',
                buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                scrollx = TRUE)
              )
  })
  
  # Download handlers ----
  
  output$blankTemplateDownload <- downloadHandler(
    filename = function() {
      paste("blankTemplate", 
            "xlsx",
            sep = ".")
    },
    
    content = function(file) {
      file.copy('blankTemplate.xlsx', file)
    },
    contentType = NULL
  )
  
  
  
  
  
  
  
  
}

shinyApp(ui, server)