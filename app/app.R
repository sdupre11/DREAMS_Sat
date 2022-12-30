library(shiny)
library(leaflet)
library(leaflet.extras)
library(sf)
library(DT)
library(tidyverse)
library(shinyglide)
library(writexl)

source("functions.R")
source("params.R")
source("data_load.R")

# css <- "
# .container-fluid {
#   padding: 0 30px;
# }
# .shinyglide {
#   border: 1px solid #888;
#   box-shadow: 0px 0px 10px #888;
#   padding: 1em;
# }
# "

USG_USERS = c("Agency", "Interagency", "Global Agency", "Global") 
PARTNER_USERS = c("Global Partner", "Partner")


ui <- uiOutput("ui")


server <- function(input, output, session) {
  
  # User information for authentication ----
  user_input  <-  reactiveValues(authenticated = FALSE,
                                 status = "",
                                 d2_session = NULL,
                                 memo_authorized = FALSE)
  
  auth_ui <- function() {
    wellPanel(
      fluidRow(
        h4("Please login with your DATIM credentials:"),
        br()
      ),
      fluidRow(
        textInput("user_name", "Username: ", width = "500px"),
        passwordInput("password", "Password:", width = "500px"),
        actionButton("login_button", "Log in!")
      )
    )
  }
  
  main_ui <- function() {
    fluidPage(
      titlePanel(title = div(h1("Welcome to DREAMS Sat", style="margin: 0;"), 
                             h4('Saturation calculation application', style="margin: 0;")), 
                 windowTitle = "DREAMS Sat"),
      fluidRow(
        column(12,
               h3(textOutput("params_country")))
      ),
      fluidRow(
        column(12,
               glide(
                 # height = "600px",
                 shinyglide::screen(
                   strong("Step 0: Welcome"),
                   br(),
                   strong("Import Saved Parameters"),
                   br(),
                   strong("[returning teams]"),
                   br(),
                   br(),
                   fileInput("importToken",
                             "Import save token (.RData)",
                             accept = ".RData"),
                   actionButton("import_button",
                                "press me"),
                   br(),
                   br(),
                   textOutput("params_popStructureType"),
                   actionButton("useDefaultParameters",
                                "Use defaults"),
                   tableOutput("table_check_import")
                 ),
                 shinyglide::screen(
                   column(4,
                          strong("Step 1: Select Your Country"),
                          selectInput("country",
                                      "Country",
                                      selected = "Lesotho",
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
                                                  "Zimbabwe"))),
                   column(8,
                          h3("DREAMS Districts"),
                          leafletOutput("map_main"))
                 ),
                 shinyglide::screen(
                   strong("Step 2: Prevalence (Default: 2%) and Vulnerability (Default: 85%)"),
                   br(),
                   p("Use default values or upload custom values (highly recommended)"),
                   br(),
                   br(),
                   column(6,
                          strong("Prevalence"),
                          strong("Custom Structure step 2a"),
                          p("Download blank prevalence\nworksheet"),
                          downloadButton("blankTemplateDownloadPrevalence",
                                         "Download blank template"),
                          br(),
                          br(),
                          strong("Step 2b"),
                          p("Open worksheet in Excel, fill out 'Prevalence_20XX' columns and save"),
                          strong("Step 2c"),
                          p("Upload completed prevalence\n worksheet"),
                          fileInput("completedTemplateUploadPrevalence",
                                    "Upload completed template (.xlsx only)",
                                    accept = ".xlsx"),
                          DT::dataTableOutput("customPrevalenceTable"),
                          actionButton("confirmCustomPrevalence",
                                       "Confirm: use custom upload"),
                          actionButton("resetToDefaultPrevalence",
                                       "Reset: use default values")
                          ),
                   column(6,
                          strong("Vulnerability"),
                          strong("Custom Structure step 2d"),
                          p("Download blank vulnerability\nworksheet"),
                          downloadButton("blankTemplateDownloadVulnerability",
                                         "Download blank template"),
                          br(),
                          br(),
                          strong("Step 2e"),
                          p("Open worksheet in Excel, fill out 'Vulnerable_20XX' columns and save"),
                          strong("Step 2f"),
                          p("Upload completed vulnerability\nworksheet"),
                          fileInput("completedTemplateUploadVulnerability",
                                    "Upload completed template (.xlsx only)",
                                    accept = ".xlsx"),
                          DT::dataTableOutput("customVulnerabilityTable"),
                          actionButton("confirmCustomVulnerability",
                                       "Confirm: use custom upload"),
                          actionButton("resetToDefaultVulnerability",
                                       "Reset: use default values")
                   )
                 ),
                 shinyglide::screen(
                   column(4,
                          strong("Step 3: Population structure (Default: 20%)"),
                          radioButtons("structure",
                                       "Set Population Structure:",
                                       choices = c("Default (20%)" = "Default",
                                                   "Match National" = "National",
                                                   "Custom" = "Custom")),
                          conditionalPanel(
                            condition = "input.structure == 'Custom'",
                            strong("Custom Structure step 2a"),
                            p("Download blank population\nstructure worksheet"),
                            downloadButton("blankTemplateDownloadStructure",
                                           "Download blank template"),
                            br(),
                            br(),
                            strong("Step 3b"),
                            p("Open worksheet in Excel, fill out 'proportion' column and save"),
                            strong("Step 3c"),
                            p("Upload completed population\nstructure worksheet"),
                            fileInput("completedTemplateUploadPopStructure",
                                      "Upload completed template (.xlsx only)",
                                      accept = ".xlsx"),
                            actionButton("confirmCustomPopStructure",
                                         "Confirm: use custom upload")
                          )
                   ),
                   column(8,
                          fluidRow(
                            selectInput("popStructureYear",
                                        "Select a year:",
                                        selected = 2022,
                                        choices = c(2019,
                                                    2020,
                                                    2021,
                                                    2022)),
                            selectInput("popStructureDistrict",
                                        "Select a district (affects custom only):",
                                        selected = "",
                                        choices = ""
                            )),
                          fluidRow(
                            column(4,
                                   plotOutput("popStructure_DefaultPlot")),
                            column(4,
                                   plotOutput("popStructure_NationalPlot")),
                            column(4,
                                   plotOutput("popStructure_CustomPlot"))
                          ))
                 ),
                 shinyglide::screen(
                   fluidRow(
                     strong("Step 4: Catchment modifier (Default: No catchment modifier)")),
                   fluidRow(
                     column(4,
                            br(),
                            br(),
                            checkboxGroupInput("checkGroup_catchment",
                                               label = "Apply DREAMS catchment to:",
                                               choices = "")
                     ),
                     column(8,
                            leafletOutput("map_catchments")))
                 ),
                 shinyglide::screen(
                   strong("Step 5: Enrollment modifier (Default: 3%)"),
                   p("Use default value or upload a custom modifier structure"),
                   strong("Custom Structure step 4a"),
                   p("Download blank enrollment\nmodifier worksheet"),
                   downloadButton("blankTemplateDownloadEnrollment",
                                  "Download blank template"),
                   br(),
                   br(),
                   strong("Step 5b"),
                   p("Open worksheet in Excel, fill out 'Enrollment' columns and save"),
                   strong("Step 5c"),
                   p("Upload completed enrollment\nmodifier worksheet"),
                   DT::dataTableOutput("customEnrollmentTable"),
                   fileInput("completedTemplateUploadEnrollment",
                             "Upload completed template (.xlsx only)",
                             accept = ".xlsx"),
                   actionButton("confirmCustomEnrollment",
                                "Confirm: use custom upload"),
                   actionButton("resetToDefaultEnrollment",
                                "Reset: use default modifier")
                 ),
                 shinyglide::screen(
                   strong("Step 6: Double count modifier (Default: 1%)"),
                   p("Use default value or upload a custom structure"),
                   strong("Custom Structure step 5a"),
                   p("Download blank population\nstructure worksheet"),
                   downloadButton("blankTemplateDownloadDoubleCount",
                                  "Download blank template"),
                   br(),
                   br(),
                   strong("Step 6b"),
                   p("Open worksheet in Excel, fill out 'PrimarySecondaryDoubleCounts_20XX' columns and save"),
                   strong("Step 6c"),
                   p("Upload completed double count\nmodifier worksheet"),
                   DT::dataTableOutput("customPSDCTable"),
                   fileInput("completedTemplateUploadDoubleCount",
                             "Upload completed template (.xlsx only)",
                             accept = ".xlsx"),
                   actionButton("confirmCustomDoubleCount",
                                "Confirm: use custom upload"),
                   actionButton("resetToDefaultDoubleCount",
                                "Reset: use default modifier")
                 ),
                 shinyglide::screen(
                   strong("Step 7"),
                   br(),
                   downloadButton("exportToken",
                                  "Export save token"),
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
        column(12,
               fluidRow(
                 h3("2022 Figures for COP Export"),
                 strong("NOTE: Exports will only include displayed content. Set to display all desired content before exporting."),
                 br(),
                 br(),
                 actionButton("initializeSelection",
                              "Accept parameters and derive/re-derive COP statistics"),
                 br(),
                 br(),
                 DT::dataTableOutput("stats_COP")
               ),
               fluidRow(
                 p("Parameters here")
               ))
      ),
      fluidRow(
        column(3,
               h3("Saturation Analytics"),
               strong("Select a focus"),
               actionButton("focusSaturation",
                            "Saturation"),
               actionButton("focusNumerator",
                            "Numerator"),
               actionButton("focusDenominator",
                            "Denominator")
               ),
        column(9,
               #DT::dataTableOutput("stats_analytics"),
               selectInput("analyticsDistrict",
                           "Select a district:",
                           selected = "",
                           choices = ""
               ),
               uiOutput("incomplete_ui"),
               uiOutput("saturation_ui"),
               uiOutput("numerator_ui"),
               uiOutput("denominator_ui")
        ),
      fluidRow(
        column(12,
               DT::dataTableOutput("workingDataPost_check")))
      )
    )
  }
  
  output$ui <- renderUI({
    main_ui()
  })
  
  # output$ui <- renderUI({
  #   if(!user_input$authenticated){
  #     auth_ui()#main_ui()#
  #   }else{
  #     main_ui()      
  #   }
  # })
  
  output$incomplete_ui <- renderUI({
    req(params$focusedAnalytic == "Incomplete")
    
    div(
      h4("Complete saturation calculation first"))
    
  })
  
  output$saturation_ui <- renderUI({
    req(params$focusedAnalytic == "Saturation")
    req(data_stats_COP())
   
     div(
      h4("Saturation"),
      p("Mapped sat here"))
    
  })
  
  output$numerator_ui <- renderUI({
    req(params$focusedAnalytic == "Numerator")
    req(data_stats_COP())
    
    div(
      h4("Numerator"),
      plotOutput("impactOfParametersPlot"))
    
  })
  
  output$denominator_ui <- renderUI({
    req(params$focusedAnalytic == "Denominator")
    req(data_stats_COP())
    
    div(
      h4("Denominator"))
    
  })
  
  
  # Setup reactiveValues objects ----
  params <- reactiveValues(
    country = "Botswana",
    popStructureType = "Default",
    catchmentsSelected = character(0),
    focusedAnalytic = "Incomplete"
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
    
    return(a)
  })
  
  # Update elements ----
  ## Run updateSelectInput() functions ----
  observeEvent(input$importToken, {
    updateSelectInput(session,
                      "country",
                      selected = importedToken()$country[1])

    updateCheckboxGroupInput(session,
                             "checkGroup_catchment",
                             selected = importedToken()$catchmentsSelected)
    
    updateRadioButtons(session,
                       "structure",
                       selected = importedToken()$popStructure[1])
    
    
    #Export the existing pre/post data entirely and update it?
    
    #Export analytics button choice
    
  })
  
  observeEvent(input$country, {
    updateSelectInput(session,
                      "popStructureDistrict",
                      choices = districts())
  })
  
  observeEvent(input$country, {
    updateSelectInput(session,
                      "analyticsDistrict",
                      choices = districts())
  })
  
  observeEvent(input$country, {
    updateCheckboxGroupInput(session, 
                             "checkGroup_catchment", 
                             choices = districts())
  })
  
  countryDataFiltered <- eventReactive(input$country, {
    #req(input$country)
    selected_data <- countryDataJoined %>%
      dplyr::filter(country == input$country)
  })
  
  ## Update parameters ----
  observeEvent(input$country, {
    params$country = input$country
  })
  
  observeEvent(input$checkGroup_catchment, {
    params$catchmentsSelected = input$checkGroup_catchment
  })
  
  observeEvent(input$focusSaturation, {
    req(data_stats_COP())
    params$focusedAnalytic <- "Saturation"
  })
  
  observeEvent(input$focusNumerator, {
    req(data_stats_COP())
    params$focusedAnalytic <- "Numerator"
  })
  
  observeEvent(input$focusDenominator, {
    req(data_stats_COP())
    params$focusedAnalytic <- "Denominator"
  })
  
  # Render text elements ----
  ## Setup country label and params check panel ----
  output$params_country <- renderText(params$country)
  output$params_popStructureType <- renderText(params$popStructureType)
  
  # Processing buttons ----
  observeEvent(input$useDefaultParameters, {
    req(countryDataFiltered())
    
    workingDataPre$data <- attachParameters_5year(countryDataFiltered(),
                                                  dataParameters_5Year) %>%
      reshapeWide() %>%
      attachParameters_1year(dataParameters_1Year) %>%
      merge(SingleYearNatAGYWPops)
    
    workingDataPost$data <- workingDataPre$data %>%
      deriveStatistics()
    
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
  
  popStructureChoice <- reactive(
    
    params$popStructureType
    
  )
  
  catchmentDistricts <- reactive({
    a = input$checkGroup_catchment
    return(a)
  })
  
  observeEvent(input$initializeSelection, {
    req(!is.null(workingDataPost$data))
    
    workingDataPost$data <- workingDataPost$data %>%
      mutate(
        IsSelected = case_when(
          ((PopStructure == popStructureChoice()) & (AREA_NAME %in% catchmentDistricts()) & (populationtx == "Expanded")) ~ as.character("Selected"),
          ((PopStructure == popStructureChoice()) & (!(AREA_NAME %in% catchmentDistricts())) & (populationtx == "DistrictOnly")) ~ as.character("Selected"),
          TRUE ~ as.character("Unselected")
        )
      )
  })
  
  data_stats_COP <- eventReactive(input$initializeSelection, {
    req(!is.null(workingDataPost$data))
    
    selected_data <- workingDataPost$data %>%
      reduceToCOPExport()
  })
  

  data_stats_analytics <- eventReactive(input$initializeSelection, {
    req(!is.null(workingDataPost$data))

    selected_data <- workingDataPost$data %>%
      reduceForAnalyticsPlots() 
  })
  
  observeEvent(input$confirmCustomPrevalence, {
    req(customPrevalence())
    req(workingDataPre$data)
    
    workingDataPre$data <- left_join(workingDataPre$data,
                                     customPrevalence(),
                                     by = c("country" = "Country", 
                                            "AREA_NAME" = "District", 
                                            "ageasentered" = "AgeCohort"))
    
    workingDataPre$data$Prev_2019 <- workingDataPre$data$Prevalence_2019_Custom
    workingDataPre$data$Prev_2020 <- workingDataPre$data$Prevalence_2020_Custom
    workingDataPre$data$Prev_2021 <- workingDataPre$data$Prevalence_2021_Custom
    workingDataPre$data$Prev_2022 <- workingDataPre$data$Prevalence_2022_Custom
    
    workingDataPost$data <- workingDataPre$data %>%
      deriveStatistics()
    
  })
  
  observeEvent(input$resetToDefaultPrevalence, {
    req(workingDataPre$data)
    
    workingDataPre$data$Prev_2019 <- 2
    workingDataPre$data$Prev_2020 <- 2
    workingDataPre$data$Prev_2021 <- 2
    workingDataPre$data$Prev_2022 <- 2
    
    workingDataPost$data <- workingDataPre$data %>%
      deriveStatistics()
    
  })
  
  observeEvent(input$confirmCustomVulnerability, {
    req(customVulnerability())
    req(workingDataPre$data)
    
    workingDataPre$data <- left_join(workingDataPre$data,
                                     customVulnerability(),
                                     by = c("country" = "Country", 
                                            "AREA_NAME" = "District", 
                                            "ageasentered" = "AgeCohort"))
    
    workingDataPre$data$Vuln_2019 <- workingDataPre$data$Vulnerable_2019_Custom
    workingDataPre$data$Vuln_2020 <- workingDataPre$data$Vulnerable_2020_Custom
    workingDataPre$data$Vuln_2021 <- workingDataPre$data$Vulnerable_2021_Custom
    workingDataPre$data$Vuln_2022 <- workingDataPre$data$Vulnerable_2022_Custom
    
    workingDataPost$data <- workingDataPre$data %>%
      deriveStatistics()
    
  })
  
  observeEvent(input$resetToDefaultVulnerability, {
    req(workingDataPre$data)
    
    workingDataPre$data$Vuln_2019 <- 85
    workingDataPre$data$Vuln_2020 <- 85
    workingDataPre$data$Vuln_2021 <- 85
    workingDataPre$data$Vuln_2022 <- 85
    
    workingDataPost$data <- workingDataPre$data %>%
      deriveStatistics()
    
  })
  
  observeEvent(input$confirmCustomEnrollment, {
    req(customEnrollment())
    req(workingDataPre$data)
    
    workingDataPre$data <- left_join(workingDataPre$data,
                                     customEnrollment(),
                                     by = c("country" = "Country", 
                                            "AREA_NAME" = "District", 
                                            "ageasentered" = "AgeCohort"))
    
    workingDataPre$data$Enrollment_2019 <- workingDataPre$data$Enrollment_2019_Custom
    workingDataPre$data$Enrollment_2020 <- workingDataPre$data$Enrollment_2020_Custom
    workingDataPre$data$Enrollment_2021 <- workingDataPre$data$Enrollment_2021_Custom
    workingDataPre$data$Enrollment_2022 <- workingDataPre$data$Enrollment_2022_Custom
    
    workingDataPost$data <- workingDataPre$data %>%
      deriveStatistics()
    
  })
  
  observeEvent(input$resetToDefaultEnrollment, {
    req(workingDataPre$data)
    
    workingDataPre$data$Enrollment_2019 <- 3
    workingDataPre$data$Enrollment_2020 <- 3
    workingDataPre$data$Enrollment_2021 <- 3
    workingDataPre$data$Enrollment_2022 <- 3
    
    workingDataPost$data <- workingDataPre$data %>%
      deriveStatistics()
    
  })
  
  observeEvent(input$confirmCustomDoubleCount, {
    req(customDoubleCount())
    req(workingDataPre$data)
    
    workingDataPre$data <- left_join(workingDataPre$data,
                                     customDoubleCount(),
                                     by = c("country" = "Country", 
                                            "AREA_NAME" = "District", 
                                            "ageasentered" = "AgeCohort"))
    
    workingDataPre$data$PSDC_2019 <- workingDataPre$data$PSDC_2019_Custom
    workingDataPre$data$PSDC_2020 <- workingDataPre$data$PSDC_2020_Custom
    workingDataPre$data$PSDC_2021 <- workingDataPre$data$PSDC_2021_Custom
    workingDataPre$data$PSDC_2022 <- workingDataPre$data$PSDC_2022_Custom
    
    workingDataPost$data <- workingDataPre$data %>%
      deriveStatistics()
    
  })
  
  observeEvent(input$resetToDefaultDoubleCount, {
    req(workingDataPre$data)
    
    workingDataPre$data$PSDC_2019 <- 1
    workingDataPre$data$PSDC_2020 <- 1
    workingDataPre$data$PSDC_2021 <- 1
    workingDataPre$data$PSDC_2022 <- 1
    
    workingDataPost$data <- workingDataPre$data %>%
      deriveStatistics()
    
  })
  
  observeEvent(input$confirmCustomPopStructure, {
    req(customPopStructure())
    req(workingDataPre$data)
    
    workingDataPre$data <- left_join(workingDataPre$data,
                                     customPopStructure(),
                                     by = c("country" = "Country",
                                            "AREA_NAME" = "District",
                                            "ageasentered" = "ageasentered"))
    
    workingDataPre$data$firstQ_2019 <- workingDataPre$data$firstQCustom_2019
    workingDataPre$data$firstQ_2020 <- workingDataPre$data$firstQCustom_2020
    workingDataPre$data$firstQ_2021 <- workingDataPre$data$firstQCustom_2021
    workingDataPre$data$firstQ_2022 <- workingDataPre$data$firstQCustom_2022
    workingDataPre$data$secondQ_2019 <- workingDataPre$data$secondQCustom_2019
    workingDataPre$data$secondQ_2020 <- workingDataPre$data$secondQCustom_2020
    workingDataPre$data$secondQ_2021 <- workingDataPre$data$secondQCustom_2021
    workingDataPre$data$secondQ_2022 <- workingDataPre$data$secondQCustom_2022
    workingDataPre$data$thirdQ_2019 <- workingDataPre$data$thirdQCustom_2019
    workingDataPre$data$thirdQ_2020 <- workingDataPre$data$thirdQCustom_2020
    workingDataPre$data$thirdQ_2021 <- workingDataPre$data$thirdQCustom_2021
    workingDataPre$data$thirdQ_2022 <- workingDataPre$data$thirdQCustom_2022
    workingDataPre$data$fourthQ_2019 <- workingDataPre$data$fourthQCustom_2019
    workingDataPre$data$fourthQ_2020 <- workingDataPre$data$fourthQCustom_2020
    workingDataPre$data$fourthQ_2021 <- workingDataPre$data$fourthQCustom_2021
    workingDataPre$data$fourthQ_2022 <- workingDataPre$data$fourthQCustom_2022
    workingDataPre$data$fifthQ_2019 <- workingDataPre$data$fifthQCustom_2019
    workingDataPre$data$fifthQ_2020 <- workingDataPre$data$fifthQCustom_2020
    workingDataPre$data$fifthQ_2021 <- workingDataPre$data$fifthQCustom_2021
    workingDataPre$data$fifthQ_2022 <- workingDataPre$data$fifthQCustom_2022
    
    workingDataPre$data <- workingDataPre$data %>%
      select(-c(firstQCustom_2019,
                firstQCustom_2020,
                firstQCustom_2021,
                firstQCustom_2022,
                secondQCustom_2019,
                secondQCustom_2020,
                secondQCustom_2021,
                secondQCustom_2022,
                thirdQCustom_2019,
                thirdQCustom_2020,
                thirdQCustom_2021,
                thirdQCustom_2022,
                fourthQCustom_2019,
                fourthQCustom_2020,
                fourthQCustom_2021,
                fourthQCustom_2022,
                fifthQCustom_2019,
                fifthQCustom_2020,
                fifthQCustom_2021,
                fifthQCustom_2022))
    
    workingDataPost$data <- workingDataPre$data %>%
      deriveStatistics()
    
  })
  
  # Render table elements ----
  output$workingDataPost_check <- DT::renderDataTable({
    req(workingDataPost)
    
    datatable(workingDataPost$data,
              options = list(scrollx = TRUE)
    )
  })
  
  output$customPrevalenceTable <- DT::renderDataTable({
    req(customPrevalence())
    
    datatable(customPrevalence(),
              rownames = FALSE,
              selection = 'none',
              options = list(
                dom = 'Blfrtip',
                scrollx = TRUE)
    )
  })
  
  output$customVulnerabilityTable <- DT::renderDataTable({
    req(customVulnerability())
    
    datatable(customVulnerability(),
              rownames = FALSE,
              selection = 'none',
              options = list(
                dom = 'Blfrtip',
                scrollx = TRUE)
    )
  })
  
  output$customEnrollmentTable <- DT::renderDataTable({
    req(customEnrollment())

    datatable(customEnrollment(),
              rownames = FALSE,
              selection = 'none',
              options = list(
                dom = 'Blfrtip',
                scrollx = TRUE)
    )
  })
  
  output$customPSDCTable <- DT::renderDataTable({
    req(customDoubleCount())
    
    datatable(customDoubleCount(),
              rownames = FALSE,
              selection = 'none',
              options = list(
                dom = 'Blfrtip',
                scrollx = TRUE)
    )
  })
  
  output$stats_COP <- DT::renderDataTable({
    req(data_stats_COP())
    
    datatable(data_stats_COP(),
              rownames = FALSE,
              selection = 'none',
              extensions = 'Buttons', 
              options = list(
                dom = 'Blfrtip',
                buttons = c('excel', 'pdf'),
                scrollx = TRUE)
    )
  })
  
  output$stats_analytics <- DT::renderDataTable({
    req(data_stats_analytics())
    
    datatable(data_stats_analytics())
    
  })
  
  
  # Render plot elements ----
  ## Pop structure plots ----
  ### Default structure plot ----
  output$popStructure_DefaultPlot <- renderPlot({
    
    age <- c(10:29)
    prop <- rep(20, 20)
    sequence <- c("10-14", "15-19", "20-24", "25-29")
    ageasentered <- rep(sequence, each = 5)
    
    defaultDF <- data.frame(age, 
                            prop,
                            ageasentered)
    
    # make so can select a District for Custom plots
    a <- defaultDF %>%
      ggplot(
        aes(
          x = age,
          y = prop,
          fill = ageasentered)) + 
      geom_bar(stat="identity") +
      coord_flip() +
      scale_fill_manual(
        values = alpha(c("10-14" = "#FF6663",
                         "15-19" = "#FFBA49",
                         "20-24" = "#FF6663",
                         "25-29" = "#FFBA49"),
                       .8)
      ) +
      ggtitle("Default",
              subtitle = "Even 20%") +
      xlab("Age band") +
      ylab("Proportion of cohort") +
      lims(y = c(0, 30)) +
      theme_plot(legend.position = "none")
    
    return(a)
  })
  
  ### National structure plot ----
  output$popStructure_NationalPlot <- renderPlot({
    
    natDF <- SingleYearNatAGYWPops %>%
      prepQDataforPopStructurePlots() %>%
      filter(fiscal_year==input$popStructureYear & country == input$country)
    
    a <- natDF %>%
      ggplot(
        aes(
          x = age,
          y = prop,
          fill = ageasentered)) +
      geom_bar(stat="identity") +
      coord_flip() +
      scale_fill_manual(
        values = alpha(c("10-14" = "#FF6663",
                         "15-19" = "#FFBA49",
                         "20-24" = "#FF6663",
                         "25-29" = "#FFBA49"),
                       .8)
      ) +
      ggtitle("National",
              subtitle = "Match national") +
      xlab("") +
      ylab("") +
      lims(y = c(0, 30)) +
      theme_plot(legend.position = "none")
    
    return(a)
  })
  
  ### Custom structure plot ----
  output$popStructure_CustomPlot <- renderPlot({
    
    custDF <- customPopStructure() %>%
      prepQDataforPopStructurePlots() %>%
      filter(fiscal_year==input$popStructureYear & Country == input$country & District == input$popStructureDistrict)
    
    a <- custDF %>%
      ggplot(
        aes(
          x = age,
          y = prop,
          fill = ageasentered)) +
      geom_bar(stat="identity") +
      coord_flip() +
      scale_fill_manual(
        values = alpha(c("10-14" = "#FF6663",
                         "15-19" = "#FFBA49",
                         "20-24" = "#FF6663",
                         "25-29" = "#FFBA49"),
                       .8)
      ) +
      ggtitle("Custom",
              subtitle = "Uploaded") +
      xlab("") +
      ylab("") +
      lims(y = c(0, 30)) +
      theme_plot(legend.position = "none")
    
    return(a)
  })
  
  ## Analytics plots ----
  ### Quasi-waterfall plot ----
  output$impactOfParametersPlot <- renderPlot({
    req(data_stats_analytics())
    
    internalDF <- data_stats_analytics() %>%
      dplyr::filter(IsSelected == "Selected" & AREA_NAME == input$analyticsDistrict) %>%
      dplyr::select(c("ageasentered",
                      "AREA_NAME",
                      "Actual_Served_2022",
                      "AGYW_PREV_Sum",
                      "DeDuplicatedAGYW_PREV_Sum",
                      "EnrollmentStandardizedAGYW_PREV_Sum"
                      )) %>%
      tidyr::pivot_longer(
        cols = Actual_Served_2022:EnrollmentStandardizedAGYW_PREV_Sum,
        names_to = "Stage",
        values_to = "AGYW_PREV"
      )
      
    a <- internalDF %>%
      ggplot(aes(x = Stage, y = AGYW_PREV)) +
      geom_bar(stat = 'identity') +
      facet_wrap(~ageasentered)
    

    return(a)
    
  })
  
  
  # Render map elements ----
  ## Main map ----
  output$map_main <- leaflet::renderLeaflet({
    a <- leaflet() %>%
      setMapWidgetStyle(list(background = "white"))
    # addResetMapButton() #currently doesn't work correctly, figure out how to set to go to the new polygons
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

    selected_country_DREAMS <- selected_country %>%
      filter(DREAMSDistrict == "Yes")
    
    selected_country_NonDREAMS <- selected_country %>%
      filter(DREAMSDistrict == "No")
    
    if (input$country %in% small_countries) {
      selected_zoom <- 7
    } else if (input$country %in% medium_countries) {
      selected_zoom <- 6
    } else if (input$country %in% large_countries) {
      selected_zoom <- 5
    }
    
    popup_DREAMS <- paste0("<strong>DREAMS District: </strong>", 
                           selected_country_DREAMS$AREA_NAME)
    
    popup_NonDREAMS <- paste0("<strong>Non-DREAMS District: </strong>", 
                              selected_country_NonDREAMS$AREA_NAME)
    
    
    leafletProxy("map_main"
    ) %>%
      clearShapes() %>%
      clearControls() %>%
      addPolygons(data = selected_country_DREAMS,
                  color = "black",
                  fillColor = "#FF6663",
                  weight = 1,
                  opacity = 1,
                  fillOpacity = 0.8,
                  popup = popup_DREAMS,
                  highlightOptions = highlightOptions(color = "white",
                                                      weight = 2,
                                                      bringToFront = TRUE)) %>%
      addPolygons(data = selected_country_NonDREAMS,
                  color = "black",
                  fillColor = "white",
                  weight = 1,
                  opacity = 1,
                  fillOpacity = 0.8,
                  popup = popup_NonDREAMS) %>%
      setView(lng = mean(st_bbox(selected_country)[c(1,3)]),
              lat = mean(st_bbox(selected_country)[c(2,4)]),
              zoom = selected_zoom) %>%
      setMapWidgetStyle(list(background = "white"))
  })
  
  ## Catchment map ----
  output$map_catchments <- leaflet::renderLeaflet({
    a <- leaflet() %>%
      setMapWidgetStyle(list(background = "white")) %>%
      # addResetMapButton() %>% #currently doesn't work correctly, figure out how to set to go to the new polygons
      addFullscreenControl()
  })

  catchments_filtered <- reactive(
    a <- neighborsLookup[neighborsLookup$parent %in% input$checkGroup_catchment, ])
# 
#   catchmentMapListener <- reactive({
# 
#     list(input$country,
#          catchments_filtered())
#   })

  # observeEvent(catchmentMapListener(), {
  # # observeEvent(c(input$country,
  # #                catchments_filtered()), {
  # 
  #   if (input$country == "Botswana") {
  #     selected_country <- botADM1.sf
  #   } else if (input$country == "Kenya") {
  #     selected_country <- kenADM1.sf
  #   } else if (input$country == "Lesotho") {
  #     selected_country <- lesADM1.sf
  #   } else if (input$country == "Zimbabwe") {
  #     selected_country <- zimADM1.sf
  #   }
  # 
  #   selected_country <- botADM1.sf
  # 
  #   selected_country_DREAMS <- selected_country %>%
  #     filter(DREAMSDistrict == "Yes")
  # 
  #   selected_country_DREAMSNeighbors <- selected_country %>%
  #     filter(ADM1_NAME %in% catchments_filtered()$child)
  # 
  #   selected_country_NonDREAMS <- selected_country %>%
  #     filter(DREAMSDistrict == "No" & (!ADM1_NAME %in% catchments_filtered()$child))
  # 
  #   if (input$country %in% small_countries) {
  #     selected_zoom <- 7
  #   } else if (input$country %in% medium_countries) {
  #     selected_zoom <- 6
  #   } else if (input$country %in% large_countries) {
  #     selected_zoom <- 5
  #   }
  # 
  #   popup_DREAMS <- paste0("<strong>DREAMS District: </strong>",
  #                          selected_country_DREAMS$AREA_NAME)
  # 
  #   popup_DREAMSNeighbors <- paste0("<strong>DREAMS Neighbor District: </strong>",
  #                                   selected_country_DREAMSNeighbors$AREA_NAME)
  # 
  #   popup_NonDREAMS <- paste0("<strong>Non-DREAMS District: </strong>",
  #                             selected_country_NonDREAMS$AREA_NAME)
  # 
  # 
  #   leafletProxy("map_catchments") %>%
  #     clearShapes() %>%
  #     clearControls() %>%
  #     addPolygons(data = selected_country_DREAMS,
  #                 color = "black",
  #                 fillColor = "#FF6663",
  #                 weight = 1,
  #                 opacity = 1,
  #                 fillOpacity = 0.8,
  #                 popup = popup_DREAMS,
  #                 highlightOptions = highlightOptions(color = "white",
  #                                                     weight = 2,
  #                                                     bringToFront = TRUE)) %>%
  #     addPolygons(data = selected_country_DREAMSNeighbors,
  #                 color = "black",
  #                 fillColor = "#20A39E",
  #                 weight = 1,
  #                 opacity = 1,
  #                 fillOpacity = 0.8,
  #                 popup = popup_DREAMSNeighbors,
  #                 highlightOptions = highlightOptions(color = "white",
  #                                                     weight = 2,
  #                                                     bringToFront = TRUE)) %>%
  #     addPolygons(data = selected_country_NonDREAMS,
  #                 color = "black",
  #                 fillColor = "white",
  #                 weight = 1,
  #                 opacity = 1,
  #                 fillOpacity = 0.8,
  #                 popup = popup_NonDREAMS) %>%
  #     setView(lng = mean(st_bbox(selected_country)[c(1,3)]),
  #             lat = mean(st_bbox(selected_country)[c(2,4)]),
  #             zoom = selected_zoom) %>%
  #     setMapWidgetStyle(list(background = "white"))
  # })

  # Save token ----
  ## Export ----
  exportCountryListener <- reactive({
    params$country
  })
  
  exportPopStructureListener <- reactive({
    params$popStructureType
  })
  
  exportCatchmentsListener <- reactive({
    params$catchmentsSelected
  })
  
  exportAnalyticListener <- reactive({
    params$focusedAnalytic
  })
  
  output$exportToken <- downloadHandler(
    filename = function() {
      paste("DREAMS_Sat_Save_Token",
            ".Rdata",
            sep = "")
    },
    content = function(file) {
      
      params_df <- data.frame(
        country = exportCountryListener(),
        popStructureType = exportPopStructureListener(),
        catchmentsSelected = exportCatchmentsListener(),
        focusedAnalytic = exportAnalyticListener())
      
      save(params_df, file = file)
    },
    
    contentType = NULL
  )
  
  ## Import ----
  
  importedToken <- eventReactive(input$importToken, {
    if ( is.null(input$importToken)) return(NULL)
    
    load(input$importToken$datapath)
    
    a <- params_df

    return(a)
    
  })
  
  observeEvent(input$import_button, {
    req(importedToken())

    print(importedToken()$catchmentsSelected)
    print(importedToken()$focusedAnalytic)
    print(params$catchmentsSelected)
    print(params$focusedAnalytic)
    print(importedToken()$popStructureType[1])
    print(importedToken()$country[1])
    print(params$popStructureType)
    print(params$country)
    
    print(class(importedToken()$country[1]))
    print(class(importedToken()$popStructureType))
    print(class(importedToken()$catchmentsSelected))
    print(class(importedToken()$focusedAnalytic))

  })
  
  output$table_check_import <- renderTable({
    req(importedToken())

    importedToken()
  })
  
  observeEvent(importedToken(), {
    params$country <- importedToken()$country[1]
    params$popStructureType <- importedToken()$popStructureType[1]
    params$catchmentsSelected <- importedToken()$catchmentsSelected
    params$focusedAnalytic <- importedToken()$focusedAnalytic[1]

  })
  
  # DownloadHandlers ----
  ## Downloads ----
  ### Prevalence ----
  default5YearTemplate_selectedCountry_P <- reactive({
    req(input$country)
    
    a <- default5YearTemplate %>%
      dplyr::filter(Country == input$country) %>%
      dplyr::select(c("Country",
                      "District",
                      "AgeCohort",
                      "Prevalence_2019",
                      "Prevalence_2020",
                      "Prevalence_2021",
                      "Prevalence_2022"))
    
    return(a)
    
  })
  
  output$blankTemplateDownloadPrevalence <- downloadHandler(
    filename = function() {
      paste("blankPrevalenceTemplate", 
            ".xlsx",
            sep = "")
    },
    
    content = function(file) {
      write_xlsx(default5YearTemplate_selectedCountry_P(), 
                 path = file)
    },
    contentType = NULL
  )
  
  
  ### Vulnerability ----
  default5YearTemplate_selectedCountry_V <- reactive({
    req(input$country)
    
    a <- default5YearTemplate %>%
      dplyr::filter(Country == input$country) %>%
      dplyr::select(c("Country",
                      "District",
                      "AgeCohort",
                      "Vulnerable_2019",
                      "Vulnerable_2020",
                      "Vulnerable_2021",
                      "Vulnerable_2022"))
    
    return(a)
    
  })
  
  output$blankTemplateDownloadVulnerability <- downloadHandler(
    filename = function() {
      paste("blankVulnerabilityTemplate", 
            ".xlsx",
            sep = "")
    },
    
    content = function(file) {
      write_xlsx(default5YearTemplate_selectedCountry_V(), 
                 path = file)
    },
    contentType = NULL
  )
  
  
  ### Population structure ----
  default1YearTemplate_selectedCountry <- reactive({
    req(input$country)
    
    a <- default1YearTemplate %>%
      filter(Country == input$country)
    
    return(a)
    
  })
  
  output$blankTemplateDownloadStructure <- downloadHandler(
    filename = function() {
      paste("blankStructureTemplate", 
            ".xlsx",
            sep = "")
    },
    
    content = function(file) {
      write_xlsx(default1YearTemplate_selectedCountry(), 
                 path = file)
    },
    contentType = NULL
  )
  
  ### Enrollment ----
  default5YearTemplate_selectedCountry_E <- reactive({
    req(input$country)
    
    a <- default5YearTemplate %>%
      dplyr::filter(Country == input$country) %>%
      dplyr::select(c("Country",
                      "District",
                      "AgeCohort",
                      "Enrollment_2019",
                      "Enrollment_2020",
                      "Enrollment_2021",
                      "Enrollment_2022"))
    
    return(a)
    
  })
  
  output$blankTemplateDownloadEnrollment <- downloadHandler(
    filename = function() {
      paste("blankEnrollmentTemplate", 
            ".xlsx",
            sep = "")
    },
    
    content = function(file) {
      write_xlsx(default5YearTemplate_selectedCountry_E(), 
                 path = file)
    },
    contentType = NULL
  )
  
  ### Primary/Secondary Double Count ----
  default5YearTemplate_selectedCountry_DC <- reactive({
    req(input$country)
    
    a <- default5YearTemplate %>%
      dplyr::filter(Country == input$country) %>%
      dplyr::select(c("Country",
                      "District",
                      "AgeCohort",
                      "PrimarySecondaryDoubleCounts_2019",
                      "PrimarySecondaryDoubleCounts_2020",
                      "PrimarySecondaryDoubleCounts_2021",
                      "PrimarySecondaryDoubleCounts_2022"))
    
    return(a)
    
  })
  
  output$blankTemplateDownloadDoubleCount <- downloadHandler(
    filename = function() {
      paste("blankDoubleCountTemplate", 
            ".xlsx",
            sep = "")
    },
    
    content = function(file) {
      write_xlsx(default5YearTemplate_selectedCountry_DC(), 
                 path = file)
    },
    contentType = NULL
  )
  
  ## Uploads ----
  ### Prevalence ----
  customPrevalence <- reactive({
    req(input$completedTemplateUploadPrevalence)
    
    file <- input$completedTemplateUploadPrevalence
    ext <- tools::file_ext(file$datapath)
    
    validate(need(ext == "xlsx",
                  "Please upload xlsx file"))
    
    a <- readxl::read_xlsx(file$datapath) %>%
      rename(Prevalence_2019_Custom = Prevalence_2019,
             Prevalence_2020_Custom = Prevalence_2020,
             Prevalence_2021_Custom = Prevalence_2021,
             Prevalence_2022_Custom = Prevalence_2022)
    
    return(a)
  })
  
  ### Vulnerability ----
  customVulnerability <- reactive({
    req(input$completedTemplateUploadVulnerability)
    
    file <- input$completedTemplateUploadVulnerability
    ext <- tools::file_ext(file$datapath)
    
    validate(need(ext == "xlsx",
                  "Please upload xlsx file"))
    
    a <- readxl::read_xlsx(file$datapath) %>%
      rename(Vulnerable_2019_Custom = Vulnerable_2019,
             Vulnerable_2020_Custom = Vulnerable_2020,
             Vulnerable_2021_Custom = Vulnerable_2021,
             Vulnerable_2022_Custom = Vulnerable_2022)
    
    return(a)
  })
  
  ### Population structure ----
  customPopStructure <- reactive({
    req(input$completedTemplateUploadPopStructure)
    
    file <- input$completedTemplateUploadPopStructure
    ext <- tools::file_ext(file$datapath)
    
    validate(need(ext == "xlsx",
                  "Please upload xlsx file"))
    
    a <- file$datapath %>%
      dataParametersImportandMutate() %>%
      dataParametersPivot1Year() %>%
      rename(firstQCustom_2019 = firstQ_2019,
             firstQCustom_2020 = firstQ_2020,
             firstQCustom_2021 = firstQ_2021,
             firstQCustom_2022 = firstQ_2022,
             secondQCustom_2019 = secondQ_2019,
             secondQCustom_2020 = secondQ_2020,
             secondQCustom_2021 = secondQ_2021,
             secondQCustom_2022 = secondQ_2022,
             thirdQCustom_2019 = thirdQ_2019,
             thirdQCustom_2020 = thirdQ_2020,
             thirdQCustom_2021 = thirdQ_2021,
             thirdQCustom_2022 = thirdQ_2022,
             fourthQCustom_2019 = fourthQ_2019,
             fourthQCustom_2020 = fourthQ_2020,
             fourthQCustom_2021 = fourthQ_2021,
             fourthQCustom_2022 = fourthQ_2022,
             fifthQCustom_2019 = fifthQ_2019,
             fifthQCustom_2020 = fifthQ_2020,
             fifthQCustom_2021 = fifthQ_2021,
             fifthQCustom_2022 = fifthQ_2022)
    
    
    return(a)
  })
  
  ### Enrollment ----
  customEnrollment <- reactive({
    req(input$completedTemplateUploadEnrollment)
    
    file <- input$completedTemplateUploadEnrollment
    ext <- tools::file_ext(file$datapath)
    
    validate(need(ext == "xlsx",
                  "Please upload xlsx file"))
    
    a <- readxl::read_xlsx(file$datapath) %>%
      rename(Enrollment_2019_Custom = Enrollment_2019,
             Enrollment_2020_Custom = Enrollment_2020,
             Enrollment_2021_Custom = Enrollment_2021,
             Enrollment_2022_Custom = Enrollment_2022)
    
    return(a)
  })
  
  ### Primary/secondary double count ----
  customDoubleCount <- reactive({
    req(input$completedTemplateUploadDoubleCount)
    
    file <- input$completedTemplateUploadDoubleCount
    ext <- tools::file_ext(file$datapath)
    
    validate(need(ext == "xlsx",
                  "Please upload xlsx file"))
    
    a <- readxl::read_xlsx(file$datapath) %>%
      rename(PSDC_2019_Custom = PrimarySecondaryDoubleCounts_2019,
             PSDC_2020_Custom = PrimarySecondaryDoubleCounts_2020,
             PSDC_2021_Custom = PrimarySecondaryDoubleCounts_2021,
             PSDC_2022_Custom = PrimarySecondaryDoubleCounts_2022)
    
    return(a)
  })
  
}

shinyApp(ui, server)