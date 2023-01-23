library(shiny)
library(leaflet)
library(leaflet.extras)
library(sf)
library(DT)
library(tidyverse)
#library(shinyglide)
library(writexl)
library(shinyjs)
library(aws.s3)
library(futile.logger)
library(readxl)
library(paws)
library(jsonlite)
library(shinyWidgets)
library(datimutils)

# source("functions.R")
# source("data_load.R")



# uncomment when running manually ------
source("functions.R")

tryCatch({
  s3_connect()
},
error = function(e) {
  print(e)
})
source("data_load.R")

### js ----
# allows for using the enter button for the log in
jscode_login <- '$(document).keyup(function(e) {
    var focusedElement = document.activeElement.id;
    console.log(focusedElement);
    if (e.key == "Enter" && focusedElement == "user_name") {
    $("#password").focus();
    } else if (e.key == "Enter" && focusedElement == "password") {
    $("#login_button").click();
    }
});'

### Initiate logging
logger <- flog.logger()
flog.appender(appender.console(), name = "usgpartners")

### OAuth Client information
if (interactive()) {
  # NOTE: The line below must be ran manually to set the port
  # OR this line can be added to .Rprofile.
  # This is not an issue when using a single file version of shiny, ie app.R
  # The order by which the files execute is the reasoning behind this.
  options(shiny.port = 3125)
  # # testing url
  APP_URL <- "http://127.0.0.1:3125/"# This will be your local host path
} else {
  # deployed URL
  APP_URL <- Sys.getenv("APP_URL") #This will be your shiny server path
}

oauth_app <- httr::oauth_app(Sys.getenv("OAUTH_APPNAME"),
                             key = Sys.getenv("OAUTH_KEYNAME"), # dhis2 = Client ID
                             secret = Sys.getenv("OAUTH_SECRET"), #dhis2 = Client Secret
                             redirect_uri = APP_URL
)

oauth_api <- httr::oauth_endpoint(base_url = paste0(Sys.getenv("BASE_URL"), "uaa/oauth"),
                                  request = NULL,
                                  authorize = "authorize",
                                  access = "token"
)

oauth_scope <- "ALL"

has_auth_code <- function(params) {
  
  return(!is.null(params$code))
}




USG_USERS = c("Agency", "Interagency", "Global Agency", "Global") 
PARTNER_USERS = c("Global Partner", "Partner")


ui <- uiOutput("ui")


server <- function(input, output, session) {
  
  
  # User information for authentication ----
  user_input  <-  reactiveValues(authenticated = FALSE,
                                 status = "",
                                 d2_session = NULL,
                                 memo_authorized = FALSE)
  
  #UI that will display when redirected to OAuth login agent
  output$ui_redirect <- renderUI({
    #print(input$login_button_oauth) useful for debugging
    if (!is.null(input$login_button_oauth)) { # nolint
      if (input$login_button_oauth > 0) { # nolint
        print("redirecting...")
        url <- httr::oauth2.0_authorize_url(oauth_api, oauth_app, scope = oauth_scope)
        redirect <- sprintf("location.replace(\"%s\");", url)
        tags$script(HTML(redirect))
      } else NULL
    } else NULL
  })
  
  
  auth_ui <- function() {
    fluidPage(
      wellPanel(
        fluidRow(
          h4("Please login with your DATIM credentials:"),
          br()
        ),
        fluidRow(
          #textInput("user_name", "Username: ", width = "500px"),
          #passwordInput("password", "Password:", width = "500px"),
          #actionButton("login_button", "Log in!"),
          
          actionButton("login_button_oauth", "Log in with DATIM"),
          uiOutput("ui_hasauth"),
          uiOutput("ui_redirect")
        )
      )
    )
  }
  

  ##########DELETE ME
  
  output$workingDataExport_check <- DT::renderDataTable({
    req(workingDataPost$data)
    
    datatable(workingDataPost$data,
              options = list(scrollx = TRUE)
    )
  })
  ##########DELETE ME

  # Login process ----
  observeEvent(input$login_button_oauth > 0, {
    print(APP_URL)
    #Grabs the code from the url
    params <- parseQueryString(session$clientData$url_search)
    #Wait until the auth code actually exists
    req(has_auth_code(params))
    
    #Manually create a token
    token <- httr::oauth2.0_token(
      app = oauth_app,
      endpoint = oauth_api,
      scope = oauth_scope,
      use_basic_auth = TRUE,
      oob_value = APP_URL,
      cache = FALSE,
      credentials = httr::oauth2.0_access_token(endpoint = oauth_api,
                                                app = oauth_app,
                                                code = params$code,
                                                use_basic_auth = TRUE)
    )
    
    loginAttempt <- tryCatch({
      user_input$uuid <- uuid::UUIDgenerate()
      datimutils::loginToDATIMOAuth(base_url =  Sys.getenv("BASE_URL"),
                                    token = token,
                                    app = oauth_app,
                                    api = oauth_api,
                                    redirect_uri = APP_URL,
                                    scope = oauth_scope,
                                    d2_session_envir = parent.env(environment())
      )
      
      # DISALLOW USER ACCESS TO THE APP-----
      
      # store data so call is made only once
      user <- list()
      user$type <- datimutils::getMyUserType()
      
      # if a user is not to be allowed deny them entry
      if (!user$type %in% c(USG_USERS, PARTNER_USERS)) {
        
        # alert the user they cannot access the app
        sendSweetAlert(
          session,
          title = "YOU CANNOT LOG IN",
          text = "You are not authorized to use this application",
          type = "error"
        )
        
        # log them out
        Sys.sleep(3)
        flog.info(paste0("User ", user_input$d2_session$me$userCredentials$username, " logged out."))
        user_input$authenticated  <-  FALSE
        user_input$user_name <- ""
        user_input$authorized  <-  FALSE
        user_input$d2_session  <-  NULL
        d2_default_session <- NULL
        gc()
        session$reload()
        
      }
    },
    # This function throws an error if the login is not successful
    error = function(e) {
      flog.info(paste0("User ", input$user_name, " login failed. ", e$message), name = "usgpartners")
    }
    )
    
    if (exists("d2_default_session")) {
      
      user_input$authenticated  <-  TRUE
      user_input$d2_session  <-  d2_default_session$clone()
      d2_default_session <- NULL
      
      # if logged in now source functions
      source("functions.R")
      source("params.R")
      
      # connect to S3
      tryCatch({
        s3_connect()
      },
      error = function(e) {
        print(e)
      })
      
      # allow reading of data now that we are connected and authorized
      source("data_load.R")
      
      #Need to check the user is a member of the PRIME Data Systems Group, COP Memo group, or a super user
      user_input$memo_authorized <-
        grepl("VDEqY8YeCEk|ezh8nmc4JbX", user_input$d2_session$me$userGroups) |
        grepl(
          "jtzbVV4ZmdP",
          user_input$d2_session$me$userCredentials$userRoles
        )
      flog.info(
        paste0(
          "User ",
          user_input$d2_session$me$userCredentials$username,
          " logged in."
        ),
        name = "usgpartners"
      )
      
      flog.info(
        paste0(
          "User ",
          user_input$d2_session$me$userCredentials$username,
          " logged in."
        ),
        name = "usgpartners"
      )
    }
    
  })
  
  # logout process ----
  observeEvent(input$logout, {
    req(input$logout)
    # Returns to the log in screen without the authorization code at top
    updateQueryString("?", mode = "replace", session = session)
    flog.info(paste0("User ", user_input$d2_session$me$userCredentials$username, " logged out."))
    #ready$ok <- FALSE
    user_input$authenticated  <-  FALSE
    user_input$user_name <- ""
    user_input$authorized  <-  FALSE
    user_input$d2_session  <-  NULL
    d2_default_session <- NULL
    gc()
    session$reload()
  })
  

  main_ui <- function() {
    fluidPage(
      tags$head(
        tags$link(rel = "stylesheet", 
        type = "text/css",
        href = "style.css")),
      tags$style(".progress-bar{
                 background-color: #20A39E;
                 }
                 "),
      shinyjs::useShinyjs(),
      titlePanel(title = div(h1("Welcome to DREAMS Sat", style="margin: 0;"), 
                             h4('Saturation calculation application', style="margin: 0;")), 
                 windowTitle = "DREAMS Sat"),
      actionButton("logout",
                   "Logout",
                   icon = icon("sign-out"),
                   style="color: #fff; background-color: #FF0000; border-color: #2e6da4"),
      fluidRow(
        column(12,
      #          DT::dataTableOutput("workingDataExport_check"),
      strong("Please note, this is a pilot/development version of the DREAMS Sat app. Testing is ongoing. Some links may not work yet. Please focus on the user experience and the process in your testing, do not use the numbers produced through the app for actual programming until the app is fully released."),
      strong("To help improve the final app, if you run into any bugs, missing geographies, or numbers that seem unexpected, please let us know by sending an email to samuel.i.dupre@census.gov."),
      )),
      fluidRow(
        column(12,
               h3("Let's Get Started"),
               wellPanel(
                   strong("Import Previous Work?"),
                   br(),
                   br(),
                   p("Please either import saved parameters and data or proceed to step 1 of 9 by clicking the blue 'Next' button below"),
                   br(),
                   p("DREAMS Sat is pre-loaded with default values for each country. To update figures after making any changes in these slides, please click the `Accept parameters and derive/re-derive COP statistics' button below."),
                   br(),
                   br(),
                   fileInput("importTokenParams",
                             "Import save token - parameters (.RData)",
                             accept = ".RData"),
                   conditionalPanel(
                     condition = "output.paramsUploaded == true",
                     p("You've uploaded data for:"),
                     strong(textOutput("importedCountry")),
                     br(),
                     p("Is this correct?"),
                     br(),
                     actionButton("importButtonParams",
                                  "Confirm parameter import"),
                     br(),
                     br()),
                   conditionalPanel(
                     condition = "input.importButtonParams != 0",
                     fileInput("importTokenData",
                               "Import saved token - data (.RData)",
                               accept = ".RData")
                   )
                 ),
               h3("Step 1 of 9: Select Your Country"),
               h4("Once selected, the application will pull population data by 5-year age cohort for each individual year for each subnational unit from the U.S. Census Bureau PEPFAR subnational estimates."),
               h4("U.S. Census Bureau estimates are used instead of WPP22/Spectrum data because WPP22 data is not available for subnational areas. The two sources are generally in close agreement."),
               wellPanel(
                 selectInput("country",
                             "Country",
                             selected = "Botswana",
                             choices = c("Botswana",
                                         #"Cote d'Ivoire",
                                         #"Eswatini",
                                         #"Haiti",
                                         "Kenya",
                                         "Lesotho",
                                         "Malawi",
                                         #"Mozambique",
                                         #"Namibia",
                                         #"Rwanda",
                                         "South Africa",
                                         #"Tanzania",
                                         #"Uganda",
                                         #"Zambia",
                                         "Zimbabwe")
                   ),
                 fluidRow(
                   column(4,
                          strong("Note: country selection is meant as a first step and sets/reverts choices to default values. Avoid changing country selection without saving progress to avoid losing work.")),
                   column(8,
                          leafletOutput("map_main")))
                 ),
               h3("Step 2 of 9: Set Prevalence"),
               h4("This is used to derive HIV-negative population for each cohort."),
               wellPanel(
                   strong("Prevalence default: 2%"),
                   br(),
                   p("Use default values or upload custom values (highly recommended)"),
                   br(),
                   br(),
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
               h3("Step 3 of 9: Set Vulnerability"),
               h4("This is used to derive the number of DREAMS-eligible HIV-negative girls from the population counts and prevalence numbers previously entered."),
               wellPanel(
                 strong("Vulnerability default: 85%"),
                 br(),
                 p("Use default values or upload custom values (highly recommended)"),
                 br(),
                 br(),
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
               ),
               h3("Step 4 of 9: Set Population Structure"),
               h4("This step addresses the aging-in/aging-out challenge, wherein AGYW_PREV accounting does allow for telling which people from a previous year cohort AGYW_PREV number are still in that age cohort."),
               h4("To address this, we apply a proxy population structure to each year's DREAMS cohort. Option 1 is to say default '20% of 10-14 girls were 10, 20% were 11, etc'. Option 2 is to say that your DREAMS population structure matches the national population structure. Option 3 is to apply your own custom population structure based on your expert contextual knowledge about your DREAMS graduating cohorts."),
               wellPanel(
                 fluidRow(
                   column(3,
                          strong("Population structure default: 20%)"),
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
                                      accept = ".xlsx")
                          )
                   ),
                   column(9,
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
                          )))
                 ),
               h3("Step 5 of 9: Set Catchment Modifier"),
               h4("This is used to address enrollment of girls from outside the DREAMS district within that district."),
               h4("PLEASE NOTE: This step currently adjusts the denominator up to account for additional girls. However, we are likely to change this pre-COP to adjust the numerator (AGYW_PREV) down instead. This will not change the saturation number, but will change the number of remaining girls and would better-reflect DREAMS messaging/goals. We welcome feedback on either approach."),
               wellPanel(
                   strong("Catchment modifier default: no modifier)"),
                   fluidRow(
                     column(4,
                            checkboxGroupInput("checkGroup_catchment",
                                               label = "Apply DREAMS catchment to:",
                                               choices = "")
                     ),
                     column(8,
                            leafletOutput("map_catchments")))
                 ),
               h3("Step 6 of 9: Set Enrollment Modifier"),
               h4("This addresses mismatches in enrollment criteria from vulnerability criteria, e.g., food insecurity may allow for enrollment, but would not have qualified that same person for 'vulnerability' in the denominator."),
               wellPanel(
                   strong("Enrollment modifier default: 3%"),
                   p("Use default value or upload a custom modifier structure"),
                   strong("Custom Structure step 6a"),
                   p("Download blank enrollment\nmodifier worksheet"),
                   downloadButton("blankTemplateDownloadEnrollment",
                                  "Download blank template"),
                   br(),
                   br(),
                   strong("Step 6b"),
                   p("Open worksheet in Excel, fill out 'Enrollment' columns and save"),
                   strong("Step 6c"),
                   p("Upload completed enrollment\nmodifier worksheet"),
                   fileInput("completedTemplateUploadEnrollment",
                             "Upload completed template (.xlsx only)",
                             accept = ".xlsx"),
                   br(),
                   br(),
                   DT::dataTableOutput("customEnrollmentTable"),
                   fluidRow(
                     actionButton("confirmCustomEnrollment",
                                  "Confirm: use custom upload"),
                     actionButton("resetToDefaultEnrollment",
                                  "Reset: use default modifier")
                   )
                 ),
               h3("Step 7 of 9: Set Double Count Modifier"),
               h4("This addresses duplication, where a person may complete primary programming while 12 in 2020 and then secondary while 13 in 2021. Without being addressed, she would appear as two counts in AGYW_PREV."),
               wellPanel(
                   strong("Double count modifier default: 1%"),
                   p("Use default value or upload a custom structure"),
                   strong("Custom Structure step 7a"),
                   p("Download blank modifier\nstructure worksheet"),
                   downloadButton("blankTemplateDownloadDoubleCount",
                                  "Download blank template"),
                   br(),
                   br(),
                   strong("Step 7b"),
                   p("Open worksheet in Excel, fill out 'PrimarySecondaryDoubleCounts_20XX' columns and save"),
                   strong("Step 7c"),
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
               h3("Step 8 of 9: Accept Parameters and Save Your Work"),
               wellPanel(
                 actionButton("initializeSelection",
                              "Accept parameters and derive/re-derive statistics"),
                 br(),
                 br(),
                 downloadButton("exportTokenParameters",
                                "Export save token - parameters"),
                 br(),
                 br(),
                 downloadButton("exportTokenData",
                                "Export save token - data"),
                 br(),
                 br()
                 ),
               h3("Step 9 of 9: Review Your Results"),
               strong("Select a focus"),
               actionButton("focusCOP",
                            "COP"),
               actionButton("focusSaturation",
                            "Saturation"),
               actionButton("focusNumerator",
                            "Numerator"),
               # actionButton("focusDenominator",
               #              "Denominator"),
               br(),
               br(),
               wellPanel(
                 uiOutput("COP_ui"),
                 uiOutput("saturation_ui"),
                 uiOutput("numerator_ui")#,
                 #uiOutput("denominator_ui")
        
               )
               )
        ),
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
  }
  
  # output$ui <- renderUI({
  #   main_ui()
  # })
  
  output$ui <- renderUI({
    if(!user_input$authenticated){
       auth_ui()
      #main_ui()
    }else{
      main_ui()
    }
  })

  output$COP_ui <- renderUI({
    req(reactiveButtons$focusedAnalytic == "COP")
    req(data_stats_COP$data)
    
    div(
      h4("2022 Figures for COP Export"),
      strong("NOTE: Exports will only include displayed content. Set to display all desired content before exporting."),
      br(),
      br(),
      DT::dataTableOutput("stats_COP"))
    
  })
  
  output$saturation_ui <- renderUI({
    req(reactiveButtons$focusedAnalytic == "Saturation")
    req(data_stats_COP$data)

     div(
      h4("Saturation"),
      selectInput("saturationCohort",
                  "Select an age cohort:",
                  selected = "10-14",
                  choices = c("10-14",
                              "15-19",
                              "20-24",
                              "25-29")),
      fluidRow(
        column(12, leafletOutput("map_saturation")))
      # fluidRow(
      #   column(7, leafletOutput("map_saturation")),
      #   column(5, DT::dataTableOutput("table_saturation")))

  )})

  output$numerator_ui <- renderUI({
    req(reactiveButtons$focusedAnalytic == "Numerator")
    req(data_stats_COP$data)

    div(
      h4("Numerator"),
      selectInput("numeratorsDistrict",
                  "Select a district:",
                  selected = "",
                  choices = ""
      ),
      plotOutput("impactOfParametersPlot"))
      # plotOutput("impactOfParametersPlot"),
      # DT::dataTableOutput("table_numerator"))

  })

  output$denominator_ui <- renderUI({
    req(reactiveButtons$focusedAnalytic == "Denominator")
    req(data_stats_COP$data)

    div(
      h4("Denominator"))

  })
  
  
  # Setup reactiveValues objects ----
  params <- reactiveValues(
    country = "Botswana",
    popStructureType = "Default",
    catchmentsSelected = ""
  )
  
  reactiveButtons <- reactiveValues(
    focusedAnalytic = "COP"
  )
  
  workingDataPost <- reactiveValues(
    data = NULL
  )
  
  data_stats_COP <- reactiveValues(
    data = defaultStatsCOP
  )
  
  spatial <- reactiveValues(
    sf1 = botADM1.sf,
    sf2 = botADM2.sf,
    sf1_DREAMS = NULL,
    sf1_notDREAMS = NULL,
    catchments = NULL,
    #sf1_DREAMSNeighbors = NULL,
    sf2_DREAMSNeighbors = botADM2.sf,
    zoom = NULL
  )
  
  ### MOVE THIS
  
  observeEvent(input$country, {
    if (input$country == "Botswana") {
      spatial$sf1 <- botADM1.sf
    } else if (input$country == "Kenya") {
      spatial$sf1 <- kenADM1.sf
    } else if (input$country == "Lesotho") {
      spatial$sf1 <- lesADM1.sf
    } else if (input$country == "Malawi") {
      spatial$sf1 <- malADM1.sf
    } else if (input$country == "South Africa") {
      spatial$sf1 <- safADM1.sf
    } else if (input$country == "Zimbabwe") {
      spatial$sf1 <- zimADM1.sf
    }
    
    if (input$country == "Botswana") {
      spatial$sf2 <- botADM2.sf
    } else if (input$country == "Kenya") {
      spatial$sf2 <- kenADM2.sf
    } else if (input$country == "Lesotho") { # for Lesotho, we treat ADM1 as ADM2 too
      spatial$sf2 <- lesADM1.sf 
    } else if (input$country == "Malawi") { # for Malawi, we treat ADM1 as ADM2 too
      spatial$sf2 <- malADM1.sf 
    } else if (input$country == "South Africa") {
      spatial$sf2 <- safADM2.sf
    } else if (input$country == "Zimbabwe") {
      spatial$sf2 <- zimADM2.sf
    }
    
    if (input$country %in% small_countries) {
      spatial$zoom <- 7
    } else if (input$country %in% medium_countries) {
      spatial$zoom <- 6
    } else if (input$country %in% large_countries) {
      spatial$zoom <- 5
    }
    
    spatial$sf1_DREAMS <- spatial$sf1 %>%
      dplyr::filter(DREAMSDistrict == "Yes")
    
    spatial$sf1_notDREAMS <- spatial$sf1 %>%
      dplyr::filter(DREAMSDistrict == "No")
    
    spatial$sf2_DREAMS <- spatial$sf2 %>%
      dplyr::filter(DREAMSDistrict == "Yes")
    
    spatial$sf2_notDREAMS <- spatial$sf2 %>%
      dplyr::filter(DREAMSDistrict == "No")

    })


  
  
  ###
  
  
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
    } else if (input$country == "Malawi") {
      a <- c("BLANTYRE", 
             "MACHINGA",
             "ZOMBA")
    } else if (input$country == "South Africa") {
      a <- c("ALFRED NZO",
             "BOJANALA",
             "BUFFALO CITY",
             "CAPRICORN",
             "CITY OF CAPE TOWN",
             "CITY OF JOHANNESBURG",
             "CITY OF TSHWANE",
             "DOCTOR KENNETH KAUNDA",
             "EHLANZENI",
             "EKURHULENI",
             "ETHEKWINI",
             "GERT SIBANDE",
             "LEJWELEPUTSWA",
             "MOPANI",
             "NGAKA MODIRI MOLEMA",
             "NKANGALA",
             "O.R. TAMBO",
             "SEDIBENG",
             "THABO MOFUTSANYANE",
             "UGU",
             "UMGUNGUNDLOVU",
             "UTHUKELA",
             "UTHUNGULU",
             "ZULULAND")
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
  observeEvent(input$importTokenParams, {
    req(importedTokenParams())
    
    updateSelectInput(session,
                      "country",
                      selected = importedTokenParams()$country[1])
    
  })
  
  # In a seperate observeEvent to make it wait for the options to refresh before intiating
  output$paramsUploaded <- reactive({
    return(!is.null(importedTokenParams()))
  })
  
  observeEvent(input$importButtonParams, {
    req(importedTokenParams())

    updateRadioButtons(session,
                       "structure",
                       selected = importedTokenParams()$popStructure[1])
    
    updateCheckboxGroupInput(session,
                             "checkGroup_catchment",
                             selected = importedTokenParams()$catchmentsSelected)
    
  })
  
  observeEvent(input$country, {
    updateSelectInput(session,
                      "popStructureDistrict",
                      choices = districts())
  })
  
  observeEvent(input$country, {
    updateSelectInput(session,
                      "numeratorsDistrict",
                      choices = districts())
  })
  
  observeEvent(input$focusNumerator, {
    updateSelectInput(session,
                      "numeratorsDistrict",
                      choices = districts())
  })
  
  observeEvent(input$country, {
    updateCheckboxGroupInput(session, 
                             "checkGroup_catchment", 
                             choices = districts())
  })
  
  observeEvent(input$country, {
    req(input$country)
    
    workingDataPost$data <- defaultData %>%
      dplyr::filter(country == input$country)
  })
  
  observeEvent(input$country, {
    req(input$country)
    
    data_stats_COP$data <- defaultStatsCOP %>%
      dplyr::filter(Country == input$country)
  })
  
  ## Update parameters ----
  observeEvent(input$country, {
    params$country = input$country
  })
  
  observeEvent(input$checkGroup_catchment, {
    params$catchmentsSelected = input$checkGroup_catchment
  })
  
  observeEvent(input$focusCOP, {
    req(data_stats_COP$data)
    reactiveButtons$focusedAnalytic <- "COP"
  })
  
  observeEvent(input$focusSaturation, {
    req(data_stats_COP$data)
    reactiveButtons$focusedAnalytic <- "Saturation"
    
    shinyjs::click("initializeSelection")
  })
  
  observeEvent(input$focusNumerator, {
    req(data_stats_COP$data)
    reactiveButtons$focusedAnalytic <- "Numerator"
    
    shinyjs::click("initializeSelection")
  })
  
  # observeEvent(input$focusDenominator, {
  #   req(data_stats_COP$data)
  #   reactiveButtons$focusedAnalytic <- "Denominator"
  # })
  
  # Render text elements ----
  output$importedCountry <- renderText(
    if (!is.null(importedTokenParams())) 
    importedTokenParams()$country[1]) 
    
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
  
  observeEvent(input$initializeSelection, {
    
    data_stats_COP$data <- workingDataPost$data %>%
      reduceToCOPExport()
  })
  

  data_stats_analytics <- eventReactive(input$initializeSelection, {
    req(!is.null(workingDataPost$data))

    selected_data <- workingDataPost$data %>%
      reduceForAnalyticsPlots() %>%
      rename(Cohort = ageasentered,
             District = AREA_NAME,
             `AGYW_PREV total` = AGYW_PREV_Sum,
             `P/S* deduplicated AGYW` = DeDuplicatedAGYW_PREV_Sum,
             `Enrollment standardized AGYW` = EnrollmentStandardizedAGYW_PREV_Sum,
             `AGYW completed` = Actual_Served_2022
             )
  })
  
  observeEvent(input$confirmCustomPrevalence, {
    req(customPrevalence())
    req(defaultData)
    
    defaultData <- left_join(defaultData,
                                     customPrevalence(),
                                     by = c("country" = "Country", 
                                            "AREA_NAME" = "District", 
                                            "ageasentered" = "AgeCohort"))
    
    defaultData$Prev_2019 <- defaultData$Prevalence_2019_Custom
    defaultData$Prev_2020 <- defaultData$Prevalence_2020_Custom
    defaultData$Prev_2021 <- defaultData$Prevalence_2021_Custom
    defaultData$Prev_2022 <- defaultData$Prevalence_2022_Custom
    
    workingDataPost$data <- defaultData %>%
      deriveStatistics()
    
  })
  
  observeEvent(input$resetToDefaultPrevalence, {
    req(defaultData)
    
    defaultData$Prev_2019 <- 2
    defaultData$Prev_2020 <- 2
    defaultData$Prev_2021 <- 2
    defaultData$Prev_2022 <- 2
    
    workingDataPost$data <- defaultData %>%
      deriveStatistics()
    
  })
  
  observeEvent(input$confirmCustomVulnerability, {
    req(customVulnerability())
    req(defaultData)
    
    defaultData <- left_join(defaultData,
                                     customVulnerability(),
                                     by = c("country" = "Country", 
                                            "AREA_NAME" = "District", 
                                            "ageasentered" = "AgeCohort"))
    
    defaultData$Vuln_2019 <- defaultData$Vulnerable_2019_Custom
    defaultData$Vuln_2020 <- defaultData$Vulnerable_2020_Custom
    defaultData$Vuln_2021 <- defaultData$Vulnerable_2021_Custom
    defaultData$Vuln_2022 <- defaultData$Vulnerable_2022_Custom
    
    workingDataPost$data <- defaultData %>%
      deriveStatistics()
    
  })
  
  observeEvent(input$resetToDefaultVulnerability, {
    req(defaultData)
    
    defaultData$Vuln_2019 <- 85
    defaultData$Vuln_2020 <- 85
    defaultData$Vuln_2021 <- 85
    defaultData$Vuln_2022 <- 85
    
    workingDataPost$data <- defaultData %>%
      deriveStatistics()
    
  })
  
  observeEvent(input$confirmCustomEnrollment, {
    req(customEnrollment())
    req(defaultData)
    
    # if ("Enrollment_2019" %in% colnames(customEnrollment())) {
    #   showModal(modalDialog(
    #     title = "Important message",
    #     "This is an important message!"
    #   ))
    # } else {
      defaultData <- left_join(defaultData,
                               customEnrollment(),
                               by = c("country" = "Country", 
                                      "AREA_NAME" = "District", 
                                      "ageasentered" = "AgeCohort"))
      
      defaultData$Enrollment_2019 <- defaultData$Enrollment_2019_Custom
      defaultData$Enrollment_2020 <- defaultData$Enrollment_2020_Custom
      defaultData$Enrollment_2021 <- defaultData$Enrollment_2021_Custom
      defaultData$Enrollment_2022 <- defaultData$Enrollment_2022_Custom
      
      workingDataPost$data <- defaultData %>%
        deriveStatistics()
    # }
  })
  
  observeEvent(input$resetToDefaultEnrollment, {
    req(defaultData)
    
    defaultData$Enrollment_2019 <- 3
    defaultData$Enrollment_2020 <- 3
    defaultData$Enrollment_2021 <- 3
    defaultData$Enrollment_2022 <- 3
    
    workingDataPost$data <- defaultData %>%
      deriveStatistics()
    
  })
  
  observeEvent(input$confirmCustomDoubleCount, {
    req(customDoubleCount())
    req(defaultData)
    
    defaultData <- left_join(defaultData,
                                     customDoubleCount(),
                                     by = c("country" = "Country", 
                                            "AREA_NAME" = "District", 
                                            "ageasentered" = "AgeCohort"))
    
    defaultData$PSDC_2019 <- defaultData$PSDC_2019_Custom
    defaultData$PSDC_2020 <- defaultData$PSDC_2020_Custom
    defaultData$PSDC_2021 <- defaultData$PSDC_2021_Custom
    defaultData$PSDC_2022 <- defaultData$PSDC_2022_Custom
    
    workingDataPost$data <- defaultData %>%
      deriveStatistics()
    
  })
  
  observeEvent(input$resetToDefaultDoubleCount, {
    req(defaultData)
    
    defaultData$PSDC_2019 <- 1
    defaultData$PSDC_2020 <- 1
    defaultData$PSDC_2021 <- 1
    defaultData$PSDC_2022 <- 1
    
    workingDataPost$data <- defaultData %>%
      deriveStatistics()
    
  })
  
  observeEvent(customPopStructure(), {
    req(customPopStructure())
    req(workingDataPost$data)
    
    workingDataPost$data <- left_join(workingDataPost$data,
                                     customPopStructure(),
                                     by = c("country" = "Country",
                                            "AREA_NAME" = "District",
                                            "ageasentered" = "ageasentered"))
    
    workingDataPost$data$firstQ_2019 <- workingDataPost$data$firstQCustom_2019
    workingDataPost$data$firstQ_2020 <- workingDataPost$data$firstQCustom_2020
    workingDataPost$data$firstQ_2021 <- workingDataPost$data$firstQCustom_2021
    workingDataPost$data$firstQ_2022 <- workingDataPost$data$firstQCustom_2022
    workingDataPost$data$secondQ_2019 <- workingDataPost$data$secondQCustom_2019
    workingDataPost$data$secondQ_2020 <- workingDataPost$data$secondQCustom_2020
    workingDataPost$data$secondQ_2021 <- workingDataPost$data$secondQCustom_2021
    workingDataPost$data$secondQ_2022 <- workingDataPost$data$secondQCustom_2022
    workingDataPost$data$thirdQ_2019 <- workingDataPost$data$thirdQCustom_2019
    workingDataPost$data$thirdQ_2020 <- workingDataPost$data$thirdQCustom_2020
    workingDataPost$data$thirdQ_2021 <- workingDataPost$data$thirdQCustom_2021
    workingDataPost$data$thirdQ_2022 <- workingDataPost$data$thirdQCustom_2022
    workingDataPost$data$fourthQ_2019 <- workingDataPost$data$fourthQCustom_2019
    workingDataPost$data$fourthQ_2020 <- workingDataPost$data$fourthQCustom_2020
    workingDataPost$data$fourthQ_2021 <- workingDataPost$data$fourthQCustom_2021
    workingDataPost$data$fourthQ_2022 <- workingDataPost$data$fourthQCustom_2022
    workingDataPost$data$fifthQ_2019 <- workingDataPost$data$fifthQCustom_2019
    workingDataPost$data$fifthQ_2020 <- workingDataPost$data$fifthQCustom_2020
    workingDataPost$data$fifthQ_2021 <- workingDataPost$data$fifthQCustom_2021
    workingDataPost$data$fifthQ_2022 <- workingDataPost$data$fifthQCustom_2022
    
    workingDataPost$data <- workingDataPost$data %>%
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
    
    workingDataPost$data <- workingDataPost$data %>%
      deriveStatistics()
    
  })
  
  # Render table elements ----
  # 
  output$customPrevalenceTable <- DT::renderDataTable({
    req(customPrevalence())
    
    datatable(customPrevalence(),
              rownames = FALSE,
              selection = 'none',
              colnames = c("Age Cohort" = "AgeCohort",
                           `2019` = "Prevalence_2019_Custom",
                           `2020` = "Prevalence_2020_Custom",
                           `2021` = "Prevalence_2021_Custom",
                           `2022` = "Prevalence_2022_Custom")
    )
  })
  
  output$customVulnerabilityTable <- DT::renderDataTable({
    req(customVulnerability())
    
    datatable(customVulnerability(),
              rownames = FALSE,
              selection = 'none',
              colnames = c("Age Cohort" = "AgeCohort",
                           `2019` = "Vulnerable_2019_Custom",
                           `2020` = "Vulnerable_2020_Custom",
                           `2021` = "Vulnerable_2021_Custom",
                           `2022` = "Vulnerable_2022_Custom")
    )
  })
  
  output$customEnrollmentTable <- DT::renderDataTable({
    req(customEnrollment())

    datatable(customEnrollment(),
              rownames = FALSE,
              selection = 'none',
              colnames = c("Age Cohort" = "AgeCohort",
                           `2019` = "Enrollment_2019_Custom",
                           `2020` = "Enrollment_2020_Custom",
                           `2021` = "Enrollment_2021_Custom",
                           `2022` = "Enrollment_2022_Custom")
    )
  })
  
  output$customPSDCTable <- DT::renderDataTable({
    req(customDoubleCount())
    
    datatable(customDoubleCount(),
              rownames = FALSE,
              selection = 'none',
              colnames = c("Age Cohort" = "AgeCohort",
                           `2019` = "PSDC_2019_Custom",
                           `2020` = "PSDC_2020_Custom",
                           `2021` = "PSDC_2021_Custom",
                           `2022` = "PSDC_2022_Custom")
    )
  })
  
  output$stats_COP <- DT::renderDataTable({
    req(data_stats_COP$data)
    
    a <- data_stats_COP$data %>%
      group_by(Country,
               District,
               Cohort) %>%
      dplyr::filter(Country == input$country) %>%
      summarize(`Total DREAMS eligible AGYW` = mean(`Total DREAMS eligible AGYW`),
                `Percent coverage (saturation)` = mean(`Percent coverage (saturation)`),
                `Remaining unserved AGYW` = mean(`Remaining unserved AGYW`)) 
    
    datatable(a %>%
                dplyr::select(-c("Country")),
              rownames = FALSE,
              selection = 'none',
              extensions = 'Buttons', 
              options = list(
                dom = 'Blfrtip',
                buttons = c('excel', 'pdf'),
                scrollx = TRUE)
    )
  })
  # 
  # output$table_saturation <- DT::renderDataTable({
  #   req(data_stats_analytics())
  #   
  #   a <- data_stats_analytics() %>%
  #     group_by(IsSelected,
  #              District,
  #              Cohort) %>%
  #     # dplyr::filter(Country == input$country) %>%
  #     summarize(Prev_2022 = mean(Prev_2022),
  #               Vuln_2022 = mean(Vuln_2022),
  #               PSDC_2022 = mean(PSDC_2022),
  #               Enrollment_2022 = mean(Enrollment_2022),
  #               Pop_2022 = mean(Pop_2022),
  #               PLHIV_2022 = mean(PLHIV_2022),
  #               NonPLHIV_2022 = mean(NonPLHIV_2022),
  #               VulnerableNonPLHIV_2022 = mean(VulnerableNonPLHIV_2022),
  #               `AGYW_PREV total` = mean(`AGYW_PREV total`),
  #               `P/S* deduplicated AGYW` = mean(`P/S* deduplicated AGYW`),
  #               `Enrollment standardized AGYW` = mean(`Enrollment standardized AGYW`),
  #               `AGYW completed` = mean(`AGYW completed`),
  #               `Total DREAMS eligible AGYW` = mean(`Total DREAMS eligible AGYW`),
  #               `Percent coverage (saturation)` = mean(`Percent coverage (saturation)`),
  #               `Remaining unserved AGYW` = mean(`Remaining unserved AGYW`)) 
  #   
  #   col_order <- c("District",
  #                  "Saturation"
  #   )
  #   
  #   b <- a %>%
  #     dplyr::filter(IsSelected == "Selected" & Cohort == input$saturationCohort) %>%
  #     dplyr::select(-c("Cohort",
  #                      "Prev_2022",
  #                      "Vuln_2022",
  #                      "PSDC_2022",
  #                      "Enrollment_2022",
  #                      "IsSelected",
  #                      "Pop_2022",
  #                      "PLHIV_2022",
  #                      "NonPLHIV_2022",
  #                      "VulnerableNonPLHIV_2022",
  #                      "AGYW_PREV total",
  #                      "P/S* deduplicated AGYW",
  #                      "Enrollment standardized AGYW",
  #                      "AGYW completed")) %>%
  #     dplyr::rename(Saturation = Sat_2022)
  #   
  #   b <- b[, col_order]
  #   
  #   datatable(
  #     b,
  #     rownames = FALSE
  #   )
  # })
  # 
  # output$table_numerator <- DT::renderDataTable({
  #   req(data_stats_analytics())
  #   
  #   col_order <- c("Cohort",
  #                  "AGYW_PREV total",
  #                  "P/S* deduplicated AGYW",
  #                  "Enrollment standardized AGYW",
  #                  "AGYW completed"
  #   )
  #   
  #   a <- data_stats_analytics() %>%
  #     dplyr::filter(IsSelected == "Selected" & District == input$numeratorsDistrict) %>%
  #     dplyr::select(-c("District",
  #                      "Sat_2022",
  #                      "Prev_2022",
  #                      "Vuln_2022",
  #                      "PSDC_2022",
  #                      "Enrollment_2022",
  #                      "IsSelected",
  #                      "Pop_2022",
  #                      "PLHIV_2022",
  #                      "NonPLHIV_2022",
  #                      "VulnerableNonPLHIV_2022"))
  #   
  #   a <- a[, col_order]
  #   
  #   datatable(
  #     a,
  #     rownames = FALSE,
  #     caption = "*Primary/Secondary"
  #   )
  #   
  # })
  
  
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
                       1)
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
                       1)
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
                       1)
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
    #req(input$country != "Lesotho")

    internalDF <- data_stats_analytics() %>%
      dplyr::filter(IsSelected == "Selected" & District == input$numeratorsDistrict) %>%
      dplyr::select(c("Cohort",
                      "District",
                      "AGYW completed",
                      "AGYW_PREV total",
                      "P/S* deduplicated AGYW",
                      "Enrollment standardized AGYW"
                      )) %>%
      tidyr::pivot_longer(
        cols = `AGYW completed`:`Enrollment standardized AGYW`,
        names_to = "Stage",
        values_to = "AGYW_PREV"
      )

    a <- internalDF %>%
      ggplot(aes(x = reorder(Stage, -AGYW_PREV), 
                 y = AGYW_PREV,
                 fill = Stage)) +
      geom_bar(stat = 'identity') +
      scale_fill_manual(
        values = alpha(c("AGYW_PREV total" = "#605B56",
                         "P/S* deduplicated AGYW" = "#605B56",
                         "Enrollment standardized AGYW" = "#605B56",
                         "AGYW completed" = "#20A39E"),
                       1)
      ) +
      ggtitle("Change in AGYW completed estimate by app stage and cohort") +
      ylab("Number of people") +
      xlab("Stage of the process") + 
      facet_wrap(~Cohort) +
      scale_x_discrete(guide = guide_axis(n.dodge = 2)) + 
      theme_minimal() +
      theme_plot(legend.position = "none")


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

    popup_DREAMS <- paste0("<strong>DREAMS District: </strong>",
                           spatial$sf1_DREAMS$AREA_NAME)

    popup_NonDREAMS <- paste0("<strong>Non-DREAMS District: </strong>",
                              spatial$sf1_notDREAMS$AREA_NAME)


    leafletProxy("map_main"
    ) %>%
      clearShapes() %>%
      clearControls() %>%
      addPolygons(data = spatial$sf1_DREAMS,
                  color = "black",
                  fillColor = "#FF6663",
                  weight = 1,
                  opacity = 1,
                  fillOpacity = 1,
                  popup = popup_DREAMS,
                  highlightOptions = highlightOptions(color = "white",
                                                      weight = 2,
                                                      bringToFront = TRUE)) %>%
      addPolygons(data = spatial$sf1_notDREAMS,
                  color = "black",
                  fillColor = "white",
                  weight = 1,
                  opacity = 1,
                  fillOpacity = 1,
                  popup = popup_NonDREAMS) %>%
      setView(lng = mean(st_bbox(spatial$sf1)[c(1,3)]),
              lat = mean(st_bbox(spatial$sf1)[c(2,4)]),
              zoom = spatial$zoom) %>%
      setMapWidgetStyle(list(background = "white"))
  })
  
  ## Catchment map ----
  output$map_catchments <- leaflet::renderLeaflet({
    a <- leaflet() %>%
      setMapWidgetStyle(list(background = "white")) %>%
      # addResetMapButton() %>% #currently doesn't work correctly, figure out how to set to go to the new polygons
      addFullscreenControl()
  })
  
  neighborsLookupFiltered <- eventReactive(input$country, {
    a <- neighborsLookup %>%
      dplyr::filter(country == input$country)
    return(a)
  })

  catchments_filtered <- reactive(
    
    neighborsLookupFiltered()[neighborsLookupFiltered()$parent %in% input$checkGroup_catchment, ])

  # catchmentMapListener <- reactive({
  #   list(#input$country, #likely unnecessary as the change in input$country already changes catchments_filtered itself
  #        catchments_filtered())
  # })

observeEvent(catchments_filtered(), { #catchmentMapListener(), {

  # This seems to need to be within the observer, can't define in spatial reactivevalues and have it work for some reason
  if(input$country %in% c("Lesotho", "Malawi")) {
    selected_country_DREAMSNeighbors <- spatial$sf2 %>%
      dplyr::filter(ADM1_NAME %in% catchments_filtered()$child)
  } else {
    selected_country_DREAMSNeighbors <- spatial$sf2 %>%
      dplyr::filter(ADM2_NAME %in% catchments_filtered()$child)
    
  }
  # 
  # selected_country_DREAMSNeighbors <- spatial$sf2 %>%
  #   dplyr::filter(ADM1_NAME %in% catchments_filtered()$child)

  # selected_country_NonDREAMS <- selected_country %>%
  #   filter(DREAMSDistrict == "No" & (!ADM1_NAME %in% catchments_filtered()$child))


  popup_DREAMS <- paste0("<strong>DREAMS District: </strong>",
                         spatial$sf1_DREAMS$AREA_NAME)

  popup_DREAMSNeighbors <- paste0("<strong>Included DREAMS Neighbor Area: </strong>",
                                  selected_country_DREAMSNeighbors$AREA_NAME)

  popup_NonDREAMS <- paste0("<strong>Non-DREAMS Area: </strong>",
                            spatial$sf2_notDREAMS$AREA_NAME)


    leafletProxy("map_catchments") %>%
      clearShapes() %>%
      clearControls() %>%
      addPolygons(data = spatial$sf2_notDREAMS,
                  color = "#0D131A",
                  fillColor = "white",
                  weight = 1,
                  opacity = 1,
                  fillOpacity = 1,
                  popup = popup_NonDREAMS) %>%
      addPolygons(data = selected_country_DREAMSNeighbors,
                  color = "black",
                  fillColor = "#20A39E",
                  weight = 1,
                  opacity = 1,
                  fillOpacity = 1,
                  popup = popup_DREAMSNeighbors) %>%
      addPolygons(data = spatial$sf1_DREAMS,
                  color = "black",
                  fillColor = "#FF6663",
                  weight = 1,
                  opacity = 1,
                  fillOpacity = 1,
                  popup = popup_DREAMS,
                  highlightOptions = highlightOptions(color = "white",
                                                      weight = 2,
                                                      bringToFront = TRUE)) %>%
      setView(lng = mean(st_bbox(spatial$sf1)[c(1,3)]),
              lat = mean(st_bbox(spatial$sf1)[c(2,4)]),
              zoom = spatial$zoom) %>%
      setMapWidgetStyle(list(background = "white"))
  })

  ## Saturation map ----
  saturation_bins <- c(0, 25.0, 50.0, 75.0, 100.0, Inf)
  
  saturation_labels <- c("0.0 - 24.9", "25.0 - 49.9", "50.0 - 74.9", "75.0 - 99.9", "100.0 or more")

  output$map_saturation <- leaflet::renderLeaflet({
    req(data_stats_COP$data)
    
    internal_df <- data_stats_COP$data %>%
      #dplyr::filter(Cohort == "10-14")
      dplyr::filter(Cohort == input$saturationCohort)
    
    selected_country_DREAMS_joined <- merge(spatial$sf1_DREAMS,
                                            internal_df,
                                            by.x = "AREA_NAME",
                                            by.y = "District",
                                            all.x = "TRUE")
    
    popup_DREAMS <- sprintf(
      "<strong>%s</strong>%s<br/>%s %g",
      "DREAMS District: ",
      selected_country_DREAMS_joined$AREA_NAME,
      "Percent Saturation: ",
      selected_country_DREAMS_joined$`Percent coverage (saturation)`)
    
    saturation_pal <- colorBin("YlOrRd",
                               domain = selected_country_DREAMS_joined$`Percent coverage (saturation)`,
                               bins = saturation_bins)
    
    a <- leaflet() %>%
      addPolygons(data = selected_country_DREAMS_joined,
                  color = "black",
                  fillColor = ~saturation_pal(`Percent coverage (saturation)`),
                  weight = 1,
                  opacity = 1,
                  fillOpacity = 1,
                  popup = popup_DREAMS,
                  highlightOptions = highlightOptions(color = "white",
                                                      weight = 2,
                                                      bringToFront = TRUE)) %>%
      addPolygons(data = spatial$sf1_notDREAMS,
                  color = "black",
                  fillColor = "white",
                  weight = 1,
                  opacity = 1,
                  fillOpacity = 1) %>%
      setView(lng = mean(st_bbox(spatial$sf1)[c(1,3)]),
              lat = mean(st_bbox(spatial$sf1)[c(2,4)]),
              zoom = spatial$zoom) %>%
      setMapWidgetStyle(list(background = "white"))
    # addResetMapButton() #currently doesn't work correctly, figure out how to set to go to the new polygons
  })

  

  observeEvent(input$saturationCohort, {
    #req(data_stats_COP$data)

    internal_df <- data_stats_COP$data %>%
      #dplyr::filter(Cohort == "10-14")
      dplyr::filter(Cohort == input$saturationCohort)

    selected_country_DREAMS_joined <- merge(spatial$sf1_DREAMS,
                                            internal_df,
                                            by.x = "AREA_NAME",
                                            by.y = "District",
                                            all.x = "TRUE")

    popup_DREAMS <- sprintf(
      "<strong>%s</strong>%s<br/>%s %g",
      "DREAMS District: ",
      selected_country_DREAMS_joined$AREA_NAME,
      "Percent Saturation: ",
      selected_country_DREAMS_joined$`Percent coverage (saturation)`)

    saturation_pal <- colorBin("YlOrRd",
                               domain = selected_country_DREAMS_joined$`Percent coverage (saturation)`,
                               bins = saturation_bins)

    leafletProxy("map_saturation"
    ) %>%
      clearShapes() %>%
      clearControls() %>%
      addPolygons(data = selected_country_DREAMS_joined,
                  color = "black",
                  fillColor = ~saturation_pal(`Percent coverage (saturation)`),
                  weight = 1,
                  opacity = 1,
                  fillOpacity = 1,
                  popup = popup_DREAMS,
                  highlightOptions = highlightOptions(color = "white",
                                                      weight = 2,
                                                      bringToFront = TRUE)) %>%
      addPolygons(data = spatial$sf1_notDREAMS,
                  color = "black",
                  fillColor = "white",
                  weight = 1,
                  opacity = 1,
                  fillOpacity = 1) %>%
      setView(lng = mean(st_bbox(spatial$sf1)[c(1,3)]),
              lat = mean(st_bbox(spatial$sf1)[c(2,4)]),
              zoom = spatial$zoom) %>%
      setMapWidgetStyle(list(background = "white"))
  })

  # Save token ----
  ## Export ----
  ### Parameters ----
  exportCountryListener <- reactive({
    params$country
  })
  
  exportPopStructureListener <- reactive({
    params$popStructureType
  })
  
  exportCatchmentsListener <- reactive({
    params$catchmentsSelected
  })
  
  output$exportTokenParameters <- downloadHandler(
    filename = function() {
      paste("DREAMS_Sat_Save_Token_Parameters",
            ".Rdata",
            sep = "")
    },
    content = function(file) {
      
      params_df <- data.frame(
        country = exportCountryListener(),
        popStructureType = exportPopStructureListener(),
        catchmentsSelected = exportCatchmentsListener())
      
      save(params_df, file = file)
      
      # add send to S3
      # write a file to a directory ----
      target_directory <- "exportTokenData/" # change to your directory
      type <- "exportTokenParameters"
      s3write_using(params_df, FUN = write.csv,
                    bucket = Sys.getenv("TEST_BUCKET_WRITE"),
                    object = paste0(
                      "system_dreams_saturation/",
                      target_directory,
                      "DREAMS_Sat_Save_Token_Parameter_",
                      Sys.time(),
                      ".csv"
                    )
      )

    },
    
    contentType = NULL
  )
  
  ### Data ----
  
  exportDataListener <- reactive({
    workingDataPost$data
  })
  
  output$exportTokenData <- downloadHandler(
    filename = function() {
      paste("DREAMS_Sat_Save_Token_Data",
            ".Rdata",
            sep = "")
    },
    content = function(file) {

      workingDataExport <- data.frame(exportDataListener())

      save(workingDataExport, file = file)

    },

  contentType = NULL
  )



  
  ## Import ----
  ### Parameters ----
  importedTokenParams <- eventReactive(input$importTokenParams, {
    if ( is.null(input$importTokenParams)) return(NULL)
    
    load(input$importTokenParams$datapath)
    
    a <- params_df

    return(a)
    
  })
  
  observeEvent(importedTokenParams(), {
    params$country <- importedTokenParams()$country[1]
    params$popStructureType <- importedTokenParams()$popStructureType[1]
    params$catchmentsSelected <- importedTokenParams()$catchmentsSelected

  })
  
  ### Data ----
  importedTokenData <- eventReactive(input$importTokenData, {
    if ( is.null(input$importTokenData)) return(NULL)
    
    load(input$importTokenData$datapath)
    
    a <- workingDataExport
    
    return(a)
    
  })
  
  observeEvent(importedTokenData(), {
    workingDataPost$data <- importedTokenData()
    
    shinyjs::click("initializeSelection")
    
  })
  
  # observeEvent(input$country, {
  #   shinyjs::click("initializeSelection")
  #   
  # })
  
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
    
    a <- readxl::read_xlsx(file$datapath) 
    
    required_columns <- c("Prevalence_2019", 
                          "Prevalence_2020",
                          "Prevalence_2021",
                          "Prevalence_2022")
    
    column_names <- colnames(a)
    
    shiny::validate(need(ext == "xlsx", ""),
                    need(all(required_columns %in% column_names), ""))
    
    b <- a %>%
      rename(Prevalence_2019_Custom = Prevalence_2019,
             Prevalence_2020_Custom = Prevalence_2020,
             Prevalence_2021_Custom = Prevalence_2021,
             Prevalence_2022_Custom = Prevalence_2022)
    
    return(b)
  })
  
  ### Vulnerability ----
  customVulnerability <- reactive({
    req(input$completedTemplateUploadVulnerability)
    
    file <- input$completedTemplateUploadVulnerability
    ext <- tools::file_ext(file$datapath)
    
    a <- readxl::read_xlsx(file$datapath) 
    
    required_columns <- c("Vulnerable_2019", 
                          "Vulnerable_2020",
                          "Vulnerable_2021",
                          "Vulnerable_2022")
    
    column_names <- colnames(a)
    
    shiny::validate(need(ext == "xlsx", ""),
                    need(all(required_columns %in% column_names), ""))
    
    b <- a %>%
      rename(Vulnerable_2019_Custom = Vulnerable_2019,
             Vulnerable_2020_Custom = Vulnerable_2020,
             Vulnerable_2021_Custom = Vulnerable_2021,
             Vulnerable_2022_Custom = Vulnerable_2022)
    
    return(b)
  })
  
  ### Population structure ----
  customPopStructure <- reactive({
    req(input$completedTemplateUploadPopStructure)
    
    file <- input$completedTemplateUploadPopStructure
    ext <- tools::file_ext(file$datapath)
    
    shiny::validate(need(ext == "xlsx",
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
    
    a <- readxl::read_xlsx(file$datapath) 
    required_columns <- c("Enrollment_2019", 
                          "Enrollment_2020",
                          "Enrollment_2021",
                          "Enrollment_2022")
    
    column_names <- colnames(a)
    
    shiny::validate(need(ext == "xlsx", ""),
                    need(all(required_columns %in% column_names), ""))
    
    b <- a %>%
      rename(Enrollment_2019_Custom = Enrollment_2019,
             Enrollment_2020_Custom = Enrollment_2020,
             Enrollment_2021_Custom = Enrollment_2021,
             Enrollment_2022_Custom = Enrollment_2022)
    
    return(b)
  })
  
  ### Primary/secondary double count ----
  customDoubleCount <- reactive({
    req(input$completedTemplateUploadDoubleCount)
    
    file <- input$completedTemplateUploadDoubleCount
    
    ext <- tools::file_ext(file$datapath)
    
    a <- readxl::read_xlsx(file$datapath)
    
    required_columns <- c("PrimarySecondaryDoubleCounts_2019", 
                          "PrimarySecondaryDoubleCounts_2020",
                          "PrimarySecondaryDoubleCounts_2021",
                          "PrimarySecondaryDoubleCounts_2022")
    
    column_names <- colnames(a)
    
    shiny::validate(need(ext == "xlsx", ""),
             need(all(required_columns %in% column_names), ""))
    
    b <- a %>%
      rename(PSDC_2019_Custom = PrimarySecondaryDoubleCounts_2019,
             PSDC_2020_Custom = PrimarySecondaryDoubleCounts_2020,
             PSDC_2021_Custom = PrimarySecondaryDoubleCounts_2021,
             PSDC_2022_Custom = PrimarySecondaryDoubleCounts_2022)
    
    return(b)
  })
  
  # Set outputOptions ----
  
  outputOptions(output, 'paramsUploaded', suspendWhenHidden = FALSE)
  #outputOptions(output, "map_saturation", suspendWhenHidden = FALSE)
  # outputOptions(output, "numerator_ui", suspendWhenHidden = FALSE)

}

shinyApp(ui, server)