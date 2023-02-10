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
      br(),
      actionButton("logout",
                   "Logout",
                   icon = icon("sign-out"),
                   style="color: #fff; background-color: #FF6663; border-color: #1C110A"),
      br(),
      br(),
      fluidRow(
        column(12,
               strong("Welcome to version 1.0 of the DREAMS Sat app. Development is ongoing, with additional features planned for post-COP. To help improve the app, if you run into any bugs, missing geographies, or numbers that seem unexpected, please let us know by sending an email to samuel.i.dupre@census.gov.")
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
                 p("DREAMS Sat is pre-loaded with default values for each country. To update figures after making any changes in these slides, please click the `Accept parameters andderive/re-derive COP statistics' button below."),
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
               h4("Once selected, the application will pull population data by 5-year age cohort for each individual year for each subnational unit from the U.S. Census Bureau PEPFAR estimates."),
               h4("U.S. Census Bureau estimates are used instead of WPP22/Spectrum data because WPP22 data is not available for subnational areas. The two sources are generally in close agreement."),
               h4("WorldPop gridded estimates are used for Cote d'Ivoire, Eswatini, and Haiti as U.S. Census Bureau estimates are on-hold pending updated national data."),
               h4("WorldPop data is only available through 2020, so 2020 totals have been extended to 2021, 2022, and 2023. For this reason, these Cote d'Ivoire, Eswatini, and Haiti population numbers should be replaced by internal national data as available in Step 2."),
               wellPanel(
                 selectInput("country",
                             "Country",
                             selected = "Botswana",
                             choices = c("Botswana",
                                         "Cote d'Ivoire",
                                         "Eswatini",
                                         "Haiti",
                                         "Kenya",
                                         "Lesotho",
                                         "Malawi",
                                         "Mozambique",
                                         "Namibia",
                                         "Rwanda",
                                         "South Africa",
                                         "Tanzania",
                                         "Uganda",
                                         "Zambia",
                                         "Zimbabwe")
                 ),
                 fluidRow(
                   column(4,
                          strong("Note: country selection is meant as a first step and sets/reverts choices to default values. Avoid changing country selection without saving progress to avoid losing work.")),
                   column(8,
                          leafletOutput("map_main")))
               ),
               h3("Step 2 of 9: Upload Custom Population   OPTIONAL STEP"),
               h4("Overwrite default population figures for your country."),
               wellPanel(
                 strong("Use default values or upload custom values."),
                 p("highly recommended for Cote d'Ivoire, Eswatini, and Haiti in 2021, 2022, and 2023 especially."),
                 br(),
                 br(),
                 strong("Population figures"),
                 strong("Custom population step 2a"),
                 p("Download blank population\nworksheet"),
                 downloadButton("blankTemplateDownloadPopulation",
                                "Download blank template"),
                 br(),
                 br(),
                 strong("Step 2b"),
                 p("Open worksheet in Excel, fill out 'Population_20XX' columns and save"),
                 strong("Step 2c"),
                 p("Upload completed population\n worksheet"),
                 fileInput("completedTemplateUploadPopulation",
                           "Upload completed template (.xlsx only)",
                           accept = ".xlsx"),
                 DT::dataTableOutput("customPopulationTable"),
                 actionButton("confirmCustomPopulation",
                              "Confirm: use custom upload",
                              style="color: #fff; background-color: #FFBA49; border-color: #1C110A"),
                 actionButton("resetToDefaultPopulation",
                              "Reset: use default values",
                              style="color: #fff; background-color: #FFBA49; border-color: #1C110A")
               ),
               h3("Step 3 of 9: Set Prevalence"),
               h4("This is used to derive HIV-negative population for each cohort."),
               wellPanel(
                 strong("Prevalence default: national figures based on most-recent PHIA or UNAIDS AIDSinfo data"),
                 br(),
                 p("Use default values or upload custom values (highly recommended)"),
                 br(),
                 br(),
                 strong("Custom prevalence step 3a"),
                 p("Download blank prevalence\nworksheet"),
                 downloadButton("blankTemplateDownloadPrevalence",
                                "Download blank template"),
                 br(),
                 br(),
                 strong("Step 3b"),
                 p("Open worksheet in Excel, fill out 'Prevalence_20XX' columns and save"),
                 strong("Step 3c"),
                 p("Upload completed prevalence\n worksheet"),
                 fileInput("completedTemplateUploadPrevalence",
                           "Upload completed template (.xlsx only)",
                           accept = ".xlsx"),
                 DT::dataTableOutput("customPrevalenceTable"),
                 actionButton("confirmCustomPrevalence",
                              "Confirm: use custom upload",
                              style="color: #fff; background-color: #FFBA49; border-color: #1C110A"),
                 actionButton("resetToDefaultPrevalence",
                              "Reset: use default values",
                              style="color: #fff; background-color: #FFBA49; border-color: #1C110A")
               ),
               h3("Step 4 of 9: Set Vulnerability"),
               h4("This is used to derive the number of DREAMS-eligible HIV-negative girls from the population counts and prevalence numbers previously entered."),
               wellPanel(
                 strong("Vulnerability default: 80%"),
                 br(),
                 p("Use default values or upload custom values (highly recommended)"),
                 br(),
                 br(),
                 strong("Custom vulnerability step 4a"),
                 p("Download blank vulnerability\nworksheet"),
                 downloadButton("blankTemplateDownloadVulnerability",
                                "Download blank template"),
                 br(),
                 br(),
                 strong("Step 4b"),
                 p("Open worksheet in Excel, fill out 'Vulnerable_20XX' columns and save"),
                 strong("Step 4c"),
                 p("Upload completed vulnerability\nworksheet"),
                 fileInput("completedTemplateUploadVulnerability",
                           "Upload completed template (.xlsx only)",
                           accept = ".xlsx"),
                 DT::dataTableOutput("customVulnerabilityTable"),
                 actionButton("confirmCustomVulnerability",
                              "Confirm: use custom upload",
                              style="color: #fff; background-color: #FFBA49; border-color: #1C110A"),
                 actionButton("resetToDefaultVulnerability",
                              "Reset: use default values",
                              style="color: #fff; background-color: #FFBA49; border-color: #1C110A")
               ),
               h3("Step 5 of 9: Set Population Structure"),
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
                            strong("Custom structure step 5a"),
                            p("Download blank population\nstructure worksheet"),
                            downloadButton("blankTemplateDownloadStructure",
                                           "Download blank template"),
                            br(),
                            br(),
                            strong("Step 5b"),
                            p("Open worksheet in Excel, fill out 'proportion' column and save"),
                            strong("Step 5c"),
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
                                        choices = c(2018,
                                                    2019,
                                                    2020,
                                                    2021,
                                                    2022))
                            # selectInput("popStructureDistrict",
                            #             "Select a district (affects custom table only):",
                            #             selected = "",
                            #             choices = ""
                            # )
                          ),
                          fluidRow(
                            column(6,
                                   plotOutput("popStructure_DefaultPlot")),
                            column(6,
                                   plotOutput("popStructure_NationalPlot"))
                          ),
                          fluidRow(
                            DT::dataTableOutput("customStructureTable")
                          )))
               ),
               h3("Step 6 of 9: Set Double Count Modifier"),
               h4("This addresses duplication, where a person may complete primary programming while 12 in 2020 and then secondary while 13 in 2021. Without being addressed, she would appear as two counts in AGYW_PREV."),
               wellPanel(
                 strong("Double count modifier default: 15%"),
                 p("Use default value or upload a custom structure"),
                 br(),
                 br(),
                 strong("Custom modifier step 6a"),
                 p("Download blank modifier\nstructure worksheet"),
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
                              "Confirm: use custom upload",
                              style="color: #fff; background-color: #FFBA49; border-color: #1C110A"),
                 actionButton("resetToDefaultDoubleCount",
                              "Reset: use default modifier",
                              style="color: #fff; background-color: #FFBA49; border-color: #1C110A")
               ),
               h3("Step 7 of 9: Set Mobility Modifier"),
               h4("This is used to address mobility of girls in and out of the DREAMS district. This includes: 1) girls leaving the district who have completed programming and so are no longer in the numerator or denominator; 2) girls being enrolled in DREAMS programming who are not resident in the district and so not included in the denominator."),
               wellPanel(
                 strong("Mobility modifier default: 0% modifier"),
                 p("Use default value or upload a custom modifier structure"),
                 br(),
                 br(),
                 strong("Custom modifier step 7a"),
                 p("Download blank mobility\nmodifier worksheet"),
                 downloadButton("blankTemplateDownloadMobility",
                                "Download blank template"),
                 br(),
                 br(),
                 strong("Step 7b"),
                 p("Open worksheet in Excel, fill out 'Mobility' columns and save"),
                 strong("Step 7c"),
                 p("Upload completed mobillity\nmodifier worksheet"),
                 DT::dataTableOutput("customMobilityTable"),
                 fileInput("completedTemplateUploadMobility",
                           "Upload completed template (.xlsx only)",
                           accept = ".xlsx"),
                 actionButton("confirmCustomMobility",
                              "Confirm: use custom upload",
                              style="color: #fff; background-color: #FFBA49; border-color: #1C110A"),
                 actionButton("resetToDefaultMobility",
                              "Reset: use default modifier",
                              style="color: #fff; background-color: #FFBA49; border-color: #1C110A")
               ),
               h3("Step 8 of 9: Set Enrollment Modifier"),
               h4("This addresses mismatches in enrollment criteria from vulnerability criteria, e.g., food insecurity may allow for enrollment, but would not have qualified that same person for 'vulnerability' in the denominator."),
               wellPanel(
                 strong("Enrollment modifier default: 5%"),
                 p("Use default value or upload a custom modifier structure"),
                 br(),
                 br(),
                 strong("Custom modifier step 8a"),
                 p("Download blank enrollment\nmodifier worksheet"),
                 downloadButton("blankTemplateDownloadEnrollment",
                                "Download blank template"),
                 br(),
                 br(),
                 strong("Step 8b"),
                 p("Open worksheet in Excel, fill out 'Enrollment' columns and save"),
                 strong("Step 8c"),
                 p("Upload completed enrollment\nmodifier worksheet"),
                 DT::dataTableOutput("customEnrollmentTable"),
                 fileInput("completedTemplateUploadEnrollment",
                           "Upload completed template (.xlsx only)",
                           accept = ".xlsx"),
                 actionButton("confirmCustomEnrollment",
                              "Confirm: use custom upload",
                              style="color: #fff; background-color: #FFBA49; border-color: #1C110A"),
                 actionButton("resetToDefaultEnrollment",
                              "Reset: use default modifier",
                              style="color: #fff; background-color: #FFBA49; border-color: #1C110A")
               ),
               h3("Step 9 of 9: Accept Parameters and Save Your Work"),
               wellPanel(
                 actionButton("initializeSelection",
                              "Accept parameters and derive/re-derive statistics",
                              style="color: #fff; background-color: #FF6663; border-color: #1C110A"),
                 br(),
                 br(),
                 downloadButton("exportTokenParameters",
                                "Export save token - parameters",
                                style="color: #fff; background-color: #20A39E; border-color: #1C110A"),
                 br(),
                 br(),
                 downloadButton("exportTokenData",
                                "Export save token - data",
                                style="color: #fff; background-color: #20A39E; border-color: #1C110A"),
                 br(),
                 br()
               ),
               h3("Review Your Results"))),
      wellPanel(
        h4("2022 Figures"),
        br(),
        br(),
        DT::dataTableOutput("stats_2022"),
        strong("Download 2022 results"),
        br(),
        downloadButton("blankTemplateDownload2022Export",
                       "Download 2022 Saturation Export",
                       style="color: #fff; background-color: #20A39E; border-color: #1C110A"),
        br(),
        br(),
        h4("Saturation Map"),
        selectInput("saturationCohort",
                    "Select an age cohort:",
                    selected = "10-14",
                    choices = c("10-14",
                                "15-19",
                                "20-24",
                                "25-29")),
        fluidRow(
          column(12, 
                 leafletOutput("map_saturation")))
        ),
      wellPanel(
        h4("COP Export Figures"),
        DT::dataTableOutput("stats_COP"),
        strong("Download to paste into AGYW tab for COP"),
        br(),
        downloadButton("blankTemplateDownloadCOPExport",
                       "Download COP Export",
                       style="color: #fff; background-color: #20A39E; border-color: #1C110A")),
      wellPanel(
        h4("Numerator"),
        selectInput("numeratorsDistrict",
                    "Select a district:",
                    selected = "",
                    choices = ""
        ),
        p("Note: if this table is empty, please click the 'Accept parameters...' button in Step 9 to load numerator table"),
        br(),
        br(),
        DT::dataTableOutput("table_numerator")),
      
      br(),
      br(),
      p(
        tags$a(
          href = "https://www.census.gov/data/software/osds.html",
          "Open Source Dissemination System, U.S. Census Bureau"
        )
      ),
      p(
        tags$a(
          href = "https://github.com/sdupre11/DREAMS_Sat",
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
    }else{
      main_ui()
    }
  })
  
  # Setup reactiveValues objects ----
  params <- reactiveValues(
    country = "Botswana",
    
    popStructureType = "Default"#,
    # catchmentsSelected = ""
  )
  
  reactiveButtons <- reactiveValues(
    focusedAnalytic = "COP"
  )
  
  workingDataPost <- reactiveValues(
    data = NULL
  )
  
  data_stats_2022 <- reactiveValues(
    data = defaultStats2022
  )
  
  data_stats_COP <- reactiveValues(
    data = defaultStatsCOP
  )
  
  spatial <- reactiveValues(
    # sf0 = NULL,
    sf1 = botADM2.sf,
    # sf2 = botADM2.sf,
    sf1_DREAMS = NULL,
    sf1_notDREAMS = NULL,
    # catchments = NULL,
    #sf1_DREAMSNeighbors = NULL,
    # sf2_DREAMSNeighbors = botADM2.sf,
    zoom = NULL
  )
  
  ### MOVE THIS
  
  observeEvent(input$country, {
    if (input$country == "Botswana") {
      spatial$sf1 <- botADM2.sf
    } else if (input$country == "Cote d'Ivoire") {
      spatial$sf1 <- cdiADM2.sf
    } else if (input$country == "Eswatini") {
      spatial$sf1 <- eswADM1.sf
    } else if (input$country == "Haiti") {
      spatial$sf1 <- haiADM2.sf
    } else if (input$country == "Kenya") {
      spatial$sf1 <- kenADM1.sf
    } else if (input$country == "Lesotho") {
      spatial$sf1 <- lesADM1.sf
    } else if (input$country == "Malawi") {
      spatial$sf1 <- malADM1.sf
    } else if (input$country == "Mozambique") {
      spatial$sf1 <- mozADM2.sf
    } else if (input$country == "Namibia") {
      spatial$sf1 <- namADM2.sf
    } else if (input$country == "Rwanda") {
      spatial$sf1 <- rwaADM1.sf
    } else if (input$country == "South Africa") {
      spatial$sf1 <- safADM1.sf
    } else if (input$country == "Tanzania") {
      spatial$sf1 <- tanADM2.sf
    } else if (input$country == "Uganda") {
      spatial$sf1 <- ugaADM1.sf
    } else if (input$country == "Zambia") {
      spatial$sf1 <- zamADM1.sf
    } else if (input$country == "Zimbabwe") {
      spatial$sf1 <- zimADM2.sf
    }
    
    # if (input$country == "Cote d'Ivoire") {
    #   spatial$sf0 <- cdiADM0.sf
    # } else if (input$country == "Eswatini") {
    #   spatial$sf0 <- eswADM0.sf
    # } else if (input$country == "Haiti") {
    #   spatial$sf0 <- haiADM0.sf
    # }
    
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
    
    # spatial$sf2_DREAMS <- spatial$sf2 %>%
    #   dplyr::filter(DREAMSDistrict == "Yes")
    # 
    # spatial$sf2_notDREAMS <- spatial$sf2 %>%
    #   dplyr::filter(DREAMSDistrict == "No")
    
  })
  
  
  ###
  
  districts <- reactive({
    req(input$country)
    
    if(input$country == "Botswana") {
      a <- c("Bobirwa District", 
             "Mahalapye District", 
             "Serowe District",
             "Kgatleng District", 
             "Kweneng East District", 
             "North East District", 
             "Gaborone District", 
             "Southern District")
    } else if (input$country == "Cote d'Ivoire") {
      a <- c("Abobo-Est", 
             "Cocody-Bingerville",
             "Daloa",
             "Man")
    } else if (input$country == "Eswatini") {
      a <-  c("Hhohho", 
              "Lubombo",
              "Manzini",
              "Shiselweni")
    } else if (input$country == "Haiti") {
      a <- c("Dessalines",
             "Saint-Marc",
             "Cap-Haïtien",
             "Port-au-Prince")
    } else if (input$country == "Kenya") {
      a <- c("Homa Bay County", 
             "Kiambu County", 
             "Kisumu County", 
             "Migori County", 
             "Mombasa County", 
             "Nairobi County", 
             "Siaya County")
    } else if (input$country == "Lesotho") {
      a <- c("Berea", 
             "Mafeteng",
             "Maseru",
             "Mohale's Hoek")
    } else if (input$country == "Malawi") {
      a <- c("Blantyre District", 
             "Machinga District",
             "Zomba District")
    } else if (input$country == "Mozambique") {
      a <- c("Pemba",
             "Chokwe",
             "Chonguene",
             "Guija",
             "Limpopo",
             "Xai-Xai",
             "Maxixe",
             "Chimoio",
             "Boane",
             "Magude",
             "Manhiça",
             "Marracuene",
             "Matola",
             "Matutuine",
             "Moamba",
             "Namaacha",
             "Erati",
             "Nampula",
             "Beira",
             "Caia",
             "Gile",
             "Ile",
             "Inhassunge",
             "Lugela",
             "Maganja Da Costa",
             "Milange",
             "Mocuba",
             "Mocubela",
             "Namacurra",
             "Nicoadala",
             "Pebane",
             "Quelimane")
    } else if (input$country == "Namibia") {
      a <- c("Andara",
             "Nyangana",
             "Rundu",
             "Windhoek",
             "Oshakati",
             "Omuthiya",
             "Onandjokwe",
             "Tsumeb",
             "Katima Mulilo")
    } else if (input$country == "Rwanda") {
      a <- c("East",
             "Kigali City",
             "South")
    } else if (input$country == "South Africa") {
      a <- c("ec Alfred Nzo District Municipality",
             "nw Bojanala Platinum District Municipality",
             "ec Buffalo City Metropolitan Municipality",
             "lp Capricorn District Municipality",
             "wc City of Cape Town Metropolitan Municipality",
             "gp City of Johannesburg Metropolitan Municipality",
             "gp City of Tshwane Metropolitan Municipality",
             "nw Dr Kenneth Kaunda District Municipality",
             "mp Ehlanzeni District Municipality",
             "gp Ekurhuleni Metropolitan Municipality",
             "kz eThekwini Metropolitan Municipality",
             "mp Gert Sibande District Municipality",
             "fs Lejweleputswa District Municipality",
             "lp Mopani District Municipality",
             "nw Ngaka Modiri Molema District Municipality",
             "mp Nkangala District Municipality",
             "ec Oliver Tambo District Municipality",
             "gp Sedibeng District Municipality",
             "fs Thabo Mofutsanyane District Municipality",
             "kz Ugu District Municipality",
             "kz uMgungundlovu District Municipality",
             "kz Uthukela District Municipality",
             "kz King Cetshwayo District Municipality",
             "kz Zululand District Municipality")
    } else if (input$country == "Tanzania") {
      a <- c("Kahama TC",
             "Kyela DC",
             "Mbarali DC",
             "Mbeya City Council",
             "Msalala DC",
             "Mufindi DC",
             "Muleba DC",
             "Nyamagana MC",
             "Shinyanga DC",
             "Shinyanga MC",
             "Temeke MC",
             "Ushetu DC")
    } else if (input$country == "Uganda") {
      a <- c("Bukomansimbi District",
             "Gomba District",
             "Kalangala District",
             "Kyotera District",
             "Lwengo District",
             "Lyantonde District",
             "Masaka District",
             "Rakai District",
             "Sembabule District",
             "Wakiso District",
             "Kassanda District",
             "Luwero District",
             "Mityana District",
             "Mubende District",
             "Mukono District",
             "Kampala District",
             "Agago District",
             "Apac District",
             "Gulu District",
             "Kwania District",
             "Lira District",
             "Omoro District",
             "Oyam District",
             "Mbarara District")
    } else if (input$country == "Zambia") {
      a <- c("Chingola District",
             "Chipata District",
             "Kabwe District", 
             "Kapiri-Mposhi District",
             "Kasama District",
             "Kitwe District",
             "Livingstone District",
             "Luanshya District",
             "Lusaka District",
             "Mazabuka District",
             "Monze District",
             "Mongu District",
             "Mufulira District",
             "Ndola District")
    } else if (input$country == "Zimbabwe") {
      a <- c("Beitbridge",
             "Bubi",
             "Bulawayo",
             "Bulilima",
             "Chipinge",
             "Gwanda",
             "Gweru",
             "Insiza",
             "Lupane",
             "Makoni",
             "Mangwe",
             "Matobo",
             "Mazowe",
             "Mutare",
             "Nkayi",
             "Tsholotsho")
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
  
  observeEvent(input$country, {
    req(input$country)
    
    workingDataPost$data <- defaultData %>%
      dplyr::filter(country == input$country)
    
  })
  
  observeEvent(input$country, {
    req(input$country)
    
    
    data_stats_2022$data <- defaultStats2022 %>%
      dplyr::filter(Country == input$country)
    
    # workingDataPost$data <- defaultData %>%
    #   dplyr::filter(country == input$country)
    
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
  
  countryChoice <- reactive(
    
    params$country
    
  )
  
  observeEvent(input$initializeSelection, {
    req(!is.null(workingDataPost$data))
    
    workingDataPost$data <- workingDataPost$data %>%
      mutate(
        IsSelected = case_when(
          (PopStructure == popStructureChoice() & country == countryChoice()) ~ as.character("Selected"),
          
          TRUE ~ as.character("Unselected")
        )
      )
  })
  
  observeEvent(input$initializeSelection, {
    
    data_stats_2022$data <- workingDataPost$data %>%
      reduceTo2022Export()
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
             `Mobility standardized AGYW` = MobilityStandardizedAGYW_PREV_Sum,
             `Enrollment standardized AGYW` = EnrollmentStandardizedAGYW_PREV_Sum,
             `AGYW completed` = Actual_Served_2022
      )
  })
  
  observeEvent(input$confirmCustomPopulation, {
    req(customPopulation())
    req(defaultData)
    
    defaultData <- left_join(defaultData,
                             customPopulation(),
                             by = c("country" = "Country", 
                                    "AREA_NAME" = "District", 
                                    "ageasentered" = "AgeCohort"))
    
    defaultData$Pop_2018 <- defaultData$Pop_2018_Custom
    defaultData$Pop_2019 <- defaultData$Pop_2019_Custom
    defaultData$Pop_2020 <- defaultData$Pop_2020_Custom
    defaultData$Pop_2021 <- defaultData$Pop_2021_Custom
    defaultData$Pop_2022 <- defaultData$Pop_2022_Custom
    defaultData$Pop_2023 <- defaultData$Pop_2023_Custom
    
    workingDataPost$data <- defaultData %>%
      deriveStatistics() %>%
      group_by(ageasentered, country, AREA_NAME, PopStructure) %>%
      slice_head(n = 1)
    
  })
  
  observeEvent(input$resetToDefaultPopulation, {
    req(defaultData)
    
    defaultData$Pop_2018 <- defaultData$Pop_2018_Default
    defaultData$Pop_2019 <- defaultData$Pop_2019_Default
    defaultData$Pop_2020 <- defaultData$Pop_2020_Default
    defaultData$Pop_2021 <- defaultData$Pop_2021_Default
    defaultData$Pop_2022 <- defaultData$Pop_2022_Default
    defaultData$Pop_2023 <- defaultData$Pop_2023_Default
    
    workingDataPost$data <- defaultData %>%
      deriveStatistics() %>%
      group_by(ageasentered, country, AREA_NAME, PopStructure) %>%
      slice_head(n = 1)
    
  })
  
  observeEvent(input$confirmCustomPrevalence, {
    req(customPrevalence())
    req(defaultData)
    
    defaultData <- left_join(defaultData,
                             customPrevalence(),
                             by = c("country" = "Country", 
                                    "AREA_NAME" = "District", 
                                    "ageasentered" = "AgeCohort"))
    
    defaultData$Prev_2018 <- defaultData$Prevalence_2018_Custom
    defaultData$Prev_2019 <- defaultData$Prevalence_2019_Custom
    defaultData$Prev_2020 <- defaultData$Prevalence_2020_Custom
    defaultData$Prev_2021 <- defaultData$Prevalence_2021_Custom
    defaultData$Prev_2022 <- defaultData$Prevalence_2022_Custom
    defaultData$Prev_2023 <- defaultData$Prevalence_2023_Custom
    
    workingDataPost$data <- defaultData %>%
      deriveStatistics() %>%
      group_by(ageasentered, country, AREA_NAME, PopStructure) %>%
      slice_head(n = 1)
    
  })
  
  observeEvent(input$resetToDefaultPrevalence, {
    req(defaultData)
    
    defaultData <- defaultData %>%
      countrySpecificPrev()
    # 
    # defaultData$Prev_2018 <- 2
    # defaultData$Prev_2019 <- 2
    # defaultData$Prev_2020 <- 2
    # defaultData$Prev_2021 <- 2
    # defaultData$Prev_2022 <- 2
    # defaultData$Prev_2023 <- 2
    
    workingDataPost$data <- defaultData %>%
      deriveStatistics() %>%
      group_by(ageasentered, country, AREA_NAME, PopStructure) %>%
      slice_head(n = 1)
    
  })
  
  observeEvent(input$confirmCustomVulnerability, {
    req(customVulnerability())
    req(defaultData)
    
    defaultData <- left_join(defaultData,
                             customVulnerability(),
                             by = c("country" = "Country", 
                                    "AREA_NAME" = "District", 
                                    "ageasentered" = "AgeCohort"))
    
    defaultData$Vuln_2018 <- defaultData$Vulnerable_2018_Custom
    defaultData$Vuln_2019 <- defaultData$Vulnerable_2019_Custom
    defaultData$Vuln_2020 <- defaultData$Vulnerable_2020_Custom
    defaultData$Vuln_2021 <- defaultData$Vulnerable_2021_Custom
    defaultData$Vuln_2022 <- defaultData$Vulnerable_2022_Custom
    defaultData$Vuln_2023 <- defaultData$Vulnerable_2023_Custom
    
    workingDataPost$data <- defaultData %>%
      deriveStatistics() %>%
      group_by(ageasentered, country, AREA_NAME, PopStructure) %>%
      slice_head(n = 1)
    
  })
  
  observeEvent(input$resetToDefaultVulnerability, {
    req(defaultData)
    
    defaultData$Vuln_2018 <- 80
    defaultData$Vuln_2019 <- 80
    defaultData$Vuln_2020 <- 80
    defaultData$Vuln_2021 <- 80
    defaultData$Vuln_2022 <- 80
    defaultData$Vuln_2023 <- 80
    
    workingDataPost$data <- defaultData %>%
      deriveStatistics() %>%
      group_by(ageasentered, country, AREA_NAME, PopStructure) %>%
      slice_head(n = 1)
    
  })
  
  observeEvent(input$confirmCustomEnrollment, {
    req(customEnrollment())
    req(defaultData)
    
    defaultData <- left_join(defaultData,
                             customEnrollment(),
                             by = c("country" = "Country", 
                                    "AREA_NAME" = "District", 
                                    "ageasentered" = "AgeCohort"))
    
    defaultData$Enrollment_2018 <- defaultData$Enrollment_2018_Custom
    defaultData$Enrollment_2019 <- defaultData$Enrollment_2019_Custom
    defaultData$Enrollment_2020 <- defaultData$Enrollment_2020_Custom
    defaultData$Enrollment_2021 <- defaultData$Enrollment_2021_Custom
    defaultData$Enrollment_2022 <- defaultData$Enrollment_2022_Custom
    
    workingDataPost$data <- defaultData %>%
      deriveStatistics() %>%
      group_by(ageasentered, country, AREA_NAME, PopStructure) %>%
      slice_head(n = 1)
    # }
  })
  
  observeEvent(input$resetToDefaultEnrollment, {
    req(defaultData)
    
    defaultData$Enrollment_2018 <- 5
    defaultData$Enrollment_2019 <- 5
    defaultData$Enrollment_2020 <- 5
    defaultData$Enrollment_2021 <- 5
    defaultData$Enrollment_2022 <- 5
    
    workingDataPost$data <- defaultData %>%
      deriveStatistics() %>%
      group_by(ageasentered, country, AREA_NAME, PopStructure) %>%
      slice_head(n = 1)
    
  })
  
  observeEvent(input$confirmCustomMobility, {
    req(customMobility())
    req(defaultData)
    
    defaultData <- left_join(defaultData,
                             customMobility(),
                             by = c("country" = "Country", 
                                    "AREA_NAME" = "District", 
                                    "ageasentered" = "AgeCohort"))
    
    defaultData$Mobility_2018 <- defaultData$Mobility_2018_Custom
    defaultData$Mobility_2019 <- defaultData$Mobility_2019_Custom
    defaultData$Mobility_2020 <- defaultData$Mobility_2020_Custom
    defaultData$Mobility_2021 <- defaultData$Mobility_2021_Custom
    defaultData$Mobility_2022 <- defaultData$Mobility_2022_Custom
    
    workingDataPost$data <- defaultData %>%
      deriveStatistics() %>%
      group_by(ageasentered, country, AREA_NAME, PopStructure) %>%
      slice_head(n = 1)
    # }
  })
  
  observeEvent(input$resetToDefaultMobility, {
    req(defaultData)
    
    defaultData$Mobility_2018 <- 0
    defaultData$Mobility_2019 <- 0
    defaultData$Mobility_2020 <- 0
    defaultData$Mobility_2021 <- 0
    defaultData$Mobility_2022 <- 0
    
    workingDataPost$data <- defaultData %>%
      deriveStatistics() %>%
      group_by(ageasentered, country, AREA_NAME, PopStructure) %>%
      slice_head(n = 1)
    
  })
  
  observeEvent(input$confirmCustomDoubleCount, {
    req(customDoubleCount())
    req(defaultData)
    
    defaultData <- left_join(defaultData,
                             customDoubleCount(),
                             by = c("country" = "Country", 
                                    "AREA_NAME" = "District", 
                                    "ageasentered" = "AgeCohort"))
    
    defaultData$PSDC_2018 <- defaultData$PSDC_2018_Custom
    defaultData$PSDC_2019 <- defaultData$PSDC_2019_Custom
    defaultData$PSDC_2020 <- defaultData$PSDC_2020_Custom
    defaultData$PSDC_2021 <- defaultData$PSDC_2021_Custom
    defaultData$PSDC_2022 <- defaultData$PSDC_2022_Custom
    
    workingDataPost$data <- defaultData %>%
      deriveStatistics() %>%
      group_by(ageasentered, country, AREA_NAME, PopStructure) %>%
      slice_head(n = 1)
    
  })
  
  observeEvent(input$resetToDefaultDoubleCount, {
    req(defaultData)
    
    defaultData$PSDC_2018 <- 15
    defaultData$PSDC_2019 <- 15
    defaultData$PSDC_2020 <- 15
    defaultData$PSDC_2021 <- 15
    defaultData$PSDC_2022 <- 15
    
    workingDataPost$data <- defaultData %>%
      deriveStatistics() %>%
      group_by(ageasentered, country, AREA_NAME, PopStructure) %>%
      slice_head(n = 1)
    
  })
  
  observeEvent(customPopStructure(), {
    req(customPopStructure())
    req(workingDataPost$data)
    
    workingDataPost$data <- left_join(workingDataPost$data,
                                      customPopStructure(),
                                      by = c("country" = "Country",
                                             "AREA_NAME" = "District",
                                             "ageasentered" = "ageasentered"))
    
    workingDataPost$data$firstQ_2018 <- workingDataPost$data$firstQCustom_2018
    workingDataPost$data$firstQ_2019 <- workingDataPost$data$firstQCustom_2019
    workingDataPost$data$firstQ_2020 <- workingDataPost$data$firstQCustom_2020
    workingDataPost$data$firstQ_2021 <- workingDataPost$data$firstQCustom_2021
    workingDataPost$data$firstQ_2022 <- workingDataPost$data$firstQCustom_2022
    
    workingDataPost$data$secondQ_2018 <- workingDataPost$data$secondQCustom_2018
    workingDataPost$data$secondQ_2019 <- workingDataPost$data$secondQCustom_2019
    workingDataPost$data$secondQ_2020 <- workingDataPost$data$secondQCustom_2020
    workingDataPost$data$secondQ_2021 <- workingDataPost$data$secondQCustom_2021
    workingDataPost$data$secondQ_2022 <- workingDataPost$data$secondQCustom_2022
    
    workingDataPost$data$thirdQ_2018 <- workingDataPost$data$thirdQCustom_2018
    workingDataPost$data$thirdQ_2019 <- workingDataPost$data$thirdQCustom_2019
    workingDataPost$data$thirdQ_2020 <- workingDataPost$data$thirdQCustom_2020
    workingDataPost$data$thirdQ_2021 <- workingDataPost$data$thirdQCustom_2021
    workingDataPost$data$thirdQ_2022 <- workingDataPost$data$thirdQCustom_2022
    
    workingDataPost$data$fourthQ_2018 <- workingDataPost$data$fourthQCustom_2018
    workingDataPost$data$fourthQ_2019 <- workingDataPost$data$fourthQCustom_2019
    workingDataPost$data$fourthQ_2020 <- workingDataPost$data$fourthQCustom_2020
    workingDataPost$data$fourthQ_2021 <- workingDataPost$data$fourthQCustom_2021
    workingDataPost$data$fourthQ_2022 <- workingDataPost$data$fourthQCustom_2022
    
    workingDataPost$data$fifthQ_2018 <- workingDataPost$data$fifthQCustom_2018
    workingDataPost$data$fifthQ_2019 <- workingDataPost$data$fifthQCustom_2019
    workingDataPost$data$fifthQ_2020 <- workingDataPost$data$fifthQCustom_2020
    workingDataPost$data$fifthQ_2021 <- workingDataPost$data$fifthQCustom_2021
    workingDataPost$data$fifthQ_2022 <- workingDataPost$data$fifthQCustom_2022
    
    workingDataPost$data <- workingDataPost$data %>%
      dplyr::select(-c(firstQCustom_2018,
                       firstQCustom_2019,
                       firstQCustom_2020,
                       firstQCustom_2021,
                       firstQCustom_2022,
                       secondQCustom_2018,
                       secondQCustom_2019,
                       secondQCustom_2020,
                       secondQCustom_2021,
                       secondQCustom_2022,
                       thirdQCustom_2018,
                       thirdQCustom_2019,
                       thirdQCustom_2020,
                       thirdQCustom_2021,
                       thirdQCustom_2022,
                       fourthQCustom_2018,
                       fourthQCustom_2019,
                       fourthQCustom_2020,
                       fourthQCustom_2021,
                       fourthQCustom_2022,
                       fifthQCustom_2018,
                       fifthQCustom_2019,
                       fifthQCustom_2020,
                       fifthQCustom_2021,
                       fifthQCustom_2022))
    
    workingDataPost$data <- workingDataPost$data %>%
      deriveStatistics() %>%
      group_by(ageasentered, country, AREA_NAME, PopStructure) %>%
      slice_head(n = 1)
    
  })
  
  # Render table elements ----
  # 
  output$customPopulationTable <- DT::renderDataTable({
    req(customPopulation())
    
    datatable(customPopulation(),
              rownames = FALSE,
              selection = 'none',
              colnames = c("Age Cohort" = "AgeCohort",
                           `2018` = "Pop_2018_Custom",
                           `2019` = "Pop_2019_Custom",
                           `2020` = "Pop_2020_Custom",
                           `2021` = "Pop_2021_Custom",
                           `2022` = "Pop_2022_Custom",
                           `2023` = "Pop_2023_Custom")
    )
  })
  
  output$customPrevalenceTable <- DT::renderDataTable({
    req(customPrevalence())
    
    datatable(customPrevalence(),
              rownames = FALSE,
              selection = 'none',
              colnames = c("Age Cohort" = "AgeCohort",
                           `2018` = "Prevalence_2018_Custom",
                           `2019` = "Prevalence_2019_Custom",
                           `2020` = "Prevalence_2020_Custom",
                           `2021` = "Prevalence_2021_Custom",
                           `2022` = "Prevalence_2022_Custom",
                           `2023` = "Prevalence_2023_Custom")
    )
  })
  
  output$customVulnerabilityTable <- DT::renderDataTable({
    req(customVulnerability())
    
    datatable(customVulnerability(),
              rownames = FALSE,
              selection = 'none',
              colnames = c("Age Cohort" = "AgeCohort",
                           `2018` = "Vulnerable_2018_Custom",
                           `2019` = "Vulnerable_2019_Custom",
                           `2020` = "Vulnerable_2020_Custom",
                           `2021` = "Vulnerable_2021_Custom",
                           `2022` = "Vulnerable_2022_Custom",
                           `2023` = "Vulnerable_2023_Custom")
    )
  })
  
  output$customStructureTable <- DT::renderDataTable({
    req(customPopStructure())
    
    a <- customPopStructure() %>%
      prepQDataforPopStructurePlots() %>%
      dplyr::select(-c("Join_Name", "ageasentered")) %>%
      pivot_wider(
        names_from = c("fiscal_year"),
        values_from = "prop"
      )
    
    
    
    datatable(a,
              rownames = FALSE,
              selection = 'none',
              colnames = c("Age" = "age")
    )
  })
  
  output$customMobilityTable <- DT::renderDataTable({
    req(customMobility())
    
    datatable(customMobility(),
              rownames = FALSE,
              selection = 'none',
              colnames = c("Age Cohort" = "AgeCohort",
                           `2018` = "Mobility_2018_Custom",
                           `2019` = "Mobility_2019_Custom",
                           `2020` = "Mobility_2020_Custom",
                           `2021` = "Mobility_2021_Custom",
                           `2022` = "Mobility_2022_Custom")
    )
  })
  
  output$customEnrollmentTable <- DT::renderDataTable({
    req(customEnrollment())
    
    datatable(customEnrollment(),
              rownames = FALSE,
              selection = 'none',
              colnames = c("Age Cohort" = "AgeCohort",
                           `2018` = "Enrollment_2018_Custom",
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
                           `2018` = "PSDC_2018_Custom",
                           `2019` = "PSDC_2019_Custom",
                           `2020` = "PSDC_2020_Custom",
                           `2021` = "PSDC_2021_Custom",
                           `2022` = "PSDC_2022_Custom")
    )
  })
  
  output$stats_2022 <- DT::renderDataTable({
    req(data_stats_2022$data)
    
    a <- data_stats_2022$data %>%
      group_by(Country,
               District,
               Cohort) %>%
      dplyr::filter(Country == input$country) %>%
      summarize(`Est. Female Population (FY22)` = mean(`Pop_2022`),
                `Total DREAMS-eligible AGYW (FY22)` = mean(`Total DREAMS eligible AGYW`),
                `Percent coverage (saturation) (FY22)` = mean(`Percent coverage (saturation)`),
                `Remaining unserved AGYW (FY22)` = mean(`Remaining unserved AGYW`)) %>%
      ungroup()
    
    datatable(a %>%
                dplyr::select(-c("Country")),
              rownames = FALSE,
              selection = 'none'
    )
  })
  
  output$stats_COP <- DT::renderDataTable({
    req(data_stats_COP$data)
    
    a <- data_stats_COP$data %>%
      group_by(Country,
               District,
               Cohort) %>%
      dplyr::filter(Country == input$country) %>%
      summarize(`Est. Female Population (FY23)` = mean(`Pop_2023`),
                `H.C. Est. # of Vulnerable AGYW (FY23)` = mean(`Total DREAMS eligible AGYW 2023`),
                `DREAMS Sat App Percent Coverage - Saturation (%, FY23)` = mean(`Percent coverage (saturation) 2023`),
                `DREAMS Sat App DREAMS Eligible Girls Not Yet Reached (FY23)` = mean(`Remaining unserved AGYW 2023`)) %>%
      ungroup()
    
    datatable(a %>%
                dplyr::select(-c("Country")),
              rownames = FALSE,
              selection = 'none'
    )
  })
  
  output$table_numerator <- DT::renderDataTable({
    req(data_stats_analytics())
    
    col_order <- c("Cohort",
                   "AGYW_PREV total",
                   "P/S* deduplicated AGYW",
                   "Mobility standardized AGYW",
                   "Enrollment standardized AGYW",
                   "AGYW completed"
    )
    
    a <- data_stats_analytics() %>%
      dplyr::filter(IsSelected == "Selected" & District == input$numeratorsDistrict) %>%
      dplyr::select(-c("District",
                       "Sat_2022",
                       "Prev_2022",
                       "Vuln_2022",
                       "Prev_2023",
                       "Vuln_2023",
                       "PSDC_2022",
                       "Enrollment_2022",
                       "Mobility_2022",
                       "IsSelected",
                       "Pop_2022",
                       "Pop_2022_Default",
                       "Pop_2023",
                       "Pop_2023_Default",
                       "PLHIV_2022",
                       "NonPLHIV_2022",
                       "VulnerableNonPLHIV_2022",
                       "VulnerableNonPLHIV_2023"))
    
    a <- a[, col_order]
    
    b <- a %>%
      group_by(Cohort) %>%
      summarize(`AGYW_PREV total` = mean(`AGYW_PREV total`),
                `P/S* deduplicated AGYW` = mean(`P/S* deduplicated AGYW`),
                `Mobility standardized AGYW` = mean(`Mobility standardized AGYW`),
                `Enrollment standardized AGYW` = mean(`Enrollment standardized AGYW`),
                `AGYW completed` = mean(`AGYW completed`)
      )
    
    datatable(
      b,
      rownames = FALSE,
      caption = "*Primary/Secondary"
    )
    
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
                       1)
      ) +
      ggtitle("Default",
              subtitle = "Even 20%") +
      xlab("Age band") +
      ylab("Proportion of cohort") +
      lims(y = c(0, 40)) +
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
      lims(y = c(0, 40)) +
      theme_plot(legend.position = "none")
    
    return(a)
  })
  
  ### Custom structure plot ----
  output$popStructure_CustomPlot <- renderPlot({
    
    custDF <- customPopStructure() %>%
      prepQDataforPopStructurePlots() %>%
      filter(fiscal_year==input$popStructureYear & Country == input$country & District == input$popStructureDistrict)
    
    # age_custom <- c(10:29)
    # prop_custom <- rep(0, 20)
    # sequence_custom <- c("10-14", "15-19", "20-24", "25-29")
    # ageasentered_custom <- rep(sequence_custom, each = 5)
    # 
    # emptyDF <- data.frame(age_custom, 
    #                       prop_custom,
    #                       ageasentered_custom)
    
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
      lims(y = c(0, 40)) +
      theme_plot(legend.position = "none")
    
    return(a)
    
    # b <- emptyDF %>%
    #   ggplot(
    #     aes(
    #       x = age_custom,
    #       y = prop_custom,
    #       fill = ageasentered_custom)) +
    #   geom_bar(stat="identity") +
    #   coord_flip() +
    #   scale_fill_manual(
    #     values = alpha(c("10-14" = "#FF6663",
    #                      "15-19" = "#FFBA49",
    #                      "20-24" = "#FF6663",
    #                      "25-29" = "#FFBA49"),
    #                    1)
    #   ) +
    #   ggtitle("Custom",
    #           subtitle = "Uploaded") +
    #   xlab("") +
    #   ylab("") +
    #   lims(y = c(0, 40)) +
    #   theme_plot(legend.position = "none")
    # 
    # if (input$structure != "Custom")  {
    #   return(b)
    # } else {
    #   return(a)
    # }
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
    
    if (input$country=="Eswatini") {
      leafletProxy("map_main"
      ) %>%
        clearShapes() %>%
        clearControls() %>%
        addPolygons(data = spatial$sf1_notDREAMS,
                    color = "black",
                    fillColor = "white",
                    weight = 1,
                    opacity = 1,
                    fillOpacity = 1,
                    popup = popup_NonDREAMS) %>%
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
    } else {
      leafletProxy("map_main"
      ) %>%
        clearShapes() %>%
        clearControls() %>%
        addPolygons(data = spatial$sf1_notDREAMS,
                    color = "black",
                    fillColor = "white",
                    weight = 1,
                    opacity = 1,
                    fillOpacity = 1,
                    popup = popup_NonDREAMS) %>%
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
      
    }
  })
  
  ## Saturation map ----
  saturation_bins <- c(0, 25.0, 50.0, 75.0, 100.0, Inf)
  
  saturation_labels <- c("0.0 - 24.9", "25.0 - 49.9", "50.0 - 74.9", "75.0 - 99.9", "100.0 or more")
  
  output$map_saturation <- leaflet::renderLeaflet({
    req(data_stats_2022$data)
    
    internal_df <- data_stats_2022$data %>%
      dplyr::filter(Cohort == input$saturationCohort)
    
    selected_country_DREAMS_joined <- merge(spatial$sf1_DREAMS,
                                            internal_df,
                                            by.x = "JOIN_NAME",
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
      addPolygons(data = spatial$sf1_notDREAMS,
                  color = "black",
                  fillColor = "white",
                  weight = 1,
                  opacity = 1,
                  fillOpacity = 0) %>%
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
      setView(lng = mean(st_bbox(spatial$sf1)[c(1,3)]),
              lat = mean(st_bbox(spatial$sf1)[c(2,4)]),
              zoom = spatial$zoom) %>%
      setMapWidgetStyle(list(background = "white"))
    # addResetMapButton() #currently doesn't work correctly, figure out how to set to go to the new polygons
  })
  
  observeEvent(input$saturationCohort, {
    #req(data_stats_2022$data)
    
    internal_df <- data_stats_2022$data %>%
      #dplyr::filter(Cohort == "10-14")
      dplyr::filter(Cohort == input$saturationCohort)
    
    selected_country_DREAMS_joined <- merge(spatial$sf1_DREAMS,
                                            internal_df,
                                            by.x = "JOIN_NAME",
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
    
    leafletProxy("map_saturation") %>%
      clearShapes() %>%
      clearControls() %>%
      addPolygons(data = spatial$sf1_notDREAMS,
                  color = "black",
                  fillColor = "white",
                  weight = 1,
                  opacity = 1,
                  fillOpacity = 0) %>%  
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
  
  output$exportTokenParameters <- downloadHandler(
    filename = function() {
      paste("DREAMS_Sat_Save_Token_Parameters",
            ".Rdata",
            sep = "")
    },
    content = function(file) {
      
      params_df <- data.frame(
        country = exportCountryListener(),
        popStructureType = exportPopStructureListener()#,
        # catchmentsSelected = exportCatchmentsListener()
      )
      
      save(params_df, file = file)
      
      # add send to S3
      # write a file to a directory ----
      target_directory <- "exportTokenParameters/" # change to your directory
      s3write_using(params_df, FUN = write.csv,
                    bucket = Sys.getenv("TEST_BUCKET_WRITE"),
                    object = paste0(
                      "system_dreams_saturation/",
                      target_directory,
                      "DREAMS_Sat_Save_Token_Parameters_",
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
      
      # add send to S3
      # write a file to a directory ----
      target_directory <- "exportTokenData/" # change to your directory
      s3write_using(workingDataExport, FUN = write.csv,
                    bucket = Sys.getenv("TEST_BUCKET_WRITE"),
                    object = paste0(
                      "system_dreams_saturation/",
                      target_directory,
                      "DREAMS_Sat_Save_Token_Data_",
                      Sys.time(),
                      ".csv"
                    )
      )
      
    },
    
    contentType = NULL
  )
  
  
  
  
  ## Import ----
  ### Parameters ----
  importedTokenParams <- eventReactive(input$importTokenParams, {
    if ( is.null(input$importTokenParams)) return(NULL)
    
    load(input$importTokenParams$datapath)
    
    if (exists("params_df")) {
    
    a <- params_df
    
    return(a)
    }
    
    
  })
  
  observeEvent(importedTokenParams(), {
    params$country <- importedTokenParams()$country[1]
    params$popStructureType <- importedTokenParams()$popStructureType[1]
    
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
  
  # DownloadHandlers ----
  ## Downloads ----
  ### 2022 Export, not COP, (FY22 Figures) ----
  selectedCountry_2022 <- reactive({
    req(input$country)
    req(data_stats_2022$data)
    
    a <- data_stats_2022$data %>%
      group_by(Country,
               District,
               Cohort) %>%
      dplyr::filter(Country == input$country) %>%
      summarize(`Est. Female Population (FY22)` = mean(`Pop_2022`),
                `Total DREAMS-eligible AGYW (FY22)` = mean(`Total DREAMS eligible AGYW`),
                `Percent coverage (saturation) (FY22)` = mean(`Percent coverage (saturation)`),
                `Remaining unserved AGYW (FY22)` = mean(`Remaining unserved AGYW`)) %>%
      ungroup() #%>%
    # dplyr::select(-c("Country"))
    
    return(a)
    
  })
  
  output$blankTemplateDownload2022Export <- downloadHandler(
    filename = function() {
      paste("2022Export", 
            ".xlsx",
            sep = "")
    },
    
    content = function(file) {
      write_xlsx(selectedCountry_2022(), 
                 path = file)
    },
    contentType = NULL
  )
  
  ### COP Export (FY23 Figures) ----
  selectedCountry_COP <- reactive({
    req(input$country)
    req(data_stats_COP$data)
    
    a <- data_stats_COP$data %>%
      group_by(Country,
               District,
               Cohort) %>%
      dplyr::filter(Country == input$country) %>%
      summarize(`Est. Female Population (FY23)` = mean(`Pop_2023`),
                `H.C. Est. # of Vulnerable AGYW (FY23)` = mean(`Total DREAMS eligible AGYW 2023`),
                `DREAMS Sat App Percent Coverage - Saturation (%, FY23)` = mean(`Percent coverage (saturation) 2023`),
                `DREAMS Sat App DREAMS Eligible Girls Not Yet Reached (FY23)` = mean(`Remaining unserved AGYW 2023`)) %>%
      ungroup() #%>%
    # dplyr::select(-c("Country"))
    
    return(a)
    
  })
  
  output$blankTemplateDownloadCOPExport <- downloadHandler(
    filename = function() {
      paste("COPExport_AGYW_Tab", 
            ".xlsx",
            sep = "")
    },
    
    content = function(file) {
      write_xlsx(selectedCountry_COP(), 
                 path = file)
    },
    contentType = NULL
  )
  
  ### Population ----
  default5YearTemplate_selectedCountry_Pop <- reactive({
    req(input$country)
    
    a <- default5YearTemplate %>%
      dplyr::filter(Country == input$country) %>%
      dplyr::select(c("Country",
                      "District",
                      "AgeCohort",
                      "Pop_2018",
                      "Pop_2019",
                      "Pop_2020",
                      "Pop_2021",
                      "Pop_2022",
                      "Pop_2023"))
    
    return(a)
    
  })
  
  output$blankTemplateDownloadPopulation <- downloadHandler(
    filename = function() {
      paste("blankPopulationTemplate", 
            ".xlsx",
            sep = "")
    },
    
    content = function(file) {
      write_xlsx(default5YearTemplate_selectedCountry_Pop(), 
                 path = file)
    },
    contentType = NULL
  )
  
  ### Prevalence ----
  default5YearTemplate_selectedCountry_P <- reactive({
    req(input$country)
    
    a <- default5YearTemplate %>%
      dplyr::filter(Country == input$country) %>%
      dplyr::select(c("Country",
                      "District",
                      "AgeCohort",
                      "Prevalence_2018",
                      "Prevalence_2019",
                      "Prevalence_2020",
                      "Prevalence_2021",
                      "Prevalence_2022",
                      "Prevalence_2023"))
    
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
                      "Vulnerable_2018",
                      "Vulnerable_2019",
                      "Vulnerable_2020",
                      "Vulnerable_2021",
                      "Vulnerable_2022",
                      "Vulnerable_2023"))
    
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
  
  ### Mobility ----
  default5YearTemplate_selectedCountry_M <- reactive({
    req(input$country)
    
    a <- default5YearTemplate %>%
      dplyr::filter(Country == input$country) %>%
      dplyr::select(c("Country",
                      "District",
                      "AgeCohort",
                      "Mobility_2018",
                      "Mobility_2019",
                      "Mobility_2020",
                      "Mobility_2021",
                      "Mobility_2022"))
    
    return(a)
    
  })
  
  output$blankTemplateDownloadMobility <- downloadHandler(
    filename = function() {
      paste("blankMobilityTemplate", 
            ".xlsx",
            sep = "")
    },
    
    content = function(file) {
      write_xlsx(default5YearTemplate_selectedCountry_M(), 
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
                      "Enrollment_2018",
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
                      "PrimarySecondaryDoubleCounts_2018",
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
  ### Population ----
  customPopulation <- reactive({
    req(input$completedTemplateUploadPopulation)
    
    file <- input$completedTemplateUploadPopulation
    ext <- tools::file_ext(file$datapath)
    
    a <- readxl::read_xlsx(file$datapath) 
    
    required_columns <- c("Pop_2018",
                          "Pop_2019", 
                          "Pop_2020",
                          "Pop_2021",
                          "Pop_2022",
                          "Pop_2023")
    
    column_names <- colnames(a)
    
    shiny::validate(need(ext == "xlsx", ""),
                    need(all(required_columns %in% column_names), ""))
    
    b <- a %>%
      rename(Pop_2018_Custom = Pop_2018,
             Pop_2019_Custom = Pop_2019,
             Pop_2020_Custom = Pop_2020,
             Pop_2021_Custom = Pop_2021,
             Pop_2022_Custom = Pop_2022,
             Pop_2023_Custom = Pop_2023)
    
    return(b)
  })
  
  ### Prevalence ----
  customPrevalence <- reactive({
    req(input$completedTemplateUploadPrevalence)
    
    file <- input$completedTemplateUploadPrevalence
    ext <- tools::file_ext(file$datapath)
    
    a <- readxl::read_xlsx(file$datapath) 
    
    required_columns <- c("Prevalence_2018",
                          "Prevalence_2019", 
                          "Prevalence_2020",
                          "Prevalence_2021",
                          "Prevalence_2022",
                          "Prevalence_2023")
    
    column_names <- colnames(a)
    
    shiny::validate(need(ext == "xlsx", ""),
                    need(all(required_columns %in% column_names), ""))
    
    b <- a %>%
      rename(Prevalence_2018_Custom = Prevalence_2018,
             Prevalence_2019_Custom = Prevalence_2019,
             Prevalence_2020_Custom = Prevalence_2020,
             Prevalence_2021_Custom = Prevalence_2021,
             Prevalence_2022_Custom = Prevalence_2022,
             Prevalence_2023_Custom = Prevalence_2023)
    
    return(b)
  })
  
  ### Vulnerability ----
  customVulnerability <- reactive({
    req(input$completedTemplateUploadVulnerability)
    
    file <- input$completedTemplateUploadVulnerability
    ext <- tools::file_ext(file$datapath)
    
    a <- readxl::read_xlsx(file$datapath) 
    
    required_columns <- c("Vulnerable_2018",
                          "Vulnerable_2019", 
                          "Vulnerable_2020",
                          "Vulnerable_2021",
                          "Vulnerable_2022",
                          "Vulnerable_2023")
    
    column_names <- colnames(a)
    
    shiny::validate(need(ext == "xlsx", ""),
                    need(all(required_columns %in% column_names), ""))
    
    b <- a %>%
      rename(Vulnerable_2018_Custom = Vulnerable_2018,
             Vulnerable_2019_Custom = Vulnerable_2019,
             Vulnerable_2020_Custom = Vulnerable_2020,
             Vulnerable_2021_Custom = Vulnerable_2021,
             Vulnerable_2022_Custom = Vulnerable_2022,
             Vulnerable_2023_Custom = Vulnerable_2023)
    
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
      rename(firstQCustom_2018 = firstQ_2018,
             firstQCustom_2019 = firstQ_2019,
             firstQCustom_2020 = firstQ_2020,
             firstQCustom_2021 = firstQ_2021,
             firstQCustom_2022 = firstQ_2022,
             secondQCustom_2018 = secondQ_2018,
             secondQCustom_2019 = secondQ_2019,
             secondQCustom_2020 = secondQ_2020,
             secondQCustom_2021 = secondQ_2021,
             secondQCustom_2022 = secondQ_2022,
             thirdQCustom_2018 = thirdQ_2018,
             thirdQCustom_2019 = thirdQ_2019,
             thirdQCustom_2020 = thirdQ_2020,
             thirdQCustom_2021 = thirdQ_2021,
             thirdQCustom_2022 = thirdQ_2022,
             fourthQCustom_2018 = fourthQ_2018,
             fourthQCustom_2019 = fourthQ_2019,
             fourthQCustom_2020 = fourthQ_2020,
             fourthQCustom_2021 = fourthQ_2021,
             fourthQCustom_2022 = fourthQ_2022,
             fifthQCustom_2018 = fifthQ_2018,
             fifthQCustom_2019 = fifthQ_2019,
             fifthQCustom_2020 = fifthQ_2020,
             fifthQCustom_2021 = fifthQ_2021,
             fifthQCustom_2022 = fifthQ_2022)
    
    
    return(a)
  })
  
  ### Mobility ----
  customMobility <- reactive({
    req(input$completedTemplateUploadMobility)
    
    file <- input$completedTemplateUploadMobility
    ext <- tools::file_ext(file$datapath)
    
    a <- readxl::read_xlsx(file$datapath) 
    required_columns <- c("Mobility_2018", 
                          "Mobility_2019", 
                          "Mobility_2020",
                          "Mobility_2021",
                          "Mobility_2022")
    
    column_names <- colnames(a)
    
    shiny::validate(need(ext == "xlsx", ""),
                    need(all(required_columns %in% column_names), ""))
    
    b <- a %>%
      rename(Mobility_2018_Custom = Mobility_2018,
             Mobility_2019_Custom = Mobility_2019,
             Mobility_2020_Custom = Mobility_2020,
             Mobility_2021_Custom = Mobility_2021,
             Mobility_2022_Custom = Mobility_2022)
    
    return(b)
  })
  
  ### Enrollment ----
  customEnrollment <- reactive({
    req(input$completedTemplateUploadEnrollment)
    
    file <- input$completedTemplateUploadEnrollment
    ext <- tools::file_ext(file$datapath)
    
    a <- readxl::read_xlsx(file$datapath) 
    required_columns <- c("Enrollment_2018", 
                          "Enrollment_2019", 
                          "Enrollment_2020",
                          "Enrollment_2021",
                          "Enrollment_2022")
    
    column_names <- colnames(a)
    
    shiny::validate(need(ext == "xlsx", ""),
                    need(all(required_columns %in% column_names), ""))
    
    b <- a %>%
      rename(Enrollment_2018_Custom = Enrollment_2018,
             Enrollment_2019_Custom = Enrollment_2019,
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
    
    required_columns <- c("PrimarySecondaryDoubleCounts_2018", 
                          "PrimarySecondaryDoubleCounts_2019", 
                          "PrimarySecondaryDoubleCounts_2020",
                          "PrimarySecondaryDoubleCounts_2021",
                          "PrimarySecondaryDoubleCounts_2022")
    
    column_names <- colnames(a)
    
    shiny::validate(need(ext == "xlsx", ""),
                    need(all(required_columns %in% column_names), ""))
    
    b <- a %>%
      rename(PSDC_2018_Custom = PrimarySecondaryDoubleCounts_2018,
             PSDC_2019_Custom = PrimarySecondaryDoubleCounts_2019,
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