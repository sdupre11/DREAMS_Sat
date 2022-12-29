library(shiny)
library(leaflet)
library(leaflet.extras)
library(sf)
library(DT)
library(tidyverse)
library(shinyglide)
library(writexl)

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
               br(),
               br(),
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
               column(4,
                      strong("Step 2: Population structure (Default: 20%)"),
                      radioButtons("structure",
                                   "Set Population Structure:",
                                   c("Default (20%)" = "Default",
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
                        strong("Step 2b"),
                        p("Open worksheet in Excel, fill out 'proportion' column and save"),
                        strong("Step 2c"),
                        p("Upload completed population\nstructure worksheet"),
                        fileInput("completedTemplateUploadPopStructure",
                                  "Upload completed template (.xlsx only)",
                                  accept = ".xlsx"),
                        actionButton("confirmCustomPopStructure",
                                     "Confirm: use custom upload")#,
                        # br(),
                        # br(),
                        # br(),
                        # tableOutput("table_check_pop")
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
               strong("Step 3: Catchment modifier (Default: No catchment modifier)"),
               column(4,
                      checkboxGroupInput("checkGroup_catchment",
                                         label = "Apply DREAMS catchment to:",
                                         choices = "")#,
                      # checkboxInput("catchment",
                      #               "Apply DREAMS catchment modifier (DEV ONLY):",
                      #               value = FALSE)#,
                      # conditionalPanel(
                      #   condition = "input.catchment == 1",
                      #   radioButtons("catchment_upordown",
                      #                "Adjust denominator(s) up or numerator(s) down",
                      #                c("Denominator up" = "dUp",
                      #                  "Numerator down" = "nDown"))
                      # )
               ),
               column(8,
                      # textOutput("testing_catchments"),
                      leafletOutput("map_catchments"))
             ),
             shinyglide::screen(
               strong("Step 4: Enrollment modifier (Default: 3%)"),
               p("Use default value or upload a custom modifier structure"),
               strong("Custom Structure step 4a"),
               p("Download blank enrollment\nmodifier worksheet"),
               downloadButton("blankTemplateDownloadEnrollment",
                              "Download blank template"),
               br(),
               br(),
               strong("Step 4b"),
               p("Open worksheet in Excel, fill out 'Enrollment' columns and save"),
               strong("Step 4c"),
               p("Upload completed enrollment\nmodifier worksheet"),
               # tableOutput("table_check"),
               fileInput("completedTemplateUploadEnrollment",
                         "Upload completed template (.xlsx only)",
                         accept = ".xlsx"),
               actionButton("confirmCustomEnrollment",
                            "Confirm: use custom upload"),
               actionButton("resetToDefaultEnrollment",
                            "Reset: use default modifier")
             ),
             shinyglide::screen(
               strong("Step 5: Double count modifier (Default: 1%)"),
               p("Use default value or upload a custom structure"),
               strong("Custom Structure step 5a"),
               p("Download blank population\nstructure worksheet"),
               downloadButton("blankTemplateDownloadDoubleCount",
                              "Download blank template"),
               br(),
               br(),
               strong("Step 5b"),
               p("Open worksheet in Excel, fill out 'PrimarySecondaryDoubleCounts_20XX' columns and save"),
               strong("Step 5c"),
               p("Upload completed double count\nmodifier worksheet"),
               # tableOutput("table_check_dc"),
               fileInput("completedTemplateUploadDoubleCount",
                         "Upload completed template (.xlsx only)",
                         accept = ".xlsx"),
               actionButton("confirmCustomDoubleCount",
                            "Confirm: use custom upload"),
               actionButton("resetToDefaultDoubleCount",
                            "Reset: use default modifier")
             ),
             shinyglide::screen(
               strong("Step 6"),
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
    column(12,
           h3("Saturation Analytics"),
           column(4,
                  h4("Saturation"),
                  p("Mapped sat here")),
           column(4,
                  h4("Numerator")),
           column(4,
                  h4("Denominator"))
    )
  ),
  fluidRow(
    column(12,
           textOutput("testprint"),
           strong("Pop Structure Type:"),
           textOutput("params_popStructureType"),
           # strong("Catchment Flag:"),
           # textOutput("params_catchmentModifierFlag"),
           # strong("Enrollment Modifier Flag:"),
           # textOutput("params_enrollmentModifierFlag"),
           # strong("Double Grad Modifier Flag:"),
           # textOutput("params_doubleGradModifierFlag"),
           DT::dataTableOutput("workingDataPost_check"))
  )
)


server <- function(input, output, session) {
  
  params <- reactiveValues(
    country = "Botswana",
    # country_SF1 = botADM1.sf,
    # country_SF2 = botADM2.sf, #Adapt to allow for Lesotho case
    popStructureType = "Default",
    # catchmentModifierFlag = FALSE,
    catchmentModifierFlaggedDistricts = NULL
    # enrollmentModifierFlag = 0,
    # doubleGradModifierFlag = 0
  )
  
  workingDataPre <- reactiveValues(
    data = NULL
  )
  
  workingDataPost <- reactiveValues(
    data = NULL
  )
  
  # workingDataTempSelected <- reactiveValues(
  #   data = NULL
  # )
  # 
  # workingDataTempUnselected <- reactiveValues(
  #   data = NULL
  # )
  
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
  
  observeEvent(input$country, {
    updateSelectInput(session,
                      "popStructureDistrict",
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
  
  
  # Update parameters ----
  observeEvent(input$country, {
    params$country = input$country
  })
  
  
  
  #### PARAMS CHECK PANEL
  output$params_country <- renderText(params$country)
  output$params_popStructureType <- renderText(params$popStructureType)
  # output$params_catchmentModifierFlag <- renderText(params$catchmentModifierFlag)
  # output$params_enrollmentModifierFlag <- renderText(params$enrollmentModifierFlag)
  # output$params_doubleGradModifierFlag <- renderText(params$doubleGradModifierFlag)
  
  #### PROCESSING BUTTONS
  
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
  
  # 
  # observeEvent(input$catchment, {
  #   
  #   if (input$catchment){
  #     params$catchmentModifierFlag <- TRUE
  #   } else {
  #     params$catchmentModifierFlag <- FALSE
  #   } 
  #   
  # })
  
  observeEvent(input$structure, {
    
    if (input$structure == "Default"){
      params$popStructureType <- "Default"
    } else if (input$structure == "National") {
      params$popStructureType <- "National"
    } else if (input$structure == "Custom") {
      params$popStructureType <- "Custom"
    }
    
  })
  
  # popCatchmentChoice <- reactive(
  #   
  #   if (params$catchmentModifierFlag == TRUE) {
  #     "Expanded"
  #   } 
  #   else {
  #     "DistrictOnly"
  #   }
  #   
  # )
  
  popStructureChoice <- reactive(
    
    params$popStructureType
    
  )
  
  catchmentDistricts <- reactive({
    a = input$checkGroup_catchment
    return(a)
  })
  
  output$testprint <- renderText(catchmentDistricts())
  
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
  # 
  # data_stats_analytics <- eventReactive(input$initializeSelection, {
  #   req(!is.null(workingDataPost$data))
  # 
  #   selected_data <- workingDataPost$data %>%
  #     reduceForAnalyticsPlots()
  # })
  
  output$workingDataPost_check <- DT::renderDataTable({
    req(workingDataPost)
    
    datatable(workingDataPost$data,
              options = list(scrollx = TRUE)
    )
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
  
  
  # output$table_check <- renderTable({
  #   req(customEnrollment())
  #   
  #   customEnrollment()
  # })
  
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
  
  # output$table_check_dc <- renderTable({
  #   req(customDoubleCount())
  #   
  #   customDoubleCount()
  # })
  
  # Render tables ----
  
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
  
  # Render plots ----
  
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
  
  output$popStructure_NationalPlot <- renderPlot({
    
    natDF <- SingleYearNatAGYWPops %>%
      prepQDataforPopStructurePlots() %>%
      filter(fiscal_year==input$popStructureYear & country == input$country)
    # filter(fiscal_year==!!input$popStructureYear)
    
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
  
  # Render maps ----
  
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
  
  
  output$map_catchments <- leaflet::renderLeaflet({
    a <- leaflet() %>%
      setMapWidgetStyle(list(background = "white")) %>%
      # addResetMapButton() %>% #currently doesn't work correctly, figure out how to set to go to the new polygons
      addFullscreenControl()
  })
  
  catchments_filtered <- reactive(
    a <- neighborsLookup[neighborsLookup$parent %in% input$checkGroup_catchment, ])
  
  catchmentMapListener <- reactive({
    list(input$country, catchments_filtered())
  })
  
  observeEvent(catchmentMapListener(), {
    
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
    
    selected_country_DREAMSNeighbors <- selected_country %>%
      filter(ADM1_NAME %in% catchments_filtered()$child)
    
    selected_country_NonDREAMS <- selected_country %>%
      filter(DREAMSDistrict == "No" & (!ADM1_NAME %in% catchments_filtered()$child))
    
    if (input$country %in% small_countries) {
      selected_zoom <- 7
    } else if (input$country %in% medium_countries) {
      selected_zoom <- 6
    } else if (input$country %in% large_countries) {
      selected_zoom <- 5
    }
    
    popup_DREAMS <- paste0("<strong>DREAMS District: </strong>",
                           selected_country_DREAMS$AREA_NAME)
    
    popup_DREAMSNeighbors <- paste0("<strong>DREAMS Neighbor District: </strong>",
                                    selected_country_DREAMSNeighbors$AREA_NAME)
    
    popup_NonDREAMS <- paste0("<strong>Non-DREAMS District: </strong>", 
                              selected_country_NonDREAMS$AREA_NAME)
    
    
    
    leafletProxy("map_catchments"
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
      addPolygons(data = selected_country_DREAMSNeighbors,
                  color = "black",
                  fillColor = "#20A39E",
                  weight = 1,
                  opacity = 1,
                  fillOpacity = 0.8,
                  popup = popup_DREAMSNeighbors,
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
  
  # Save token ----
  ## Download handler (move later)
  
  exportCountryListener <- reactive({
    params$country
  })
  
  exportPopStructureListener <- reactive({
    params$popStructureType
  })
  
  output$exportToken <- downloadHandler(
    filename = function() {
      paste("DREAMS_Sat_Save_Token",
            ".Rdata",
            sep = "")
    },
    content = function(file) {
      
      country <- exportCountryListener()
      popStructure <- exportPopStructureListener()
      
      export_df <- data.frame(
        country = exportCountryListener(),
        popStructure = exportPopStructureListener())
      
      save(export_df, file = file)
    },
    
    # content = function(file) {
    #   
    #   save(params, file = file)
    # },
    contentType = NULL
  )
  
  ## Import
  importedToken <- reactive({
    req(input$importToken)
    
    file <- input$importToken
    ext <- tools::file_ext(file$datapath)
    
    validate(need(ext == "Rdata",
                  "Please upload Rdata file"))
    
    a <- file$datapath %>%
      load()
    
    
    return(a)
  })
  
  output$table_check_import <- renderTable({
    req(importedToken())
    
    importedToken()
  })
  
  # observeEvent(importedToken(), {
  #   params$country <- importedToken()$country
  #   params$popStructureType <- importedToken()$popStructureType
  # 
  # })
  
  
  
  
  # Download handlers ----
  
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
  
  # output$table_check_pop <- renderTable({
  #   req(customPopStructure())
  #   
  #   customPopStructure()
  # })
  
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