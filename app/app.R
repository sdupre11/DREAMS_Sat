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
           glide(
             height = "600px",
             shinyglide::screen(
               column(4,
                      strong("Step 0: Select Your Country"),
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
               strong("Step 1"),
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
               actionButton("useDefaultParameters",
                            "Use defaults"),
               br(),
               br(),
               actionButton("initializeSelection",
                            "initialize selection")
             ),
             shinyglide::screen(
               column(4, 
                      strong("Step 2"),
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
                        actionButton("completedTemplateUploadStructure",
                                     "Upload completed template"))),
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
                                    "Select a district (custom only):",
                                    selected = "",
                                    choices = ""
                        )),
                      fluidRow(
                        column(4,
                               plotOutput("popStructure_DefaultPlot")),
                        column(4,
                               plotOutput("popStructure_NationalPlot"))#,
                        # column(4,
                        #        plotOutput("popStructure_CustomPlot"))
                      ))
               ),
             shinyglide::screen(
               strong("Step 3"),
               column(4,
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
               column(8,
                      textOutput("testing_catchments"),
                      leafletOutput("map_catchments"))
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
    column(12,
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
    updateSelectInput(session,
                      "popStructureDistrict",
                      choices = districts())
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
      attachParameters_1year(dataParameters_1Year) %>%
      merge(SingleYearNatAGYWPops)
    
    
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
  
  data_stats_COP <- eventReactive(input$initializeSelection, {
    
    selected_data <- workingDataPost$data %>%
      reduceToCOPExport()
  })
  
  
  output$workingDataPost_check <- DT::renderDataTable({
    req(workingDataPost)
    
    datatable(workingDataPost$data,
              options = list(scrollx = TRUE)
    )
  })
  
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
    
    custDF <- dataParameters_1Year %>%
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
      setMapWidgetStyle(list(background = "white")) %>%
      addFullscreenControl()
  })
  
  # output$popStructure_NatPlot <- renderPlot({
  #   req(workingDataPost)
  #   # make so can select a District for Custom plots
  #   a <- workingDataPost$data %>%
  #     ggplot(
  #       aes(x = )
  #     )
  # 
  # })
  
  
  output$map_catchments <- leaflet::renderLeaflet({
    a <- leaflet() %>%
      setMapWidgetStyle(list(background = "white"))
  })
  
  catchments_filtered <- reactive(
    a <- neighborsLookup[neighborsLookup$parent %in% input$checkGroup_catchment, ])

  catchmentListener <- reactive({
    list(input$country, catchments_filtered())
  })
  
  observeEvent(catchmentListener(), {
    
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
      setMapWidgetStyle(list(background = "white")) %>%
      addFullscreenControl()
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