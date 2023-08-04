server <- function(input, output, session) {
  # 1 Set up ----
  ## 1.1 Loading screen ----
  # Call initial loading screen
  hide(
    id = "loading-content",
    anim = TRUE,
    animType = "fade"
  )
  show("app-content")

  ## 1.2 Load chart colours ----
  # https://analysisfunction.civilservice.gov.uk/policy-store/data-visualisation-colours-in-charts/
  # England, geo1, geo2, then any others
  chartColors6 <-
    c(
      "#BFBFBF",
      "#12436D",
      "#28A197",
      "#801650",
      "#F46A25",
      "#A285D1",
      "#3D3D3D",
      "#2073BC",
      "#6BACE6"
    )
  # for when no England
  chartColors5 <-
    c(
      "#12436D",
      "#28A197",
      "#801650",
      "#F46A25",
      "#A285D1",
      "#3D3D3D",
      "#2073BC",
      "#6BACE6"
    )

  ## 1.3 Set up cookies
  # output if cookie is unspecified
  observeEvent(input$cookies, {
    if (!is.null(input$cookies)) {
      if (!("dfe_analytics" %in% names(input$cookies))) {
        shinyalert(
          inputId = "cookie_consent",
          title = "Cookie consent",
          text = "This site uses cookies to record traffic flow using Google Analytics",
          size = "s",
          closeOnEsc = TRUE,
          closeOnClickOutside = FALSE,
          html = FALSE,
          type = "",
          showConfirmButton = TRUE,
          showCancelButton = TRUE,
          confirmButtonText = "Accept",
          confirmButtonCol = "#AEDEF4",
          timer = 0,
          imageUrl = "",
          animation = TRUE
        )
      } else {
        msg <- list(
          name = "dfe_analytics",
          value = input$cookies$dfe_analytics
        )
        session$sendCustomMessage("analytics-consent", msg)
        if ("cookies" %in% names(input)) {
          if ("dfe_analytics" %in% names(input$cookies)) {
            if (input$cookies$dfe_analytics == "denied") {
              ga_msg <- list(name = paste0("_ga_", google_analytics_key))
              session$sendCustomMessage("cookie-remove", ga_msg)
            }
          }
        }
      }
    }
  })

  observeEvent(input$cookie_consent, {
    msg <- list(
      name = "dfe_analytics",
      value = ifelse(input$cookie_consent, "granted", "denied")
    )
    session$sendCustomMessage("cookie-set", msg)
    session$sendCustomMessage("analytics-consent", msg)
    if ("cookies" %in% names(input)) {
      if ("dfe_analytics" %in% names(input$cookies)) {
        if (input$cookies$dfe_analytics == "denied") {
          ga_msg <- list(name = paste0("_ga_", google_analytics_key))
          session$sendCustomMessage("cookie-remove", ga_msg)
        }
      }
    }
  })

  observeEvent(input$remove, {
    msg <- list(name = "dfe_analytics", value = "denied")
    session$sendCustomMessage("cookie-remove", msg)
    session$sendCustomMessage("analytics-consent", msg)
  })

  cookies_data <- reactive({
    input$cookies
  })

  output$cookie_status <- renderText({
    cookie_text_stem <- "To better understand the reach of our dashboard tools, this site uses cookies to identify numbers of unique users as part of Google Analytics. You have chosen to"
    cookie_text_tail <- "the use of cookies on this website."
    if ("cookies" %in% names(input)) {
      if ("dfe_analytics" %in% names(input$cookies)) {
        if (input$cookies$dfe_analytics == "granted") {
          paste(cookie_text_stem, "accept", cookie_text_tail)
        } else {
          paste(cookie_text_stem, "reject", cookie_text_tail)
        }
      }
    } else {
      paste("Cookies consent has not been confirmed.")
    }
  })

  # 2 Main page ----
  ## 2.1 Homepage ----
  ### 2.1.1 Make links ----
  # Create link to overview tab
  observeEvent(input$link_to_tabpanel_overview, {
    updateTabsetPanel(session, "navbar", "Overview")
  })

  # Create link to local skills tab
  observeEvent(input$link_to_tabpanel_localskills, {
    updateTabsetPanel(session, "navbar", "Local skills")
  })
  observeEvent(input$link_to_tabpanel_localskills2, {
    updateTabsetPanel(session, "navbar", "Local skills")
  })

  # Create link to further resources tab
  observeEvent(input$link_to_tabpanel_furtherresources, {
    updateTabsetPanel(session, "navbar", "Further resources")
  })

  # Create link to accessibility tab
  observeEvent(input$link_to_tabpanel_accessibility, {
    updateTabsetPanel(session, "navbar", "Accessibility")
  })

  # Create link to support and feedback tab
  observeEvent(input$link_to_tabpanel_supportandfeedback, {
    updateTabsetPanel(session, "navbar", "Support and feedback")
  })

  # Create link to employment data
  observeEvent(input$link_to_tabpanel_employment, {
    updateTabsetPanel(session, "navbar", "Local skills")
    updateSelectInput(session, "splashMetric",
      selected = "inemploymentRate"
    )
  })
  
  # Create link to employment data
  observeEvent(input$link_to_tabpanel_employment2, {
    updateTabsetPanel(session, "navbar", "Local skills")
    updateSelectInput(session, "splashMetric",
                      selected = "inemployment"
    )
  })
  
  # Create link to job advert
  observeEvent(input$link_to_tabpanel_vacancies, {
    updateTabsetPanel(session, "navbar", "Local skills")
    updateSelectInput(session, "splashMetric",
      selected = "vacancies"
    )
  })
  # Create link to skills data tab
  observeEvent(input$link_to_tabpanel_FE, {
    updateTabsetPanel(session, "navbar", "Local skills")
    updateSelectInput(session, "splashMetric",
      selected = "achievements_rate_per_100000_population"
    )
  })
  # Create link to enterprises
  observeEvent(input$link_to_tabpanel_enterprise, {
    updateTabsetPanel(session, "navbar", "Local skills")
    updateSelectInput(session, "splashMetric",
      selected = "enterpriseCount"
    )
  })
  # Create link to qualification
  observeEvent(input$link_to_tabpanel_qualification, {
    updateTabsetPanel(session, "navbar", "Local skills")
    updateSelectInput(session, "splashMetric",
      selected = "L3PlusRate"
    )
  })
  # Create link to destinations
  observeEvent(input$link_to_tabpanel_destinations, {
    updateTabsetPanel(session, "navbar", "Local skills")
    updateSelectInput(session, "splashMetric",
      selected = "sustainedPositiveDestinationKS4Rate"
    )
  })
  # Create link to working futures
  observeEvent(input$link_to_tabpanel_wf1, {
    updateTabsetPanel(session, "navbar", "Local skills")
    updateSelectInput(session, "splashMetric",
      selected = "employmentProjection"
    )
  })
  # Create link to data tab
  observeEvent(input$link_to_tabpanel_data, {
    updateTabsetPanel(session, "navbar", "Data sources")
  })
  ## 2.5 Data information ----
  ### 2.5.1 Data table downloads ----
  output$downloadData1 <- downloadHandler(
    filename = function() {
      "EmploymentVolumes.xlsx"
    },
    content = function(file) {
      write_xlsx(list("1b.Employment volumes" = C_datahub %>%
        filter(metric %in% c("all", "inemployment", "selfemployed", "unemployed", "inactive"), Breakdown != "Occupation", Breakdown != "Industry") %>%
        select(-metric, -Breakdown, -Subgroup) %>%
        rename("Volume" = valueText, Metric = metricNeat)), path = file)
    }
  )
  output$downloadData2 <- downloadHandler(
    filename = function() {
      "EmploymentByOccupation.xlsx"
    },
    content = function(file) {
      write_xlsx(list("1a.Employment by occupation" = C_datahub %>%
        filter(metric == "inemployment", Breakdown == "Occupation") %>%
        select(-metric, -metricNeat, -Breakdown) %>%
        rename("Employment volume" = valueText, Occupation = Subgroup)), path = file)
    }
  )
  output$downloadData3 <- downloadHandler(
    filename = function() {
      "EmploymentByIndustry.xlsx"
    },
    content = function(file) {
      write_xlsx(list("1c.Employment by industry" = C_datahub %>%
        filter(metric == "inemployment", Breakdown == "Industry") %>%
        select(-metric, -metricNeat, -Breakdown, Industry = Subgroup) %>%
        rename("Employment volume" = valueText)), path = file)
    }
  )
  output$downloadData4 <- downloadHandler(
    filename = function() {
      "FeAchievementParticipation.xlsx"
    },
    content = function(file) {
      write_xlsx(list("3b.FE achievement&participation" = C_datahub %>%
        filter(metric %in% c("achievements", "participation"), Breakdown != "SSA") %>%
        select(-metric) %>%
        rename(Volume = valueText, Metric = metricNeat)), path = file)
    }
  )
  output$downloadData5 <- downloadHandler(
    filename = function() {
      "FeAchievementBySSA.xlsx"
    },
    content = function(file) {
      write_xlsx(list("3a.FE achievements by SSA" = C_datahub %>%
        filter(metric == "achievements", Breakdown == "SSA") %>%
        select(-metric, -metricNeat, -Breakdown) %>%
        rename(Achievements = valueText, "Sector subject area tier 1" = Subgroup)), path = file)
    }
  )
  output$downloadData6 <- downloadHandler(
    filename = function() {
      "HighestQualification.xlsx"
    },
    content = function(file) {
      write_xlsx(
        list("4.Highest qualification" = C_datahub %>%
          filter(metric %in% c("qualNone", "qualL1", "qualL2", "qualApp", "qualL3", "qualL4", "qualOther")) %>%
          select(-metric) %>%
          rename("16-64 year olds" = valueText, "Highest qualification" = metricNeat)),
        path = file
      )
    }
  )
  output$downloadData7 <- downloadHandler(
    filename = function() {
      "EnterpriseBySize.xlsx"
    },
    content = function(file) {
      write_xlsx(list("5a.Enterprises by size" = C_datahub %>%
        filter(metric == "enterpriseCount", Breakdown != "Industry") %>%
        select(-metric, -metricNeat, -Breakdown) %>%
        rename("Enterprise count" = valueText, "Size band" = Subgroup)), path = file)
    }
  )
  output$downloadData8 <- downloadHandler(
    filename = function() {
      "EnterpriseByIndustry.xlsx"
    },
    content = function(file) {
      write_xlsx(
        list("5b.Enterprises by industry" = C_datahub %>%
          filter(metric == "enterpriseCount", Breakdown != "Size") %>%
          select(-metric, -metricNeat, -Breakdown) %>%
          rename("Enterprise count" = valueText, Industry = Subgroup)),
        path = file
      )
    }
  )
  output$downloadData9 <- downloadHandler(
    filename = function() {
      "EnterpriseDemography.xlsx"
    },
    content = function(file) {
      write_xlsx(
        list("5c.Enterprise demography" = C_datahub %>%
          filter(metric %in% c("births", "deaths", "active")) %>%
          select(-metric, -Breakdown, -Subgroup) %>%
          rename("Enterprise count" = valueText, Metric = metricNeat)),
        path = file
      )
    }
  )
  output$downloadData10 <- downloadHandler(
    filename = function() {
      "KS4Destinations.xlsx"
    },
    content = function(file) {
      write_xlsx(
        list("6a.Key Stage 4 destinations" = C_datahub %>%
          filter(metric == "sustainedPositiveDestinationKS4Rate") %>%
          select(-metric, -metricNeat, -Breakdown) %>%
          rename("KS4 sustained positive destination rate" = valueText, Outcome = Subgroup)),
        path = file
      )
    }
  )
  output$downloadData11 <- downloadHandler(
    filename = function() {
      "KS5Destinations.xlsx"
    },
    content = function(file) {
      write_xlsx(
        list("6b.Key Stage 5 destinations" = C_datahub %>%
          filter(metric == "sustainedPositiveDestinationKS5Rate") %>%
          select(-metric, -metricNeat) %>%
          rename("KS5 sustained positive destination rate" = valueText)),
        path = file
      )
    }
  )
  output$downloadData12 <- downloadHandler(
    filename = function() {
      "OnlineJobAdverts.xlsx"
    },
    content = function(file) {
      write_xlsx(
        list(
          "2a.Job adverts" = C_datahub %>%
            filter(metric == "vacancies", Breakdown == "Total") %>%
            select(-metric, -metricNeat, -Breakdown, -Subgroup) %>%
            rename("Online job adverts" = valueText),
          "2b.Job adverts by profession" = C_datahub %>%
            filter(metric == "vacancies", Breakdown != "Total") %>%
            select(-metric, -metricNeat) %>%
            rename("Online job adverts" = valueText, "Detailed/Summary" = Breakdown, Profession = Subgroup)
        ),
        path = file
      )
    }
  )
  output$downloadData13 <- downloadHandler(
    filename = function() {
      "EmploymentProjection.xlsx"
    },
    content = function(file) {
      write_xlsx(
        list(
          "7.Projected employment" = C_datahub %>%
            filter(metric == "employmentProjection") %>%
            select(-metric, -metricNeat) %>%
            rename("Projected employment" = valueText)
        ),
        path = file
      )
    }
  )

  ### 2.5.2 Create download links ----
  output$hidden_downloads <- renderUI(lapply(1:13, function(i) {
    downloadLink(paste0("downloadData", i), "download", class = "hiddenLink")
  }))
  ### 2.5.3 Data table ----
  output$DataTbl <- renderDataTable({
    DT::datatable(
      I_DataTable %>%
        mutate("Dashboard data" = lapply(
          1:n(),
          function(i) {
            paste0(
              '<a onClick=document.getElementById("downloadData',
              i,
              '").click() >Download</a>'
            )
          }
        )),
      escape = FALSE,
      options = list(dom = "t", "pageLength" = 15),
      rownames = FALSE
    )
  })

  ### 2.2.1 Filters ----

  ###  2.2.3 Downloads ----
  # download all indicators
  output$download_btn0a <- downloadHandler(
    filename = function() {
      "AllAreasIndicators.xlsx"
    },
    content = function(file) {
      file.copy("Data/AppData/CoreIndicators.xlsx", file)
    }
  )

  # Download current area indicators
  filtered_data0 <- reactive({
    currentGeogconcat <- C_datahub %>%
      filter(Area == input$geoChoiceOver)
    list(
      "1a.Employment by occupation" = currentGeogconcat %>%
        filter(metric == "inemployment", Breakdown == "Occupation") %>%
        select(-metric, -metricNeat, -Breakdown) %>%
        rename("Employment volume" = valueText, Occupation = Subgroup),
      "1b.Employment volumes" = currentGeogconcat %>%
        filter(metric %in% c("all", "inemployment", "selfemployed", "unemployed", "inactive"), Breakdown != "Occupation", Breakdown != "Industry") %>%
        select(-metric, -Breakdown, -Subgroup) %>%
        rename("Volume" = valueText, Metric = metricNeat),
      "1c.Employment by industry" = currentGeogconcat %>%
        filter(metric == "inemployment", Breakdown == "Industry") %>%
        select(-metric, -metricNeat, -Breakdown, Industry = Subgroup) %>%
        rename("Employment volume" = valueText),
      "2a.Job adverts" = currentGeogconcat %>%
        filter(metric == "vacancies", Breakdown == "Total") %>%
        select(-metric, -metricNeat, -Breakdown, -Subgroup) %>%
        rename("Online job adverts" = valueText),
      "2b.Job adverts by profession" = currentGeogconcat %>%
        filter(metric == "vacancies", Breakdown != "Total") %>%
        select(-metric, -metricNeat) %>%
        rename("Online job adverts" = valueText, "Detailed/Summary" = Breakdown, Profession = Subgroup),
      "3a.FE achievements by SSA" = currentGeogconcat %>%
        filter(metric == "achievements", Breakdown == "SSA") %>%
        select(-metric, -metricNeat, -Breakdown) %>%
        rename(Achievements = valueText, "Sector subject area tier 1" = Subgroup),
      "3b.FE achievement&participation" = currentGeogconcat %>%
        filter(metric %in% c("achievements", "participation"), Breakdown != "SSA") %>%
        select(-metric) %>%
        rename(Volume = valueText, Metric = metricNeat),
      "4.Highest qualification" = currentGeogconcat %>%
        filter(metric %in% c("qualNone", "qualL1", "qualL2", "qualApp", "qualL3", "qualL4", "qualOther")) %>%
        select(-metric) %>%
        rename("16-64 year olds" = valueText, "Highest qualification" = metricNeat),
      "5a.Enterprises by size" = currentGeogconcat %>%
        filter(metric == "enterpriseCount", Breakdown != "Industry") %>%
        select(-metric, -metricNeat, -Breakdown) %>%
        rename("Enterprise count" = valueText, "Size band" = Subgroup),
      "5b.Enterprises by industry" = currentGeogconcat %>%
        filter(metric == "enterpriseCount", Breakdown != "Size") %>%
        select(-metric, -metricNeat, -Breakdown) %>%
        rename("Enterprise count" = valueText, Industry = Subgroup),
      "5c.Enterprise demography" = currentGeogconcat %>%
        filter(metric %in% c("births", "deaths", "active")) %>%
        select(-metric, -Breakdown, -Subgroup) %>%
        rename("Enterprise count" = valueText, Metric = metricNeat),
      "6a.Key Stage 4 destinations" = currentGeogconcat %>%
        filter(metric == "sustainedPositiveDestinationKS4Rate") %>%
        select(-metric, -metricNeat, -Breakdown) %>%
        rename("KS4 sustained positive destination rate" = valueText, Outcome = Subgroup),
      "6b.Key Stage 5 destinations" = currentGeogconcat %>%
        filter(metric == "sustainedPositiveDestinationKS5Rate") %>%
        select(-metric, -metricNeat) %>%
        rename("KS5 sustained positive destination rate" = valueText),
      "7.Projected employment" = currentGeogconcat %>%
        filter(metric == "employmentProjection") %>%
        select(-metric, -metricNeat) %>%
        rename("Projected employment" = valueText)
    )
  })
  output$download_btn0b <- downloadHandler(
    filename = function() {
      paste0(input$geoChoiceOver, " Indicators.xlsx")
    },
    content = function(file) {
      write_xlsx(filtered_data0(), path = file)
    }
  )

   ### 2.3.3 Screenshot----
  output$screenshotFile <- renderUI({
    capture::capture(
      selector = "body",
      filename = if(is.null(input$sectorChoice)==FALSE){paste0(input$sectorChoice,".png")}
      else{paste0(input$socChoice[1],".png")},
      icon("camera"),
      "Screenshot"
    )
  })

  # create subheading
  output$subheading <- renderUI({
    validate(
      need("geoChoice" %in% names(input), ""),
      need(input$geoChoice != "", "")
    )
    paste0(
      (I_DataText %>% filter(metric == input$splashMetric))$subheading,
      if (input$splashMetric %in% c("vacancies", "employmentProjection")) {
        if (input$geoChoice == "Dorset LSIP") {
          " The data presented here for Dorset LSIP is correct, however for the Skills Imperative and Job Advert data, it does not match the published data. We are working to update the published data. "
        } else if (input$geoChoice == "Enterprise M3 LEP (including all of Surrey) LSIP") {
          " The published data for Skills Imperative and Job Advert for Enterprise M3 LEP (including all of Surrey) LSIP is incorrect due to the wrong LAs being included in the area. Presented here is an estimate for this LSIP compiled from other LEP and LSIP regions. As such there may be some rounding issues. We are working to update the published data. "
        } else {
          ""
        }
      } else {
        ""
      }
    )
  })

  ### 2.3.4 Data note----
  # create data source
  output$dataSource <- renderUI({
    HTML(paste0("<p>Source: ", (
      I_DataText %>% filter(metric == input$splashMetric)
    )$sourceText, "<p>"))
  })
  # create data note
  output$dataNote <- renderUI({
    HTML((I_DataText %>% filter(metric == input$splashMetric))$dataText)
  })
  # create data caveat
  output$dataCaveat <- renderUI({
    HTML((I_DataText %>% filter(metric == input$splashMetric))$caveatText)
  })
##Update filters----
  ## 2.3.5 Update occupaton filter with sector ----
  observeEvent(input$sectorChoice, ignoreNULL=FALSE, {

    updatePickerInput(session, "socChoice",
                         choices = 
                        if(is.null(input$sectorChoice)){(C_time%>%
                                                           filter(breakdown==paste0("4-digit occupation ",input$codeChoice))%>%
                                                           distinct(subgroup)%>%
                                                           arrange(subgroup))$subgroup}
                        else{if(input$sectorChoice %in% c("Science and tech")){
                           (I_stemLookup%>%filter(Sector %in% input$sectorChoice)%>%arrange(SOC2020desc))$SOC2020desc}
                      else {if(input$sectorChoice == input$occGroupName)
                      {input$occGroup}
                       }}
                      ,selected= 
                        if(is.null(input$sectorChoice)){NULL}else{
                        if(input$sectorChoice %in% c("Science and tech")){
                          (I_stemLookup%>%filter(Sector %in% input$sectorChoice)%>%arrange(SOC2020desc))$SOC2020desc
         }
                      else {if(input$sectorChoice == input$occGroupName)
                      {input$occGroup}}}
                        
                         #, options = list()
    )
  })
  
  ### 2.3.6 Update occupaton filter with soc type ----
  observeEvent(list(input$codeChoice), {
    updatePickerInput(session, "socChoice",
                         choices = (C_time%>%
                           filter(breakdown==paste0("4-digit occupation ",input$codeChoice))%>%
                           distinct(subgroup)%>%
                             arrange(subgroup))$subgroup
    )
  })
  
  # ### 2.3.6 Update occupaton filter with soc type ----
  # observeEvent(input$codeChoice, {
  #   updateRadioGroupButtons(session, "socNumChoice",
  #                        choices = if(input$codeChoice=="TextKernel")
  #                          {c("Summary","Detailed")}
  #                        else{ c(1, 2, 3, 4)}
  #                        ,selected=if(input$codeChoice=="TextKernel")
  #                        {"Detailed"}
  #                        else{4}
  #   )
  # })
  
  ### 2.3.6 Update sector filter with soc type and user input ----
  observeEvent(list(input$occGroupName,input$codeChoice), {
    updateSelectizeInput(session, "sectorChoice",
                         choices = if(input$codeChoice=="SOC2020"){
                           c("Science and tech",input$occGroupName)}
                         else{c(input$occGroupName)}
                            )
  })

  ### 2.3.6 Update time filter with soc type  ----
  observeEvent(input$codeChoice, {
    updateSelectizeInput(session, "timeChoice",
                         choices =if(input$codeChoice=="SOC2020"){
                           (C_mapData%>%
                             filter(breakdown=="4-digit occupation SOC2020")%>%
                             distinct(chartPeriod))$chartPeriod}
                         else{(C_mapData%>%
                             filter(breakdown=="4-digit occupation SOC2010")%>%
                             distinct(chartPeriod))$chartPeriod}
                         ,selected=if(input$codeChoice=="SOC2020"){
                           "Jan 2022-Dec 2022"}
                         else{"Jan 2021-Dec 2021"}
    )
  })
  
  ### 2.3.6 Update sector filter with soc type and user input ----
  observeEvent(list(input$socChoice,input$sectorChoice), {
    updateSelectizeInput(session, "mapOccChoice",
                         choices = c(input$socChoice,input$sectorChoice)
    )
  })
  
  ### 2.3.6 Update data with new group added  ----
  # observeEvent(input$occGroupName, {
  #   C_time<-C_time%>%
  #     bind_rows(
  #       C_time%>%
  #         filter(subgroup %in% input$occGroup)%>%
  #         mutate(sumPopMean=case_when(metric=="Earnings" ~ population*value
  #                                     ,TRUE ~ value))%>%
  #         group_by(geogConcat,chartPeriod,timePeriod,latest,metric,breakdown,breakdown2,subgroup2)%>%
  #         summarise(value=if_else(first(metric=="Earnings"),sum(sumPopMean,na.rm = TRUE)/sum(population,na.rm = TRUE)
  #                                   ,sum(value,na.rm = TRUE)))%>%
  #         mutate(subgroup=input$occGroupName)%>%
  #     mutate(valueText = as.character(value)))
  #  
  #     
  #   
  # }) 
  
  #### 2.3.5.3 Map ----
  mapAllYears <- reactive({
    if(input$occGroupName!="" && is.null(input$mapOccChoice)==FALSE && input$mapOccChoice==input$occGroupName){
      #add in chosen groups 
    C_mapDataChosen<-C_mapData%>%
      filter(subgroup %in% input$occGroup)%>%
      group_by(chartPeriod,breakdown,metric,geogConcat)%>%
      summarise(value=sum(value,na.rm = TRUE))%>%
      mutate(subgroup=input$occGroupName)
    
    C_mapData<-C_mapData%>%
      bind_rows(C_mapDataChosen)
    }else{}
    
    C_Geog %>%
      left_join(C_mapData %>% 
                  filter(subgroup==input$mapOccChoice
                         ,breakdown==paste0("4-digit occupation ",input$codeChoice)),
                by = c("geogConcat" = "geogConcat")
      )
  })
  
  output$map <- renderLeaflet({
    # validate(
    #   need(input$geoChoice != "", ""),
    #   need(input$splashGeoType != "", "")
    # )
    mapData <-mapAllYears()  %>% filter(chartPeriod== input$timeChoice,metric =='Employed')
    pal <- colorNumeric("Blues", mapData$value)
    labels <-
      # if a percentage then format as %, else big number
      # if (
       # str_sub(input$splashMetric, start = -4) == "Rate" | input$splashMetric == "employmentProjection") {
   #     sprintf(
      #    "<strong>%s</strong><br/>%s: %s%%",
          paste0(mapData$areaName,": ",
                 format(round(mapData$value), big.mark = ","))
       # ) %>% lapply(htmltools::HTML)
      # } else {
      #   sprintf(
      #     "<strong>%s</strong><br/>%s: %s",
      #     mapData$areaName,
      #     (I_DataText %>% filter(metric == input$splashMetric))$mapPop,
      #     format(round(mapData[[input$splashMetric]]), big.mark = ",")
      #   ) %>% lapply(htmltools::HTML)
      # }

    leaflet(options = leafletOptions(zoomSnap = 0.1)) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        data = mapData,
        fillColor = ~ pal(value),
        color = "black",
        layerId = ~areaCode,
        weight = 1,
        highlightOptions = highlightOptions(
          weight = 2,
          bringToFront = TRUE
        ),
        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "12px",
          direction = "auto"
        )
      ) %>%
      setView(
        lng = -1.6,
        lat = 52.8,
        zoom = 5.7
      )
  })
  
  output$mapEarn <- renderLeaflet({
    # validate(
    #   need(input$geoChoice != "", ""),
    #   need(input$splashGeoType != "", "")
    # )
    mapData <-mapAllYears()  %>% filter(chartPeriod== input$timeChoice,metric =='Earnings')
    pal <- colorNumeric("Blues", mapData$value)
    labels <-
      # if a percentage then format as %, else big number
      # if (
      # str_sub(input$splashMetric, start = -4) == "Rate" | input$splashMetric == "employmentProjection") {
      #     sprintf(
      #    "<strong>%s</strong><br/>%s: %s%%",
      paste0(mapData$areaName,": ",
             format(round(mapData$value), big.mark = ","))
    # ) %>% lapply(htmltools::HTML)
    # } else {
    #   sprintf(
    #     "<strong>%s</strong><br/>%s: %s",
    #     mapData$areaName,
    #     (I_DataText %>% filter(metric == input$splashMetric))$mapPop,
    #     format(round(mapData[[input$splashMetric]]), big.mark = ",")
    #   ) %>% lapply(htmltools::HTML)
    # }
    
    leaflet(options = leafletOptions(zoomSnap = 0.1)) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        data = mapData,
        fillColor = ~ pal(value),
        color = "black",
        layerId = ~areaCode,
        weight = 1,
        highlightOptions = highlightOptions(
          weight = 2,
          bringToFront = TRUE
        ),
        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "12px",
          direction = "auto"
        )
      ) %>%
      setView(
        lng = -1.6,
        lat = 52.8,
        zoom = 5.7
      )
  })
  # observe({
  #   #validate(need("geoChoice" %in% names(input), ""))
  #   mapData <- C_Geog %>% filter(geogConcat == input$geoChoice)
  #   labels <-
  #     # if a percentage then format as %, else big number
  #     if (str_sub(input$splashMetric, start = -4) == "Rate" | input$splashMetric == "employmentProjection") {
  #       sprintf(
  #         "<strong>%s</strong><br/>%s: %s%%",
  #         mapData$areaName,
  #         (I_DataText %>% filter(metric == input$splashMetric))$mapPop,
  #         round(mapData[[input$splashMetric]] * 100)
  #       ) %>% lapply(htmltools::HTML)
  #     } else {
  #       sprintf(
  #         "<strong>%s</strong><br/>%s: %s",
  #         mapData$areaName,
  #         (I_DataText %>% filter(metric == input$splashMetric))$mapPop,
  #         format(round(mapData[[input$splashMetric]]), big.mark = ",")
  #       ) %>% lapply(htmltools::HTML)
  #     }
  #   proxy <- leafletProxy("map")
  #   addPopups(
  #     proxy,
  #     lng = C_Geog$LONG[C_Geog$geogConcat == input$geoChoice],
  #     lat = C_Geog$LAT[C_Geog$geogConcat == input$geoChoice],
  #     popup = labels,
  #     layerId = "popup",
  #     options = popupOptions(
  #       className = "myspecial-popup",
  #       textsize = "12px",
  #       direction = "auto",
  #       closeOnClick = TRUE,
  #       closeButton = FALSE
  #     )
  #   )
  # })

  #### 2.3.5.4 Map footnote ----
  output$mapFoot <- renderUI({
    paste0(
      (I_DataText %>% filter(metric == input$splashMetric))$LatestPeriod, ". Click an area to update dashboard."
    )
  })

  ### 2.5.3 Data table ----
  output$dataPeriodComment <- renderUI({
    if(input$codeChoice=="SOC2020"){"Jan 2022-Dec 2022 data"}else{"Jan 2020-Dec 2020 data"}
  })
    
  output$OccTbl <- renderDataTable({

    if(input$occGroupName!="" && is.null(input$sectorChoice)==FALSE && input$sectorChoice==input$occGroupName){
     #add in chosen groups
    C_tableChosen<-C_table%>%
      filter(subgroup %in% input$occGroup)%>%
      group_by(breakdown)%>%
      summarise(Employed=sum(Employed,na.rm = TRUE)
                ,EmployedLast=sum(EmployedLast,na.rm = TRUE)
                ,EmployedFemale=sum(EmployedFemale,na.rm = TRUE)
                ,EarnPop=sum(EarnPop,na.rm = TRUE)
                ,population=sum(population,na.rm = TRUE)
                ,EarnPopLast=sum(EarnPopLast,na.rm = TRUE)
                ,populationLast=sum(populationLast,na.rm = TRUE)
                )%>%
      mutate(growth=(Employed-EmployedLast)/EmployedLast
             ,femalePerc=EmployedFemale/Employed
             ,subgroup=input$occGroupName
             ,Earnings=EarnPop/population
             ,growthEarn=(EarnPop/population-EarnPopLast/populationLast)/(EarnPopLast/populationLast)
             )%>%
      select(-EmployedLast,-EmployedFemale,-EarnPop,-population,-EarnPopLast,-populationLast)
    
    #occupation split by region 
    occSplitByRegionChosen<-
      C_localSkillsDataset%>%
      filter(subgroup %in% c(input$occGroup,"Total")
             ,latest==1
             ,geogConcat!="England"
             ,metric =='Employed'
             ,breakdown %in% c("4-digit occupation SOC2010","4-digit occupation SOC2020","Total")
             ,subgroup2=="Total"
      )%>%
      mutate(breakdown=case_when(breakdown== "Total" & chartPeriod=="Jan 2022-Dec 2022" ~ "4-digit occupation SOC2020"
                                 ,breakdown== "Total" & chartPeriod=="Jan 2021-Dec 2021" ~ "4-digit occupation SOC2010"
                                 ,TRUE ~ breakdown))%>%
      select(geogConcat,breakdown,subgroup,value)%>%
      mutate(subgroup=case_when(subgroup!="Total"~input$occGroupName, TRUE~ subgroup))%>%
      group_by(geogConcat,breakdown,subgroup)%>%
      summarise(value=sum(value,na.rm=TRUE))%>%
      group_by(subgroup,breakdown)%>%
      mutate(occSplitByRegion =  value/sum(value, na.rm=TRUE))%>%#input$occGroupName) %>% 
      ungroup%>%select(-value)
      
    #comapre to natinal splpit
    Underepresented<-occSplitByRegionChosen%>%filter(subgroup!="Total")%>%
      left_join(occSplitByRegionChosen%>%filter(subgroup=="Total")%>%select(-subgroup)%>%rename(allOccsSplitByRegion=occSplitByRegion))%>%
      mutate(representation=occSplitByRegion-allOccsSplitByRegion)%>%
      group_by(subgroup,breakdown)%>%
      arrange(representation)%>%
      slice(1)%>%
      select(subgroup,breakdown,Underepresented=geogConcat)
    Overepresented<-occSplitByRegionChosen%>%filter(subgroup!="Total")%>%
      left_join(occSplitByRegionChosen%>%filter(subgroup=="Total")%>%select(-subgroup)%>%rename(allOccsSplitByRegion=occSplitByRegion))%>%
      mutate(representation=occSplitByRegion-allOccsSplitByRegion)%>%
      group_by(subgroup,breakdown)%>%
      arrange(desc(representation))%>%
      slice(1)%>%
      select(subgroup,breakdown,Overepreseneted=geogConcat)
    #add on to table
    C_tableChosen<-C_tableChosen%>%
      left_join(Underepresented)%>%
      left_join(Overepresented)%>%
      mutate(subgroup=case_when(subgroup=="Total" ~ "All occupations"
                                ,TRUE ~ subgroup))
    C_table<-C_table%>%
      bind_rows(C_tableChosen)
    }else{}

    DT::datatable(
      C_table%>%
        filter(subgroup  %in% input$socChoice | subgroup  %in% input$sectorChoice |subgroup=="All occupations"
               ,breakdown==paste0("4-digit occupation ",input$codeChoice))%>%
        # mutate(fixRows=case_when(subgroup=="Total" ~ 2
        #                          ,subgroup %in% input$sectorChoice ~1
        #                          ,TRUE ~ 0))        %>%
        rename(Occupation=subgroup, `Change since last year`=growth,`Female %`=femalePerc, `Underepresented in`=Underepresented,`Overepresented in`=Overepreseneted,`Earnings growth`=growthEarn)%>%
        select(-breakdown,-EmployedLast,-EmployedFemale,-population,-populationLast,-EarnPop,-EarnPopLast),
      escape = FALSE,
      options = list("pageLength" = 5,order = list(1, 'desc'),dom = 'tp'),
      rownames = FALSE
    )%>% 
      formatPercentage(c("Change since last year", "Female %","Earnings growth"), 0)%>%
      formatRound('Employed', digits = 0)%>%
      formatCurrency('Earnings', digits = 0,currency = "£")
  })

  #### 2.3.7.2 Emp Chart ----
  occTime <-
    eventReactive(
      c(
        input$socChoice,input$sectorChoice,input$demChoice, input$sectorSwitch, input$occSwitch, input$demSwitch,input$numOccs
      ),
      { 
        if(input$occGroupName!="" && is.null(input$sectorChoice)==FALSE && input$sectorChoice==input$occGroupName){
 #add in chosen groups
        C_timeChosen<-C_time%>%
          filter(subgroup %in% input$occGroup)%>%
          mutate(sumPopMean=case_when(metric=="Earnings" ~ population*value
                                      ,TRUE ~ value))%>%
          group_by(geogConcat,chartPeriod,timePeriod,latest,metric,breakdown,breakdown2,subgroup2)%>%
          summarise(value=if_else(first(metric=="Earnings"),sum(sumPopMean,na.rm = TRUE)/sum(population,na.rm = TRUE)
                                    ,sum(value,na.rm = TRUE)))%>%
          mutate(subgroup=input$occGroupName)%>%
      mutate(valueText = as.character(value))
        
        C_time<-C_time%>%
          bind_rows(C_timeChosen)
        }else{}
        
        #get top 5 occs chosen
        occsTop5<-(C_time %>%
                     filter(breakdown==paste0("4-digit occupation ",input$codeChoice)
                            ,subgroup  %in% input$socChoice
                            ,breakdown2=="Total"
                            ,!metric %in% c('Earnings')
                            ,latest==1
                     )%>%
                     arrange(desc(value))%>%
                     slice(1:input$numOccs))$subgroup
        occs<-
          if(input$sectorSwitch==TRUE&input$occSwitch==TRUE){c(occsTop5,input$sectorChoice)}
          else{if(input$sectorSwitch==TRUE){c(input$sectorChoice)}
          else{if(input$occSwitch==TRUE){c(occsTop5)}
            else{TRUE}}}

        SplashTime <- C_time %>%
          filter(breakdown==paste0("4-digit occupation ",input$codeChoice)
          ,subgroup  %in% occs
          ,(breakdown2=="Total"|breakdown2 %in% if(input$demSwitch==TRUE){ input$demChoice}else{"NULL"})
            ,!metric %in% c('Earnings')
          )
        # # add an extra column so the colours work in ggplot when sorting alphabetically
        # SplashTime$Areas <- factor(SplashTime$geogConcat,
        #   levels = c("England", input$geoChoice, input$geoComps) # paste0(laClicked()," LADU"),
        # )

        ggplot(
          SplashTime,
          aes(
            x = as.Date(timePeriod),
            y = value,
            color =  interaction(metric, subgroup,subgroup2),
            group = interaction(metric, subgroup,subgroup2),
            text = paste0(
              "Period: ",
              chartPeriod,
              "<br>",
              metric,": ",
              subgroup," ", subgroup2,
              "<br>",
              value
            )
          )
        ) +
          geom_line() +
          theme_minimal() +
          theme(
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            legend.position = "bottom",
            legend.title = element_blank()
          ) +
          scale_y_continuous(labels =
            label_number(accuracy = 0.1, scale_cut = cut_short_scale())
          ) +
          labs(colour = "") +
          # scale_color_manual(values =
          #   chartColors5
          # ) +
          scale_x_date(
            name = "My date axis title",
            date_breaks = "1 years",
            date_labels = "%Y"
          )
      }
    )

  output$occTime <- renderPlotly({
    validate(
      need((is.null(input$socChoice)==FALSE&&input$occSwitch==TRUE)||(input$sectorChoice != ""&&input$sectorSwitch==TRUE) , "")
    )
    ggplotly(occTime(), tooltip = "text") %>%
      layout(
        legend = list(
          orientation = "h",
          x = 0,
          y = -0.1
        ),
        xaxis = list(fixedrange = TRUE),
        yaxis = list(fixedrange = TRUE)
      ) %>% # disable zooming because it's awful on mobile
      config(displayModeBar = FALSE)
  })

  
  #### 2.3.7.2 Earn Chart ----
  earnTime <-
    eventReactive(
      c(
        input$socChoice,input$sectorChoice,input$demChoice, input$sectorSwitch, input$occSwitch, input$demSwitch,input$numOccs
      ),
      { 
        if(input$occGroupName!="" && is.null(input$sectorChoice)==FALSE && input$sectorChoice==input$occGroupName){
        #add in chosen groups
        C_timeChosen<-C_time%>%
          bind_rows(
            C_time%>%
              filter(subgroup %in% input$occGroup)%>%
              mutate(sumPopMean=case_when(metric=="Earnings" ~ population*value
                                          ,TRUE ~ value))%>%
              group_by(geogConcat,chartPeriod,timePeriod,latest,metric,breakdown,breakdown2,subgroup2)%>%
              summarise(value=if_else(first(metric=="Earnings"),sum(sumPopMean,na.rm = TRUE)/sum(population,na.rm = TRUE)
                                      ,sum(value,na.rm = TRUE)))%>%
              mutate(subgroup=input$occGroupName)%>%
              mutate(valueText = as.character(value)))
        
        C_time<-C_time%>%
          bind_rows(C_timeChosen)
        }else{}
        #get top 5 occs chosen
        occsTop5<-(C_time %>%
                     filter(breakdown==paste0("4-digit occupation ",input$codeChoice)
                            ,subgroup  %in% input$socChoice
                            ,breakdown2=="Total"
                            ,!metric %in% c('Earnings')
                            ,latest==1
                     )%>%
                     arrange(desc(value))%>%
                     slice(1:input$numOccs))$subgroup
        occs<-
          if(input$sectorSwitch==TRUE&input$occSwitch==TRUE){c(occsTop5,input$sectorChoice)}
        else{if(input$sectorSwitch==TRUE){c(input$sectorChoice)}
          else{if(input$occSwitch==TRUE){c(occsTop5)}
            else{TRUE}}}
        
        earnPlot <- C_time %>%
          filter(breakdown==paste0("4-digit occupation ",input$codeChoice)
                 ,subgroup  %in% occs
                 ,(breakdown2=="Total"|breakdown2 %in% if(input$demSwitch==TRUE){ input$demChoice}else{"NULL"})
                 ,metric %in% c('Earnings')
          )
        # # add an extra column so the colours work in ggplot when sorting alphabetically
        # SplashTime$Areas <- factor(SplashTime$geogConcat,
        #   levels = c("England", input$geoChoice, input$geoComps) # paste0(laClicked()," LADU"),
        # )
        
        ggplot(
          earnPlot,
          aes(
            x = as.Date(timePeriod),
            y = value,
            color =  interaction(metric, subgroup,subgroup2),
            group = interaction(metric, subgroup,subgroup2),
            text = paste0(
              "Period: ",
              chartPeriod,
              "<br>",
              metric,": ",
              subgroup,
              "<br>",
              value
            )
          )
        ) +
          geom_line() +
          theme_minimal() +
          theme(
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            legend.position = "bottom",
            legend.title = element_blank()
          ) +
          scale_y_continuous(labels =
                               label_number(accuracy = 0.1, scale_cut = cut_short_scale())
          ) +
          labs(colour = "") +
          # scale_color_manual(values =
          #                      chartColors5
          # ) +
          scale_x_date(
            name = "My date axis title",
            date_breaks = "1 years",
            date_labels = "%Y"
          )
      }
    )
  
  output$earnTime <- renderPlotly({
    validate(
      need((is.null(input$socChoice)==FALSE&&input$occSwitch==TRUE)||(input$sectorChoice != ""&&input$sectorSwitch==TRUE) , "")
    )
    ggplotly(earnTime(), tooltip = "text") %>%
      layout(
        legend = list(
          orientation = "h",
          x = 0,
          y = -0.1
        ),
        xaxis = list(fixedrange = TRUE),
        yaxis = list(fixedrange = TRUE)
      ) %>% # disable zooming because it's awful on mobile
      config(displayModeBar = FALSE)
  })
  
  
  ### 2.3.8 Breakdown chart ----
  #### 2.3.8.1 Breakdown filter ----
  distinctSubgroups <- C_breakdown %>%
    distinct(metric, breakdown, subgroup)
  distinctBreakdowns <- C_breakdown %>%
    distinct(metric, breakdown)
  output$breakdownFilter <- renderUI({
    validate(
      need(input$splashMetric %in% distinctBreakdowns$metric, "")
    )
    selectizeInput(
      inputId = "barBreakdown",
      label = NULL,
      choices =
        (as.vector(
          distinctBreakdowns %>%
            filter(metric == input$splashMetric)
        ))$breakdown
    )
  })
  #### 2.3.8.2 Optional summary profession filter ----
  summaryCategories <- c("All", (as.vector(
    distinctSubgroups %>%
      filter(breakdown == "Summary Profession Category")
  ))$subgroup)
  output$professionFilter <- renderUI({
    validate(
      need(input$barBreakdown != "", ""),
      need(input$barBreakdown == "Detailed Profession Category", ""),
      need(input$splashMetric %in% distinctBreakdowns$metric, "")
    )
    selectizeInput(
      inputId = "summaryProfession",
      label = "Limit to particular summary profession",
      choices = summaryCategories
    )
  })

  #### 2.3.8.2 Subgroup filter ----
  output$subgroupFilter <- renderUI({
    validate(
      need(input$barBreakdown != "", ""),
      need(input$splashMetric %in% distinctBreakdowns$metric, "")
    )
    pickerInput(
      inputId = "barSubgroup",
      label = NULL,
      choices =
        as.vector((
          distinctSubgroups %>%
            filter(
              metric == input$splashMetric,
              breakdown == input$barBreakdown,
              if (input$barBreakdown == "Detailed Profession Category" & "summaryProfession" %in% names(input) && input$summaryProfession != "All") {
                subgroup %in%
                  (C_detailLookup %>% filter(`Summary Profession Category` == input$summaryProfession))$`Detailed Profession Category`
              } else {
                TRUE
              }
            )
        ))$subgroup,
      multiple = TRUE,
      selected = (as.vector(
        C_topTenEachBreakdown %>%
          filter(
            metric == input$splashMetric,
            breakdown == input$barBreakdown,
            geogConcat == input$geoChoice,
            if (input$barBreakdown == "Detailed Profession Category" & "summaryProfession" %in% names(input) && input$summaryProfession != "All") {
              `Summary Profession Category` == input$summaryProfession
            } else {
              `Summary Profession Category` == "All"
            }
          )
      ))$subgroup,
      options = list(`actions-box` = TRUE, `live-search` = TRUE)
    )
  })

  #### 2.3.8.3 Title ----
  output$titleBreakdown <- renderUI({
    validate(
      need(input$barBreakdown != "", ""),
      need(input$splashMetric %in% distinctBreakdowns$metric, "")
    )
    paste0(
      "How do ",
      (I_DataText %>% filter(metric == input$splashMetric))$breakdownTitle,
      " vary by ",
      tolower(input$barBreakdown),
      "?"
    )
  })

  #### 2.3.8.4 Comment ----
  output$commentBreakdown <- renderUI({
    validate(
      need("barBreakdown" %in% names(input) | !input$splashMetric %in% distinctBreakdowns$metric, ""),
      need(input$geoChoice != "", "")
    )

    if (!input$splashMetric %in% distinctBreakdowns$metric) {
      paste0(
        str_to_sentence(currentMetric()),
        " currently has no breakdowns.",
        if (input$splashMetric %in% c(
          "inemploymentRate",
          "selfemployedRate",
          "unemployedRate",
          "inactiveRate",
          "selfemployed",
          "unemployed",
          "Inactive"
        )) {
          " Switch to Employment metric for occupation and industry breakdowns."
        } else {
          ""
        }
      )
    } else {
      breakdownDiff <- C_breakdown %>%
        filter(
          geogConcat == input$geoChoice |
            geogConcat == "England",
          breakdown == input$barBreakdown,
          metric == input$splashMetric
        ) %>%
        group_by(subgroup) %>%
        mutate(change = (value - lag(value, default = 1)) / value) %>%
        ungroup() %>%
        filter(geogConcat == input$geoChoice) %>%
        mutate(ranking = rank(desc(abs(change)), ties.method = c("first"))) %>%
        filter(ranking == 1)

      breakdownDirection <-
        if (isTRUE(breakdownDiff$change) && breakdownDiff$change > 0) {
          "higher"
        } else {
          "lower"
        }

      paste0(
        input$geoChoice,
        " has a ",
        breakdownDirection,
        " ",
        (I_DataText %>% filter(metric == input$splashMetric))$breakdownComment,
        " in ",
        breakdownDiff$subgroup,
        " than the national average. ",
        if (nrow(C_breakdown %>%
          filter(breakdown == input$barBreakdown) %>%
          distinct(subgroup)) > 10) {
          "The top 10 subgroups are shown. Use the filter to add or remove subgroups. "
        } else {
          ""
        }
      )
    }
  })

  #### 2.3.8.3 Bar chart ----
  Splash_pc <- eventReactive(
    c(
      # input$map_shape_click,
      input$geoChoice,
      input$geoComps,
      # input$barBreakdown,
      input$barSubgroup,
      # input$mapLA_shape_click,
      input$splashMetric
    ),
    {
      validate(
        need(input$barBreakdown != "", ""),
        need(input$barSubgroup != "", ""),
        need(input$splashMetric != "", ""),
        need(input$barBreakdown != "No breakdowns available", "")
      )
      Splash_21 <- C_breakdown %>% filter(
        breakdown == input$barBreakdown,
        subgroup %in% input$barSubgroup,
        metric == input$splashMetric,
        # get lep/lsip/mca areas
        (geogConcat == input$geoChoice | geogConcat %in% if ("geoComps" %in% names(input)) {
          input$geoComps
        } else {
          "\nNone"
        }) |
          # get england for comparison
          (geogConcat == "England")
      )
      # if no rows (because of filter lag) then don't plot
      if (nrow(Splash_21) == 0) {
        "x"
      } else {
        # add an extra column so the colours work in ggplot when sorting alphabetically
        Splash_21$Area <- factor(Splash_21$geogConcat,
          levels = c("England", input$geoChoice, input$geoComps) # paste0(laClicked()," LADU"),
        )
        ggplot(
          Splash_21,
          aes(
            x = reorder(subgroup, value, mean),
            y = value,
            fill = Area,
            text = paste0(
              "Area: ",
              Area,
              "<br>",
              currentMetric(),
              ": ",
              if (str_sub(input$splashMetric, start = -4) == "Rate" |
                input$splashMetric == "inemployment" |
                input$splashMetric == "vacancies" |
                input$splashMetric == "enterpriseCount" |
                input$splashMetric == "achievements" |
                input$splashMetric == "participation" |
                input$splashMetric == "employmentProjection" |
                input$splashMetric == "starts") {
                scales::percent(round(value, 3))
              } else {
                round(value, 0)
              },
              "<br>"
            )
          )
        ) +
          geom_col(position = "dodge") +
          scale_y_continuous(labels = if (str_sub(input$splashMetric, start = -4) == "Rate" |
            input$splashMetric == "inemployment" |
            input$splashMetric == "vacancies" |
            input$splashMetric == "enterpriseCount" |
            input$splashMetric == "achievements" |
            input$splashMetric == "participation" |
            input$splashMetric == "employmentProjection" |
            input$splashMetric == "starts") {
            scales::percent
          } else {
            label_number(accuracy = 1, scale_cut = cut_short_scale())
          }) +
          scale_x_discrete(
            labels = function(x) {
              str_wrap(x, width = 26)
            }
          ) +
          coord_flip() +
          theme_minimal() +
          labs(fill = "") +
          theme(
            legend.position = "none",
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.y = element_text(size = 7),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()
          ) +
          scale_fill_manual(values = chartColors6)
      }
    }
  )

  output$Splash_pc <- renderPlotly({
    # check it exists
    validate(need(Splash_pc() != "x", ""))
    ggplotly(Splash_pc(),
      tooltip = c("text")
    ) %>%
      layout(
        legend = list(
          orientation = "h",
          x = 0,
          y = -0.1
        ),
        xaxis = list(fixedrange = TRUE),
        yaxis = list(fixedrange = TRUE)
      ) %>% # disable zooming because it's awful on mobile
      config(displayModeBar = FALSE)
  })

  output$breadownPlot <- renderUI({
    if ("barBreakdown" %in% names(input) && input$barBreakdown == "No breakdowns available") {} else {
      withSpinner(plotlyOutput("Splash_pc"))
    }
  })

  #### 2.3.8.6 Bar footnote ----
  output$breakdownFoot <- renderUI({
    validate(
      need(input$barBreakdown != "", ""),
      need(input$barBreakdown != "No breakdowns available", "")
    )
    if (input$splashMetric == "inemployment" & input$barBreakdown == "Occupation") {
      "Jan 2021-Dec 2021 data."
    } # Occupation data is older because of the SOC issue
    else {
      paste0(
        (I_DataText %>% filter(metric == input$splashMetric))$LatestPeriod, "."
      )
    }
  })

  ### 2.3.9 Downloads local skills ----
  # all areas
  listDownloadV1All <- reactive({
    list(
      "Table" =       C_table%>%
        filter(subgroup  %in% input$socChoice | subgroup  %in% input$sectorChoice |subgroup=="All occupations"
               ,breakdown==paste0("4-digit occupation ",input$codeChoice))%>%
        # mutate(fixRows=case_when(subgroup=="Total" ~ 2
        #                          ,subgroup %in% input$sectorChoice ~1
        #                          ,TRUE ~ 0))        %>%
        rename(Occupation=subgroup, `Change since last year`=growth,`Female %`=femalePerc, `Underepresented in`=Underepresented,`Overepresented in`=Overepreseneted,`Earnings growth`=growthEarn)%>%
        select(-breakdown,-EmployedLast,-EmployedFemale,-population,-populationLast,-EarnPop,-EarnPopLast),
      "Employment over time" = C_time %>%
        filter(breakdown==paste0("4-digit occupation ",input$codeChoice)
               ,subgroup  %in%  input$socChoice | subgroup  %in% input$sectorChoice |subgroup=="All occupations"
               ,(breakdown2=="Total"|breakdown2 %in% if(input$demSwitch==TRUE){ input$demChoice}else{"NULL"})
               ,!metric %in% c('Earnings')
        ),
      "Earnings over time" = C_time %>%
        filter(breakdown==paste0("4-digit occupation ",input$codeChoice)
               ,subgroup  %in%  input$socChoice | subgroup  %in% input$sectorChoice |subgroup=="All occupations"
               ,(breakdown2=="Total"|breakdown2 %in% if(input$demSwitch==TRUE){ input$demChoice}else{"NULL"})
               ,metric %in% c('Earnings')
        ),
      "Map" = mapAllYears()  %>% filter(chartPeriod== input$timeChoice,metric =='Employed')
      )
  })
  nameDownloadV1All <- reactive({
    if(is.null(input$sectorChoice)==FALSE){paste0(input$sectorChoice,".xlsx")}
    else{paste0(input$socChoice[1],".xlsx")}
  })
  output$downloadV1All <- downloadHandler(
    filename = function() {
      nameDownloadV1All()
    },
    content = function(file) {
      write_xlsx(listDownloadV1All(), path = file)
    }
  )

  # current area
  listDownloadV1Current <- reactive({
    list(
      "CurrentArea" = filter(
        C_time,
        metric == input$splashMetric,
        (geogConcat == input$geoChoice |
          geogConcat %in% input$geoComps |
          geogConcat == "England")
      ) %>%
        mutate(metric = case_when(metric == "employmentProjection" ~ "ProjectedYearOnYearEmploymentGrowth", TRUE ~ metric)) %>%
        select(-latest, -valueText, -timePeriod) %>%
        rename(Area = geogConcat, Period = chartPeriod, Metric = metric, Value = value),
      "CurrentAreaBreakdown" = filter(
        C_breakdown,
        metric == input$splashMetric,
        (geogConcat == input$geoChoice |
          geogConcat %in% input$geoComps |
          geogConcat == "England")
      ) %>%
        mutate(metric = case_when(metric == "employmentProjection" ~ "ProjectedEmploymentGrowthTo2035", TRUE ~ metric)) %>%
        select(-valueText) %>%
        rename(Area = geogConcat, Metric = metric, Value = value, Breakdown = breakdown, Subgroup = subgroup)
    )
  })
  nameDownloadV1Current <- reactive({
    paste0(currentMetric(), "-", input$geoChoice, ".xlsx")
  })
  output$downloadV1Current <- downloadHandler(
    filename = function() {
      nameDownloadV1Current()
    },
    content = function(file) {
      write_xlsx(listDownloadV1Current(), path = file)
    }
  )

  ## 2.4 DataHub----
  ### 2.4.1 Filters----
  output$hubAreaInput <- renderUI({
    selectizeInput(
      "hubArea",
      multiple = TRUE,
      label = NULL,
      options = list(placeholder = "Choose LEP, LSIPs, MCAs, LAs*"),
      choices = areaChoices
    )
  })

  output$hubMetricInput <- renderUI({
    selectizeInput(
      "hubMetric",
      choices = C_datahub %>% filter(
        if (is.null(input$hubArea) == TRUE) {
          TRUE
        } else {
          Area %in% input$hubArea
        }
      ) %>%
        distinct(Metrics = metricNeat),
      multiple = TRUE,
      label = NULL,
      options = list(placeholder = "Choose metrics*")
    )
  })

  output$hubBreakdownInput <- renderUI({
    selectizeInput(
      "hubBreakdowns",
      choices = C_datahub %>% filter(
        if (is.null(input$hubArea) == TRUE) {
          TRUE
        } else {
          Area %in% input$hubArea
        },
        if (is.null(input$hubMetric) == TRUE) {
          TRUE
        } else {
          metricNeat %in% input$hubMetric
        }
      ) %>% distinct(Breakdown),
      multiple = TRUE,
      label = NULL,
      options = list(placeholder = "Choose breakdowns")
    )
  })

  output$hubYearInput <- renderUI({
    selectizeInput(
      "hubYears",
      choices = C_datahub %>% filter(
        if (is.null(input$hubArea) == TRUE) {
          TRUE
        } else {
          Area %in% input$hubArea
        },
        if (is.null(input$hubMetric) == TRUE) {
          TRUE
        } else {
          metricNeat %in% input$hubMetric
        },
        if (is.null(input$hubBreakdowns) == TRUE) {
          TRUE
        } else {
          Breakdown %in% input$hubBreakdowns
        }
      ) %>%
        distinct("Time period" = Period),
      multiple = TRUE,
      label = NULL,
      options = list(placeholder = "Choose period*")
    )
  })

  ### 2.4.2 Table----
  datahubDataset <- reactive({
    C_datahub %>%
      filter(
        (if (is.null(input$hubArea) == TRUE) {
          TRUE
        } else {
          {
            Area %in% input$hubArea
          } |
            (if ("Yes" %in% input$hubLA) {
              Area %in% (
                C_Geog %>% filter(geog == "LADU", (LEP %in% input$hubArea | LSIP %in% input$hubArea | MCA %in% input$hubArea))
                  %>% distinct(geogConcat)
              )$geogConcat
            } else {
              Area == "xxx"
            }) |
            (if ("National" %in% input$hubComparators) {
              Area == "England"
            } else {
              Area == "xxx"
            })
        }),
        if (is.null(input$hubYears) == TRUE) {
          TRUE
        } else {
          Period %in% input$hubYears
        },
        if (is.null(input$hubMetric) == TRUE) {
          TRUE
        } else {
          metricNeat %in% input$hubMetric
        },
        (if (is.null(input$hubBreakdowns) == TRUE) {
          TRUE
        } else {
          Breakdown %in% input$hubBreakdowns
        })
      ) %>%
      select(
        Period = Period,
        Area,
        Data = metricNeat,
        Breakdown,
        Subgroup,
        Value = valueText
      )
  })

  output$hubTable <- renderDataTable({
    DT::datatable(datahubDataset())
  })

  # Download button
  filtered_data1 <- reactive({
    list("LocalSkillIndicators" = datahubDataset())
  })
  output$hubDownload <- downloadHandler(
    filename = function() {
      "LocalSkillsDataset.xlsx"
    },
    content = function(file) {
      write_xlsx(filtered_data1(), path = file)
    }
  )

  ### 2.4.3 Unique code----
  # allOptions <- bind_rows(
  #   data.frame(
  #     Choice = c("LEP", "LSIP", "MCA", "LA"),
  #     filterID = "a"
  #   ),
  #   C_datahub %>% distinct(Choice = area) %>% mutate(filterID = "b"),
  #   data.frame(Choice = c("Yes", "No"), filterID = "c"),
  #   data.frame(
  #     Choice = c("National", "Regional (to come)"),
  #     filterID = "d"
  #   ),
  #   C_datahub %>% distinct(Choice = metricNeat) %>% mutate(filterID = "e"),
  #   C_datahub %>% distinct(Choice = breakdown) %>% mutate(filterID = "f"),
  #   C_datahub %>% distinct(Choice = as.character(time_period)) %>% mutate(filterID = "g")
  # ) %>%
  #   group_by(filterID) %>%
  #   mutate(ChoiceNo = row_number()) %>%
  #   mutate(ChoiceID = paste0(filterID, ChoiceNo)) %>%
  #   ungroup() %>%
  #   select(-filterID, -ChoiceNo)
  #
  # output$uniqueCode <- renderUI({
  #   allOptions %>%
  #     mutate(
  #       chosen = case_when(
  #         Choice %in% input$hubArea ~ 1,
  #         Choice %in% input$hubMetric ~ 1,
  #         Choice %in% input$hubGeog ~ 1,
  #         Choice %in% input$hubComparators ~ 1,
  #         Choice %in% input$hubLA ~ 1,
  #         Choice %in% input$hubBreakdowns ~ 1,
  #         Choice %in% input$hubYears ~ 1,
  #         TRUE ~ 0
  #       )
  #     ) %>%
  #     filter(chosen == 1) %>%
  #     select(ChoiceID) %>%
  #     summarize(
  #       strong = str_c(ChoiceID, collapse = ""),
  #       .groups = "drop"
  #     )
  # })

  # ## 2.6 FE interventions table----
  # output$interventionTable <- DT::renderDataTable({
  #   DT::datatable(
  #     I_InterventionTable,
  #     escape = FALSE,
  #     options = list(dom = "t"),
  #     rownames = FALSE
  #   )
  # })

  ## 2.6 FE tools table----
  output$toolsTable <- DT::renderDataTable({
    DT::datatable(
      I_ToolsTable,
      escape = FALSE,
      options = list(dom = "t"),
      rownames = FALSE
    )
  })

  ## 2.7 FE sources table----
  output$sourcesTable <- DT::renderDataTable({
    DT::datatable(
      I_SourcesTable,
      escape = FALSE,
      options = list(dom = "t"),
      rownames = FALSE
    )
  })

  # 3.Stop app -----
  session$onSessionEnded(function() {
    stopApp()
  })
}
