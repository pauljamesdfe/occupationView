# 1. Set up page, header, js and css ----
## 1.1 Set up page and browser info ----
fluidPage(
  title = tags$head(tags$link(
    rel = "shortcut icon",
    href = "dfefavicon.png"
  )),
  shinyjs::useShinyjs(),
  useShinydashboard(),
  # Setting up cookie consent based on a cookie recording the consent:
  # https://book.javascript-for-r.com/shiny-cookies.html
  tags$head(
    tags$script(
      src = paste0(
        "https://cdn.jsdelivr.net/npm/js-cookie@rc/",
        "dist/js.cookie.min.js"
      )
    ),
    tags$script(src = "cookie-consent.js")
  ),
  tags$head(
    tags$link(
      rel = "stylesheet",
      type = "text/css",
      href = "dfe_shiny_gov_style.css"
    )
  ),
  # use_tota11y(), # accessibility layer for local testing

  # Set metadata for browser
  tags$html(lang = "en"),
  tags$head(
    tags$meta(name = "application_name", content = "Unit for Future Skills - Occupations Dashboard"),
    tags$meta(name = "description", content = "Data dashboard presenting occupations data from the Unit for Future Skills in the Department for Education."),
    tags$meta(name = "subject", content = "Education data dashboards.")
  ),

  # Set title for search engines
  HTML("<title>Occupations Dashboard</title>"),
  # Setting up cookie consent based on a cookie recording the consent:
  # https://book.javascript-for-r.com/shiny-cookies.html
  tags$head(
    tags$script(
      src = paste0(
        "https://cdn.jsdelivr.net/npm/js-cookie@rc/",
        "dist/js.cookie.min.js"
      )
    ),
    tags$script(src = "cookie-consent.js")
  ),
  tags$head(includeHTML(("google-analytics.html"))),

  ## 1.2. Internal CSS ----
  tags$head(
    tags$style(
      HTML(
        "

    /* remove the max width of the main panel so spreads across screen*/
.govuk-width-container {
    max-width: 100%;
}

/*style filter row grey background*/
.filterRow{
background-color: #f3f2f1; /*#1d70b8*/
border-radius: 4px;
padding: 15px 15px 0px 15px;
}

/*filter labels*/
.control-label {
    color: #000;
}

/*add white border*/
.chartBox{
background-color:#f3f2f1;
border-right: 5px solid white;
}

/*right allign links*/
.rightAlignLinks{
text-align: right;
padding-right:15px;
padding-top:15px
}

/*on the data table hide links to overlay with clicakble links*/
.hiddenLink {
  visibility: hidden;
}

/* styles for menu button*/
    #menuButton {
      display: none;
      width: auto;
    }

    .menuBtn {
      color: #fff;
      float: left;
      padding: 10px;
    }

        .menuBtn:focus {
      color: #fff;
      background-color: #000;
    }

/* for mobile*/
    @media (max-width: 767px) {
      .nav-stacked {
        display: none;
      }
      .nav-stacked.active {
        display: block;
      }

      #menuButton {
        display: block;
      }

      .menuBtn.active {
        background-color: #fff;
        color: #000;
      }
    }

    /* style KPI boxes. Here are some colour options for easy access
    #12436D - govt analytical function blue
    #AAE3F0 - dfe corp turquoise 40% tint
    #28A197 - govt analytical function turquoise
    #DAEEBF - dfe corp lime 40% tint
    #718ea7 - govt analytical function blue 40% tint
    #7ec7c1 - govt analytical function turquoise 40% tint
    */

.small-box.bg-geo1{
    background-color: #12436D !important;
    color: #fff;
    padding: 2px 2px 4px 12px;
}

.small-box.bg-geo2{
    background-color: #28A197 !important;
    color: #fff;
    padding: 2px 2px 4px 12px;
}

.small-box.bg-geo3{
    background-color: #BFBFBF !important;
    color: #fff;
    padding: 2px 2px 4px 12px;
}

/* map popup styling*/
div.myspecial-popup div.leaflet-popup-content-wrapper {
          padding: 0px 0px 1px 0px;

}

 /* overwrite ccs to keep margin*/
@media (min-width:1020px) {
    .govuk-width-container {
        margin-right: max(30px, calc(15px + env(safe-area-inset-left)));
        margin-left: max(30px, calc(15px + env(safe-area-inset-left)))
    }
    @supports (margin:max(calc(0px))) {
        .govuk-width-container {
            margin-right: max(30px, calc(15px + env(safe-area-inset-left)));
            margin-left: max(30px, calc(15px + env(safe-area-inset-left)))
        }
    }
}

"
      )
    ),
    ## 1.3. Javascript and HTML banner----
    # Collapsible menu js
    tags$script(
      HTML(
        '
    /* javascript function for menu button */
    function collapseMenu() {
      var x = document.getElementById("navbar");
      x.classList.toggle("active");

      var x = document.getElementById("menuButton");
      x.classList.toggle("active");
    }
    '
      )
    )
  ),

  # Force the top nav bar to left align and centre the title
  HTML(
    '<header class="govuk-header" role="banner">
    <div class="govuk-header__container">
    <div class="govuk-header__logo" style="width: 15%; margin-left: 15px;float:left;">
    <a href="https://www.gov.uk/government/organisations/department-for-education" class="govuk-header__link govuk-header__link--homepage">
    <span class="govuk-header__logotype">
   <img src="images/DfE_logo.png" class="govuk-header__logotype-crown-fallback-image"/>
    <span class="govuk-header__logotype-text">DfE</span>
    </span>
    </a>
    </div>
    <div class="govuk-header__content" style="width: 70%; text-align: center;float:left;">
    <a href="https://www.gov.uk/government/groups/unit-for-future-skills" class="govuk-header__link govuk-header__link--service-name" style="font-size: 24px;">Unit for Future Skills - Occupations Dashboard</a>
    </div>
        <a href="javascript:void(0);" id="menuButton" class="menuBtn" onclick="collapseMenu()">
    <i class="fa fa-bars" style="font-size:24px;"></i></a>
    </div>
    </header>'
  ),

  # Add bug header
  HTML(
    '<div class="govuk-phase-banner govuk-width-container govuk-main-wrapper" id="beta banner" style="margin-left:0px;margin-right:0px">
  <p class="govuk-phase-banner__content">
    <strong class="govuk-tag govuk-phase-banner__content__tag ">beta</strong>
    <span class="govuk-phase-banner__text">We are aware of performance issues that require some users to reload the page. We are working to fix this.
</span>
  </p>
</div>'
  ),

  # Force page to scroll to top when links clicked
  tags$script(
    " $(document).ready(function () {
         $('#navbar a[data-toggle=\"tab\"]').on('click', function (e) {
          window.scrollTo(0, 0)
               });
               });"
  ),

  # 2 Main page ----
  navlistPanel(
    id = "navbar",
    widths = c(2, 10),
    well = FALSE,
    selected = "Occupations",

    ## 2.1 User guide ----

    tabPanel(
      "User guide",
      ### 2.1.1 Intro ----
      fluidRow(column(
        12,
        h1("Occupations Dashboard"),
        p(
          "The Occupations dashboard provides published occupation data from a variety of sources in an easy to navigate format. "
        ),
        p(
          "Data is available to view and download for various geographies, including: local authority (LA), local skills improvement plan (LSIP) area, local enterprise partnership (LEP), Mayoral Combined Authority (MCA), regional and national."
        ),
        p(
          "This dashboard is produced by the ",
          # "To access the additional dashboards developed to help users further understand the labour market outcomes of training use the links below, or from the ",
          a(
            href = "https://www.gov.uk/government/groups/unit-for-future-skills",
            "Unit for Future Skills",
            .noWS = c("after")
          ),
          ", an analytical and research unit within the Department for Education. For more information on the Unit's aims and to access additional dashboards and data to help users further understand the labour market outcomes of training visit our",
          a(
            href = "https://www.gov.uk/government/groups/unit-for-future-skills",
            "webpage.",
            .noWS = c("after")
          ),
          # " webpage."
        )
      )),
      # end intro text row

      ### 2.1.2 Contents ----
      fluidRow(column(
        12,
        div(
          class = "panel panel-info",
          div(
            class = "panel-heading",
            style = "color: white;font-size: 18px;font-style: bold; background-color: #1d70b8;",
            h2("How to use this dashboard")
          ),
          div(
            class = "panel-body",
            p("Use the navigation bar on the left to select the tab you want to view."),
            h2("Dashboard structure"),
            tags$ul(
              tags$li(actionLink("link_to_tabpanel_occupations", "Occupations"), " - this tab provides a time series summary of employment, qualifications, and further education outcomes for the selected geographic area. Metrics are divided into two columns: Labour market and Skills. Labour market includes employment, online job adverts (experimental), and micro business count (0-9 employees). Skills covers education and training ahcievements, apprenticehsip achievements, highest qualification level, and Key Stage 5 positive destinations."),
            tags$li(actionLink("link_to_tabpanel_accessibility", "Accessibility"), "- provides the Occupations dashboard accessibility statement, compliance requirmeents, limitations and opportunity to feedback on accessibility of the dashboard."),
              tags$li(actionLink("link_to_tabpanel_supportandfeedback", "Support and feedback"), " - provides links to the Unit for Future Skills and Department for Education Statistics Development inboxes for feedback and if you hve any questions about the dashboard or the data it contains. There is also a link to the GitHub repository if you wish to view the dashboard source code.")
            ),
            h2("Local skills metrics"),
            p(
              "Where published figures are not available, area totals for LEP, LSIP or MCA are calculated by adding up the relevant local authorities - rounding errors may be present in these geographic areas where local authority total volumes are rounded and small volumes are suppressed."
            ),
            p(
              "The ONS have announced that, due to a coding error, their occupational data should be used with caution. For more information see this ONS ",
              a(
                href = "https://www.ons.gov.uk/employmentandlabourmarket/peopleinwork/employmentandemployeetypes/articles/theimpactofmiscodingofoccupationaldatainofficefornationalstatisticssocialsurveysuk/2022-09-26",
                "article",
                .noWS = c("after")
              ),
              "."
            ),
            h3(actionLink(
              "link_to_tabpanel_occupations", "Occupations"
            )),
            p(
              "The occupations tab provides data for the selected geographic area. "
            ),
            p(""),
            p(
              "SOC2020 data for occupations is available for the latest period via NOMIS but is not included here due to ongoing ONS coding issues."
            ),
            p(
              "This is an experimental metric of online job advert data, split by profession, for the selected geographic area, and the option to compare against another area in England at the same geographic level. "
            ),
            p(
              "Projected employment growth until 2035. Sector, industry, occupation and qualification projected growths are available. LA level data is not available for this dataset."
            )
          )
        )
      )),
      # end of dashboard contents row

      ### 2.1.3 Version control ----
      fluidRow(column(
        12,
        div(
          class = "panel panel-info",
          div(
            class = "panel-heading",
            style = "color: white;font-size: 18px;font-style: bold; background-color: #1d70b8;",
            h2("Update history")
          ),
          div(
            class = "panel-body",
            h2("Latest update"),
            p("x July 2023 (1.0.1)"),
            p("Corrected the Worcestershire LEP and Stoke-on-Trent and Staffordshire LEP LA level map to include all areas."),
            details(
              label = "Previous updates",
              inputId = "PreviousUpdate",
              p(
                p("xx June 2023 (1.0.x)"),
                  p("Started using the Nomis API to get NOMIS data. More information on the API ",
                          a(
                            href = "https://www.nomisweb.co.uk/api/v01/help",
                            "here",
                            .noWS = c("after", "before")
                          ),
                          "."),
               )
            ),
            h2("Future development"),
            p(
              "The dashboard will be kept up to date with the latest data shortly after it is released – check the data downloads page for dates when new data is published. If there are further data or dashboard features that you would find useful please contact us at ",
              # p(
              #   "We will be regularly adding more data and visualisations to the dashboard based on the user feedback we have received.  If there are further data or dashboard features that you would find useful please contact us at ",
              a(
                href = "mailto:ufs.contact@education.gov.uk",
                "ufs.contact@education.gov.uk",
                .noWS = c("after")
              ),
              "."
            )
          )
        )
      )) # end of version control row
    ),
    # end of homepage Panel

    ## 2.3 Occupations ----
    tabPanel(
      "Occupations",
      br(),
      ### 2.3.1 Filters ----
      fluidRow(column(4,
                      p("Sector"),
                      materialSwitch(inputId = "sectorSwitch", label = NULL, status = "primary"),
                      selectizeInput(
                        "sectorChoice",
                        multiple = TRUE,
                        label = NULL,
                        choices = c("Science and Tech","Green")
                        ,selected="Science and Tech"
                      )
                      ),
        column(
          4,
          p("Occupations (SOC submajor)"),
          materialSwitch(inputId = "socSwitch", label = NULL, status = "primary"),
          selectizeInput(
            "socChoice",
            multiple = TRUE,
            label = NULL,
            choices = C_time%>%
              filter(metric=="Jobs")%>%
            distinct(subgroup)
            ,selected="Corporate managers and directors"
          )
        ),
      column(
        4,
        p("Demographies"),
        materialSwitch(inputId = "demSwitch", label = NULL, status = "primary"),
        selectizeInput(
          "demChoice",
          multiple = TRUE,
          label = NULL,
          choices = c("region","full-time/part-time","industry (SIC 2007)","flexibility","Ethnic group")
          ,selected="Ethnic group"
        ),
        selectInput(
          "ethChoice",
          multiple = TRUE,
          label = NULL,
          choices = c("White","Mixed")
          ,selected="Mixed"
        )
      ),
        # column(          4,
        #                  p("Choose an occupation(s) (Summary Profession Category)"),
        #                  selectizeInput(
        #                    "profChoice",
        #                    multiple = TRUE,
        #                    label = NULL,
        #                    choices = C_time%>%
        #                      filter(metric=="Online job adverts")%>%
        #                      distinct(subgroup)
        #                  )),
        # column(
        #   3,
        #   uiOutput("screenshotFile")
        # )
      ),
      # fluidRow(
      #   column(
      #     12,
      #     p(uiOutput("subheading"))
      #   )
      # ),
      fluidRow(
        column(
          4,
          materialSwitch(inputId = "gridSwitch", label = "Grid charts", status = "primary",right=TRUE),

          materialSwitch(inputId = "changeSwitch", label = "Annual change", status = "primary",right=TRUE)
        )),
      ### 2.3.2 Visuals row 1 ----
      fluidRow(
        column(
          12,
           h3("How is employment changing over time?"),
          # p(uiOutput("commentTime")),
          # uiOutput("geoComp"),
          withSpinner(plotlyOutput("occTime")),
          p("NB non-zero axis.")
        )
      ),
      fluidRow(column(
        12,
        h3("How are earnings changing over time?")
      )),
      fluidRow(column(
        12,
        h3("How is employment different across the England?")
      )),
      fluidRow(
        column(
          4,
          p("Choose a geography"),
          radioGroupButtons(
            inputId = "splashGeoType",
            choices = c("LEP", "LSIP", "MCA")
          ),
          # p(uiOutput("commentMap")),

          # p(uiOutput("mapFoot"))
        ),
        column(          4,
                         p("Choose a year"),
                         sliderInput(
                           "timeChoice",NULL,min=2017,max=2021,value=2021,sep=""
                           #multiple = TRUE,
                           #label = NULL,
                           # choices = C_mapData%>%
                           #   filter(metric=="Jobs")%>%
                           #   distinct(chartPeriod)
                         ))

      ),
      fluidRow(
        column(6,
        withSpinner(leafletOutput("map"))
        )
      ),
      br(),
      ### 2.3.3 Downloads ----
      fluidRow(
        column(
          width = 3,
          downloadButton(
            outputId = "downloadV1All",
            label = "All areas   ",
            icon = shiny::icon("download"),
            class = "downloadButton"
          )
        ),
        column(
          width = 9,
          "Download metric data for all geographies (LEPs, LSIP, MCA areas, LAs, regions and England)",
        )
      ),
      fluidRow(
        column(
          width = 3,
          downloadButton(
            outputId = "downloadV1Current",
            label = "Current geographic areas",
            icon = shiny::icon("download"),
            class = "downloadButton"
          )
        ),
        column(width = 9, "Download metric data for the selected geographic areas")
      ),
      ### 2.3.3 Data notes ----
      fluidRow(column(
        12,
        h2("Data notes"),
        # p(uiOutput("dataSource")),
        # p(uiOutput("dataNote")),
        # p("Caveats:"),
        # p(uiOutput("dataCaveat"))
      )),
      br()
    ),

    ## 2.5 Data information ----
    tabPanel(
      "Data sources",
      ### 2.5.1 Data sources table ----
      fluidRow(column(
        12,
        h1("Data sources"),
        dataTableOutput("DataTbl"),
        uiOutput("hidden_downloads")
      )),
      # end of data table row
      ### 2.5.2 Data details text ----
      fluidRow(
        column(
          12,
          h2("Data information"),
          h3("Annual Population Survey"),
          p(
            "The Annual Population Survey (APS) is a continuous household survey covering the UK.
          Topics included cover employment and unemployment, and education as well as housing, ethnicity and religion.
            This dashboard currently shows employment volumes for each geographic area and by occupation (SOC2010)."
          ),
          p(
            "The ONS have announced that, due to a coding error, their occupational data should be used with caution. For more information see this ONS  ",
            a(
              href = "https://www.ons.gov.uk/employmentandlabourmarket/peopleinwork/employmentandemployeetypes/articles/theimpactofmiscodingofoccupationaldatainofficefornationalstatisticssocialsurveysuk/2022-09-26",
              "article",
              .noWS = c("after")
            ),
            ". SOC2020 data is available for the latest period via NOMIS but is not included here due to ongoing ONS coding issues."
          ),
          p(
            "Each estimate from the Annual Population Survey carries a margin of error.
            These are available in the original data via NOMIS. Large margins of error are usually associated with groups with only a small number of respondents.
            Therefore, please take caution when interpreting data from small subgroups."
          ),
          h3("ONS-Textkernel online job adverts"),
          p(
            "These data tables are based on experimental data based on Textkernel online job adverts. Textkernel data is web-scraped from job advert information from approximately 90,000 job boards and recruitment pages.
            The dashboard shows the monthly average number of live adverts from 2017 to 2022."
          ),
          p(
            "Advert volumes are shown split by profession. Textkernel have derived these professions from the job advert job title. These professions do not align directly to the Standard Occupation Classification (SOC2020). ONS are working on using SOC coding in future releases of this data."
          ),
          p(
            "Counts have been rounded to the nearest 5 and so totals may not add due to this rounding. The scope of online job adverts does not fully capture the scope of UK economic activity because of differing advertising methods, for example, casual work may be advertised by word-of-mouth or in shop windows as opposed to online."
          ),
          p(
            "As this data is experimental, there are some quality issues with the data. The ONS dataset has a full rundown on its cover sheet (link on downloads page). In brief:"
          ),
          tags$ul(
            tags$li(
              "There are methodological changes throughout the time series (classification of profession and location) that may result in step-changes. "
            ),
            tags$li(
              "When job location information is limited, the centroid of the region is used. This may lead to clustering of job counts."
            ),
          ),
          p("There are errors in the published data for two areas: Dorset LSIP and Enterprise M3 LEP (including all of Surrey) LSIP. This is due to the incorrect mapping of LAs. In the dashboard we have corrected Dorset LSIP (by using the values for the Dorset LEP which has the same geography) so this is accurate in the dashboard but will not match the published data. For the Enterprise M3 LEP (including all of Surrey) LSIP we have estimated the value by looking the broader region and calculating the value of the Enterprise LSIP having removed other LSIPs in the region. This will come with some rounding issues. We are working to get the published data corrected."),
          
          h3("Skills Imperative 2035 employment projections"),
          p(
            "The occupation projections are based on ONS survey data that has been impacted by a coding error, so should be used with caution.  For more information on the miscoding of occupation data see this ",
            a(
              href = "https://www.ons.gov.uk/employmentandlabourmarket/peopleinwork/employmentandemployeetypes/articles/theimpactofmiscodingofoccupationaldatainofficefornationalstatisticssocialsurveysuk/2022-09-26",
              "ONS article",
              .noWS = c("after")
            ),
            "."
          ),
          p(
            "Skills Imperative 2035 projects the future size and shape of the labour market by considering employment prospects by industry, occupation, qualification level.
            The dashboard shows the year on year growth of employment as well as the long term growth from 2023 to 2035.
            The employment volumes are available in the data downloads.
            "
          ),
          p("The projections are calculated from a number of different data sources, and as such precise margin errors have not been assigned.
            Care should be taken when using projections with small volumes of individuals (see Skills Imperative 2035 datasets for more detail). "),
          p("There are errors in the published data for four areas: Dorset LSIP, Enterprise M3 LEP (including all of Surrey) LSIP, Stoke-on-Trent and Staffordshire LEP, and Worcestershire LEP. This is due to the incorrect mapping of LAs. In the dashboard we have corrected Dorset LSIP, Stoke-on-Trent and Staffordshire LEP, and Worcestershire LEP (by using the values for the Dorset LEP, Stoke-on-Trent and Staffordshire LSIP, and Worcestershire LSIP which have the same geographies) so these are accurate in the dashboard but will not match the published data. For the Enterprise M3 LEP (including all of Surrey) LSIP we have estimated the value by looking the broader region and calculating the value of the Enterprise LSIP having removed other LSIPs in the region. This will come with some rounding issues. We are working to get the published data corrected."),
          br()
        )
      )
    ),
    ## 2.8 Accessibility ----
    tabPanel(
      "Accessibility",
      fluidRow(
        column(
          width = 12,
          h1("Accessibility statement"),
          p(
            "This accessibility statement applies to the Local skills dashboard.
            This dashboard is run by the Department for Education. We want as many people as possible to be able to use this application,
            and have actively developed this dashboard with accessibilty in mind."
          ),
          h2("WCAG 2.1 compliance"),
          p(
            "We follow the reccomendations of the ",
            a(href = "https://www.w3.org/TR/WCAG21/", "WCAG 2.1 requirements. "),
            "This application has been checked using the ",
            a(href = "https://github.com/ewenme/shinya11y", "Shinya11y tool "),
            ", which did not detect accessibility issues.
             This dashboard also fully passes the accessibility audits checked by the ",
            a(href = "https://developers.google.com/web/tools/lighthouse", "Google Developer Lighthouse tool"),
            ". This means that this dashboard:"
          ),
          tags$div(tags$ul(
            tags$li("uses colours that have sufficient contrast"),
            tags$li(
              "allows you to zoom in up to 300% without the text spilling off the screen"
            ),
            tags$li(
              "has its performance regularly monitored, with a team working on any feedback to improve accessibility for all users"
            )
          )),
          h2("Limitations"),
          p(
            "We recognise that there are still potential issues with accessibility in this dashboard, but we will continue
             to review updates to technology available to us to keep improving accessibility for all of our users. For example, these
            are known issues that we will continue to monitor and improve:"
          ),
          tags$div(tags$ul(
            tags$li(
              "Keyboard navigation through the interactive charts is currently limited, and some features are unavailable for keyboard only users"
            ),
            tags$li(
              "Alternative text in interactive charts is limited to titles and could be more descriptive (although this data is available in csv format)"
            )
          )),
          h2("Feedback"),
          p(
            "If you have any feedback on how we could further improve the accessibility of this dashboard, please contact us at",
            a(href = "mailto:statistics.development@education.gov.uk", "statistics.development@education.gov.uk")
          ),
          br()
        )
      )
    ),
    # End of accessibility tab

    ## 2.9 Support ----
    tabPanel(
      "Support and feedback",
      support_links() # defined in R/supporting_links.R))
    )
  ),
  # End of navBarPage
  # 3 Footer ----

  shinyGovstyle::footer(TRUE)
)
