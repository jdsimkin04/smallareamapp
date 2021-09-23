# library(tidyverse)
# library(magrittr)
# library(ggthemes)
# library(readr)
# library(epitools)
# library(viridis)
# library(viridisLite)
# library(kableExtra)
# library(DT)
# library(sf)
# library(tmap)
# library(tmaptools)
# library(spdep)
# # library(INLA)
# library(cowplot)
# library(shiny)
# library(shinythemes)
# library(shinyscreenshot)
# library(shinycssloaders)
# library(bs4Dash)
# library(shinydashboardPlus)

options(spinner.color="#69B4FB", #spinner.color.background="#ffffff",
        spinner.size=1, spinner.type = 7, hide.ui = FALSE)

options(shiny.maxRequestSize = 30*1024^2)

ui <- bs4DashPage(
  #Title for Header
  bs4DashNavbar(
    title = "Small area mApp",
    skin = "dark",
    status = "gray-dark"
                  ),

  #Sidebar menu with sub-menus
  bs4DashSidebar(
    skin = "dark",
    bs4SidebarMenu(
      id = "sidebarMenu",
    menuItem(
      "Welcome",
      tabName = "menu_1",
      icon = icon("info")
    ),
    menuItem(
      "File Upload",
      tabName = "menu_2",
      icon = icon("database"),
      #Subitem to upload event data
      menuSubItem(
        "Disease event data",
        tabName = "menu_3",
        icon = NULL
      ),
      #subitem to upload shapefile
      menuSubItem(
        "Geography shapefile",
        tabName = "menu_4",
        icon = NULL
      )
    ),

    menuItem(
      "Analytics",
      tabName = "pre_menu_5",
      icon = icon("chart-bar"),
      menuSubItem(
        "Spatial modeling",
      tabName = "menu_5",
      icon = NULL),
      menuSubItem(
        "Model validation",
        tabName = "menu_6",
        icon = NULL),
      menuSubItem(
        "Spatial autocorrelation",
        tabName = "menu_7",
        icon = NULL)
    )
  ),
  actionButton("map1", " Snapshot map", status = "secondary", size = "sm", icon = icon("camera-retro")),
  # actionButton("map2", " Snapshot map #2", status = "secondary", size = "sm", icon = icon("camera-retro")),
  # actionButton("model_summary", " Download  model results as RData", status = "secondary", size = "sm", icon = icon("download"))
  downloadButton('model_summary', 'Download Model', class="dlButton")


  ),
  controlbar = bs4DashControlbar(
    #controlbar settings
    skin = "dark",
    title = "Analytics panel",
    width = 250,
    id = "controlbar",


    controlbarMenu(
    id = "controlbarMenu",
    #controlbar items
    controlbarItem(
      "Analytics menu",
    selectInput("cancer_var", "Cancer type",""),
    selectInput("sex_var", "Sex", ""),
    p(strong("Bayesian modeling of cases")),
    selectInput("model_choice", "Choose a model", c("bym2", "bym"), selected = "bym2"),
      selectInput("spatial_choice", "Implement Spatial Poisson w/INLA?", c("No", "Yes"), selected = "No"),
      p("For exceedence probabilities, please specify a threshold"),
      numericInput(inputId = "threshold", label="Relative Risk Threshold", value = 1.10),
    p("Customize your map"),
    conditionalPanel(
      condition = "input.spatial_choice == 'No'",
    selectInput("variable_var", "Variable for Map", c("cases", "exp", "SIR", "area_pop"), selected = "SIR")
    ),
    conditionalPanel(
      condition = "input.spatial_choice != 'No'",
      selectInput("variable_var1", "Variable for Map", c("cases", "exp", "SIR", "RR", "exc", "area_pop"), selected = "RR")
    )

    # selectInput("variable_var2", "Variable for Map #2", c("cases", "exp", "SIR","RR", "exc", "area_pop"), selected = "cases")
    # selectInput("map_style", "Map style", c("fixed", "jenks", "sd", "cont"), selected = "cont"),
    # conditionalPanel(
    #   condition = "input.map_style == 'fixed'",
    #   wellPanel(
    #   numericInput("breaks_min", "min: ", 1, min = 0, max = NA),
    #   numericInput("breaks_max", "max: ", 2, min = 0, max = NA),
    #   numericInput("breaks_step", "step: ", 0.25, min = 0, max = NA)
    #   )),
    # selectInput("map_palette", "Palette", c("Reds", "viridis", "RdBu", "Greens"), selected = "Reds")
  ))),
  # Body items
  bs4DashBody(
    # Welcome page
    bs4TabItems(
      bs4TabItem(
        tabName = "menu_1",
        #Title for welcome page
    jumbotron(
          title = p("Welcome to the", em("small area mApp!")),
          lead = "This Shiny app helps you estimate disease risk across small geographical areas using Bayesian spatial modelling.",
          h4(em("Getting Started")),
          "Check out the sidebar on the left. Upload data on the ", em("File Upload"), " tab. Analyze data with ", em("Analytics"), " tab.",
          br(),
          "Interested in the back end R code? Or a quick demo of the App functions? Check out the GitHub Repo.",
          status = "info",
          btnName = "App GitHub Repo",
          href = "https://github.com/jdsimkin04"
        ),
    fluidRow(
      # Developer/contributor information
      bs4UserCard(
        title = bs4UserDescription(
          title = "Jonathan Simkin",
          subtitle = "Developer",
          "text here..",
          type = 2,
          image = "https://avatars2.githubusercontent.com/u/41752372?s=460&u=b5974705888e814e9d4661b77be1c2f2357779b3&v=4"
        ),
        "The methodological approach used in this App was co-designed with a fantastic team of people. Please see our journal article for a use case of this App in population oncology research and a full description of the methods involved.",
        "If you have any feedback, issues, or ideas, please reach out on Twitter or GitHub.",
        br(),
        br(),
        collapsible = F,
        status = "gray",
        gradient = TRUE,
        background = "white",
        boxToolSize = "xl",
        # "Some text here!",
        bs4ListGroup(
          bs4ListGroupItem( # Link to github
            tags$div(HTML('<i class="fa fa-book"></i>', '<a href = https://github.com/jdsimkin04> Journal article!</a>')),
            type = "action",
            src = "https://github.com/jdsimkin04"
          ),
          bs4ListGroupItem( #Link to twitter
            tags$div(HTML('<i class="fa fa-twitter" style = "color:#0072B2;"></i>', '<a href = https://twitter.com/jdsimkin04?lang=en> @jdsimkin04</a>')),
            type = "action",
            src = "https://twitter.com/jdsimkin04?lang=en"
          ),
          bs4ListGroupItem( # Link to github
            tags$div(HTML('<i class="fa fa-github"></i>', '<a href = https://github.com/jdsimkin04> Github</a>')),
            type = "action",
            src = "https://github.com/jdsimkin04"
          )
        )
      )
    )
    ),
    # Disease event data file upload
    bs4TabItem(
      tabName = "menu_3",

        column(
          width = 12,
          # Instructions Card
          bs4Card(
            "You need ", strong("two"), " things for this app to work:",
               tags$ol(tags$li("A CSV file with observed cases, age-adjutsed expected counts, and standardized incidence ratios by area. You can upload this data below.", br(), "To prep this data, we recommend using the", a("epitools", href = "https://cran.r-project.org/web/packages/epitools/epitools.pdf"), "R package and run the ", em("ageadjust.indirect() "), "function."),
                       tags$li("Boundary files for the regions of interest. Upload this on the ", em("Geography shapefile"), "tab.")),
            br(),
            h3("No data? Don't worry! We got you covered."), "Select 'Yes' below for Scotland Lip Cancer data. Now go", em("Geography shapefile"), "tab", "to create the weight's matrix.",
            selectInput("scotland_lip", label = "Scotland Lip Cancer sample dataset", choices = c("No", "Yes"), selected = "No"),
            title = "Instructions",
            status = "info",
            solidHeader = T,
            collapsible = F,
            collapsed = F,
            closable = FALSE,
            labelStatus = "info",
            labelText = "",
            width = 12
          )
        ),
        fluidRow(
          #Health data upload card
        column(6,
          bs4Card(
          title = "1. Upload health data",
          status = "info",
          solidHeader = T,
          collapsible = TRUE,
          collapsed = T,
          closable = FALSE,
          labelStatus = "info",
          labelText = "text",
          width = 12,
          strong("IMPORTANT!: "), "Your csv file MUST have the same headers as the table in the preview tab. See the preview table to the right for an example.",
          br(),
          fluidRow(
            #Columns for the data table to be uploaded
            column(6,
          tags$ol(tags$li("area_name = Areal unit name (must match shapefile area name)"), tags$li("cancer = Cancer type"), tags$li("sex = Biological sex"), tags$li("cases = Observed cases"), tags$li("exp = Expected cases"), tags$li("sir = Standardized incidence ratio"), tags$li("lci = Lower confidence interval"), tags$li("uci = Upper confidence interval"), tags$li("area_pop = Areal unit populations"))),
          #Buttons for upload, previewing the table and reseting the table
          column(6,
            fileInput(inputId = "file_case",
                            label = "Upload csv.",
                            accept = c(".csv")),
          actionButton("do", "Update preview table"), actionButton("reset", "Reset to template table"),
          br(),
          br(),
          selectInput("area_name_table", "What column is your area name?","")))
        )),
        column(6,
        bs4Card(
          title = "Preview data",
          status = "info",
          solidHeader = T,
          collapsible = TRUE,
          collapsed = T,
          closable = FALSE,
          labelStatus = "info",
          labelText = "text",
          width = 12,
          DT::dataTableOutput("table_input")
          )))),
    bs4TabItem(
      tabName = "menu_4",
      fluidRow(
        #Geography shapefile upload
        column(6,
               bs4Card(
                 title = "2. Upload map shapefile",
                 status = "info",
                 solidHeader = T,
                 collapsible = TRUE,
                 collapsed = FALSE,
                 closable = FALSE,
                 labelStatus = "info",
                 labelText = "text",
                 width = 12,
                 "Please upload a shapefile for the geography of interest.",
                 br(),
                 "Note that the Scotland map is already uploaded, you just need a spatial weights matrix with the button below.",
                 br(),
                 br(),
                 strong("IMPORTANT!: "), em("The area id must match that area_name column in your csv file!"),
                 br(),
                 fileInput(inputId = "file_map",
                           label = "Upload map. Choose relevant shapefiles (e.g. .shp, .shx, .dbf, and .prj files).",
                           multiple = TRUE,
                           accept = c('.shp','.dbf','.sbn','.sbx','.shx','.prj')),
                 selectInput("area_name_map", "What column is your area name?",""),
                 # "A weights matrix is required for spatial modelling",
                 actionButton("weight_matrix", strong("Create weights matrix")), textOutput("wm_text"))),

        column(6,
               bs4Card(
                 title = "Preview map",
                 status = "info",
                 solidHeader = T,
                 collapsible = TRUE,
                 collapsed = T,
                 closable = FALSE,
                 labelStatus = "info",
                 labelText = "text",
                 width = 12,
                 tmapOutput("preview_map")
               )))
    ),
    bs4TabItem(
      tabName = "menu_5",
      fluidRow(
        #Card for mapping variable #1, default relative risk
          bs4Card(
            title = "Map (Please select variable from control bar)",
            status = "info",
            solidHeader = T,
            collapsible = T,
            collapsed = FALSE,
            closable = FALSE,
            labelStatus = "info",
            labelText = "",
            width = 12,
            fluidRow(
            column(
              8,
            tmapOutput("var_map", height = 500) %>% withSpinner(hide.ui = FALSE)),
            column(
              4,
            selectInput("map_style1", "Map style", c("fixed", "pretty", "jenks", "sd", "cont"), selected = "pretty"),
            textInput("map_palette1", "Map Palette", "BuPu"),
            div("Run tmaptools::palette_explorer() to explore colour palettes", style = "font-size:12px;"),
            conditionalPanel(
              condition = "input.map_style1 == 'fixed'",
              wellPanel(
                div(style="display:inline-block", numericInput("breaks_min1", "min: ", 0, min = 0, max = NA)),
                div(style="display:inline-block", numericInput("breaks_max1", "max: ", 2, min = 0, max = NA)),
                div(style="display:inline-block",numericInput("breaks_step1", "step: ", 0.5, min = 0, max = NA))
              )),
           conditionalPanel(
             condition = "input.map_style1 != 'fixed'",
             wellPanel(
               numericInput("bins1", "Number of bins: ", 5, min = 0, max = NA)
             )),
           conditionalPanel(
             condition = "input.spatial_choice == 'Yes'",
           selectInput("border", "Border areas with excess risk?", c("No", "Yes"), selected = "No"),
           conditionalPanel(
             condition = "input.border == 'Yes'",
             wellPanel(
               sliderInput("pe", "Exceedance probability:", min = 0, max = 1, value = 0.8)
             )
           ))
           )
            )
          )
        #,
        #Card for mapping variable #2, default exceedance probability
        # column(
        #   6,
        #   bs4Card(
        #     title = "Map #2 (Please select variable from control bar)",
        #     status = "info",
        #     solidHeader = T,
        #     collapsible = T,
        #     collapsed = FALSE,
        #     closable = FALSE,
        #     labelStatus = "info",
        #     labelText = "",
        #     width = 12,
        #     tmapOutput("var_map2", height = 400) %>% withSpinner(hide.ui = FALSE),
        #     selectInput("map_style2", "Map style", c("pretty", "fixed", "jenks", "sd", "cont"), selected = "pretty"),
        #     textInput("map_palette2", "Map Palette", "Reds"),
        #     div("Run tmaptools::palette_explorer() to explore colour palettes", style = "font-size:12px;"),
        #     conditionalPanel(
        #       condition = "input.map_style2 == 'fixed'",
        #       wellPanel(
        #         div(style="display:inline-block", numericInput("breaks_min2", "min: ", 0, min = 0, max = NA)),
        #         div(style="display:inline-block", numericInput("breaks_max2", "max: ", 50, min = 0, max = NA)),
        #         div(style="display:inline-block",numericInput("breaks_step2", "step: ", 10, min = 0, max = NA))
        #       )),
        #     conditionalPanel(
        #       condition = "input.map_style2 != 'fixed'",
        #       wellPanel(
        #         numericInput("bins2", "Number of bins: ", 5, min = 0, max = NA)
        #       ))
        #   )
        # )
      ),
      #Card for data table include, SIR data, RR data, and Exc data
      column(
        width = 12,
        # Datatable
        bs4Card(
          title = "Datatable",
          status = "info",
          solidHeader = T,
          collapsible = T,
          collapsed = FALSE,
          closable = FALSE,
          labelStatus = "info",
          labelText = "",
          width = 12,
          background = "white",
          DT::dataTableOutput("table1") %>% withSpinner(hide.ui = FALSE)
        )
      ),
      # Datatable for calculating # of elevated areas, and excess cases
      fluidRow(
      column(
        width = 6,
        # Datatable
        bs4Card(
          title = "Excess cases among elevated areas",
          status = "info",
          solidHeader = T,
          collapsible = T,
          collapsed = T,
          closable = FALSE,
          labelStatus = "info",
          labelText = "",
          width = 12,
          htmlOutput("excess_table")
        )
      ),
      column(
        width = 6,
        # Datatable
        bs4Card(
          title = "Spatially structured effect",
          status = "info",
          solidHeader = T,
          collapsible = T,
          collapsed = T,
          closable = FALSE,
          labelStatus = "info",
          labelText = "",
          width = 12,
          htmlOutput("spatial_effect")
        )
      )
      )
    ),
    bs4TabItem(
      tabName = "menu_6",
      fluidRow(
        #Card for mapping variable #1, default relative risk
        # column(
        #   6,
        #   bs4Card(
        #     title = "Conditional predictive ordinate (CPO) Plot",
        #     status = "info",
        #     solidHeader = T,
        #     collapsible = T,
        #     collapsed = F,
        #     closable = FALSE,
        #     labelStatus = "info",
        #     labelText = "",
        #     width = 12,
        #     plotlyOutput("cpo_plot") %>% withSpinner(hide.ui = FALSE),
        #     footer = "The CPO is the probability density of an observed response based on the model fit to the rest of the data. Small CPO values reflect unexpected responses or potential outliers."
        #
        #   )
        # ),
        column(
          6,
          bs4Card(
            title = "Fitted vs. Observed values",
            status = "info",
            solidHeader = T,
            collapsible = T,
            collapsed = F,
            closable = FALSE,
            labelStatus = "info",
            labelText = "",
            width = 12,
            plotlyOutput("pred_plot") %>% withSpinner(hide.ui = FALSE),
            footer = "Scatterplot for the observed versus  predicted risk obtained from the BYM2 model."

          )
        ),
        #Card for mapping variable #2, default exceedance probability
        column(
          6,
          bs4Card(
            title = "Predictive integral transformed Plot",
            status = "info",
            solidHeader = T,
            collapsible = T,
            collapsed = F,
            closable = FALSE,
            labelStatus = "info",
            labelText = "",
            width = 12,
            plotlyOutput("pit_plot") %>% withSpinner(hide.ui = FALSE),
            footer = "PIT values are plotted on a log scale. The PIT is the probability that a new response is lower than the corresponding observed value. A uniform distribution is expected if the model represents the observations well."
          )
        )
      )
      #,
      #MSPE, R squared and potential outliers
      # fluidRow(
      #   column(
      #     width = 6,
      #     # Datatable
      #     bs4Card(
      #       title = "Model measurements",
      #       status = "info",
      #       solidHeader = T,
      #       collapsible = T,
      #       collapsed = T,
      #       closable = FALSE,
      #       labelStatus = "info",
      #       labelText = "",
      #       width = 12,
      #       htmlOutput("diagnostics_table")
      #     )
      #   ),
      #   column(
      #     6,
      #     bs4Card(
      #       title = "Alternative PIT CFD Plot",
      #       status = "info",
      #       solidHeader = T,
      #       collapsible = T,
      #       collapsed = T,
      #       closable = FALSE,
      #       labelStatus = "info",
      #       labelText = "",
      #       width = 12,
      #       plotOutput("alt_pit_plot") %>% withSpinner(hide.ui = FALSE),
      #       footer = "PIT values are plotted as a cumulative density function. The PIT is the probability that a new response is lower than the corresponding observed value. We would expect a somewhat uniform CFD line."
      #     )
      #   )
      # )
    ),
    bs4TabItem(
      tabName = "menu_7",
      fluidRow(
        #Moran's density plot
        # column(
        #   6,
          bs4Card(
            title = "Moran's I test",
            status = "info",
            solidHeader = T,
            collapsible = T,
            collapsed = F,
            closable = FALSE,
            labelStatus = "info",
            labelText = "",
            width = 12,
            plotOutput("morans_plot") %>% withSpinner(hide.ui = FALSE),
            footer = "Testing the null hypothesis that there is no spatial autocorrelation using the Moran's test for global autocorrelation. The area under the curve represents the reference distirbution for the Moran's I derived from MC simulations.
Vertical black line represents the Moran's I for the actual data."

          )
        # )
        #,
        #Local moran's scatterplot
        # column(
        #   6,
        #   bs4Card(
        #     title = "Moran's scatterplot",
        #     status = "info",
        #     solidHeader = T,
        #     collapsible = T,
        #     collapsed = F,
        #     closable = FALSE,
        #     labelStatus = "info",
        #     labelText = "",
        #     width = 12,
        #     plotlyOutput("lmorans_plot") %>% withSpinner(hide.ui = FALSE),
        #     footer = "*SIR has been scaled and centered."
        #   )
        # )
      )
      )
    )
    ),
  footer = dashboardFooter()
  )
