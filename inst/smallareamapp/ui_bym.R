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

ui <- bs4DashPage(
  #Title for Header
  dashboardHeader(
    title = "Small area risk mApp",
    skin = "dark",
    status = "gray-dark"
                  ),

  #Sidebar menu with sub-menus
  dashboardSidebar(
    skin = "dark",
    sidebarMenu(
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
        icon = NULL,
      )
    ),

    menuItem(
      "Analytics",
      tabName = "menu_5",
      icon = icon("chart-bar")
    ),

    menuItem(
      "Model diagnostics",
      tabName = "menu_6",
      icon = icon("glasses")
    )
  ),
  actionButton("map1", " Snapshot map #1", status = "secondary", size = "sm", icon = icon("camera-retro")),
  actionButton("map2", " Snapshot map #2", status = "secondary", size = "sm", icon = icon("camera-retro"))
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
      selectInput("spatial_choice", "Bayesian spatial Poisson model using INLA?", c("No", "Yes"), selected = "No"),
      selectInput("model_choice", "Please choose a spatial model fit", c("bym2", "bym"), selected = "bym2"),
      p("For exceedence probabilities, please specify a threshold"),
      numericInput(inputId = "threshold", label="Relative Risk Threshold", value = 1.10),
    p("Choose a variable to map"),
    selectInput("variable_var", "Map #1", c("cases", "exp", "SIR", "RR", "exc", "area_pop"), selected = "SIR"),
    selectInput("variable_var2", "Map #2", c("cases", "exp", "SIR","RR", "exc", "area_pop"), selected = "cases")
  ))),
  # Body items
  bs4DashBody(
    # Welcome page
    bs4TabItems(
      bs4TabItem(
        tabName = "menu_1",
        #Title for welcome page
    jumbotron(
          title = p("Welcome to the", em("small area risk mApp!")),
          lead = "This Shiny app helps you estimate disease risk across small geographical areas using Bayesian spatial modelling",
          "Check out the sidebar. Upload data on the ", em("File Upload"), " tab. Analyze data with ", em("Analytics"), " tab.",
          status = "info",
          btnName = "App GitHub Repository",
          href = "https://github.com/jdsimkin04"
        ),
    fluidRow(
      # Developer/contributor information
      bs4UserCard(
        title = bs4UserDescription(
          title = "Jonathan Simkin",
          subtitle = "Developer",
          type = 2,
          image = "https://avatars2.githubusercontent.com/u/41752372?s=460&u=b5974705888e814e9d4661b77be1c2f2357779b3&v=4"
        ),
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
               tags$ol(tags$li("Previously calculated observed cases, age-adjutsed expected counts, and standardized incidence ratios"), tags$li("Boundary files for the regions of interest.")),
            "Before going to the ", em("Analytics"), " page, upload health data on this tab.",
            "See the ", em("Georgraphy shapefiles"), " to upload a map boundary files and don't forget to.", strong("create a spatial weights matrix."),
            br(),
            br(),
            h3("No data? Don't worry!"), "Select 'Yes' below for Scotland Lip Cancer data. Everything will be ready to go. Don't forget to create the weight's matrix!",
            selectInput("scotland_lip", label = "Scotland Lip Cancer sample data", choices = c("No", "Yes"), selected = "No"),
            title = "Instructions",
            status = "info",
            solidHeader = T,
            collapsible = F,
            collapsed = FALSE,
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
          collapsed = FALSE,
          closable = FALSE,
          labelStatus = "info",
          labelText = "text",
          width = 12,
          "Upload data with the area name, cases, expected cases, and population size.",
          br(),
          strong("IMPORTANT!: "), em("Your csv file MUST have the same headers the table in the preview tab!"),
          br(),
          strong("Column headers below ('*' require data, other's can be left blank):"),
          br(),
          br(),
          fluidRow(
            #Columns for the data table to be uploaded
            column(6,
          tags$ol(tags$li("area_name = Areal unit name (must match shapefile area name)"), tags$li("cancer = Cancer type"), tags$li("sex = Biological sex"), tags$li("cases = Observed cases"), tags$li("exp = Expected cases"), tags$li("sir = Standardized incidence ratio"), tags$li("lci = Lower confidence interval"), tags$li("uci = Upper confidence interval"), tags$li("area_pop = Areal unit populations"))),
          #Buttons for upload, previewing the table and reseting the table
          column(6,
            fileInput(inputId = "file_case",
                            label = "Upload data. Choose a csv file",
                            accept = c(".csv")),
          actionButton("do", "Update preview table"), actionButton("reset", "Reset to template table"),
          br(),
          br(),
          selectInput("area_name_table", "What column is your area name?","")))
        )),
        column(6,
        bs4Card(
          title = "Preview your data",
          status = "info",
          solidHeader = T,
          collapsible = TRUE,
          collapsed = FALSE,
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
                 title = "2. Upload map shapefiles",
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
                 strong("IMPORTANT!: "), em("The area id must match that area_name column in your csv file!"),
                 fileInput(inputId = "file_map",
                           label = "Upload map. Choose relevant shapefiles. This typically includes .shp, .shx, .dbf, and .prj files.",
                           multiple = TRUE,
                           accept = c('.shp','.dbf','.sbn','.sbx','.shx','.prj')),
                 selectInput("area_name_map", "What column is your area name?",""),
                 # "A weights matrix is required for spatial modelling",
                 actionButton("weight_matrix", strong("Create weight matrix")), textOutput("wm_text"))),

        column(6,
               bs4Card(
                 title = "Preview map",
                 status = "info",
                 solidHeader = T,
                 collapsible = TRUE,
                 collapsed = FALSE,
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
        column(
          6,
          bs4Card(
            title = "Map #1 (Please select variable from control bar)",
            status = "info",
            solidHeader = T,
            collapsible = T,
            collapsed = FALSE,
            closable = FALSE,
            labelStatus = "info",
            labelText = "",
            width = 12,
            tmapOutput("var_map", height = 500) %>% withSpinner(hide.ui = FALSE)
          )
        ),
        #Card for mapping variable #2, default exceedance probability
        column(
          6,
          bs4Card(
            title = "Map #2 (Please select variable from control bar)",
            status = "info",
            solidHeader = T,
            collapsible = T,
            collapsed = FALSE,
            closable = FALSE,
            labelStatus = "info",
            labelText = "",
            width = 12,
            tmapOutput("var_map2", height = 500) %>% withSpinner(hide.ui = FALSE)
          )
        )
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
        column(
          6,
          bs4Card(
            title = "CPO Plot",
            status = "info",
            solidHeader = T,
            collapsible = T,
            collapsed = F,
            closable = FALSE,
            labelStatus = "info",
            labelText = "",
            width = 12,
            plotlyOutput("cpo_plot") %>% withSpinner(hide.ui = FALSE)

          )
        ),
        #Card for mapping variable #2, default exceedance probability
        column(
          6,
          bs4Card(
            title = "PIT Plot",
            status = "info",
            solidHeader = T,
            collapsible = T,
            collapsed = F,
            closable = FALSE,
            labelStatus = "info",
            labelText = "",
            width = 12,
            plotlyOutput("pit_plot") %>% withSpinner(hide.ui = FALSE)
          )
        )
      ),
      #MSPE, R squared and potential outliers
      fluidRow(
        column(
          width = 6,
          # Datatable
          bs4Card(
            title = "Model measurements",
            status = "info",
            solidHeader = T,
            collapsible = T,
            collapsed = T,
            closable = FALSE,
            labelStatus = "info",
            labelText = "",
            width = 12,
            htmlOutput("diagnostics_table")
          )
        )
      )
    )
      )
    ),
  footer = dashboardFooter()
  )
