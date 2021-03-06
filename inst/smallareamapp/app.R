# Libraries

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
# library(INLA)
# library(cowplot)
# library(shiny)
# library(shinythemes)

# library(shinyscreenshot)
# library(shinycssloaders)
# library(bs4Dash)
# library(shinydashboardPlus)


source("df_prepv2.R")
source("ui_bym.R")
source("server_bym.R")

shinyApp(ui = ui, server = server)
