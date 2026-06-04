# =========================
# LIBRAIRIES
# =========================

# Shiny core
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)
library(shiny.i18n)
library(shinyjs)
library(shinycssloaders)
library(shinyBS)
library(bslib)
library(shinymanager)

# Data manipulation
library(dplyr)
library(purrr)
library(plyr)
library(reshape)
library(lubridate)
library(jsonlite)
library(rjson)

# Spatial
library(sf)
library(leaflet)
library(leaflet.extras)
library(htmlwidgets)
library(raster)
library(OpenStreetMap)
library(ggspatial)
library(osrm)
library(rpostgis)

# Async / performance
library(promises)
library(ipc)

# Statistics
library(mgcv)

# Plot
library(ggplot2)
library(ggpubr)
library(ggrepel)
library(prettymapr)

# Reporting
library(rmarkdown)

# Utilities
library(dipsaus)

future::plan(future::multisession)

# =========================
# OPTIONS
# =========================

options(
  shiny.sanitize.errors = FALSE,
  spinner.color = "#cf4565",
  spinner.color.background = "#ffffff",
  spinner.size = 0.75
)

# =========================
# JAVASCRIPT FULLSCREEN
# =========================

jsToggleFS <- "
shinyjs.toggleFullScreen = function() {

  var element = document.documentElement;

  var enterFS =
      element.requestFullscreen ||
      element.msRequestFullscreen ||
      element.mozRequestFullScreen ||
      element.webkitRequestFullscreen;

  var exitFS =
      document.exitFullscreen ||
      document.msExitFullscreen ||
      document.mozCancelFullScreen ||
      document.webkitExitFullscreen;

  if (
      !document.fullscreenElement &&
      !document.msFullscreenElement &&
      !document.mozFullScreenElement &&
      !document.webkitFullscreenElement
  ) {
      enterFS.call(element);
  } else {
      exitFS.call(document);
  }
}
"

# =========================
# HEADER
# =========================

header <- dashboardHeader(
  titleWidth = 275,
  
  tags$li(
    class = "dropdown",
    id = "logoUSAID",
    tags$a(
      href = "https://malariabehaviorsurvey.org/",
      target = "_blank",
      style = "padding:5px;",
      tags$img(
        src = "logo_mbs.png",
        title = "Malaria Behavior Survey",
        height = "40px"
      )
    )
  ),
  
  tags$li(
    class = "dropdown",
    id = "logoIPM",
    tags$a(
      href = "https://www.pasteur.mg/",
      target = "_blank",
      style = "padding:5px;",
      tags$img(
        src = "logo_ipm.png",
        title = "Institut Pasteur de Madagascar (IPM)",
        height = "40px"
      )
    )
  ),
  
  tags$li(
    class = "dropdown",
    id = "logoIRD",
    tags$a(
      href = "https://en.ird.fr/",
      target = "_blank",
      style = "padding:5px;",
      tags$img(
        src = "logoIRD.png",
        title = "IRD",
        height = "40px"
      )
    )
  ),
  
  tags$li(
    class = "dropdown",
    id = "logoPivot",
    tags$a(
      href = "http://pivotworks.org",
      target = "_blank",
      style = "padding:5px;",
      tags$img(
        src = "logoPivot.png",
        title = "PIVOT",
        height = "40px"
      )
    )
  )
)

# Logo Pivot Science
pivotScience <- tags$a(
  href = "https://research.pivot-dashboard.org/",
  tags$img(src = "logoPivotScience.png", height = "40px")
)

header$children[[2]]$children <- tags$div(
  pivotScience,
  class = "pivotscience"
)

# =========================
# SIDEBAR
# =========================

sidebar <- dashboardSidebar(
  width = 275,
  sidebarMenuOutput("pageSidebarMenu")
)

# =========================
# BODY
# =========================

body <- dashboardBody(
  
  # ShinyJS
  useShinyjs(),
  extendShinyjs(
    text = jsToggleFS, 
    functions = c("winprint")
  ),
  
  # CSS / favicon
  tags$head(
    tags$link(
      rel = "stylesheet",
      type = "text/css",
      href = "style.css"
    ),
    
    tags$link(
      rel = "shortcut icon",
      href = "favicon.ico"
    )
  ),
  
  # Browser language detection
  tags$script(
    HTML("
      $(document).on('shiny:connected', function() {

        var language =
          window.navigator.userLanguage ||
          window.navigator.language;

        if(language.substring(0,2) == 'fr'){
          language = 'french';
        } else {
          language = 'english';
        }

        Shiny.setInputValue(
          'selected_language',
          language,
          {priority: 'event'}
        );

        console.log(language);
      });
    ")
  ),
  
  # OS detection
  tags$script(
    HTML("
      $(document).on('shiny:connected', function() {

        Shiny.setInputValue(
          'os_version',
          window.navigator.platform,
          {priority: 'event'}
        );

      });
    ")
  ),
  
  # Hidden fullscreen button
  shinyjs::hidden(
    actionButton(
      inputId = "button",
      label = "Set full screen application",
      onclick = "shinyjs.toggleFullScreen();"
    )
  ),
  
  # Floating buttons
  fab_button(
    position = "bottom-left",
    
    actionButton(
      inputId = "refreshPage",
      label = "Refresh page",
      icon = icon("redo")
    ),
    
    actionButton(
      inputId = "fullScreen",
      label = "View Fullscreen",
      icon = icon("expand")
    ),
    
    inputId = "fabBt"
  ),
  
  # Main content
  uiOutput("pageContent")
)

# =========================
# UI
# =========================

ui <- dashboardPage(
  skin = "black",
  title = "Malaria Remote Populations (MRP)",
  
  header = header,
  sidebar = sidebar,
  body = body,
  
  footer = uiOutput('uiFoot')
)