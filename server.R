# =========================
# APPLICATION ROOT PATH
# =========================

# Root application directory
var.path <- normalizePath(getwd(), winslash = "/")

# Data directory
path.data <- file.path(var.path, "data")

# IMPORTANT:
# avoid using setwd() in multi-user Shiny applications
# setwd(path.data)

# =========================
# LOGGER
# =========================

# Console logger function
r.console <- function(text) {
  cat(
    paste0(
      "[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] ",
      text,
      "\n"
    )
  )
}

# =========================
# APPLICATION LANGUAGES
# =========================

# Default application languages
country <- c("french", "english")

# =========================
# ADMINISTRATIVE BOUNDARIES
# =========================

# Import district administrative boundaries
limitDistrict <- readRDS(
  file.path(path.data, "limit", "emprise_FRG_IFD_MNK_VHP.rds")
)

# Import split district boundaries
limitDistrictSplit <- readRDS(
  file.path(path.data, "limit", "empriseSplit_FRG_IFD_MNK_VHP.rds")
)

# Import district names
nameLimitAminDistrict <- readRDS(
  file.path(path.data, "limit", "nameLimiteAdmin.rds")
)

# District abbreviation mapping
varSelectAbrvDistrict <- c(
  "farafangana" = "frg",
  "ifanadiana" = "ifd",
  "manakara" = "mnk",
  "vohipeno" = "vhp"
)

# =========================
# DATABASE CONNECTION
# =========================

# PostgreSQL/PostGIS connection function
fun.connexionDB <- function() {
  
  DBI::dbConnect(
    drv = RPostgres::Postgres(),
    
    dbname = Sys.getenv(
      "POSTGRES_DB",
      "mrp"
    ),
    
    host = Sys.getenv(
      "POSTGRES_HOST",
      "postgis_db" # localhost
    ),
    
    port = as.integer(
      Sys.getenv(
        "POSTGRES_PORT_INTERNAL", #POSTGRES_PORT_EXTERNAL
        "5432" #5433
      )
    ),
    
    user = Sys.getenv(
      "POSTGRES_USER",
      "shiny"
    ),
    
    password = Sys.getenv(
      "POSTGRES_PASSWORD",
      "sh1nY@pp"
    )
  )
}

# Safe database disconnection helper
fun.closeDB <- function(conn) {
  
  if (!is.null(conn)) {
    
    tryCatch(
      DBI::dbDisconnect(conn),
      error = function(e) NULL
    )
    
  }
  
}

# =========================
# TEMPORARY FOLDERS
# =========================

# Create temporary folders for each user session
create.folder.temp <- function(var.session) {
  
  base_tmp <- file.path(
    var.path,
    "www",
    "tmp",
    var.session
  )
  
  dirs <- c(
    base_tmp,
    file.path(base_tmp, "exploration"),
    file.path(base_tmp, "exportation"),
    file.path(base_tmp, "parcours"),
    file.path(base_tmp, "resultat")
  )
  
  purrr::walk(
    dirs,
    ~ if (!dir.exists(.x)) {
      dir.create(.x, recursive = TRUE)
    }
  )
  
}

# =========================
# OSRM OPTIONS
# =========================

# Ignore future RNG warnings
options(
  future.rng.onMisuse = "ignore"
)

# =========================
# RASTER COLORS
# =========================

# Default raster color palette
colorsRasterDistribution <- terrain.colors(10)

# =========================
# USER SESSION STORAGE
# =========================

# Reactive user session storage
sessionUser <- reactiveValues()

# =========================
# MAIN SERVER
# =========================

server <- function(input, output, session) { 
  
  # Information shiny sur le démmarage de shiny
  write(
    r.console(
      paste0("Démmarage de la sesion de l'application : ", 
             session$token)), 
    stderr()
  )
  
  # =====================================
  # CERATE FOLDER TEMPORARY FOR SESSION
  # =====================================
  
  create.folder.temp(session$token)
  
  # =========================
  # LOAD APPLICATION MODULES
  # =========================
  
  source(
    file.path(
      ".",
      "script",
      "mod_menu.R"
    ),
    local = TRUE
  )
  
  source(
    file.path(
      ".",
      "script",
      "mod_route.R"
    ),
    local = TRUE
  )

  source(
    file.path(
      ".",
      "script",
      "mod_about.R"
    ),
    local = TRUE
  )

  source(
    file.path(
      ".",
      "script",
      "mod_estimate.R"
    ),
    local = TRUE
  )

  source(
    file.path(
      ".",
      "script",
      "mod_filter.R"
    ),
    local = TRUE
  )

  source(
    file.path(
      ".",
      "script",
      "mod_distribution.R"
    ),
    local = TRUE
  )
  
  # UI content
  output$pageContent <- renderUI({
    
    tabItems(
      tabItem("tabAbout", withSpinner(uiOutput("tabAboutPage"), type = 1)),
      tabItem("tabEstimate", withSpinner(uiOutput("tabEstimatePage"), type = 1)),
      tabItem("tabFilter", withSpinner(uiOutput("tabFilterPage"), type = 1)),
      tabItem("tabDistribution", withSpinner(uiOutput("tabDistributionPage"), type = 1))
    )
    
  })
  
  # UI footer
  observeEvent(input$tabs, {
    
    if (input$tabs == "tabAbout") {
      
      output$uiFoot <- renderUI({
        dashboardFooter(
          left = HTML(
            paste0(
              'Developed by <a href="https://www.linkedin.com/in/mauricianot/" target="_blank">',
              'Mauricianot RANDRIAMIHAJA',
              '</a>'
            )
          ),
          
          right = paste0(
            "Madagascar, ",
            format(Sys.time(), "%Y")
          )
        )
      })
    }
    
    if (input$tabs == "tabEstimate") {
      output$uiFoot <- renderUI({ NULL })
    }
    
    if (input$tabs == "tabFilter") {
      output$uiFoot <- renderUI({ NULL })
    }
    
    if (input$tabs == "tabDistribution") {
      output$uiFoot <- renderUI({ NULL })
    }
    
  })
  
  #################################
  ##      END SESSION CLEANUP
  #################################
  
  # Evènements session fermeture d'application
  session$onSessionEnded(function() {
    
    # Remove temporary session files
    unlink(
      file.path(
        var.path,
        "www",
        "tmp",
        session$token
      ),
      recursive = TRUE
    )
    
    write(
      r.console(
        paste0("Nettoyer l'espace de mémoire utiliser par l'application : ", 
               session$token)), 
      stderr()
    )
    
    isolate({
      data_futureReactiveTif(NULL);
    })
    
    write(
      r.console(paste0("Nettoyer l'espace de mémoire utiliser par l'application termine : ", 
                       session$token)), 
      stderr()
    )
    
    # Durée d'expiration d'utilisateur
    # Lancher la liaison avec le base de données RPostgreSQL
    conn <- fun.connexionDB()
    DBI::dbBegin(conn)
    
    # Table session ID
    sqlSessionId <- paste0( "select s.id from session s where s.user =", "'",
                            session$token,"'")
    dataSession <- dbGetQuery(conn, sqlSessionId)
    
    # Update date end table session
    sqlUpdateSession <- paste0("UPDATE session SET dateend=", "'",
                               strftime(Sys.time(), format = '%Y-%m-%d %R'),"'",
                               " where id=",dataSession$id)
    dbGetQuery(conn, sqlUpdateSession)
    
    # Déconnexion à la base de données
    DBI::dbCommit(conn) # Fin de la transaction
    fun.closeDB(conn)
    
    # Information shiny sur la ferture de la session de shiny
    write(
      r.console(paste0("Session fermer pour l'application : ", 
                       session$token)), 
      stderr()
    )
    
    session$close()
    
  })
  
  # Set this to "force" instead of TRUE for testing locally (without Shiny Server)
  session$allowReconnect(TRUE)
}