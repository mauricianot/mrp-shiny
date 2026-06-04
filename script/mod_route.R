# =========================
# SERVER INITIALIZATION
# =========================

# IMPORTANT:
# Avoid using setwd() in Shiny multi-user applications
# setwd(path.data)

###################################
# SERVER CODE
###################################

# =========================
# FULLSCREEN MODE
# =========================

# Enable fullscreen application mode
observeEvent(input$fullScreen, {
  
  shinyjs::click("button")
  
})

# =========================
# REFRESH APPLICATION
# =========================

# Reload application session
observeEvent(input$refreshPage, {
  
  session$reload()
  
})

# =========================
# USER IP LOCALIZATION
# =========================

# Store user IP information
IPLocalisation <- reactiveVal(NULL)

# Async API request
promises::future_promise({
  
  jsonlite::fromJSON(
    "https://ipapi.co/json/",
    flatten = TRUE
  )
  
}) %...!% {
  
  warning("Unable to retrieve IP localization")
  
} %...>% IPLocalisation()

# =========================
# USER SESSION LOGGING
# =========================

# Prevent duplicate insertions
sessionInserted <- reactiveVal(FALSE)

observeEvent(
  
  list(IPLocalisation(), input$os_version),
  
  {
    req(
      IPLocalisation(),
      input$os_version
    )
    
    # Avoid multiple insertions
    req(!sessionInserted())
    
    sessionInserted(TRUE)
    
    # =========================
    # USER INFORMATION
    # =========================
    
    ip_data <- IPLocalisation()
    
    ip <- ip_data$ip %||% ""
    city <- ip_data$city %||% ""
    region <- ip_data$region %||% ""
    country <- ip_data$country_name %||% ""
    
    latitude <- ip_data$latitude %||% NA
    longitude <- ip_data$longitude %||% NA
    
    localisationXY <- paste0(
      latitude,
      ",",
      longitude
    )
    
    # =========================
    # SESSION DATAFRAME
    # =========================
    
    session.df <- data.frame(
      
      datestart = strftime(
        Sys.time(),
        format = "%Y-%m-%d %H:%M"
      ),
      
      user = as.character(session$token),
      
      ip = ip,
      city = city,
      region = region,
      country = country,
      
      os = input$os_version,
      
      loc = localisationXY,
      
      stringsAsFactors = FALSE
      
    )
    
    # =========================
    # DATABASE INSERTION
    # =========================
    
    conn <- NULL
    
    tryCatch({
      
      # Open PostgreSQL connection
      conn <- fun.connexionDB()
      
      DBI::dbBegin(conn)
      
      DBI::dbWriteTable(
        conn,
        "session",
        session.df,
        row.names = FALSE,
        append = TRUE
      )
      
      DBI::dbCommit(conn)
      
      r.console(
        paste(
          "New session inserted:",
          session$token
        )
      )
      
    }, error = function(e) {
      
      r.console(
        paste(
          "Database insertion error:",
          e$message
        )
      )
      
    }, finally = {
      
      # Always close connection
      fun.closeDB(conn)
      
    })
    
  },
  
  once = TRUE
  
)

# =========================
# TRANSLATION SYSTEM
# =========================

translator <- Translator$new(
  translation_json_path = file.path(
    path.data,
    "translation",
    "translation.json"
  )
)

# Reactive translation object
i18n <- reactive({
  
  selected <- input$selected_language
  
  if (
    !is.null(selected) &&
    length(selected) > 0 &&
    selected %in% translator$get_languages()
  ) {
    
    translator$set_translation_language(selected)
    
  }
  
  translator
  
})

# Translation shortcut
tr <- reactive({
  
  i18n()$t
  
})