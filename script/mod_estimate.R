# =========================
# ROUTE ESTIMATION MAP
# =========================

fun.mapEstimate <- function() {
  
  # Extract center safely
  center_x <- mean(st_bbox(limitDistrict)[c("xmin", "xmax")])
  center_y <- mean(st_bbox(limitDistrict)[c("ymin", "ymax")])
  zoom_lvl <- limitDistrict@data$ZOOM[1]
  
  leaflet(options = leafletOptions(
    maxZoom = 17,
    zoomControl = FALSE
  )) %>%
    
    # Base layers
    addTiles(group = "OpenStreetMap (OSM)") %>%
    addProviderTiles(
      "Esri.WorldImagery",
      group = "Satellite"
    ) %>%
    
    # Administrative boundaries
    addPolygons(
      data = limitDistrict,
      color = "purple",
      fill = FALSE,
      group = "Administrative boundary",
      layerId = "administrativeBoundary"
    ) %>%
    
    # Initial view
    setView(
      lng = center_x,
      lat = center_y,
      zoom = zoom_lvl
    ) %>%
    
    # Prevent world wrapping
    addTiles(options = providerTileOptions(noWrap = TRUE)) %>%
    
    # Custom zoom control
    onRender("
      function(el, x) {
        L.control.zoom({
          position: 'topright'
        }).addTo(this);
      }
    ") %>%
    
    # Download / export button
    addEasyButton(
      easyButton(
        id = "easyButtonEstimate",
        icon = icon("download"),
        title = i18n()$t("Téléchargement"),
        position = "topright",
        onClick = JS("
          function(btn, map) {
            Shiny.onInputChange(
              'easyButtonEstimate',
              {
                index: 'infoEstimate',
                random: Math.random()
              }
            );
          }
        ")
      )
    ) %>%
    
    # Scale bar
    addScaleBar(position = "bottomright") %>%
    
    # Layer control
    addLayersControl(
      baseGroups = c("OpenStreetMap (OSM)", "Satellite"),
      options = layersControlOptions(collapsed = TRUE)
    )
}

# =========================
# REACTIVE MAP HOLDER
# =========================

var.estimateMap <- reactiveValues(base = NULL)

# =========================
# ROUTE ESTIMATION PAGE
# =========================

output$tabEstimatePage <- renderUI({
  
  fluidRow(
    column(width = 12,
           box(width = 12, #status = "primary",
               id = 'estimate',
               title = NULL,
               headerBorder = FALSE,
               withSpinner(leafletOutput("estimateMap", height = "120vh"), type = 8),
               absolutePanel(class = "bord-well",
                             top = "auto", left = "auto", right = 20, bottom = 50,
                             height = "auto", width = 375, draggable = TRUE, #width = 325
                             bsAlert("alert")),
               absolutePanel(class = "bord-well",
                             top = 15, left = 15, right = "auto", bottom = "auto",
                             height = "auto", width = 375, draggable = TRUE, #width = 325
                             wellPanel(
                               box(width =12, status = "warning", 
                                   id = 'filter',
                                   title = NULL,
                                   headerBorder = FALSE,
                                   selectInput("sVitesse", label = i18n()$t("Vitesse"),
                                               choices = c( "Rapide" = "paysan", 
                                                            "Normale" = "acc") %>% 
                                                 stats::setNames(c(i18n()$t('Rapide'), 
                                                                   i18n()$t('Normale'))),
                                               selected = "acc"),
                                   conditionalPanel("!output.fichierCSV",
                                                    p(
                                                      class = "text-muted", style="text-align: initial;",
                                                      paste(i18n()$t("Veuillez cocher 'Marqueurs' si le point de départ et d'arrivée sont inconnus" ))
                                                    ),
                                                    checkboxInput("addMarker", i18n()$t("Marqueurs")),
                                                    bsPopover(id = "addMarker", title = i18n()$t("Aide"), 
                                                              content = i18n()$t("Veuillez clicquer sur la carte pour ajouter le point de départ et arrivé. Puis clicker sur le boutton calculer itineraire."), 
                                                              placement = "right", options = list(container = "body"))
                                   ),
                                   
                                   conditionalPanel(
                                     condition = "input.addMarker == false & !output.fichierCSV",
                                     selectInput("sDistrict", 
                                                 label = i18n()$t("Limites Districts"),
                                                 choices = c( "Farafangana" = "frg", 
                                                              "Ifanadiana" = "ifd", 
                                                              "Manakara" = "mnk", 
                                                              "Vohipeno" = "vhp"),
                                                 selected = "frg"),
                                     
                                     # START
                                     card(
                                       height = 200, #full_screen = TRUE,
                                       card_header(
                                         p(
                                           class = "text-muted", 
                                           style="text-align: initial;",
                                           paste(i18n()$t("Veuillez choisir un point de départ (Formation sanitaire/Village - Commune)" ))
                                         )
                                       ),
                                       card_body(
                                         fill = TRUE,
                                         selectInput("sFSTypeStart", 
                                                     label = i18n()$t("Type de point de départ"),
                                                     choices = c("Centre de Santé de Base (CSB)" = "csb", "Site Communautaire" = "sitecom", "Village" = "village", "Select" ="") %>% stats::setNames(c(i18n()$t('Centre de Santé de Base (CSB)'), i18n()$t('Site Communautaire'), i18n()$t('Village'), i18n()$t('Select'))),
                                                     selected = ""),
                                         uiOutput('sCommuneStartUi'),
                                         uiOutput('sCsbSiteVillageStartUi')
                                       )
                                     ),
                                     
                                     # END
                                     card(
                                       height = 200, #full_screen = TRUE,
                                       card_header(
                                         p(
                                           class = "text-muted", style="text-align: initial;",
                                           paste(i18n()$t("Veuillez choisir un point d'arrivée (Formation sanitaire/Village - Commune)") )
                                         )
                                       ),
                                       card_body(
                                         fill = TRUE,
                                         selectInput("sFSTypeArrive", label = i18n()$t("Type de point d'arrivée"),
                                                     choices = c("Centre de Santé de Base (CSB)" = "csb", "Site Communautaire" = "sitecom", "Village" = "village", "Select" ="") %>% stats::setNames(c(i18n()$t('Centre de Santé de Base (CSB)'), i18n()$t('Site Communautaire'), i18n()$t('Village'), i18n()$t('Select'))),
                                                     selected = ""),
                                         uiOutput('sCommuneArriveUi'),
                                         uiOutput('sCsbSiteVillageArriveUi')
                                       )
                                     )
                                     
                                   ),
                                   conditionalPanel("output.fichierCSV", 
                                                    p(
                                                      class = "text-muted", style="text-align: initial;",
                                                      paste(i18n()$t("Veuillez saisir votre adresse email pour envoyer les résultats une fois terminer") )
                                                    ),
                                                    textInput("email", 
                                                              label = i18n()$t("Veuillez entrez votre adresse email"))
                                   ),
                                   p(
                                     class = "text-muted", style="text-align: initial;",
                                     paste(i18n()$t("Note: l'estimation ne peut se faire qu'à l'intérieur de la limite des districts (Ifanadiana, Farafangana, Manakara et Vohipeno)") )
                                   ),
                                   conditionalPanel("!output.fichierCSV",
                                                    fluidRow(
                                                      p(class = 'text-center',
                                                        actionButton("supMarkers", 
                                                                     i18n()$t("Actualiser"), 
                                                                     icon = icon("sync")),
                                                        actionButtonStyled("estTemps", 
                                                                           i18n()$t("Calculer l'itineraire"), 
                                                                           icon = icon("hourglass-end"), 
                                                                           btn_type = "primary")
                                                      )) 
                                   ),
                                   conditionalPanel("output.fichierCSV",
                                                    fluidRow(
                                                      p(class = 'text-center',
                                                        actionButton("refreshBt", 
                                                                     i18n()$t("Actualiser"), 
                                                                     icon = icon("sync")),
                                                        actionButton("envoieBt", 
                                                                     i18n()$t("Envoyer"), 
                                                                     icon = icon("paper-plane"))
                                                      )) 
                                   ),
                                   p(class = "text-muted", style="text-align: initial;",
                                     textOutput("message"),
                                     textOutput("infotraitement"),
                                   )
                               )
                               
                             ),
                             style = "opacity: 0.75; z-index: 10; padding-top: 5px;")
           )
    )
  )
  
})

# =========================
# MAP OUTPUT
# =========================

output$estimateMap <- renderLeaflet({
  
  #req(var.estimateMap$base)
  var.estimateMap$base <- fun.mapEstimate()
  
})

# =========================
# MARKER STATE
# =========================

# valClick <- reactiveValues(
#   nbrClick = 0
# )
# 
# vClick <- reactiveValues(
#   nbrClick = TRUE
# )

clickState <- reactiveValues(
  count = 0,
  start = NULL,
  end = NULL
)

# XY point de départ et destination
vDepartXY <- reactiveValues(
  longDepart = 0, 
  latDepart = 0, 
  name =NULL
)

vDestinationXY <- reactiveValues(
  longDest = 0, 
  latDest = 0, 
  name =NULL
)

vCompletCoordsXY <- reactiveValues(
  validClickStart = FALSE,
  validClickArrive = FALSE
)

leafIcons.blue <- awesomeIcons(
  icon = "ios-close",
  iconColor = "white",
  library = "ion",
  markerColor = "blue"
)

leafIcons.red <- awesomeIcons(
  icon = "ios-close",
  iconColor = "white",
  library = "ion",
  markerColor = "red"
)

# =========================
# MAP CLICK HANDLER
# =========================

observeEvent(input$estimateMap_click, {
  
  req(input$addMarker)
  req(input$estimateMap_click)
  
  # LIMIT TO 2 POINTS
  if (clickState$count >= 2) return()
  
  clickState$count <- clickState$count + 1
  
  lng <- input$estimateMap_click$lng
  lat <- input$estimateMap_click$lat
  
  # =========================
  # START POINT
  # =========================
  
  if (clickState$count == 1) {
    
    clickState$start <- c(lng, lat)
    
    leafletProxy("estimateMap") %>%
      clearGroup("manual_points") %>%
      addAwesomeMarkers(
        lng = lng,
        lat = lat,
        group = "manual_points",
        layerId = "start_point",
        label = i18n()$t("Point de départ"),
        icon = leafIcons.blue
      )
    
  }
  
  # =========================
  # END POINT
  # =========================
  
  if (clickState$count == 2) {
    
    clickState$end <- c(lng, lat)
    
    leafletProxy("estimateMap") %>%
      addAwesomeMarkers(
        lng = lng,
        lat = lat,
        group = "manual_points",
        layerId = "end_point",
        label = i18n()$t("Point d'arrivée"),
        icon = leafIcons.red
      )
  }
  
})

# =========================
# RESET BUTTON (IMPORTANT)
# =========================

observeEvent(input$supMarkers, {
  
  clickState$count <- 0
  clickState$start <- NULL
  clickState$end <- NULL
  
  leafletProxy("estimateMap") %>%
    clearGroup("manual_points")
  
})

###########################################################
# START POINT SELECTION
###########################################################

varChoseSelectGroupStart <- reactiveVal(NULL)
dataChoseSelectGroupStart <- reactiveVal(NULL)

observeEvent(c(input$sFSTypeStart, input$sDistrict), {
  
  varNameDistrict <- as.character(input$sDistrict)
  varNameCommuneDistrict <- nameLimitAminDistrict[
    nameLimitAminDistrict$abrv %in% varNameDistrict, 
  ]
  
  # COMMON COMMUNE SELECT UI
  output$sCommuneStartUi <- renderUI({
    selectInput(
      "sCommuneStart",
      label = i18n()$t("Commune"),
      choices = c(
        unique(as.character(varNameCommuneDistrict$commune)),
        "Select" = ""
      ),
      selected = ""
    )
  })
  
  # =========================
  # CSB
  # =========================
  if (input$sFSTypeStart == "csb") {
    
    csbData <- readRDS(
      paste0("./data/rds/", varNameDistrict, "/csb_", varNameDistrict, ".rds")
    )
    
    output$sCsbSiteVillageStartUi <- renderUI({
      
      dataChoseSelectGroupStart(
        subset(csbData, commune == as.character(input$sCommuneStart))
      )
      
      selectInput(
        "sCsbSiteVillageStart",
        label = i18n()$t("Nom de formation sanitaire"),
        choices = c(
          unique(as.character(dataChoseSelectGroupStart()$csb)),
          "Select" = ""
        ),
        selected = ""
      )
    })
    
    # =========================
    # SITE COMMUNAUTAIRE
    # =========================
  } else if (input$sFSTypeStart == "sitecom") {
    
    siteData <- readRDS(
      paste0("./data/rds/", varNameDistrict, "/site_", varNameDistrict, ".rds")
    )
    
    output$sCsbSiteVillageStartUi <- renderUI({
      
      dataChoseSelectGroupStart(
        subset(siteData, commune == as.character(input$sCommuneStart))
      )
      
      selectInput(
        "sCsbSiteVillageStart",
        label = i18n()$t("Nom de formation sanitaire"),
        choices = c(
          unique(as.character(dataChoseSelectGroupStart()$site)),
          "Select" = ""
        ),
        selected = ""
      )
    })
    
    # =========================
    # VILLAGE
    # =========================
  } else if (input$sFSTypeStart == "village") {
    
    villageData <- readRDS(
      paste0("./data/rds/", varNameDistrict, "/village_", varNameDistrict, ".rds")
    )
    
    output$sCsbSiteVillageStartUi <- renderUI({
      
      dataChoseSelectGroupStart(
        subset(villageData, commune == as.character(input$sCommuneStart))
      )
      
      villageTemp <- split(
        dataChoseSelectGroupStart(),
        dataChoseSelectGroupStart()$fokontany
      )
      
      varChoseSelectGroupStart(lapply(villageTemp, `[[`, "village"))
      
      selectInput(
        "sCsbSiteVillageStart",
        label = i18n()$t("Village"),
        choices = varChoseSelectGroupStart(),
        selected = ""
      )
    })
  }
})

###########################################################
# START MARKER ON MAP
###########################################################

observeEvent(input$sCsbSiteVillageStart, {
  
  varSelectCommuneStart <- input$sCommuneStart
  varSelectGroupStart   <- input$sCsbSiteVillageStart
  
  if (input$sFSTypeStart == "csb") {
    
    rowDataSelect <- subset(
      dataChoseSelectGroupStart(),
      commune == varSelectCommuneStart & csb == varSelectGroupStart
    )
    
  } else if (input$sFSTypeStart == "sitecom") {
    
    rowDataSelect <- subset(
      dataChoseSelectGroupStart(),
      commune == varSelectCommuneStart & site == varSelectGroupStart
    )
    
  } else if (
    input$sFSTypeStart == "village" &&
    nrow(dataChoseSelectGroupStart()) > 0
  ) {
    
    varSelectFokontanyStart <- names(varChoseSelectGroupStart())[
      varChoseSelectGroupStart() %>% purrr::map_lgl(~ input$sCsbSiteVillageStart %in% .)
    ]
    
    rowDataSelect <- subset(
      dataChoseSelectGroupStart(),
      commune == varSelectCommuneStart &
        fokontany == varSelectFokontanyStart &
        village == varSelectGroupStart
    )
  }
  
  if (exists("rowDataSelect") && nrow(rowDataSelect) > 0) {
    
    names(rowDataSelect)[2] <- "name"
    
    vDepartXY$longDepart <- as.numeric(rowDataSelect$X)
    vDepartXY$latDepart  <- as.numeric(rowDataSelect$Y)
    vDepartXY$name       <- as.character(rowDataSelect$name)
    
    leafletProxy("estimateMap", data = rowDataSelect) %>%
      addAwesomeMarkers(
        lng = rowDataSelect$X,
        lat = rowDataSelect$Y,
        popup = ~paste(i18n()$t("Départ :"), rowDataSelect$name),
        layerId = "depart",
        labelOptions = labelOptions(noHide = TRUE),
        label = ~paste(i18n()$t("Départ :"), rowDataSelect$name),
        icon = leafIcons.blue
      ) %>%
      setView(lng = rowDataSelect$X, lat = rowDataSelect$Y, zoom = 12)
    
    vCompletCoordsXY$validClickStart <- TRUE
    
  } else {
    vCompletCoordsXY$validClickStart <- FALSE
  }
})

###########################################################
# ARRIVE POINT SELECTION
###########################################################

varChoseSelectGroupArrive <- reactiveVal(NULL)
dataChoseSelectGroupArrive <- reactiveVal(NULL)

observeEvent(c(input$sFSTypeArrive, input$sDistrict), {
  
  varNameDistrict <- as.character(input$sDistrict)
  varNameCommuneDistrict <- nameLimitAminDistrict[
    nameLimitAminDistrict$abrv %in% varNameDistrict,
  ]
  
  output$sCommuneArriveUi <- renderUI({
    selectInput(
      "sCommuneArrive",
      label = i18n()$t("Commune"),
      choices = c(
        unique(as.character(varNameCommuneDistrict$commune)),
        "Select" = ""
      ),
      selected = ""
    )
  })
  
  if (input$sFSTypeArrive == "csb") {
    
    csbData <- readRDS(
      paste0("./data/rds/", varNameDistrict, "/csb_", varNameDistrict, ".rds")
    )
    
    output$sCsbSiteVillageArriveUi <- renderUI({
      
      dataChoseSelectGroupArrive(
        subset(csbData, commune == as.character(input$sCommuneArrive))
      )
      
      selectInput(
        "sCsbSiteVillageArrive",
        label = i18n()$t("Nom de formation sanitaire"),
        choices = c(
          unique(as.character(dataChoseSelectGroupArrive()$csb)),
          "Select" = ""
        ),
        selected = ""
      )
    })
    
  } else if (input$sFSTypeArrive == "sitecom") {
    
    siteData <- readRDS(
      paste0("./data/rds/", varNameDistrict, "/site_", varNameDistrict, ".rds")
    )
    
    output$sCsbSiteVillageArriveUi <- renderUI({
      
      dataChoseSelectGroupArrive(
        subset(siteData, commune == as.character(input$sCommuneArrive))
      )
      
      selectInput(
        "sCsbSiteVillageArrive",
        label = i18n()$t("Nom de formation sanitaire"),
        choices = c(
          unique(as.character(dataChoseSelectGroupArrive()$site)),
          "Select" = ""
        ),
        selected = ""
      )
    })
    
  } else if (input$sFSTypeArrive == "village") {
    
    villageData <- readRDS(
      paste0("./data/rds/", varNameDistrict, "/village_", varNameDistrict, ".rds")
    )
    
    output$sCsbSiteVillageArriveUi <- renderUI({
      
      dataChoseSelectGroupArrive(
        subset(villageData, commune == as.character(input$sCommuneArrive))
      )
      
      villageTemp <- split(
        dataChoseSelectGroupArrive(),
        dataChoseSelectGroupArrive()$fokontany
      )
      
      varChoseSelectGroupArrive(lapply(villageTemp, `[[`, "village"))
      
      selectInput(
        "sCsbSiteVillageArrive",
        label = i18n()$t("Village"),
        choices = varChoseSelectGroupArrive(),
        selected = ""
      )
    })
  }
})

###########################################################
# ARRIVE MARKER ON MAP
###########################################################

observeEvent(input$sCsbSiteVillageArrive, {
  
  varSelectCommuneArrive <- input$sCommuneArrive
  varSelectGroupArrive   <- input$sCsbSiteVillageArrive
  
  if (input$sFSTypeArrive == "csb") {
    
    rowDataSelect <- subset(
      dataChoseSelectGroupArrive(),
      commune == varSelectCommuneArrive & csb == varSelectGroupArrive
    )
    
  } else if (input$sFSTypeArrive == "sitecom") {
    
    rowDataSelect <- subset(
      dataChoseSelectGroupArrive(),
      commune == varSelectCommuneArrive & site == varSelectGroupArrive
    )
    
  } else if (
    input$sFSTypeArrive == "village" &&
    nrow(dataChoseSelectGroupArrive()) > 0
  ) {
    
    varSelectFokontanyArrive <- names(varChoseSelectGroupArrive())[
      varChoseSelectGroupArrive() %>% purrr::map_lgl(~ input$sCsbSiteVillageArrive %in% .)
    ]
    
    rowDataSelect <- subset(
      dataChoseSelectGroupArrive(),
      commune == varSelectCommuneArrive &
        fokontany == varSelectFokontanyArrive &
        village == varSelectGroupArrive
    )
  }
  
  if (exists("rowDataSelect") && nrow(rowDataSelect) > 0) {
    
    names(rowDataSelect)[2] <- "name"
    
    vDestinationXY$longDest <- as.numeric(rowDataSelect$X)
    vDestinationXY$latDest  <- as.numeric(rowDataSelect$Y)
    vDestinationXY$name     <- as.character(rowDataSelect$name)
    
    leafletProxy("estimateMap", data = rowDataSelect) %>%
      addAwesomeMarkers(
        lng = rowDataSelect$X,
        lat = rowDataSelect$Y,
        popup = ~paste(i18n()$t("Arrivée :"), rowDataSelect$name),
        layerId = "arrivee",
        labelOptions = labelOptions(noHide = TRUE),
        label = ~paste(i18n()$t("Arrivée :"), rowDataSelect$name),
        icon = leafIcons.red
      ) %>%
      setView(lng = rowDataSelect$X, lat = rowDataSelect$Y, zoom = 12)
    
    vCompletCoordsXY$validClickArrive <- TRUE
    
  } else {
    vCompletCoordsXY$validClickArrive <- FALSE
  }
})

###########################
# Estimate travel time
###########################

# Manage processing queue
queue <- shinyQueue()
queue$consumer$start(100)

data_report <- reactiveVal(list(NULL))
satuts_dir <- reactiveVal(0)

# Estimate travel time
observeEvent(input$estTemps, {
  
  # Check if valid start/end points are provided
  if ( (clickState$count > 1 & isTRUE(input$addMarker)) | 
      (vCompletCoordsXY$validClickStart == TRUE & vCompletCoordsXY$validClickArrive == TRUE)) {
    
    shinyjs::disable("estTemps") # Desactiver le bouton
    
    # Info sur l'action du boutton d'estimation d'itineraires
    write(r.console("Lancher le traitement d'estimation d'itineraires"), stderr())
    
    tryCatch({
      
      system.time({
        
        start_traitement <- Sys.time()
        var.token <- session$token # Session
        
        # Creation de dossier temporaire
        if (satuts_dir()==0) {
          create.folder.temp(var.token)
          queue$producer$fireAssignReactive("satuts_dir", 1)
        }
        
        withProgress(message = i18n()$t("Merci de patienter, s'il vous plaît !") , detail = i18n()$t('Traitement en cours...'), value = 0, 
                     {
                       
                       data.report.id <- list(NULL);
                       
                       # Liaison entre le serveur osrm backend et plugin osrm sur R
                       options(osrm.server = "http://osrm-routed:5000/")
                       options(osrm.profile = "foot")
                       
                       waspointsParcours <- data.frame(id.parcours=numeric(), 
                                                       long = numeric(), 
                                                       lat = numeric(), 
                                                       distance=numeric(), 
                                                       stringsAsFactors = F)
                       
                       markerParcours <- data.frame(ID = numeric(), 
                                                    longDep = numeric(), 
                                                    latDep = numeric(), 
                                                    longDest = numeric(), 
                                                    latDest = numeric(), 
                                                    stringsAsFactors = F)
                       
                       # Convertir les coordonnées XY selon le point déprat et arrivé choisi en table
                       if (isTRUE(input$addMarker)) {
                         write(r.console(paste(1, clickState$start[1], 
                                               clickState$start[2], 
                                               clickState$end[1], 
                                               clickState$end[2])), 
                               stderr())
                         markerParcours[nrow(markerParcours) + 1,] <- c(1, clickState$start[1], 
                                                                        clickState$start[2], 
                                                                        clickState$end[1], 
                                                                        clickState$end[2])
                       } else {
                         write(r.console(paste(1, vDepartXY$longDepart, 
                                               vDepartXY$latDepart, 
                                               vDestinationXY$longDest, 
                                               vDestinationXY$latDest)), 
                               stderr())
                         markerParcours[nrow(markerParcours) + 1,] <- c(1, vDepartXY$longDepart, 
                                                                        vDepartXY$latDepart, 
                                                                        vDestinationXY$longDest, 
                                                                        vDestinationXY$latDest)
                       }
                       
                       print(markerParcours)
                       tryCatch({
                         i <- 1
                         routeItineraire <- osrmRoute(src = c(lon = as.numeric(markerParcours$longDep), 
                                                              lat = as.numeric(markerParcours$latDep)),
                                                      dst = c(lon = as.numeric(markerParcours$longDest), 
                                                              lat = as.numeric(markerParcours$latDest)),
                                                      overview = "simplified") #exclude = NULL, sp = T
                         print(routeItineraire)
                         parcours <- st_coordinates(routeItineraire)
                         my.length <- nrow(parcours)
                         pos.actuelle <- i
                         waspointsParcours[pos.actuelle:(pos.actuelle+my.length-1),1] <- markerParcours$ID[i]
                         waspointsParcours[pos.actuelle:(pos.actuelle+my.length-1),2] <- parcours[,1]
                         waspointsParcours[pos.actuelle:(pos.actuelle+my.length-1),3] <- parcours[,2]
                         waspointsParcours[pos.actuelle:(pos.actuelle+my.length-1),4] <- routeItineraire$distance
                         Sys.sleep(0.5)
                         
                         # Info sur la récuperation du ligne de parcours
                         write(r.console("Récuperation du ligne de parcours sur OSRM à réussis"), stderr())
                         
                       }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
                       
                       # Exportation waspoints parcours
                       write.csv(waspointsParcours, 
                                 file = paste0(var.path, "/www/tmp/", 
                                               var.token , "/parcours/parcoursComplet.csv"))
                       parcourscomplet <- read.csv(paste0(var.path, "/www/tmp/", 
                                                          var.token ,"/parcours/parcoursComplet.csv"))
                       
                       # Afficher au carte l'itinéraire à effecuter
                       leafletProxy("estimateMap", 
                                    data = parcourscomplet) %>% 
                         addPolylines(lng = ~long, 
                                      lat = ~lat, 
                                      layerId = "lineparcours", 
                                      color = "red")  %>% 
                         fitBounds(~min(long), ~min(lat), ~max(long), ~max(lat))
                       
                       # Extraction d'information géographique sur le parcours
                       shape <- saveRDS(parcourscomplet, 
                                        file = paste0(var.path, "/www/tmp/", 
                                                      var.token , "/parcours/routeParcours.rds"))
                       
                       # Mettre en décoirssante les valeurs X et Y
                       parcours.News <- arrange(parcourscomplet, desc(X))
                       write.csv(parcours.News, 
                                 file = paste0(var.path, "/www/tmp/", 
                                               var.token , "/parcours/RouteParcours.csv"))
                       
                       # Info sur la sauvgarde de la ligne de parcours en CSV
                       write(r.console("Sauvegarde réussis sur le CSV du RouteParcours.csv"), stderr())
                       
                       # Convertion SpatialPointsDataFrame à SpatialLinesDataFrame
                       coordinates(parcours.News) <- ~long+lat
                       class(parcours.News)
                       crs(parcours.News) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 "
                       
                       # Liste pour les lines parcours.News id
                       x <- lapply(split(parcours.News, parcours.News$id.parcours),
                                   function(parcours.News) Lines(list(Line(coordinates(parcours.News))), parcours.News$id.parcours[1L]))
                       lines <- SpatialLines(x)
                       data <- data.frame(id = unique(parcours.News$id.parcours))
                       rownames(data) <- data$id
                       pathRoad <- SpatialLinesDataFrame(lines, data)
                       crs(pathRoad) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 "
                       pathRoad_sf <- st_as_sf(pathRoad)
                       
                       # Sauvegarder en Shapefile
                       st_write(
                         obj = pathRoad_sf,
                         dsn = paste0(
                           var.path,
                           "/www/tmp/",
                           var.token,
                           "/parcours/pathRoadWGS_84.shp"
                         ),
                         delete_layer = TRUE,
                         quiet = TRUE
                       )
                       
                       # Info sur la sauvgarde de la ligne de parcours en Sahpefile
                       write(r.console("Sauvegarde réussis sur le shapefile nommée PathRoadWGS_84.shp"), stderr())
                       
                       # Description du shapefile
                       str(pathRoad)
                       
                       # Progress étape 1/3
                       incProgress(1/3)
                       
                       ####################################################
                       ## Mode exportation de la base de données analysable
                       ####################################################
                       
                       # Manipulation de données shapefile en utilisant PostGIS
                       # Lancher la liaison avec le base de données RPostgreSQL
                       conn <- fun.connexionDB()
                       
                       # Test de connexion
                       pgPostGIS(conn)
                       
                       # Insertion lines
                       pgInsert(conn, 
                                name=c("public","parcours"), 
                                data.obj=pathRoad, 
                                geom = "geom", 
                                new.id = "gid", 
                                overwrite = TRUE)
                       dbAddKey(conn, 
                                name = c("public", "parcours"), 
                                colname = "gid", 
                                type = "primary")
                       dbIndex(conn, 
                               name = c("public", "parcours"), 
                               colname = "geom", 
                               method = "gist")
                       
                       # Info sur l'importation de fichier shapefile dans la base de données postgis
                       write(r.console("Importation réussis sur l'importation du PathRoadWGS_84.shp dans la base de données"), stderr())
                       
                       # Info sur lancement du traitement de fichier shapefile dans la base de données postgis
                       write(r.console("Lancer le traitement de fichier PathRoadWGS_84.shp dans la base de données"), stderr())
                       
                       # Update transformation in postGIS wgs84 to utm
                       dbSendQuery(conn,  
                                   "
                                      ALTER TABLE parcours 
                                      ALTER COLUMN geom 
                                      TYPE Geometry(linestring, 32738) 
                                      USING ST_Transform(geom, 32738);
                                  "
                       )
                       
                       # Requête du décomposition par 100 mètres
                       dbBegin(conn)
                       
                       dbExecute(conn, "DROP TABLE IF EXISTS temparcours")
                       
                       dbExecute(conn, "
                                        CREATE TABLE temparcours AS (
                                          SELECT row_number() over() as id,
                                                 concat('track',t.gid) as track,
                                                 ST_LineSubstring(
                                                   t.geom,
                                                   (100.00 * n.n::numeric)::double precision / t.length,
                                                   CASE
                                                     WHEN (100.00 * (n.n + 1)::numeric)::double precision < t.length
                                                     THEN (100.00 * (n.n + 1)::numeric)::double precision / t.length
                                                     ELSE 1::double precision
                                                   END
                                                 ) AS geom
                                          FROM (
                                              SELECT p.gid,
                                                     st_linemerge(p.geom) AS geom,
                                                     st_length(p.geom) AS length
                                              FROM parcours p
                                          ) t
                                          CROSS JOIN generate_series(0,10000) n(n)
                                          WHERE ((n.n::numeric * 100.00)::double precision / t.length) < 1
                                        )
                                        "
                        )
        
                        dbExecute(conn, "
                                        ALTER TABLE temparcours
                                        ALTER COLUMN geom
                                        TYPE Geometry(linestring,4326)
                                        USING ST_Transform(geom,4326)
                                        "
                        )
                        
                        dbExecute(conn, "DROP TABLE IF EXISTS xyparcours")
                        
                        dbExecute(conn, "
                                      CREATE TABLE xyparcours AS
                                      WITH line AS (
                                        SELECT t.id,
                                               (st_dump(t.geom)).geom AS geom
                                        FROM temparcours t
                                      ),
                                      linemeasure AS (
                                        SELECT line.id,
                                               st_addmeasure(line.geom,0,st_length(line.geom)) AS linem,
                                               generate_series(0,st_length(line.geom)::integer,10) AS i
                                        FROM line
                                      ),
                                      geometries AS (
                                        SELECT linemeasure.id,
                                               (st_dump(
                                                   st_geometryn(
                                                       st_locatealong(linemeasure.linem,
                                                                      linemeasure.i::double precision),1)
                                               )).geom AS geom
                                        FROM linemeasure
                                      ),
                                      points AS (
                                        SELECT geometries.id,
                                               st_setsrid(
                                                   st_makepoint(
                                                       st_x(geometries.geom),
                                                       st_y(geometries.geom)
                                                   ),4326
                                               ) AS geom
                                        FROM geometries
                                      )
                                      SELECT p.id,
                                             p.geom,
                                             st_x(p.geom) AS x,
                                             st_y(p.geom) AS y
                                      FROM points p
                                      "
                      )
                      
                      dbExecute(conn, "
                                      ALTER TABLE xyparcours
                                      ALTER COLUMN geom
                                      TYPE Geometry(point,4326)
                                      USING ST_Transform(geom,4326)
                                      "
                      )
                      
                      dbCommit(conn)
                       
                       # Execute requête SQL qXYparcours avec recupération X et Y du parcours
                       xyparcours <- pgGetGeom(conn, "xyparcours")
                       
                       qRiver <- 
                         "
                             SELECT t.id,
                                CASE
                                    WHEN st_crosses(t.geom, r.geom) THEN 1
                                    ELSE 0
                                END AS river,
                            r.up_cells,
                                CASE
                                    WHEN r.up_cells <= 500 OR (r.name IS NULL AND r.up_cells IS NULL) THEN 'faible étendue'::text
                                    WHEN (r.up_cells > 500 AND r.up_cells <= 1000) OR (r.name IS NOT NULL AND r.up_cells IS NULL) THEN 'étendue moyenne'::text
                                    ELSE 'vaste étendue'::text
                                END AS categoryriver,
                            r.name
                           FROM river r,
                            temparcours t
                          WHERE st_intersects(r.geom, t.geom)
                          ORDER BY t.id;
                        "
                       
                       # Execute requête SQL qRiver en intersect parcours et rivière
                       riverparcours <- dbGetQuery(conn,  qRiver)
                       
                       qEleparcours <-
                         "
                          select t.id, (ST_SummaryStats(st_clip(m.rast, t.geom, true))).max AS ele, t.track from mnt30 m, temparcours t  WHERE st_intersects(m.rast, t.geom);
                      
                          "
                       # Execute requête SQL qEleparcours en intersect parcours et MNT 30m
                       eleparcours <- dbGetQuery(conn,  qEleparcours)
                       
                       # Nombre track
                       gpsTrack <- dbGetQuery(conn,  " SELECT t.track FROM temparcours t GROUP BY t.track ORDER BY t.track; ")
                       
                       # Afficher la liste des couches géometries
                       
                       qLandcover <-
                         "
                          -- Intersection entre temparcours et landcover
                          WITH temparcoursIntersect AS (
                            SELECT t.id, st_intersection(t.geom, l.geom) as geom, st_length(st_intersection(t.geom, l.geom)::geography)/1000 as length_km, l.gridcode
                            FROM temparcours t, landcover l WHERE st_intersects(t.geom, l.geom)
                          ),
                          -- Difference entre temparcours et landcover
                          temparcoursDifference AS (
                            SELECT temparcours.id, ST_CollectionExtract(ST_Multi(COALESCE(ST_Difference(temparcours.geom, landcover.geom), temparcours.geom)), 2 )::geography(MultiLineString, 4326) As geom, st_length(ST_CollectionExtract(ST_Multi(COALESCE(ST_Difference(temparcours.geom, landcover.geom), temparcours.geom)), 2 )::geography(MultiLineString, 4326))/1000 as length_km, coalesce(5)::bigint as gridcode
                            FROM temparcours LEFT JOIN landcover ON ST_Intersects(temparcours.geom, landcover.geom)
                          )
                          -- Fussionner les deux couches (intersection et différence)
                          SELECT id, length_km, gridcode FROM temparcoursIntersect UNION SELECT id, length_km, gridcode FROM temparcoursDifference
                        
                        "
                       
                       # Execute requête SQL qLandcover en intersect parcours et landcover
                       landcoverParcours <- dbGetQuery(conn,  qLandcover)
                       
                       # Info sur le traitement et les résultat de fichier shapefile dans la base de données
                       write(r.console("Traitement réussis avec le retour de l'information du shapefile"), stderr())
                       
                       # Déconnexion à la base de données
                       fun.closeDB(conn)
                       
                       #################################################
                       # Assembler et structurer les données du parcours
                       #################################################
                       
                       # Info sur le traitement des résultat de fichier shapefile
                       write(r.console("Lancer le traitement de l'information du shapefile pour avoir une base de données analysable"), stderr())
                       
                       # Landcover
                       # Mettre à jour le label du landcover
                       landcoverParcours$typelandcover <- ifelse(landcoverParcours$gridcode %in% '1', "Zone_Habitation", 
                                                                 ifelse(landcoverParcours$gridcode %in% '2', "Eau_de_surface", 
                                                                        ifelse(landcoverParcours$gridcode %in% '3', "Foret_dense", 
                                                                               ifelse(landcoverParcours$gridcode %in% '4', "Riziere", "Savane_Arboree"))))
                       # Effacher la columne gridecode
                       landcoverParcours <- landcoverParcours[, -c(3)]
                       
                       # Clalculer la pourcentage
                       landcoverParcours <- ddply(landcoverParcours, .(id), mutate, percent = formatC((length_km / sum(length_km) * 100), digits = 0, format = "f") )
                       landcoverParcours$value <- as.numeric(landcoverParcours$percent)
                       landcoverParcours <- landcoverParcours[, c ( 'id' , 'typelandcover', 'value')]
                       landcoverParcours <- aggregate(.~id+typelandcover, landcoverParcours, sum)
                       
                       # Mettre en horizontale les valeurs du landcover
                       landcoverParcours <- cast(landcoverParcours,id~typelandcover,value="value")
                       
                       # Vérification s'il passe sur un/des rivière(s) ou Eau de Surface (ES)
                       # Mettre une valeur pardefaut 0 pour qui ne passe pas des zones d'habitaion
                       nbrColumnZH <- grep("Zone_Habitation", colnames(landcoverParcours))
                       if (is.integer(nbrColumnZH) && length(nbrColumnZH) == 0) {
                         landcoverParcours$Zone_Habitation  <- NA
                       }
                       # Mettre une valeur pardefaut 0 pour qui ne passe pas de l'eau de surface
                       nbrColumnES <- grep("Eau_de_surface", colnames(landcoverParcours))
                       if (is.integer(nbrColumnES) && length(nbrColumnES) == 0) {
                         landcoverParcours$Eau_de_surface  <- NA
                       }
                       # Mettre une valeur pardefaut 0 pour qui ne passe pas des forêts
                       nbrColumnFD <- grep("Foret_dense", colnames(landcoverParcours))
                       if (is.integer(nbrColumnFD) && length(nbrColumnFD) == 0) {
                         landcoverParcours$Foret_dense  <- NA
                       }
                       # Mettre une valeur pardefaut 0 pour qui ne passe pas des rizières
                       nbrColumnRZ <- grep("Riziere", colnames(landcoverParcours))
                       if (is.integer(nbrColumnRZ) && length(nbrColumnRZ) == 0) {
                         landcoverParcours$Riziere  <- NA
                       }
                       # Mettre une valeur pardefaut 0 pour qui ne passe pas des savanes arborées
                       nbrColumnSA <- grep("Savane_Arboree", colnames(landcoverParcours))
                       if (is.integer(nbrColumnSA) && length(nbrColumnSA) == 0) {
                         landcoverParcours$Savane_Arboree  <- NA
                       }
                       
                       # Track du parcours
                       # On a ajoute deux colunnes : individu (identification de la vitesse) et valeur (pluie)
                       eleparcours$individu <- 'paysan'
                       eleparcours$rain <- 0
                       
                       # Fusionner les données du parcours avec les coordonnées X et Y
                       parcoursreel <- merge(x = eleparcours, y= xyparcours, by="id")
                       
                       ##############################################
                       # Calcul des varibales : distance et pente
                       ##############################################
                       
                       # Créer une table dataframe pour enregistre le calcul
                       data.track <- data.frame(NULL)
                       
                       # Calcul de la distance en km
                       my.dists <- spDists(x=as.matrix(parcoursreel[,6:7]),longlat=T, segments=T) ; my.dists=c(NA,my.dists) 
                       my.dists <- unlist(tapply(my.dists,parcoursreel$track, function(x){x[1]=NA; return(x)}),use.names=F)
                       parcoursreel$distance <- my.dists 
                       
                       #variable regouper du track
                       var.track <- gpsTrack$track
                       
                       # boucle pour calculer les différentes varibales
                       for (var.temp in var.track){
                         
                         #print(var.temp)
                         track.temp <- subset(parcoursreel, track == var.temp)
                         
                         # Supprimer la première ligne du données
                         dataRowsFirst.df <- track.temp[-1,]
                         dataRowsFirst.df <- dataRowsFirst.df[, c ('ele','id')]
                         
                         # Supprimer la dernière ligne du données
                         track.id <- track.temp[-nrow(track.temp),]
                         dataRowsFirst.df$id <- track.id[, c ( 'id')]
                         
                         # Renommer le nom d'un columne
                         names(dataRowsFirst.df)[names(dataRowsFirst.df) == 'ele'] <- 'ele1'
                         
                         # Créer une temporaire dataframe pour la fusion d'élevation du terrain (ele  et ele1)
                         track.temp <- merge(x = track.temp, y = dataRowsFirst.df, by = "id", all = TRUE)
                         
                         # Calcul de la pente (%)
                         track.temp$slope <- (abs(track.temp$ele1-track.temp$ele)/(track.temp$distance*1000))*100
                         
                         # Rajouter  à la ligne les informations calculer
                         if(is.data.frame(data.track) && nrow(data.track)==0){
                           data.track <- track.temp
                         }else{
                           data.track <- rbind(data.track, track.temp)
                         }
                         
                       }
                       
                       # Catégoriser la valeur de la pente
                       data.track$categoryslope <- cut.default(data.track$slope, c(0, 30, 70, 100, 150) , include.lowest = TRUE)
                       data.track$categoryslope <- as.character(data.track$categoryslope)
                       # Mettre une label pour la catégorisation de la pente
                       data.track$typeslope <- ifelse(data.track$categoryslope %in% '(0,30]', "Horizontal", 
                                                      ifelse(data.track$categoryslope %in% '(30,70]', "Moderate slopes", 
                                                             ifelse(data.track$categoryslope %in% '(70,100]', "Strong slopes", 
                                                                    ifelse(data.track$categoryslope %in% '(100,150]', "Street slopes", NA))))
                       data.track$typeslope <- ifelse(data.track$slope %in% 0.0000000, "Horizontal", data.track$typeslope)
                       data.track$categoryslope <- as.factor(data.track$categoryslope)
                       data.track$typeslope[data.track$typeslope %in% NA] <- "Horizontal"
                       
                       ##############################################
                       # Fin calcul des varibales : distance et pente
                       ##############################################
                       
                       # Rajouter l'information de la rivière à la base de données analysable
                       if(is.data.frame(riverparcours) && nrow(riverparcours)!=0){
                         bdParcours <- merge(x=data.track, y=riverparcours, by="id", all = TRUE)
                         # Adment un valeur 0 pour qui ne passe pas de rivier
                         bdParcours$river[bdParcours$river %in% NA] <- 0
                         
                       }else{
                         # Auccune information de la rivière
                         bdParcours <- data.track
                       }
                       
                       # Rajouter l'information du landcover à la base de données analysable
                       bdParcours <- merge(x=bdParcours, y=landcoverParcours, by="id", sort = TRUE)
                       
                       # Exportation de la base de données en détail
                       write.csv(bdParcours, file = paste0(var.path, "/www/tmp/", var.token , "/exportation/output_databaseDetail.csv"), 
                                 row.names = TRUE)
                       
                       # Structure de la base de données analysable
                       database.df <- bdParcours[, c ('id', 'track', 'distance','slope', 'typeslope', 'rain')]
                       database.df <- merge(x=database.df, y=landcoverParcours, by="id", sort = TRUE)
                       
                       # Remplacer la valeur NA or NULL en 0
                       database.df[is.na(database.df)] <- 0
                       
                       # Exportation de la base de données analysable
                       write.csv(database.df, file = paste0(var.path, "/www/tmp/", var.token , "/exportation/output_databaseAnalysable.csv"), 
                                 row.names = FALSE)
                       
                       # Info sur le traitement des résultat de fichier shapefile qui sont réussis
                       write(r.console("Traitement de l'information du shapefile pour avoir une base de données analysable sont réussis"), stderr())
                       
                       # Progress étape 2/3
                       incProgress(2/3)
                       
                       ##############################################
                       ## Mode exploration sur les données analysable
                       ##############################################
                       
                       # Info sur l'analyse exploratoire du données
                       write(r.console("Lancer l'analyse exploratoire de donnés à analyser"), stderr())
                       
                       # Importation les données de la prédiction
                       prediction.df <- read.csv(paste0(var.path, "/www/tmp/", var.token , "/exportation/output_databaseAnalysable.csv"))
                       
                       # Description des données à explorer
                       names(prediction.df)
                       summary(prediction.df)
                       str(prediction.df)
                       #describe(prediction.df)
                       
                       # Calculer la somme cumulée (cumsum) par groupe de track
                       prediction.df$track <- as.factor(prediction.df$track) 
                       prediction.df$distance.origine <- ave(prediction.df$distance, prediction.df$track, FUN=cumsum) # Calculate cumulative sum (cumsum) by group
                       
                       # Catégorisation de la distance original des parcours effectuée durant les trajets
                       var.maxDistance <- round(max(prediction.df$distance.origine), digits = 2) # max distance length
                       if (var.maxDistance <= 13) {
                         prediction.df$categorydistance <- cut.default(prediction.df$distance.origine, c(0, 13), include.lowest = TRUE)
                       }else{
                         prediction.df$categorydistance <- cut.default(prediction.df$distance.origine, 
                                                                       c(0, 13 , var.maxDistance ), include.lowest = TRUE)
                       }
                       
                       # Creation d'une varibale pour la mojoritée du landcover avec +50%
                       sum.cat <- apply(prediction.df[,c('Savane_Arboree','Foret_dense','Zone_Habitation','Riziere','Eau_de_surface')],1,sum)
                       sum.cat.total <- ifelse(sum.cat==100,1,0)
                       
                       main.cat <- apply(prediction.df[,c('Savane_Arboree','Foret_dense','Zone_Habitation','Riziere','Eau_de_surface')], 1, function(x){which(x>50)})
                       ii <- which(lapply(main.cat,length)==0) ; main.cat[ii] <- 'Mixte'
                       
                       main.cat.temp <- sapply(main.cat, function(x){as.character(x[1])})
                       prediction.df <- cbind(prediction.df, main.cat.temp)
                       prediction.df$occupation <- ifelse(prediction.df$main.cat.temp %in% '1', "Savane_Arboree", 
                                                          ifelse(prediction.df$main.cat.temp %in% '2', "Foret_dense", 
                                                                 ifelse(prediction.df$main.cat.temp %in% '3', "Zone_Habitation", 
                                                                        ifelse(prediction.df$main.cat.temp %in% '4', "Riziere", 
                                                                               ifelse(prediction.df$main.cat.temp %in% '5', "Eau_de_surface", "Mixte")))))
                       
                       # Effacher la colomne main.cat.temp
                       prediction.df <- prediction.df[, -c(14)]
                       
                       # Préparation des données concernant la pente
                       prediction.df <- prediction.df[!(prediction.df$slope > 150),]
                       prediction.df$slope <- abs(prediction.df$slope)/15
                       
                       # Exporter les données du prédiction en csv nommé : prediction.df
                       write.csv(prediction.df, file = paste0(var.path, "/www/tmp/", var.token , "/exploration/output_prediction_parcours.csv"),
                                 na = "0")
                       
                       # Info sur l'analyse exploratoire du données
                       write(r.console("L'analyse exploratoire de donnés analysable sont réussis"), stderr())
                       
                       ##############################################
                       ## Mode prédiction en utilisant le modèl multivariée
                       ##############################################
                       
                       # Info sur la prédiction des données
                       write(r.console("Lancement du prédiction de données sur le données analysable"), stderr())
                       
                       if (!exists("model.test")) {
                         # Importion des données du modèle
                         model.test <- readRDS(file = paste0(path.data, "/model/model_pied.rds"))
                       }
                       
                       # Récuperation valeur maximun du distance du parcours 
                       # et adapter le par rapport à la modèle
                       if (max(prediction.df$distance.origine) < 13) {
                         
                         labelMaxDistance <- paste0("[0,", round(max(prediction.df$distance.origine)), "]")
                         
                         prediction.df$categorydistance <- as.character(prediction.df$categorydistance)
                         prediction.df$categorydistance[prediction.df$categorydistance == labelMaxDistance] <- "[0,13]" 
                         
                       }else if (max(prediction.df$distance.origine) > 13){
                         
                         prediction.df$categorydistance <- cut(prediction.df$distance.origine, c(0, 13 , ceiling(max(prediction.df$distance.origine))), include.lowest = TRUE)
                         labelMaxDistance <- paste0("(13,", ceiling(max(prediction.df$distance.origine)), "]")
                         
                         prediction.df$categorydistance <- as.character(prediction.df$categorydistance)
                         prediction.df$categorydistance[prediction.df$categorydistance == labelMaxDistance] <- "(13,22.9]"
                         
                       }
                       
                       # Mettre en format facteur la columne categorydistance
                       prediction.df$categorydistance <- as.factor(prediction.df$categorydistance)
                       
                       #préparation des données à la prédiction
                       model.test$categorydistance <- factor(model.test$categorydistance, levels(model.test$categorydistance)[c(2,1)])
                       prediction.df$categorydistance <- factor(prediction.df$categorydistance, levels(prediction.df$categorydistance)[c(2,1)])
                       
                       prediction.df$individual <- as.character(input$sVitesse)
                       
                       if (!exists("modelPredict")) {
                         #+s(track,bs="re")
                         modelPredict  <- gam(speed~s(slope)+rain+categorydistance+occupation+individual, data = model.test)
                       }
                       
                       # Changer la valeur de la pluie en valeur min du modèle
                       prediction.df$rain <- min(model.test$rain)
                       prediction.df$rain.min <- prediction.df$rain
                       prediction.df$speed.rain.min <- predict(modelPredict, prediction.df, exclude="s(track)")
                       
                       # Changer la valeur de la pluie en valeur max du modèle
                       prediction.df$rain <- max(model.test$rain)
                       prediction.df$rain.max <- prediction.df$rain
                       prediction.df$speed.rain.max <- predict(modelPredict, prediction.df, exclude="s(track)")
                       
                       # Enlever les valeurs négative sur la colonne de vitesse min et max
                       prediction.df <- filter_at(prediction.df, vars(starts_with("speed.rain.min")), any_vars(. > 0))
                       prediction.df <- filter_at(prediction.df, vars(starts_with("speed.rain.max")), any_vars(. > 0))
                       
                       # Calcul des variables temps en heure
                       prediction.df$time.rain.min <- prediction.df$distance/prediction.df$speed.rain.min
                       prediction.df$time.rain.max <- prediction.df$distance/prediction.df$speed.rain.max
                       
                       # Exportation des données détailés de parcours
                       # Mettre une valeur pardefaut 0 pour qui ne passe pas de l'eau de surface
                       nbrColumnRiver <- grep("river", colnames(bdParcours))
                       if (is.integer(nbrColumnRiver) && length(nbrColumnRiver) != 0) {
                         
                         prediction.df <- merge(x=prediction.df, y=riverparcours, bx="id",  all = TRUE)
                         prediction.df <- subset(prediction.df, distance > 0)
                         
                       }
                       
                       write.csv(prediction.df, file = paste0(var.path, "/www/tmp/", var.token , 
                                                              "/resultat/resultatdetail.predictResultatMinMax.csv"))
                       
                       # Information du rapport
                       infoRapport <- prediction.df[,c('occupation', 'distance')]
                       
                       # Afficher le temps de parcours sur la prédiction sans pluie et avec
                       output.prediction <- prediction.df[,c('track', 'time.rain.min' , 'time.rain.max')]
                       output.prediction <- aggregate(.~track, output.prediction, sum)
                       
                       output.prediction$time.rain.min <- output.prediction$time.rain.min*60
                       output.prediction$time.rain.max <- output.prediction$time.rain.max*60
                       
                       output.prediction$id <- substring(output.prediction$track, 6)
                       output.prediction <- output.prediction[, -c(1)]
                       write.csv(output.prediction, file = paste0(var.path, "/www/tmp/", var.token , 
                                                                  "/resultat/output_data.predictResultatMinMax.csv"), na = "0")
                       
                       # Info sur la prédiction des données
                       write(r.console("La prédiction d'information sur le données analysable sont terminées"), stderr())
                       
                       ###################################
                       ## Exportation des résultats
                       ###################################
                       
                       # Changement du format de la valeur du distance lorsque la valeur est inf 1 km
                       if (routeItineraire$distance < 1) {
                         distance.p <- paste0(round(routeItineraire$distance * 1000, digits=2), " m")
                         tempsMin.p <- paste0(round(output.prediction$time.rain.min * 60, digits=0), " sec")
                         tempsMax.p <- paste0(round(output.prediction$time.rain.max * 60, digits=0), " sec")
                       }else{
                         distance.p <- paste0(round(routeItineraire$distance, digits = 2), " km")
                       }
                       
                       # Changement du format du temps lorsque la valeur est sup 60 min
                       if (output.prediction$time.rain.min >= 1440) {
                         
                         tdtimemin <- seconds_to_period(output.prediction$time.rain.min * 60)
                         tempsMin.p <- paste(day(tdtimemin), "jr", tdtimemin@hour ,"h", minute(tdtimemin), "min")
                         
                       }else if(output.prediction$time.rain.min > 60 && output.prediction$time.rain.min < 1440){
                         
                         tdtimemin <- seconds_to_period(output.prediction$time.rain.min * 60)
                         tempsMin.p <- paste(tdtimemin@hour ,"h", minute(tdtimemin), "min")
                         
                       }else{
                         tempsMin.p <- paste0(round(output.prediction$time.rain.min, digits = 0), " min")
                       }
                       
                       if (output.prediction$time.rain.max  >= 1440) {
                         
                         tdtimemax <- seconds_to_period(output.prediction$time.rain.max * 60)
                         tempsMax.p <- paste(day(tdtimemax), "jr", tdtimemax@hour ,"h", minute(tdtimemax), "min")
                         
                       }else if(output.prediction$time.rain.max > 60 && output.prediction$time.rain.max < 1440){
                         
                         tdtimemax <- seconds_to_period(output.prediction$time.rain.max * 60)
                         tempsMax.p <- paste(tdtimemax@hour ,"h", minute(tdtimemax), "min")
                         
                       }else{
                         tempsMax.p <- paste0(round(output.prediction$time.rain.max, digits = 0), " min")
                       }
                       
                       # Data frame pour stocker les résultats et utiliser pour le rapport
                       if(isTRUE(input$addMarker)){
                         
                         resultatParcours <- data.frame("XY" = c(paste0(i18n()$t("Point de départ : "), 
                                                                        clickState$start[1], ",", 
                                                                        clickState$start[2]), 
                                                                 paste0(i18n()$t("Point d'arrivée : "), 
                                                                        clickState$end[1], ",", 
                                                                        clickState$end[2])),
                                                        "Localisation" = c(paste0(i18n()$t("Point de départ : "), "**" ,
                                                                                  round(clickState$start[1],digits = 5), "**" , ",",  "**" ,
                                                                                  round(clickState$start[2],digits = 5), "**"), 
                                                                           paste0(i18n()$t("Point d'arrivée : "), "**" ,
                                                                                  round(clickState$end[1],digits = 5), "**" ,",", "**" ,
                                                                                  round(clickState$end[2],digits = 5), "**")), 
                                                        "Distance" = c("**0 km**", paste0("**",distance.p,"**")), 
                                                        "Duration" = c(paste0("Min : ", paste0("**",tempsMin.p, "**"), 
                                                                              " (", round(mean(prediction.df$speed.rain.min), digits = 2)," km/h)"), 
                                                                       paste0("Max : ", paste0("**",tempsMax.p,"**"), 
                                                                              " (", round(mean(prediction.df$speed.rain.max), digits = 2)," km/h)")), 
                                                        "Elevation" = c(paste0("Min : **",min(apply(bdParcours, 1,
                                                                                                    FUN = function(x) {min(bdParcours$ele[bdParcours$ele > 0])}))," m**"),
                                                                        paste0("Max : **",max(bdParcours$ele)," m**")),
                                                        stringsAsFactors = F, check.names = F)
                         
                       }else{
                         
                         resultatParcours <- data.frame("XY" = c(paste0(vDepartXY$name, " : ", 
                                                                        vDepartXY$longDepart, ",", 
                                                                        vDepartXY$latDepart), 
                                                                 paste0(vDestinationXY$name, " : ", 
                                                                        vDestinationXY$longDest, ",", 
                                                                        vDestinationXY$latDest)),
                                                        "Localisation" = c(paste0(i18n()$t("Lieu de départ : "), "**" , 
                                                                                  vDepartXY$name, "**"), 
                                                                           paste0(i18n()$t("Lieu d'arrivée : "), "**", 
                                                                                  vDestinationXY$name, "**")), 
                                                        "Distance" = c("**0 km**", paste0("**",distance.p,"**")), 
                                                        "Duration" = c(paste0("Min : ", paste0("**",tempsMin.p, "**"), 
                                                                              " (", round(mean(prediction.df$speed.rain.min), digits = 2)," km/h)"), 
                                                                       paste0("Max : ", paste0("**",tempsMax.p,"**"), 
                                                                              " (", round(mean(prediction.df$speed.rain.max), digits = 2)," km/h)")), 
                                                        "Elevation" = c(paste0("Min : **",min(apply(bdParcours, 1,
                                                                                                    FUN = function(x) {min(bdParcours$ele[bdParcours$ele > 0])}))," m**"),
                                                                        paste0("Max : **",max(bdParcours$ele)," m**")),
                                                        stringsAsFactors = F, check.names = F)
                         
                       }
                       
                       # Changer les labels des tables sur l'information du filtre
                       if(input$selected_language == "english" && !is.null(input$selected_language)){
                         
                         daterep <- format(Sys.time(), '%Y-%m-%d')
                         
                       } else {
                         
                         daterep <- format(Sys.time(), '%d/%m/%Y')
                         
                       }
                       
                       # Labels du resultat du rapport à imprimer
                       rapportLabels <- data.frame("titre" = i18n()$t("Rapport d'itinéraire"), "p1"= i18n()$t("Le trajet du parcours"),
                                                   "p2"= i18n()$t("Information sur le trajet"), "g1" = i18n()$t("Rivière"),
                                                   "g2" = i18n()$t("Forêt dense"), "g3"= i18n()$t("Végétation herbeuse"), "g4" = i18n()$t("Rizière"),
                                                   "g5" = i18n()$t("Zone d'habitation"), "daterep" = daterep, "tabletitre" = i18n()$t("Rivière à traverser") )
                       
                       # Information du rapport
                       infoRapport <- aggregate(.~occupation, infoRapport, sum)
                       
                       # Convertion d'unité en Km 
                       infoRapport$distance <- paste0(round(infoRapport$distance, digits = 2), " km")
                       
                       # La distance des rivières sur le parcours
                       if (!nrow(riverparcours) == 0) {
                         
                         infoRiver <- subset(prediction.df, river > 0)
                         infoRiver <- infoRiver[,c('distance', "distance.origine", "categoryriver", "up_cells", "name")]
                         
                         # Changer les labels des tables sur l'information du categorie des rivières
                         if(input$selected_language == "english" && !is.null(input$selected_language)){
                           
                           infoRiver$categoryriver <- ifelse(infoRiver$categoryriver %in% 'faible étendue', "small extent", 
                                                             ifelse(infoRiver$categoryriver %in% 'étendue moyenne', "medium range", "vast expanse")) 
                           
                         }
                         
                         infoRiverSumDistance <- as.numeric(round(sum(infoRiver$distance), digits = 2))
                         
                       }else{
                         infoRiver <- data.frame(NULL)
                       }
                       
                       # En cas plus d'intersection des rivières on change le label
                       if ( nrow(infoRiver) > 1) {
                         
                         # Changer les labels des tables sur l'information du nom columne
                         if(input$selected_language == "english" && !is.null(input$selected_language)){
                           
                           infoRiver$name <- ifelse(!is.na(infoRiver$name), paste0("(", infoRiver$name, ")"), "")
                           infoRiverLabel <- data.frame("River to cross" = paste0("**",round( as.numeric(substr(distance.p, 0 , regexpr('km', distance.p)-2)) - infoRiver$distance.origine,digits = 2), " km**",i18n()$t(" il y a une rivière à **"), 
                                                                                  infoRiver$categoryriver, " ",infoRiver$name, "**"), check.names = FALSE)
                           # Mettre en ordre ascendante
                           infoRiverLabel$"River to cross" <- as.factor(infoRiverLabel$"River to cross")
                           infoRiverLabel <- data.frame("River to cross" =  levels(infoRiverLabel$"River to cross") , check.names = FALSE)
                           
                         } else {
                           
                           infoRiver$name <- ifelse(!is.na(infoRiver$name), paste0("(", infoRiver$name, ")"), "")
                           infoRiverLabel <- data.frame("Rivière à traverser" = paste0("**",round( as.numeric(substr(distance.p, 0 , regexpr('km', distance.p)-2)) - infoRiver$distance.origine,digits = 2), " km**",i18n()$t(" il y a une rivière à **"), 
                                                                                       infoRiver$categoryriver, " ",infoRiver$name, "**"), check.names = FALSE)
                           # Mettre en ordre ascendante
                           infoRiverLabel$"Rivière à traverser" <- as.factor(infoRiverLabel$"Rivière à traverser")
                           infoRiverLabel <- data.frame("Rivière à traverser" =  levels(infoRiverLabel$"Rivière à traverser"), check.names = FALSE)
                           
                         }
                         
                       }else{
                         infoRiverLabel <- data.frame(NULL)
                       }
                       
                       # Mettre une valeur pardefaut 0 pour qui ne passe pas de zone d'habitation
                       avgZH <- "Zone_Habitation" %in% infoRapport$occupation
                       if (!avgZH) {
                         infoRapport[nrow(infoRapport) + 1,] <- c("Zone_Habitation", "0 km")
                       }
                       
                       # Mettre une valeur pardefaut 0 pour qui ne passe pas de l'eau de surface
                       avgES <- "Eau_de_surface" %in% infoRapport$occupation
                       
                       if (!avgES && nrow(riverparcours) != 0) {
                         
                         if ( nrow(infoRiverLabel) == 0 || is.null(infoRiverLabel) ) {
                           
                           testNumberId <- riverparcours[1,]
                           
                           if (testNumberId$id == 1) {
                             
                             infoRiver <- prediction.df[1,]
                             infoRiver$river <- riverparcours$river
                             infoRiver$up_cells <- riverparcours$up_cells
                             infoRiver$categoryriver <- riverparcours$categoryriver
                             infoRiver$name <- riverparcours$name
                             
                           }
                           
                           infoRapport[nrow(infoRapport) + 1,] <- c("Eau_de_surface", paste0("**", round(infoRiver$distance,digits = 2), " km","**",i18n()$t(" | à **"),
                                                                                             round( as.numeric(substr(distance.p, 0 , regexpr('km', distance.p)-2)) - infoRiver$distance.origine,digits = 2), " km**", i18n()$t(" il y a une rivière à **"),
                                                                                             infoRiver$categoryriver, "**") )
                         }else{
                           
                           infoRapport[nrow(infoRapport) + 1,] <- c("Eau_de_surface", paste0("**", infoRiverSumDistance," km","**") )
                           
                         }
                         
                       }else if (avgES && nrow(riverparcours) != 0){
                         
                         if ( nrow(infoRiverLabel) == 0 || is.null(infoRiverLabel) ) {
                           
                           # Affichage pour le parcours qui ne passe que sur un rivière
                           eauSurface.df <- subset(infoRapport, occupation =="Eau_de_surface")
                           infoRapport <- subset(infoRapport, occupation !="Eau_de_surface")
                           infoRapport[nrow(infoRapport) + 1,] <- c("Eau_de_surface", paste0("**", round( (as.numeric(substr(eauSurface.df$distance, 0 , regexpr('km', eauSurface.df$distance)-2))  + infoRiver$distance), digits = 2), " km", "**",
                                                                                             i18n()$t(" | à **"), round( as.numeric(substr(distance.p, 0 , regexpr('km', distance.p)-2)) - infoRiver$distance.origine,digits = 2), " km**", i18n()$t(" il y a une rivière à **"),
                                                                                             infoRiver$categoryriver, "**"))
                           
                         }else{
                           
                           # Affichage pour le parcours qui ne passe que sur un rivière
                           eauSurface.df <- subset(infoRapport, occupation =="Eau_de_surface")
                           infoRapport <- subset(infoRapport, occupation !="Eau_de_surface")
                           infoRapport[nrow(infoRapport) + 1,] <- c("Eau_de_surface", paste0("**", round(as.numeric(substr(eauSurface.df$distance, 0 , regexpr('km', eauSurface.df$distance)-2)) + infoRiverSumDistance), " km", "**"))
                           
                         }
                         
                       }else{
                         
                         infoRapport[nrow(infoRapport) + 1,] <- c("Eau_de_surface", "**0 km**")
                         
                       }
                       # Mettre une valeur pardefaut 0 pour qui ne passe pas des forêts
                       avgFD <- "Foret_dense" %in% infoRapport$occupation
                       if (!avgFD) {
                         infoRapport[nrow(infoRapport) + 1,] <- c("Foret_dense", "0 km")
                       }
                       # Mettre une valeur pardefaut 0 pour qui ne passe pas des rizières
                       avgRZ <- "Riziere" %in% infoRapport$occupation
                       if (!avgRZ) {
                         infoRapport[nrow(infoRapport) + 1,] <- c("Riziere", "0 km")
                       }
                       # Mettre une valeur pardefaut 0 pour qui ne passe pas des savanes arborées
                       avgSA <- "Savane_Arboree" %in% infoRapport$occupation
                       if (!avgSA) {
                         infoRapport[nrow(infoRapport) + 1,] <- c("Savane_Arboree", "0 km")
                       }
                       
                       # Tansposter les valeurs
                       infoRapport <- cast(infoRapport,~occupation,value="distance")
                       
                       # Attendre 1 secondes avant de terminer
                       Sys.sleep(1)
                       
                       # Exportation des données détailés de parcours
                       write.csv(resultatParcours, file = paste0(var.path, "/www/tmp/", var.token , 
                                                                 "/resultat/resultatsummarize.predictResultatMinMax.csv"), na = "0")
                       
                       # Info sur les résultats
                       write(r.console("Traitements réussis avec les résultats"), stderr())
                       
                       # Progress étape 3/3
                       incProgress(3/3)
                       
                     }
        )
        
        end_traitement <- Sys.time()
        
      })
      
      # Données rapport
      data.report.id[["rapportLabels"]] <- rapportLabels
      data.report.id[["pathRoad"]] <- pathRoad
      data.report.id[["resultatParcours"]] <- resultatParcours
      data.report.id[["infoRapport"]] <- infoRapport
      data.report.id[["infoRiverLabel"]] <- infoRiverLabel
      
      # change value
      queue$producer$fireAssignReactive("data_report", data.report.id)
      
      # Affichage des résultats temporaire
      output$inforesultat <- renderUI({
        
        tags$div(
          tags$p(style ="text-align:center;", HTML(
            paste0(
              "Distance: ", tags$b(distance.p), 
              tags$br(i18n()$t("Temps de trajet sans pluie: "), tags$b(tempsMin.p), " (", round(mean(prediction.df$speed.rain.min), digits = 2)," km/h)"), 
              i18n()$t("Temps de trajet avec pluie: "), tags$b(tempsMax.p), " (", round(mean(prediction.df$speed.rain.max), digits = 2)," km/h)"
            ) 
          )
          )
        )
        
      })
      
      # Difference de temps de traitement
      t1 <- as.POSIXct(start_traitement,'EAT' , format = "%H:%M:%S")
      t2 <- as.POSIXct(end_traitement,'EAT' , format = "%H:%M:%S")
      
      # Temps de l'execution du traitement
      output$infotraitement <- renderText(paste0(i18n()$t("Traitement réussi en "), round(as.numeric(difftime(t2,t1, units='secs')), digits = 2), i18n()$t(" seconds." )))
      
      # Alert du resultat du traitement
      closeAlert(session, "alertResultat") # effacher l'alert afficher sur la carte
      createAlert(session, "alert", "alertResultat", title = i18n()$t("Résultats"), 
                  style = "success", 
                  content = HTML(
                    paste0(
                      "Distance: ", tags$b(distance.p), 
                      tags$br(i18n()$t("Temps de trajet sans pluie: "), tags$b(tempsMin.p), " (", round(mean(prediction.df$speed.rain.min), digits = 2)," km/h)"), 
                      i18n()$t("Temps de trajet avec pluie: "), tags$b(tempsMax.p), " (", round(mean(prediction.df$speed.rain.max), digits = 2)," km/h)"
                    )) , append = FALSE)
      
      # Activier le button de téléchargement
      output$btnDownloadReport <- renderUI({
        downloadButton('downloadReport', i18n()$t('Imprimer les résultats'), icon = icon("print") , class = "btDownload")
      })
      
      # Affichage sur le console de l'application
      print(paste0(i18n()$t("L'estimation du temps de parcours sont réussis en "), round(as.numeric(difftime(t2,t1, units='secs')), digits = 2), i18n()$t(" seconds." )))
    }, error = function(x) {
      print("errors...")
      print(x)
    })
    
    shinyjs::enable("estTemps") # Activer le bouton
    
  }else{
    
    sendSweetAlert(
      session = session,
      title = "Message",
      text = paste(i18n()$t("Veuillez entrer le point de départ et le point d'arrivée; soit en utilisant le choix marqueurs ou les filtres géolocalisés.")),
      type = "info"
    )
    
  }
  
})

# Affichage du résultat d'itinéraire de l'estimation
varEasyButtonEstimate <- reactiveVal(NULL)

observeEvent(input$easyButtonEstimate, {
  
  btnClicked <- as.character(input$easyButtonEstimate$index)
  
  labelInfo <- i18n()$t(
    "Veuillez réaliser au moins une estimation d'itinéraires et réessayer s'il vous plaît."
  )
  
  # Vérifier que le bouton concerné est bien "infoEstimate"
  if (!identical(btnClicked, "infoEstimate")) {
    return()
  }
  
  # Vérification des données du rapport
  reportData <- data_report()
  
  hasReport <- !is.null(reportData) &&
    length(reportData) > 0 &&
    length(unlist(reportData)) > 0
  
  if (!hasReport) {
    
    sendSweetAlert(
      session = session,
      title = "Message !",
      text = labelInfo,
      type = "info"
    )
    
    return()
    
  }
  
  # Stocker le bouton actif
  varEasyButtonEstimate(btnClicked)
  
  # Afficher la fenêtre modale
  showModal(
    modalDialog(
      size = "m",
      
      title = tags$b(i18n()$t("Résultats")),
      
      div(
        id = "IdShowModalEstimate",
        class = "text-center",
        
        uiOutput("inforesultat"),
        
        hr(),
        
        radioButtons(
          inputId = "format",
          label = i18n()$t("Document sous format"),
          choices = c("PDF", "HTML", "Word"),
          inline = TRUE
        )
      ),
      
      footer = tagList(
        div(
          style = "display:inline-flex; gap:10px;",
          uiOutput("btnDownloadReport"),
          modalButton(
            i18n()$t("Fermer"),
            icon = icon("close")
          )
        )
      )
    )
  )
  
})

# Exporter le rapport du parcours d'itinéraires
output$downloadReport <- downloadHandler(
  
  filename = function() {
    
    paste0(
      "reportCourse_",
      switch(
        input$format,
        PDF  = "PDF",
        HTML = "HTML",
        Word = "DOCX"
      ),
      ".zip"
    )
    
  },
  
  content = function(file) {
    
    # Désactiver le bouton pendant le traitement
    output$btnDownloadReport <- renderUI({
      disabled(
        downloadButton(
          "downloadReport",
          i18n()$t("Imprimer les résultats"),
          icon = icon("print"),
          class = "btDownload"
        )
      )
    })
    
    # Notification de téléchargement
    showNotification(
      paste0(i18n()$t("Téléchargement en cours...")),
      duration = NULL,
      type = "default",
      id = "notifDownloadReport",
      closeButton = FALSE
    )
    
    # Nettoyage automatique
    on.exit({
      
      removeNotification("notifDownloadReport")
      
      output$btnDownloadReport <- renderUI({
        downloadButton(
          "downloadReport",
          i18n()$t("Imprimer les résultats"),
          icon = icon("print"),
          class = "btDownload"
        )
      })
      
    }, add = TRUE)
    
    # ============================
    # Données du rapport
    # ============================
    list.data.report <- data_report()
    var.session <- session$token
    
    if (is.null(list.data.report)) {
      stop("Aucune donnée disponible pour générer le rapport.")
    }
    
    # ============================
    # Variables du rapport
    # ============================
    rapportLabels    <- list.data.report$rapportLabels
    resultatParcours <- list.data.report$resultatParcours
    infoRiverLabel   <- list.data.report$infoRiverLabel
    infoRapport      <- list.data.report$infoRapport
    pathRoad         <- list.data.report$pathRoad
    
    # ============================
    # Coordonnées départ / arrivée
    # ============================
    dep.xy <- resultatParcours[1, ]$XY
    
    dep.xy <- c(
      sapply(strsplit(dep.xy, ",|: "), "[", 1),
      sapply(strsplit(dep.xy, ",|: "), "[", 2),
      sapply(strsplit(dep.xy, ",|: "), "[", 3)
    )
    
    arr.xy <- resultatParcours[2, ]$XY
    
    arr.xy <- c(
      sapply(strsplit(arr.xy, ",|: "), "[", 1),
      sapply(strsplit(arr.xy, ",|: "), "[", 2),
      sapply(strsplit(arr.xy, ",|: "), "[", 3)
    )
    
    data.plot <- tibble::tibble(
      label = c(dep.xy[1], arr.xy[1]),
      long  = as.numeric(c(dep.xy[2], arr.xy[2])),
      lat   = as.numeric(c(dep.xy[3], arr.xy[3]))
    )
    
    # ============================
    # Points spatiaux
    # ============================
    
    road_sf <- sf::st_as_sf(pathRoad) |> sf::st_transform(3857)
    
    pts_sf <- sf::st_as_sf(
      data.plot,
      coords = c("long", "lat"),
      crs = 4326
    ) |> sf::st_transform(3857)
    
    pts_df <- sf::st_drop_geometry(pts_sf)
    coords <- sf::st_coordinates(pts_sf)
    
    pts_df$x <- coords[,1]
    pts_df$y <- coords[,2]
    
    # ============================
    # Carte du parcours
    # ============================
    road.fig <- ggplot2::ggplot() +
      
                ggspatial::annotation_map_tile(
                  type = "osm",
                  zoom = 15) +
                
                ggplot2::geom_sf(
                  data = road_sf,
                  fill = NA,
                  linewidth = 2,
                  colour = "#51ABE4"
                ) +
                
                ggplot2::geom_sf(
                  data = pts_sf[1, ],
                  size = 4,
                  colour = "green"
                ) +
                
                ggplot2::geom_sf(
                  data = pts_sf[2, ],
                  size = 4,
                  colour = "red"
                ) +
                
                ggrepel::geom_label_repel(
                  data = pts_df,
                  ggplot2::aes(x = x, y = y, label = label),
                  force = 100,
                  seed = 10,
                  max.overlaps = Inf
                ) +
                
                ggplot2::labs(
                  x = "Longitude",
                  y = "Latitude"
                ) +
          
                ggspatial::annotation_scale(location = "br") +
                ggspatial::annotation_north_arrow(location = "tl") +
          
                ggplot2::theme_minimal() +
          
                ggplot2::theme(
                  plot.title = ggplot2::element_text(
                    size = 18,
                    hjust = 0.5,
                    face = "bold"
                  ),
                  plot.subtitle = ggplot2::element_text(
                    size = 14,
                    hjust = 0.5,
                    face = "italic"
                  )
      )
    
    # ============================
    # Sauvegarde de la carte
    # ============================
    mapFile <- file.path(
      tempdir(),
      paste0(
        "road_map_",
        format(Sys.time(), "%Y%m%d%H%M%S"),
        ".png"
      )
    )
    
    ggplot2::ggsave(
      filename = mapFile,
      plot = road.fig,
      width = 10,
      height = 8,
      dpi = 300
    )
    
    # ============================
    # Objet transmis au Rmd
    # ============================
    tmp.data <- list(
      rapportLabels    = rapportLabels,
      resultatParcours = resultatParcours,
      infoRiverLabel   = infoRiverLabel,
      infoRapport      = infoRapport,
      pathRoad         = pathRoad,
      road.fig         = road.fig,
      mapFile          = mapFile
    )
    
    # ============================
    # Copie du template
    # ============================
    tempReport <- file.path(
      tempdir(),
      "reportCourse.Rmd"
    )
    
    file.copy(
      from = "./data/report/reportCourse.Rmd",
      to = tempReport,
      overwrite = TRUE
    )
    
    # ============================
    # Format de sortie
    # ============================
    output_extension <- switch(
      input$format,
      PDF  = "pdf",
      HTML = "html",
      Word = "docx"
    )
    
    output_format <- switch(
      input$format,
      PDF  = rmarkdown::pdf_document(),
      HTML = rmarkdown::html_document(),
      Word = rmarkdown::word_document()
    )
    
    # ============================
    # Répertoire de sortie
    # ============================
    output_dir <- file.path(
      var.path,
      "www",
      "tmp",
      var.session
    )
    
    if (!dir.exists(output_dir)) {
      dir.create(
        output_dir,
        recursive = TRUE
      )
    }
    
    output_file <- file.path(
      output_dir,
      paste0(
        "reportCourse_",
        input$format,
        "_(ID-1).",
        output_extension
      )
    )
    
    # ============================
    # Environnement du rapport
    # ============================
    render_env <- new.env(
      parent = globalenv()
    )
    
    render_env$tmp.data <- tmp.data
    
    # ============================
    # Génération du rapport
    # ============================
    rmarkdown::render(
      input = tempReport,
      output_file = output_file,
      output_format = output_format,
      envir = render_env,
      clean = TRUE,
      quiet = TRUE
    )
    
    # ============================
    # Vérification
    # ============================
    if (!file.exists(output_file)) {
      stop("Le rapport n'a pas été généré.")
    }
    
    # ============================
    # Fichiers à zipper
    # ============================
    files <- list.files(
      output_dir,
      pattern = paste0("\\.", output_extension, "$"),
      recursive = TRUE,
      full.names = TRUE
    )
    
    if (length(files) == 0) {
      stop("Aucun fichier généré.")
    }
    
    # ============================
    # Création du ZIP
    # ============================
    zip::zipr(
      zipfile = file,
      files = files
    )
    
  },
  
  contentType = "application/zip"
  
)

##############################################
# Nettoyer la carte de l'estimation d'itinéraires
##############################################
observeEvent(input$supMarkers, {
  
  # Réinitialisation du nombre de clics
  # valClick$nbrClick <- 0
  # vClick$nbrClick <- TRUE
  clickState$count <- 0
  
  # Réinitialisation des coordonnées
  vCompletCoordsXY$validClickStart  <- FALSE
  vCompletCoordsXY$validClickArrive <- FALSE
  
  # Réinitialisation du point de départ
  vDepartXY$longDepart <- 0
  vDepartXY$latDepart  <- 0
  vDepartXY$name       <- NULL
  
  # Réinitialisation du point d'arrivée
  vDestinationXY$longDest <- 0
  vDestinationXY$latDest  <- 0
  vDestinationXY$name     <- NULL
  
  # Nettoyage des résultats affichés
  output$infotraitement <- renderText("")
  
  # Suppression des données du rapport
  data_report(NULL)
  
  # Réinitialisation des sélections de départ
  updateSelectInput(
    session,
    inputId = "sFSTypeStart",
    selected = ""
  )
  
  output$sCommuneStartUi <- renderUI(NULL)
  output$sCsbSiteVillageStartUi <- renderUI(NULL)
  
  # Réinitialisation des sélections d'arrivée
  updateSelectInput(
    session,
    inputId = "sFSTypeArrive",
    selected = ""
  )
  
  output$sCommuneArriveUi <- renderUI(NULL)
  output$sCsbSiteVillageArriveUi <- renderUI(NULL)
  
  # Fermeture des alertes
  closeAlert(session, "alertResultat")
  
  # Suppression des couches Leaflet
  leafletProxy("estimateMap") %>%
    clearMarkers() %>%
    clearControls() %>%
    clearShapes() %>%
    clearGroup("Route") %>%
    clearGroup("Départ") %>%
    clearGroup("Arrivée") %>%
    addPolygons(
      data = limitDistrict,
      color = "purple",
      fill = FALSE,
      group = "Administrative boundary",
      layerId = "administrativeBoundary"
    )
  
  # Message dans la console
  write(
    r.console(
      "Nettoyage de la carte d'estimation et réinitialisation des paramètres terminé."
    ),
    stderr()
  )
  
})

