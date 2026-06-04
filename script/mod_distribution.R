# Tab de l'onglet "Distribution"
output$tabDistributionPage <- renderUI({
  
  fluidRow(
    
    # =========================
    # BOX 1 : MAP DISTRIBUTION
    # =========================
    box(
      id = "distribution1",
      title = NULL,
      headerBorder = FALSE,
      
      p(
        class = "text-muted",
        paste(i18n()$t("Carte de la distance (km) et du temps de trajet (min) pour rejoindre les CSBs et les sites communautaires plus proche des ménages. Cette carte a été faite avec les informations des plus de 60 000, 100 000, 110 000 et 35 000 ménages; pour le district de Farafangana, Ifanadiana, Manakara  et Vohipeno." ))
      ),
      
      hr(),
      
      withSpinner(
        uiOutput("uiMapDistribution"),
        type = 8
      ),
      
      # =========================
      # FILTRE DISTRICT (OVERLAY)
      # =========================
      absolutePanel(
        class = "bord-well",
        top = 120,
        left = "30%",
        right = "auto",
        bottom = "auto",
        height = "auto",
        width = 350,
        draggable = TRUE,
        style = "opacity: 0.75; z-index: 10; padding-top: 5px;",
        
        wellPanel(
          box(
            width = 12,
            id = "boxMapDistribution",
            title = NULL,
            headerBorder = FALSE,
            
            fluidRow(
              column(
                3,
                tags$b("DISTRICTS:")
              ),
              column(
                9,
                selectInput(
                  "sDistrictDistribution",
                  label = NULL,
                  choices = c(
                    "FARAFANGANA" = "FARAFANGANA",
                    "IFANADIANA" = "IFANADIANA",
                    "MANAKARA" = "MANAKARA",
                    "VOHIPENO" = "VOHIPENO"
                  ),
                  selected = "FARAFANGANA"
                )
              )
            )
          )
        )
      ),
      
      hr(),
      
      # =========================
      # METADATA TIF
      # =========================
      tabBox(
        title = i18n()$t("Métadonnées TIF"),
        side = "left",
        id = "tabset2", 
        height = NULL, 
        width = NULL,
        
        # -------- CSB --------
        tabPanel(
          i18n()$t("CSB"),
          
          fluidRow(
            column(
              8,
              class = "text-muted",
              paste(i18n()$t("Cette carte raster provient de l'interpolation de la distance entre les ménages et les CSBs. La distance a été calculée à partir de l'outil OSRM."))
            ),
            column(
              4,
              class = "text-center dashdist",
              uiOutput("uiDownloadCsbDistance")
            )
          ),
          
          hr(),
          
          fluidRow(
            column(
              8,
              class = "text-muted",
              paste(i18n()$t("Cette carte raster represente le temps minimum pour rejoindre les CSBs. Le temps a été prédit à partir d'un modèle statistique qui tient en compte l'occupation du sol, la pente, la précipitation et la vitesse de marche. Cette vitesse à été obtenue à partir des échantillons d'itinéraire dans le district d'Ifanadiana. Le temps minimum est l'équivalent au temps de parcours sans pluie."))
            ),
            column(
              4,
              class = "text-center dash",
              uiOutput("uiDownloadCsbWithoutRain")
            )
          ),
          
          hr(),
          
          fluidRow(
            column(
              8,
              class = "text-muted",
              paste(i18n()$t("Cette carte raster represente le temps maximum pour rejoindre les CSBs. Le temps a été prédit à partir d'un modèle statistique qui tient en compte l'occupation du sol, la pente, la précipitation et la vitesse de marche. Cette vitesse à été obtenue à partir des échantillons d'itinéraire dans le district d'Ifanadiana. Le temps maximum est l'équivalent au temps de parcours avec pluie."))
            ),
            column(
              4,
              class = "text-center dash",
              uiOutput("uiDownloadCsbWithRain")
            )
          )
        ),
        
        # -------- SITE --------
        tabPanel(
          i18n()$t("SITE"),
          
          fluidRow(
            column(
              8,
              class = "text-muted",
              paste(i18n()$t("Cette carte raster provient de l'interpolation de la distance entre les ménages et les sites communautaires. La distance a été calculée à partir de l'outil OSRM."))
            ),
            column(
              4,
              class = "text-center dashdist",
              uiOutput("uiDownloadSiteDistance")
            )
          ),
          
          hr(),
          
          fluidRow(
            column(
              8,
              class = "text-muted",
              paste(i18n()$t("Cette carte raster represente le temps minimum pour rejoindre les sites communautaires. Le temps a été prédit à partir d'un modèle statistique qui tient en compte l'occupation du sol, la pente, la précipitation et la vitesse de marche. Cette vitesse à été obtenue à partir des échantillons d'itinéraire dans le district d'Ifanadiana. Le temps minimum est l'équivalent au temps de parcours sans pluie."))
            ),
            column(
              4,
              class = "text-center dash",
              uiOutput("uiDownloadSiteWithoutRain")
            )
          ),
          
          hr(),
          
          fluidRow(
            column(
              8,
              class = "text-muted",
              paste(i18n()$t("Cette carte raster represente le temps maximum pour rejoindre les sites communautaires. Le temps a été prédit à partir d'un modèle statistique qui tient en compte l'occupation du sol, la pente, la précipitation et la vitesse de marche. Cette vitesse à été obtenue à partir des échantillons d'itinéraire dans le district d'Ifanadiana. Le temps maximum est l'équivalent au temps de parcours avec pluie."))
            ),
            column(
              4,
              class = "text-center dash",
              uiOutput("uiDownloadSiteWithRain")
            )
          )
        )
      )
    ),
    
    # =========================
    # BOX 2 : STAT TABLE
    # =========================
    box(
      status = "primary",
      id = "distribution2",
      title = NULL,
      headerBorder = FALSE,
      
      p(
        class = "text-muted",
        paste(i18n()$t("Pourcentage (%) de la population par commune ou fokontany selon la distance (km) et le temps de trajet (min) vers une formation sanitaire dans le district de Farafangana, Ifanadiana, Manakara et Vohipeno."))
      ),
      
      hr(),
      
      fluidRow(
        
        column(
          4,
          selectInput(
            "sDistributionLimitDistrict",
            label = "Districts",
            choices = c(
              "Farafangana" = "frg",
              "Ifanadiana" = "ifd",
              "Manakara" = "mnk",
              "Vohipeno" = "vhp"
            ),
            selected = "frg"
          )
        ),
        
        column(
          4,
          selectInput(
            "sDistributionInfoGeographique",
            label = i18n()$t("Information géographique"),
            choices = stats::setNames(
              c("distance", "tempsminmax"),
              c(i18n()$t("Distance de parcours"), i18n()$t("Temps de parcours"))
            ),
            selected = "distance"
          )
        ),
        
        column(
          4,
          selectInput(
            "sDistributionFormationSanitaire",
            label = i18n()$t("Formation sanitaire"),
            choices = stats::setNames(
              c("CSB", "Site Communautaire"),
              c(
                i18n()$t("Centre de Santé de Base (CSB)"),
                i18n()$t("Site Communautaire")
              )
            ),
            selected = "CSB"
          )
        )
      ),
      
      fluidRow(
        column(4, uiOutput("sDashboardTempsMinMaxUi")),
        column(4, uiOutput("sDashboardLocalisationUi")),
        column(4, uiOutput("sDashboardLocalisationComUi"))
      ),
      
      withSpinner(
        DT::dataTableOutput("distributionTable"),
        type = 8
      ),
      
      hr(),
      
      p(
        class = "text-center",
        downloadButton(
          "downloadDashbFilter",
          i18n()$t("Exporter les données filtrées")
        )
      )
    )
  )
})

# Fonction d'importation données par Ditrsict
fun.importRdsTableDistribution <- function(paramsDistrict) {
  
  files <- c(
    "dashboardCsbCommune",
    "dashboardCsbCommuneTempsMax",
    "dashboardCsbCommuneTempsMin",
    "dashboardCsbFokontany",
    "dashboardCsbFokontanyTempsMax",
    "dashboardCsbFokontanyTempsMin",
    "dashboardSiteCommune",
    "dashboardSiteCommuneTempsMax",
    "dashboardSiteCommuneTempsMin",
    "dashboardSiteFokontany",
    "dashboardSiteFokontanyTempsMax",
    "dashboardSiteFokontanyTempsMin"
  )
  
  lapply(files, function(f) {
    obj <- readRDS(file.path("./data/rds", paramsDistrict, paste0(f, ".rds")))
    names(obj) <- tolower(names(obj))
    assign(f, obj, envir = .GlobalEnv)
  })
}

# =========================
# LIMIT DISTRICT REACTIVE
# =========================
varLimitDistrictDistribution <- reactiveVal()

observeEvent(input$sDistributionLimitDistrict, {
  
  req(input$sDistributionLimitDistrict)
  
  varSelectdistrict <- as.character(input$sDistributionLimitDistrict)
  
  # Import data
  fun.importRdsTableDistribution(varSelectdistrict)
  
  # Limit admin
  varNameLimitDistrictTemp <- nameLimitAminDistrict[
    nameLimitAminDistrict$abrv %in% varSelectdistrict,
  ]
  
  varLimitDistrictDistribution(varNameLimitDistrictTemp)
  
  # UI localisation commune/fokontany
  output$sDashboardLocalisationUi <- renderUI({
    selectInput(
      "sDashboardLocalisation",
      label = i18n()$t("Limite administrative"),
      choices = c("Commune", "Fokontany"),
      selected = "Commune"
    )
  })
})

# =========================
# UI TEMPS MIN / MAX (DISABLED INIT)
# =========================
output$sDashboardTempsMinMaxUi <- renderUI({
  
  disabled(
    selectInput(
      "sDashboardTempsMinMax",
      label = i18n()$t("Précipitation"),
      choices = c(
        "Sans pluie" = "minpluie",
        "Avec pluie" = "maxpluie"
      ),
      selected = "minpluie"
    )
  )
})

# Enable / disable precipitation filter
observeEvent(input$sDistributionInfoGeographique, {
  
  if (identical(input$sDistributionInfoGeographique, "tempsminmax")) {
    enable("sDashboardTempsMinMax")
    
  } else {
    
    output$sDashboardTempsMinMaxUi <- renderUI({
      disabled(
        selectInput(
          "sDashboardTempsMinMax",
          label = i18n()$t("Précipitation"),
          choices = c(
            "Sans pluie" = "minpluie",
            "Avec pluie" = "maxpluie"
          ),
          selected = "minpluie"
        )
      )
    })
  }
})

# =========================
# ENABLE LOCALISATION COMMUNE
# =========================
observeEvent(input$sDashboardLocalisation, {
  
  if (identical(input$sDashboardLocalisation, "Fokontany")) {
    enable("sDashboardLocalisationCom")
    
  } else {
    
    output$sDashboardLocalisationComUi <- renderUI({
      disabled(
        selectInput(
          "sDashboardLocalisationCom",
          label = i18n()$t("Commune"),
          choices = c(unique(as.character(levels(factor(
            varLimitDistrictDistribution()$commune
          )))), "Select" = ""),
          selected = ""
        )
      )
    })
  }
})

# =========================
# DATA FORMATTER
# =========================
fun.arrDataTable <- function(paramsData, paramsDistrict) {
  
  if (paramsDistrict != "ifd") {
    
    paramsData <- paramsData[, c(
      (ncol(paramsData)-1):ncol(paramsData),
      2:(ncol(paramsData)-2)
    )]
    
  } else {
    
    paramsData <- paramsData[, c(2, 1, 3:ncol(paramsData))]
  }
  
  names(paramsData)[1:2] <- c("Commune", "Fokontany")
  
  paramsData
}

# =========================
# DATA SELECTOR (CORE LOGIC)
# =========================
getDataset <- function(type_geo, rain, structure) {
  
  if (type_geo == "distance") {
    
    if (structure == "CSB") {
      list(
        commune = dashboardCsbCommune,
        fokontany = dashboardCsbFokontany
      )
    } else {
      list(
        commune = dashboardSiteCommune,
        fokontany = dashboardSiteFokontany
      )
    }
    
  } else {
    
    if (structure == "CSB") {
      
      if (rain == "minpluie") {
        list(
          commune = dashboardCsbCommuneTempsMin,
          fokontany = dashboardCsbFokontanyTempsMin
        )
      } else {
        list(
          commune = dashboardCsbCommuneTempsMax,
          fokontany = dashboardCsbFokontanyTempsMax
        )
      }
      
    } else {
      
      if (rain == "minpluie") {
        list(
          commune = dashboardSiteCommuneTempsMin,
          fokontany = dashboardSiteFokontanyTempsMin
        )
      } else {
        list(
          commune = dashboardSiteCommuneTempsMax,
          fokontany = dashboardSiteFokontanyTempsMax
        )
      }
    }
  }
}

# =========================
# DATA TABLE OUTPUT
# =========================
varDataTableDistribution <- reactiveValues(
  baseData = NULL,
  district = NULL
)

output$distributionTable <- DT::renderDataTable({
  
  req(
    input$sDistributionLimitDistrict,
    input$sDistributionInfoGeographique,
    input$sDistributionFormationSanitaire,
    input$sDashboardLocalisation
  )
  
  district <- input$sDistributionLimitDistrict
  
  # Load datasets
  datasets <- getDataset(
    type_geo  = input$sDistributionInfoGeographique,
    rain      = input$sDashboardTempsMinMax,
    structure = input$sDistributionFormationSanitaire
  )
  
  dashbComm <- datasets$commune
  dashbFkt  <- datasets$fokontany
  
  # Filter commune if needed
  if (!is.null(input$sDashboardLocalisationCom) &&
      nzchar(input$sDashboardLocalisationCom) &&
      input$sDashboardLocalisationCom != "Select") {
    
    dashbFkt <- subset(
      dashbFkt,
      commune == input$sDashboardLocalisationCom
    )
  }
  
  # Choose level
  dashbTable <- if (input$sDashboardLocalisation == "Commune") {
    dashbComm
  } else {
    fun.arrDataTable(dashbFkt, district)
  }
  
  # Store for download
  varDataTableDistribution$baseData <- dashbTable
  varDataTableDistribution$district  <- district
  
  dashbTable
},
extensions = "FixedColumns",
options = list(
  initComplete = JS(
    "function(settings, json) {",
    "$(this.api().table().header()).css({'background-color': 'steelblue', 'color': 'white'});",
    "}"
  ),
  scrollX = TRUE,
  pageLength = 18,
  dom = "tip",
  fixedColumns = list(leftColumns = 2, rightColumns = 0),
  language = fromJSON(i18n()$t("./data/other/French.json"))
))


# Liste des noms des rasters à importer dans la distribution
rasterMap <- list(
  
  FARAFANGANA = list(
    CSB = list(
      distance = "./data/dashboard/frg/CSB/travelDistanceCSB.tif",
      timeMin  = "./data/dashboard/frg/CSB/travelTimeWithoutRainCSB.tif",
      timeMax  = "./data/dashboard/frg/CSB/travelTimeWithRainCSB.tif"
    ),
    SITE = list(
      distance = "./data/dashboard/frg/SITE/travelDistanceSite.tif",
      timeMin  = "./data/dashboard/frg/SITE/travelTimeWithoutRainSite.tif",
      timeMax  = "./data/dashboard/frg/SITE/travelTimeWithRainSite.tif"
    )
  ),
  
  IFANADIANA = list(
    CSB = list(
      distance = "./data/dashboard/ifd/CSB/travelDistanceCSB.tif",
      timeMin  = "./data/dashboard/ifd/CSB/travelTimeWithoutRainCSB.tif",
      timeMax  = "./data/dashboard/ifd/CSB/travelTimeWithRainCSB.tif"
    ),
    SITE = list(
      distance = "./data/dashboard/ifd/SITE/travelDistanceSite.tif",
      timeMin  = "./data/dashboard/ifd/SITE/travelTimeWithoutRainSite.tif",
      timeMax  = "./data/dashboard/ifd/SITE/travelTimeWithRainSite.tif"
    )
  ),
  
  MANAKARA = list(
    CSB = list(
      distance = "./data/dashboard/mnk/CSB/travelDistanceCSB.tif",
      timeMin  = "./data/dashboard/mnk/CSB/travelTimeWithoutRainCSB.tif",
      timeMax  = "./data/dashboard/mnk/CSB/travelTimeWithRainCSB.tif"
    ),
    SITE = list(
      distance = "./data/dashboard/mnk/SITE/travelDistanceSite.tif",
      timeMin  = "./data/dashboard/mnk/SITE/travelTimeWithoutRainSite.tif",
      timeMax  = "./data/dashboard/mnk/SITE/travelTimeWithRainSite.tif"
    )
  ),
  
  VOHIPENO = list(
    CSB = list(
      distance = "./data/dashboard/vhp/CSB/travelDistanceCSB.tif",
      timeMin  = "./data/dashboard/vhp/CSB/travelTimeWithoutRainCSB.tif",
      timeMax  = "./data/dashboard/vhp/CSB/travelTimeWithRainCSB.tif"
    ),
    SITE = list(
      distance = "./data/dashboard/vhp/SITE/travelDistanceSite.tif",
      timeMin  = "./data/dashboard/vhp/SITE/travelTimeWithoutRainSite.tif",
      timeMax  = "./data/dashboard/vhp/SITE/travelTimeWithRainSite.tif"
    )
  )
)

safeRaster <- function(path) {
  
  if (is.null(path) || is.na(path) || !file.exists(path)) {
    warning(paste("Raster manquant :", path))
    return(NULL)
  }
  
  raster(path)
}

makeLayer <- function(path) {
  
  img <- safeRaster(path)
  
  if (is.null(img)) {
    return(NULL)
  }
  
  pal <- colorNumeric(
    colorsRasterDistribution,
    raster::values(img),
    na.color = "transparent"
  )
  
  list(img, pal)
}

loadRasterSet <- function(district) {
  
  d <- rasterMap[[district]]
  
  if (is.null(d)) {
    stop(paste("District introuvable :", district))
  }
  
  layers <- list(
    
    makeLayer(d$CSB$distance),
    makeLayer(d$CSB$timeMin),
    makeLayer(d$CSB$timeMax),
    
    makeLayer(d$SITE$distance),
    makeLayer(d$SITE$timeMin),
    makeLayer(d$SITE$timeMax)
  )
  
  layers[!sapply(layers, is.null)]
}

# Importer le données raster à visuabler dans la distribution
data_futureReactiveTif <- reactiveVal()
fun.mapDistribution <- function(paramsDistricts) {
  
  data_futureReactiveTif(loadRasterSet(paramsDistricts))
  
  limitDistrictSplit <- limitDistrictSplit[
    limitDistrictSplit@data$ADM2_EN == paramsDistricts,
  ]
  
  abrv <- tolower(limitDistrictSplit$ABRV)
  
  csbData  <- readRDS(paste0("./data/rds/", abrv, "/csb_", abrv, ".rds"))
  siteData <- readRDS(paste0("./data/rds/", abrv, "/site_", abrv, ".rds"))
  
  leaflet(options = leafletOptions(maxZoom = 17, zoomControl = FALSE)) %>%
    
    setView(lng = limitDistrictSplit$X,
            lat = limitDistrictSplit$Y,
            zoom = limitDistrictSplit$ZOOM) %>%
    
    # Fond de carte
    addTiles(group = "OpenStreetMap (OSM)") %>%
    addPolygons(data = limitDistrictSplit, color = "purple", fill = FALSE) %>%
    
    # Sites
    addCircleMarkers(
      data = siteData,
      lng = ~X, lat = ~Y,
      color = "blue",
      radius = 5,
      label = ~paste0(i18n()$t("Site"), " (", site, ")"),
      group = i18n()$t("Site")
    ) %>%
    
    # CSB
    addMarkers(
      data = csbData,
      lng = ~X, lat = ~Y,
      icon = icons(
        iconUrl = ifelse(substr(csbData$csb, 0, 4) == "CSB1",
                         "blue-plus.png",
                         "red-plus.png"),
        iconWidth = 15, iconHeight = 15
      ),
      label = ~csb,
      group = i18n()$t("CSB")
    ) %>%
    
    # =========================
  # RASTER LAYERS (AUTO)
  # =========================
  addRasterImage(data_futureReactiveTif()[[1]][[1]],
                 colors = data_futureReactiveTif()[[1]][[2]],
                 opacity = 0.5,
                 group = paste0(i18n()$t("Distance CSB"), "_", abrv)) %>%
    
    addRasterImage(data_futureReactiveTif()[[2]][[1]],
                   colors = data_futureReactiveTif()[[2]][[2]],
                   opacity = 0.5,
                   group = paste0(i18n()$t("Temps Min CSB"), "_", abrv)) %>%
    
    addRasterImage(data_futureReactiveTif()[[3]][[1]],
                   colors = data_futureReactiveTif()[[3]][[2]],
                   opacity = 0.5,
                   group = paste0(i18n()$t("Temps Max CSB"), "_", abrv)) %>%
    
    addRasterImage(data_futureReactiveTif()[[4]][[1]],
                   colors = data_futureReactiveTif()[[4]][[2]],
                   opacity = 0.5,
                   group = paste0(i18n()$t("Distance Site"), "_", abrv)) %>%
    
    addRasterImage(data_futureReactiveTif()[[5]][[1]],
                   colors = data_futureReactiveTif()[[5]][[2]],
                   opacity = 0.5,
                   group = paste0(i18n()$t("Temps Min Site"), "_", abrv)) %>%
    
    addRasterImage(data_futureReactiveTif()[[6]][[1]],
                   colors = data_futureReactiveTif()[[6]][[2]],
                   opacity = 0.5,
                   group = paste0(i18n()$t("Temps Max Site"), "_", abrv)) %>%
    
    hideGroup(c(i18n()$t("CSB"), i18n()$t("Site"))) %>%
    
    onRender(
      "function(el, x) {
            L.control.zoom({position:'topright'}).addTo(this);
          }"
    ) %>% 
    
    addScaleBar(position = "bottomright") %>%
    addFullscreenControl(position = "topright") %>%
    
    addEasyButton(easyButton(id = 'easyButtonResetView', 
                             icon = icon("glyphicon glyphicon-refresh", 
                                         lib="glyphicon"), 
                             title = "Reset View",
                             position = "topright",
                             onClick = JS(paste0("function(btn, map){Shiny.onInputChange('easyButtonResetView',{ index: 'resetView', random : Math.random()} );}"))
    )) %>%
    
    addLayersControl(
      baseGroups = c(
        paste0(i18n()$t("Distance CSB"), "_", abrv), 
        paste0(i18n()$t("Temps Min CSB"), "_", abrv), 
        paste0(i18n()$t("Temps Max CSB"), "_", abrv),
        paste0(i18n()$t("Distance Site"), "_", abrv), 
        paste0(i18n()$t("Temps Min Site"), "_", abrv), 
        paste0(i18n()$t("Temps Max Site"), "_", abrv)
      ),
      overlayGroups = c(i18n()$t("CSB"), i18n()$t("Site")),
      options = layersControlOptions(collapsed = FALSE,
                                     position ="topleft")
    )
}

output$mapDistribution <- renderLeaflet({
  fun.mapDistribution("FARAFANGANA")
})

# Fonction pemret d'initialiser le zoom de l'affichage de la carte
fun.ResetViewCarte <- function(paramsLimits) {
  
  req(paramsLimits)
  
  # Filtrage sécurisé
  limit <- limitDistrictSplit[
    limitDistrictSplit@data$ADM2_EN == paramsLimits,
  ]
  
  # Protection si district introuvable
  if (is.null(limit) || nrow(limit) == 0) {
    warning("District introuvable : ", paramsLimits)
    return(NULL)
  }
  
  leafletProxy("mapDistribution") %>%
    setView(
      lng = limit$X[1],
      lat = limit$Y[1],
      zoom = limit$ZOOM[1]
    )
}

# Reset View des cartes de la distribution
observeEvent(input$easyButtonResetView, {
  
  req(input$sDistrictDistribution)
  
  fun.ResetViewCarte(as.character(input$sDistrictDistribution))
  
})

fun.sohwLegendCarteDistribution <- function(paramsLimits, paramsGroupLayers) {
  
  limitDistrictSplit <- limitDistrictSplit[
    limitDistrictSplit@data$ADM2_EN == paramsLimits,
  ]
  
  abrv <- tolower(limitDistrictSplit$ABRV)
  proxy <- leafletProxy("mapDistribution")
  
  proxy %>% clearControls()
  
  tif <- data_futureReactiveTif()
  
  # configuration centralisée
  cfg <- list(
    
    distance_csb = list(
      idx = 1,
      group = paste0(i18n()$t("Distance CSB"), "_", abrv),
      layer = paste0(i18n()$t("Distance aux CSBs (km)"), "_", abrv),
      title = "Distance (km)"
    ),
    
    time_min_csb = list(
      idx = 2,
      group = paste0(i18n()$t("Temps Min CSB"), "_", abrv),
      layer = paste0(i18n()$t("Temps min aux CSBs (min)"), "_", abrv),
      title = i18n()$t("Temps (min)")
    ),
    
    time_max_csb = list(
      idx = 3,
      group = paste0(i18n()$t("Temps Max CSB"), "_", abrv),
      layer = paste0(i18n()$t("Temps max aux CSBs (min)"), "_", abrv),
      title = i18n()$t("Temps (min)")
    ),
    
    distance_site = list(
      idx = 4,
      group = paste0(i18n()$t("Distance Site"), "_", abrv),
      layer = paste0(i18n()$t("Distance aux Sites (km)"), "_", abrv),
      title = "Distance (km)"
    ),
    
    time_min_site = list(
      idx = 5,
      group = paste0(i18n()$t("Temps Min Site"), "_", abrv), 
      layer = paste0(i18n()$t("Temps min aux Sites (min)"), "_", abrv),
      title = i18n()$t("Temps (min)")
    ),
    
    time_max_site = list(
      idx = 6,
      group = paste0(i18n()$t("Temps Max Site"), "_", abrv),
      layer = paste0(i18n()$t("Temps max aux Sites (min)"), "_", abrv),
      title = i18n()$t("Temps (min)")
    )
  )
  
  # boucle unique (remplace 6 blocs if/else)
  for (k in names(cfg)) {
    
    c <- cfg[[k]]
    
    if (c$group %in% paramsGroupLayers) {
      
      img <- tif[[c$idx]][[1]]
      pal <- tif[[c$idx]][[2]]
      
      proxy %>%
        leafem::addImageQuery(
          img,
          type = c("mousemove", "click"),
          layerId = c$layer,
          digits = 2,
          group = c$group
        ) %>%
        addLegend(
          position = "bottomleft",
          pal = pal,
          values = raster::values(img),
          title = c$title,
          layerId = c$layer,
          group = c$group
        )
      
      return()
    }
  }
  
  proxy %>% clearControls()
}

# Variable de la base des cartes de la distribution
var.distributionMap <- reactiveVal(NULL)

# Charger l'interface de la carte de distribution
output$uiMapDistribution <- renderUI({
  leafletOutput("mapDistribution", height = "510px")
})

# Changement du district de la distribution "CARTE"
observeEvent(input$sDistrictDistribution, {
  
  var.tabsetDistribution <- as.character(input$sDistrictDistribution)
  
  var.distributionMap(
    fun.mapDistribution(var.tabsetDistribution)
  )
  
  output$mapDistribution <- renderLeaflet({
    var.distributionMap()
  })
  
  # FARAFANGANA
  # Changement de la tabset 2 de la distribution "CARTE"
  if (var.tabsetDistribution == "FARAFANGANA") {
    
    # Afficher les bouttons de téléchargemet des Métadonnées TIF
    # CSB
    output$uiDownloadCsbDistance <- renderUI({ downloadButton('downloadCsbDistanceFrg', i18n()$t('Distance CSB')) })
    output$uiDownloadCsbWithoutRain <- renderUI({ downloadButton('downloadTempsMinCsbFrg', i18n()$t('Temps Min CSB')) })
    output$uiDownloadCsbWithRain <- renderUI({ downloadButton('downloadTempsMaxCsbFrg', i18n()$t('Temps Max CSB')) })
    
    # SITE
    output$uiDownloadSiteDistance <- renderUI({ downloadButton('downloadSiteDistanceFrg', i18n()$t('Distance Site')) })
    output$uiDownloadSiteWithoutRain <- renderUI({ downloadButton('downloadTempsMinSiteFrg', i18n()$t('Temps Min Site')) })
    output$uiDownloadSiteWithRain <- renderUI({ downloadButton('downloadTempsMaxSiteFrg', i18n()$t('Temps Max Site')) })
    
    # Affichier la carte par défaut de la distribution 1
    var.distributionMap(fun.mapDistribution(var.tabsetDistribution))
    output$mapDistribution <- renderLeaflet({ var.distributionMap() })
    
  }
  
  # IFANADIANA
  # Changement de la tabset 2 de la distribution "CARTE"
  if (var.tabsetDistribution == "IFANADIANA") {
    
    # Afficher les bouttons de téléchargemet des Métadonnées TIF
    # CSB
    output$uiDownloadCsbDistance <- renderUI({ downloadButton('downloadCsbDistanceIfd', i18n()$t('Distance CSB')) })
    output$uiDownloadCsbWithoutRain <- renderUI({ downloadButton('downloadTempsMinCsbIfd', i18n()$t('Temps Min CSB')) })
    output$uiDownloadCsbWithRain <- renderUI({ downloadButton('downloadTempsMaxCsbIfd', i18n()$t('Temps Max CSB')) })
    
    # SITE
    output$uiDownloadSiteDistance <- renderUI({ downloadButton('downloadSiteDistanceIfd', i18n()$t('Distance Site')) })
    output$uiDownloadSiteWithoutRain <- renderUI({ downloadButton('downloadTempsMinSiteIfd', i18n()$t('Temps Min Site')) })
    output$uiDownloadSiteWithRain <- renderUI({ downloadButton('downloadTempsMaxSiteIfd', i18n()$t('Temps Max Site')) })
    
    # Affichier la carte par défaut de la distribution 1
    var.distributionMap(fun.mapDistribution(var.tabsetDistribution))
    output$mapDistribution <- renderLeaflet({ var.distributionMap() })
    
  }
  
  # MANAKARA
  # Changement de la tabset 2 de la distribution "CARTE"
  if (var.tabsetDistribution == "MANAKARA") {
    
    # Afficher les bouttons de téléchargemet des Métadonnées TIF
    # CSB
    output$uiDownloadCsbDistance <- renderUI({ downloadButton('downloadCsbDistanceMnk', i18n()$t('Distance CSB')) })
    output$uiDownloadCsbWithoutRain <- renderUI({ downloadButton('downloadTempsMinCsbMnk', i18n()$t('Temps Min CSB')) })
    output$uiDownloadCsbWithRain <- renderUI({ downloadButton('downloadTempsMaxCsbMnk', i18n()$t('Temps Max CSB')) })
    
    # SITE
    output$uiDownloadSiteDistance <- renderUI({ downloadButton('downloadSiteDistanceMnk', i18n()$t('Distance Site')) })
    output$uiDownloadSiteWithoutRain <- renderUI({ downloadButton('downloadTempsMinSiteMnk', i18n()$t('Temps Min Site')) })
    output$uiDownloadSiteWithRain <- renderUI({ downloadButton('downloadTempsMaxSiteMnk', i18n()$t('Temps Max Site')) })
    
    # Affichier la carte par défaut de la distribution 2
    var.distributionMap(fun.mapDistribution(var.tabsetDistribution))
    output$mapDistribution <- renderLeaflet({ var.distributionMap() })
    
  }
  
  # VOHIPENO
  # Changement de la tabset 2 de la distribution "CARTE"
  if (var.tabsetDistribution == "VOHIPENO") {
    
    # Afficher les bouttons de téléchargemet des Métadonnées TIF
    # CSB
    output$uiDownloadCsbDistance <- renderUI({ downloadButton('downloadCsbDistanceVhp', i18n()$t('Distance CSB')) })
    output$uiDownloadCsbWithoutRain <- renderUI({ downloadButton('downloadTempsMinCsbVhp', i18n()$t('Temps Min CSB')) })
    output$uiDownloadCsbWithRain <- renderUI({ downloadButton('downloadTempsMaxCsbVhp', i18n()$t('Temps Max CSB')) })
    
    # SITE
    output$uiDownloadSiteDistance <- renderUI({ downloadButton('downloadSiteDistanceVhp', i18n()$t('Distance Site')) })
    output$uiDownloadSiteWithoutRain <- renderUI({ downloadButton('downloadTempsMinSiteVhp', i18n()$t('Temps Min Site')) })
    output$uiDownloadSiteWithRain <- renderUI({ downloadButton('downloadTempsMaxSiteVhp', i18n()$t('Temps Max Site')) })
    
    # Affichier la carte par défaut de la distribution 3
    var.distributionMap(fun.mapDistribution(var.tabsetDistribution))
    output$mapDistribution <- renderLeaflet({ var.distributionMap() })
    
  }
  
  # Modifier la section de la district dela table de distribution depend de la tabset choisi
  varTolowerAbrvDistrict <- tolower(var.tabsetDistribution)
  varAbrvDistrict <- varSelectAbrvDistrict[varTolowerAbrvDistrict][[1]]
  updateSelectInput(session, "sDistributionLimitDistrict", label = i18n()$t("Districts"),
                    choices = c( "Farafangana" = "frg", "Ifanadiana" = "ifd", "Manakara" = "mnk", "Vohipeno" = "vhp"),
                    selected = varAbrvDistrict)
  
  # Charger les noms de la commune de district choisi
  varNameLimitDistrictTemp <- nameLimitAminDistrict[nameLimitAminDistrict$abrv %in% c(varAbrvDistrict),]
  output$sDashboardLocalisationComUi <- renderUI({
    disabled(selectInput("sDashboardLocalisationCom", label = i18n()$t("Commune"),
                         choices = c(unique(as.character(levels(factor(varNameLimitDistrictTemp$commune)))),
                                     "Select"=""),
                         selected = ""))
  })
  
})

# Changement de legend des cartes
observeEvent(input$mapDistribution_groups, {
  
  req(input$mapDistribution_groups)
  
  fun.sohwLegendCarteDistribution(
    input$sDistrictDistribution,
    input$mapDistribution_groups
  )
  
})

###############################################################
## TELECHARGEMENT DONNÉES RASTERS DANS L'ONGLET DISTRIBUTION
###############################################################

# FARAFANGANA
# CSB
output$downloadCsbDistanceFrg <- downloadHandler(
  filename <- function() {
    paste("travelDistanceToCSBFarafangana", "zip", sep=".")
  },
  
  content <- function(file) {
    file.copy("./data/dashboard/frg/CSB/travelDistanceCSB.zip", file)
  },
  contentType = "application/zip"
)

output$downloadTempsMinCsbFrg <- downloadHandler(
  filename <- function() {
    paste("travelTimeWithoutRainToCSBFarafangana", "zip", sep=".")
  },
  
  content <- function(file) {
    file.copy("./data/dashboard/frg/CSB/travelTimeWithoutRainCSB.zip", file)
  },
  contentType = "application/zip"
)

output$downloadTempsMaxCsbFrg <- downloadHandler(
  filename <- function() {
    paste("travelTimeWithRainToCSBFarafangana", "zip", sep=".")
  },
  
  content <- function(file) {
    file.copy("./data/dashboard/frg/CSB/travelTimeWithRainCSB.zip", file)
  },
  contentType = "application/zip"
)

# SITE
output$downloadSiteDistanceFrg <- downloadHandler(
  filename <- function() {
    paste("travelDistanceToSiteFarafangana", "zip", sep=".")
  },
  
  content <- function(file) {
    file.copy("./data/dashboard/frg/SITE/travelDistanceSite.zip", file)
  },
  contentType = "application/zip"
)

output$downloadTempsMinSiteFrg <- downloadHandler(
  filename <- function() {
    paste("travelTimeWithoutRainToSiteFarafangana", "zip", sep=".")
  },
  
  content <- function(file) {
    file.copy("./data/dashboard/frg/SITE/travelTimeWithoutRainSite.zip", file)
  },
  contentType = "application/zip"
)

output$downloadTempsMaxSiteFrg <- downloadHandler(
  filename <- function() {
    paste("travelTimeWithRainToSiteFarafangana", "zip", sep=".")
  },
  
  content <- function(file) {
    file.copy("./data/dashboard/frg/SITE/travelTimeWithRainSite.zip", file)
  },
  contentType = "application/zip"
)

# IFANADIANA
# CSB
output$downloadCsbDistanceIfd <- downloadHandler(
  filename <- function() {
    paste("travelDistanceToCSBIfanadiana", "zip", sep=".")
  },
  
  content <- function(file) {
    file.copy("./data/dashboard/ifd/CSB/travelDistanceCSB.zip", file)
  },
  contentType = "application/zip"
)

output$downloadTempsMinCsbIfd <- downloadHandler(
  filename <- function() {
    paste("travelTimeWithoutRainToCSBIfanadiana", "zip", sep=".")
  },
  
  content <- function(file) {
    file.copy("./data/dashboard/ifd/CSB/travelTimeWithoutRainCSB.zip", file)
  },
  contentType = "application/zip"
)

output$downloadTempsMaxCsbIfd <- downloadHandler(
  filename <- function() {
    paste("travelTimeWithRainToCSBIfanadiana", "zip", sep=".")
  },
  
  content <- function(file) {
    file.copy("./data/dashboard/ifd/CSB/travelTimeWithRainCSB.zip", file)
  },
  contentType = "application/zip"
)

# SITE
output$downloadSiteDistanceIfd <- downloadHandler(
  filename <- function() {
    paste("travelDistanceToSiteIfanadiana", "zip", sep=".")
  },
  
  content <- function(file) {
    file.copy("./data/dashboard/ifd/SITE/travelDistanceSite.zip", file)
  },
  contentType = "application/zip"
)

output$downloadTempsMinSiteIfd <- downloadHandler(
  filename <- function() {
    paste("travelTimeWithoutRainToSiteIfanadiana", "zip", sep=".")
  },
  
  content <- function(file) {
    file.copy("./data/dashboard/ifd/SITE/travelTimeWithoutRainSite.zip", file)
  },
  contentType = "application/zip"
)

output$downloadTempsMaxSiteIfd <- downloadHandler(
  filename <- function() {
    paste("travelTimeWithRainToSiteIfanadiana", "zip", sep=".")
  },
  
  content <- function(file) {
    file.copy("./data/dashboard/ifd/SITE/travelTimeWithRainSite.zip", file)
  },
  contentType = "application/zip"
)

# MANAKARA
# CSB
output$downloadCsbDistanceMnk <- downloadHandler(
  filename <- function() {
    paste("travelDistanceToCSBManakara", "zip", sep=".")
  },
  
  content <- function(file) {
    file.copy("./data/dashboard/mnk/CSB/travelDistanceCSB.zip", file)
  },
  contentType = "application/zip"
)

output$downloadTempsMinCsbMnk <- downloadHandler(
  filename <- function() {
    paste("travelTimeWithoutRainToCSBFManakara", "zip", sep=".")
  },
  
  content <- function(file) {
    file.copy("./data/dashboard/mnk/CSB/travelTimeWithoutRainCSB.zip", file)
  },
  contentType = "application/zip"
)

output$downloadTempsMaxCsbMnk <- downloadHandler(
  filename <- function() {
    paste("travelTimeWithRainToCSBManakara", "zip", sep=".")
  },
  
  content <- function(file) {
    file.copy("./data/dashboard/mnk/CSB/travelTimeWithRainCSB.zip", file)
  },
  contentType = "application/zip"
)

# SITE
output$downloadSiteDistanceMnk <- downloadHandler(
  filename <- function() {
    paste("travelDistanceToSiteManakara", "zip", sep=".")
  },
  
  content <- function(file) {
    file.copy("./data/dashboard/mnk/SITE/travelDistanceSite.zip", file)
  },
  contentType = "application/zip"
)

output$downloadTempsMinSiteMnk <- downloadHandler(
  filename <- function() {
    paste("travelTimeWithoutRainToSiteManakara", "zip", sep=".")
  },
  
  content <- function(file) {
    file.copy("./data/dashboard/mnk/SITE/travelTimeWithoutRainSite.zip", file)
  },
  contentType = "application/zip"
)

output$downloadTempsMaxSiteMnk <- downloadHandler(
  filename <- function() {
    paste("travelTimeWithRainToSiteManakara", "zip", sep=".")
  },
  
  content <- function(file) {
    file.copy("./data/dashboard/mnk/SITE/travelTimeWithRainSite.zip", file)
  },
  contentType = "application/zip"
)


# VOHIPENO
# CSB
output$downloadCsbDistanceVhp <- downloadHandler(
  filename <- function() {
    paste("travelDistanceToCSBVohipeno", "zip", sep=".")
  },
  
  content <- function(file) {
    file.copy("./data/dashboard/vhp/CSB/travelDistanceCSB.zip", file)
  },
  contentType = "application/zip"
)

output$downloadTempsMinCsbVhp <- downloadHandler(
  filename <- function() {
    paste("travelTimeWithoutRainToCSBFVohipeno", "zip", sep=".")
  },
  
  content <- function(file) {
    file.copy("./data/dashboard/vhp/CSB/travelTimeWithoutRainCSB.zip", file)
  },
  contentType = "application/zip"
)

output$downloadTempsMaxCsbVhp <- downloadHandler(
  filename <- function() {
    paste("travelTimeWithRainToCSBVohipeno", "zip", sep=".")
  },
  
  content <- function(file) {
    file.copy("./data/dashboard/vhp/CSB/travelTimeWithRainCSB.zip", file)
  },
  contentType = "application/zip"
)

# SITE
output$downloadSiteDistanceVhp <- downloadHandler(
  filename <- function() {
    paste("travelDistanceToSiteVohipeno", "zip", sep=".")
  },
  
  content <- function(file) {
    file.copy("./data/dashboard/vhp/SITE/travelDistanceSite.zip", file)
  },
  contentType = "application/zip"
)

output$downloadTempsMinSiteVhp <- downloadHandler(
  filename <- function() {
    paste("travelTimeWithoutRainToSiteVohipeno", "zip", sep=".")
  },
  
  content <- function(file) {
    file.copy("./data/dashboard/vhp/SITE/travelTimeWithoutRainSite.zip", file)
  },
  contentType = "application/zip"
)

output$downloadTempsMaxSiteVhp <- downloadHandler(
  filename <- function() {
    paste("travelTimeWithRainToSiteVohipeno", "zip", sep=".")
  },
  
  content <- function(file) {
    file.copy("./data/dashboard/vhp/SITE/travelTimeWithRainSite.zip", file)
  },
  contentType = "application/zip"
)


# Expoter les données filtrées du dataTable distribution
output$downloadDashbFilter <- downloadHandler(
  
  # Renommer le données à téléchrager selon le filtre
  filename <- function() {
    
    if (input$sDistributionFormationSanitaire =="CSB") {
      
      if (input$sDashboardLocalisation =="Commune") {
        paste0("populationParCommuneCSB_", as.character(varDataTableDistribution$district), ".csv")
      }else{
        paste0("populationParFokontanyCSB_", as.character(varDataTableDistribution$district), ".csv")
      }
      
    } else{
      
      if (input$sDashboardLocalisation =="Commune") {
        paste0("populationParCommuneSite_", as.character(varDataTableDistribution$district), ".csv")
      }else{
        paste0("populationParFokontanySite_", as.character(varDataTableDistribution$district), ".csv")
      }
      
    }
    
  },
  
  # Recuperer les informations et nommer avec le nom précédent
  content = function(file) {
    s = input$distributionTable_rows_all
    write.csv2( varDataTableDistribution$baseData[s, , drop = FALSE], file, row.names = F)
  }
  
)