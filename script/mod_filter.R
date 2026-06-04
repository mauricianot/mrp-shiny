##############################################
# Onglet : Filtre géographique
##############################################
output$tabFilterPage <- renderUI({
  
  fluidRow(
    
    column(
      width = 12,
      
      box(
        width = 12,
        id = "filterGeographique",
        title = NULL,
        headerBorder = FALSE,
        
        withSpinner(
          leafletOutput(
            outputId = "filterMap",
            height = "90vh"
          ),
          type = 8
        ),
        
        absolutePanel(
          class = "bord-well",
          top = 20,
          left = 15,
          right = "auto",
          bottom = "auto",
          width = 375,
          height = "auto",
          draggable = TRUE,
          
          wellPanel(
            
            box(
              width = 12,
              status = "warning",
              id = "filter",
              title = NULL,
              headerBorder = FALSE,
              
              ###################################
              # District
              ###################################
              selectInput(
                inputId = "sFilterLimitDistrict",
                label = i18n()$t("Districts"),
                choices = c(
                  "Farafangana" = "frg",
                  "Ifanadiana"  = "ifd",
                  "Manakara"    = "mnk",
                  "Vohipeno"    = "vhp"
                ),
                selected = "frg"
              ),
              
              ###################################
              # Commune
              ###################################
              uiOutput("sFilterCommuneUi"),
              
              ###################################
              # Type de formation sanitaire
              ###################################
              selectInput("sFilterLocalFormation", 
                          label = i18n()$t("Type de formation sanitaire"),
                          choices = list("Centre de Santé de Base (CSB)" = 1, 
                                         "Site Communautaire" = 2) %>% 
                            stats::setNames(c(i18n()$t('Centre de Santé de Base (CSB)'), 
                                              i18n()$t('Site Communautaire'))),
                          selected = 1),
              
              ###################################
              # CSB ou Site communautaire
              ###################################
              uiOutput("sCsbSiteUi"),
              
              ###################################
              # Type d'information géographique
              ###################################
              selectInput("sInformationGeo", 
                          label = i18n()$t("Information géographique"), 
                          choices = c("Distance de parcours" ="distance", 
                                      "Temps de parcours" ="tempsminmax") %>% 
                            stats::setNames(c(i18n()$t('Distance de parcours'), 
                                              i18n()$t('Temps de parcours'))), 
                          selected = "distance"),
              
              ###################################
              # Distance ou temps
              ###################################
              uiOutput("sInformationGeoDisTempsUi"),
              
              ###################################
              # Boutons
              ###################################
              p(
                class = "text-center",
                
                actionButton(
                  inputId = "infoTableFilterGeo",
                  label = i18n()$t("Table d'information"),
                  icon = icon("info-circle")
                ),
                
                actionButtonStyled(
                  inputId = "showFilterGeo",
                  label = i18n()$t("Afficher sur la carte"),
                  icon = icon("eye"),
                  type = "success"
                )
              )
              
            )
          ),
          
          style = "
            opacity: 0.75;
            z-index: 10;
            padding-top: 5px;
          "
        )
      )
    )
  )
})

# Carte d'estimation / filtre géographique
fun.mapFilter <- function(){
  
  # Centre de la carte (sécurisé)
  center_lon <- mean(st_bbox(limitDistrict)[c("xmin", "xmax")])
  center_lat <- mean(st_bbox(limitDistrict)[c("ymin", "ymax")])
  
  leaflet(
    data = quakes,
    options = leafletOptions(
      maxZoom = 17,
      zoomControl = FALSE
    )
  ) %>%
    
    # Vue initiale
    setView(
      lng = center_lon,
      lat = center_lat,
      zoom = 8
    ) %>%
    
    # Fond de carte
    addTiles(group = "OpenStreetMap (OSM)") %>%
    addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
    
    # Limite administrative
    addPolygons(
      data = limitDistrict,
      color = "purple",
      fill = FALSE,
      group = "Administrative boundary",
      layerId = "administrativeBoundaryFilter"
    ) %>%
    
    # Zoom control custom
    onRender("
      function(el, x) {
        L.control.zoom({position:'topright'}).addTo(this);
      }
    ") %>%
    
    # Échelle
    addScaleBar(position = "bottomright") %>%
    
    # Contrôle des couches
    addLayersControl(
      baseGroups = c("OpenStreetMap (OSM)", "Satellite"),
      options = layersControlOptions(collapsed = TRUE)
    )
}


var.filterGeoMap <- reactiveValues()
output$filterMap <- renderLeaflet({ var.filterGeoMap$base <- fun.mapFilter()})

fun.filterDataGeo <- function() {
  
  ############################################
  # 1. Charger les données
  ############################################
  varNameFilterDistrict <- as.character(input$sFilterLimitDistrict)
  
  village <- if (input$sFilterLocalFormation == 1) {
    readRDS(paste0("./data/rds/", varNameFilterDistrict, "/csbFilter.rds"))
  } else {
    readRDS(paste0("./data/rds/", varNameFilterDistrict, "/siteFilter.rds"))
  }
  
  ############################################
  # 2. Filtre CSB / Site (FIX logique)
  ############################################
  if (!is.null(input$sCsbSite) &&
      input$sCsbSite != "" &&
      input$sCsbSite != "Select") {
    
    if (input$sFilterLocalFormation == 1) {
      village <- subset(village, csb_proche %in% input$sCsbSite)
    } else {
      village <- subset(village, chefLieuSite_proche %in% input$sCsbSite)
    }
  }
  
  if (nrow(village) == 0) return(NULL)
  
  ############################################
  # 3. Fonction de filtre générique
  ############################################
  ############################################
  # RANGE FILTER FUNCTION (FIXED)
  ############################################
  
  apply_range_filter <- function(df, col, breaks, values) {
    
    values <- as.numeric(values)
    
    if (length(values) == 0) return(df)
    
    res_list <- lapply(values, function(v) {
      
      if (is.na(v)) return(NULL)
      
      # last class
      if (v == max(breaks)) {
        
        subset(
          df,
          df[[col]] > max(breaks[-length(breaks)])
        )
        
      } else {
        
        idx <- match(v, breaks)
        
        # safety check
        if (is.na(idx) || idx <= 1) return(NULL)
        
        lower <- breaks[idx - 1]
        
        subset(
          df,
          df[[col]] > lower & df[[col]] <= v
        )
      }
    })
    
    res <- do.call(rbind, res_list)
    
    if (is.null(res)) return(df)
    
    unique(res)
  }
  
  ############################################
  # 4. Appliquer filtre géographique
  ############################################
  if (!is.null(input$sInformationGeoDisTemps)) {
    
    values <- input$sInformationGeoDisTemps
    
    if (input$sInformationGeo == "distance") {
      
      if (input$sFilterLocalFormation == 1) {
        village <- apply_range_filter(village, "distance_min",
                                      c(0,5,10,15,20,25,Inf),
                                      values)
      } else {
        village <- apply_range_filter(village, "distance_min",
                                      c(0,1,2,5,10,Inf),
                                      values)
      }
      
    } else {
      
      if (input$sFilterLocalFormation == 1) {
        village <- apply_range_filter(village, "timeWithoutRain",
                                      c(0,60,120,240,360,Inf),
                                      values)
      } else {
        village <- apply_range_filter(village, "timeWithoutRain",
                                      c(0,30,60,120,Inf),
                                      values)
      }
    }
  }
  
  ############################################
  # 5. Renommage des colonnes (clean)
  ############################################
  if (input$selected_language == "french") {
    names(village)[names(village) == "timeWithoutRain"] <- "tempsSansPluie"
    names(village)[names(village) == "timeWithRain"] <- "tempsAvecPluie"
  }
  
  if (input$selected_language == "english") {
    
    if (input$sFilterLocalFormation == 1) {
      names(village)[names(village) == "csb_proche"] <- "closest_PHC"
    } else {
      names(village)[names(village) == "chefLieuSite_proche"] <- "closest_CHS"
    }
  }
  
  ############################################
  # 6. Retour final
  ############################################
  village <- na.omit(village)
  return(village)
}


# Affichage de la table d'information du filtre
observeEvent(input$infoTableFilterGeo, {
  
  dataGeo <- fun.filterDataGeo()
  
  if (is.null(dataGeo) || nrow(dataGeo) == 0) {
    
    labelInfo <- i18n()$t("Veuillez remplir le filtre d'information géographique et réessayer s'il vous plaît.")
    
    sendSweetAlert(
      session = session,
      title = "Message!",
      text = labelInfo,
      type = "info"
    )
    
  } else {
    
    var.titleModal <- i18n()$t("Table d'information du filtre géographique")
    
    showModal(
      modalDialog(
        size = "l",
        title = var.titleModal,
        withSpinner(DT::dataTableOutput("filterTable"), type = 8),
        footer = tagList(
          downloadButton(
            'downloadFilterGeoCsv',
            i18n()$t('Télécharger'),
            icon = icon("file-excel"),
            class = "btDownload"
          ),
          modalButton(i18n()$t("Fermer"), icon = icon("close"))
        )
      )
    )
  }
})

# Expoter les données filtrées géographiques
output$downloadFilterGeoCsv <- downloadHandler(
  
  filename = function() {
    paste0("filter_geo_", Sys.Date(), ".csv")
  },
  
  content = function(file) {
    
    dataGeo <- fun.filterDataGeo()
    
    if (is.null(dataGeo)) {
      write.csv(data.frame(), file, row.names = FALSE)
    } else {
      write.csv(dataGeo, file, row.names = FALSE)
    }
  }
)


limit <- reactiveValues(building = NULL)
csbsite.temp <- reactiveValues(data = NULL)
labcsbsite <- reactiveValues(csbsite = "")
labcsbsitevillage <- reactiveValues(village = "")

# Aficher les données filtrés sur la carte en polygone
# Afficher les données filtrées sur la carte en polygone
observeEvent(input$showFilterGeo, {
  
  withProgress(
    message = i18n()$t("Merci de patienter, s'il vous plaît !"),
    detail = i18n()$t("Affichage en cours..."),
    value = 0,
    {
      
      write(r.console("Lancement du traitement du filtre géographique"), stderr())
      
      tryCatch({
        
        ########################################################
        # Vérification des inputs
        ########################################################
        
        req(input$sFilterLocalFormation)
        
        cat(
          "\n=====================================\n",
          "sFilterLocalFormation = ", input$sFilterLocalFormation,
          "\nClass = ", class(input$sFilterLocalFormation),
          "\n=====================================\n"
        )
        
        isCSB <- isTRUE(as.numeric(input$sFilterLocalFormation) == 1)
        
        ########################################################
        # Données filtrées
        ########################################################
        
        xyAll <- fun.filterDataGeo()
        
        req(xyAll)
        
        cat(
          "\n=====================================\n",
          "Nombre de lignes :", nrow(xyAll),
          "\n=====================================\n"
        )
        
        if (nrow(xyAll) == 0) {
          
          showNotification(
            i18n()$t("Aucune donnée trouvée."),
            type = "warning"
          )
          
          return()
          
        }
        
        print(head(xyAll))
        print(names(xyAll))
        
        ########################################################
        # Connexion BD
        ########################################################
        
        conn <- fun.connexionDB()
        
        on.exit({
          try(fun.closeDB(conn), silent = TRUE)
        })
        
        ########################################################
        # Nettoyage carte
        ########################################################
        
        leafletProxy("filterMap") %>%
          clearMarkers() %>%
          clearControls() %>%
          clearShapes() %>%
          addPolygons(
            data = limitDistrict,
            color = "purple",
            fill = FALSE,
            group = "Administrative boundary",
            layerId = "administrativeBoundaryFilter"
          )
        
        ########################################################
        # CAS CSB
        ########################################################
        
        process_geo_case <- function(xyAll, conn, isCSB, lang) {
          
          # ----------------------------
          # Column mapping (ONLY difference)
          # ----------------------------
          
          if (isCSB) {
            
            if (lang == "french") {
              
              xyAll <- xyAll[, c(
                "village",
                "XMenage", "YMenage",
                "Xcsb", "Ycsb",
                "csb_proche"
              )]
              
              names(xyAll) <- c(
                "village",
                "xvillage", "yvillage",
                "x", "y",
                "label"
              )
              
            } else {
              
              xyAll <- xyAll[, c(
                "village",
                "XMenage", "YMenage",
                "Xcsb", "Ycsb",
                "closest_PHC"
              )]
              
              names(xyAll) <- c(
                "village",
                "xvillage", "yvillage",
                "x", "y",
                "label"
              )
            }
            
            group_name <- "CSB"
            label_main <- "Centre de Santé de Base (CSB)"
            label_village <- "Village CSB"
            
          } else {
            
            if (lang == "french") {
              
              xyAll <- xyAll[, c(
                "village",
                "XMenage", "YMenage",
                "XChefLSite", "YChefLSite",
                "chefLieuSite_proche"
              )]
              
              names(xyAll) <- c(
                "village",
                "xvillage", "yvillage",
                "x", "y",
                "label"
              )
              
            } else {
              
              xyAll <- xyAll[, c(
                "village",
                "XMenage", "YMenage",
                "XChefLSite", "YChefLSite",
                "closest_CHS"
              )]
              
              names(xyAll) <- c(
                "village",
                "xvillage", "yvillage",
                "x", "y",
                "label"
              )
            }
            
            group_name <- "SITE"
            label_main <- "Site Communautaire"
            label_village <- "Village SITE"
          }
          
          # ----------------------------
          # DB part (identical for both)
          # ----------------------------
          print("TONGA ETO")
          xyAll$types <- "ResidentialOrBuilding"
          
          dbExecute(conn, "TRUNCATE tempvillage CASCADE")
          
          dbWriteTable(
            conn,
            "tempvillage",
            xyAll[, c("types", "xvillage", "yvillage", "village")],
            append = TRUE,
            row.names = FALSE
          )
          
          limitBulding <- pgGetGeom(
            conn,
            query = "
                      SELECT v.village, b.types, b.geom
                      FROM tempvillage v
                      JOIN building b
                        ON ST_Intersects(
                          ST_SetSRID(ST_MakePoint(v.xvillage, v.yvillage), 4326),
                          b.geom
                        )
                    "
          )
          
          list(
            data = xyAll,
            buildings = limitBulding,
            group = group_name,
            label_main = label_main,
            label_village = label_village
          )
        }
        
        res <- process_geo_case(
          xyAll = xyAll,
          conn = conn,
          isCSB = isCSB,
          lang = input$selected_language
        )
        
        limit$bulding <- res$buildings
        xyAll <- res$data
        
        leafletProxy("filterMap", data = xyAll) %>%
          clearControls() %>%
          fitBounds(
            ~min(xvillage), 
            ~min(yvillage), 
            ~max(xvillage), 
            ~max(yvillage)
          ) %>%
          addPolygons(
            data = limit$bulding,
            color = "red",
            fill = FALSE,
            group = "Village"
          ) %>%
          addCircleMarkers(
            lng = ~x,
            lat = ~y,
            data = xyAll,
            radius = 7.5,
            color = "green",
            stroke = FALSE,
            group = res$group,
            popup = ~paste(res$group, ":", label),
            label = ~paste(res$group, ":", label)
          ) %>%
          addLegend(
            position = "bottomleft",
            colors = c("red", "green"),
            labels = c(res$label_village, res$label_main),
            title = i18n()$t("Légende")
          )
        
        fun.closeDB(conn) # close connexion database
        
        write(
          r.console("Traitement terminé"),
          stderr()
        )
        
      }, error = function(e) {
        
        cat(
          "\n=====================================\n",
          "ERREUR : ",
          conditionMessage(e),
          "\n=====================================\n"
        )
        
        showNotification(
          conditionMessage(e),
          type = "error",
          duration = 10
        )
        
      })
      
    }
  )
  
})

# Evènement de la select district filter géographique
observeEvent(input$sFilterLimitDistrict, {
  
  req(input$sFilterLimitDistrict)
  
  varNameFilterDistrict <- as.character(input$sFilterLimitDistrict)
  
  varNameCommuneDistrict <- nameLimitAminDistrict[
    nameLimitAminDistrict$abrv %in% varNameFilterDistrict,
  ]
  
  communes <- NULL
  
  if (!is.null(varNameCommuneDistrict) && nrow(varNameCommuneDistrict) > 0) {
    communes <- sort(unique(as.character(varNameCommuneDistrict$commune)))
  }
  
  output$sFilterCommuneUi <- renderUI({
    
    selectInput(
      "sFilterCommune",
      label = i18n()$t("Commune"),
      choices = c(communes, "Select" = ""),
      selected = ""
    )
    
  })
})

# Affichage des liste déroulant du csb ou site
observeEvent(c(input$sFilterLocalFormation, input$sFilterCommune), {
  
  varNameFilterDistrict <- as.character(input$sFilterLimitDistrict)
  
  csbData <- readRDS(
    paste0("./data/rds/", varNameFilterDistrict, "/csb_", varNameFilterDistrict, ".rds")
  )
  
  siteData <- readRDS(
    paste0("./data/rds/", varNameFilterDistrict, "/site_", varNameFilterDistrict, ".rds")
  )
  
  # Sécurité : données absentes
  if (is.null(csbData) || is.null(siteData)) return()
  
  # Sécurité input commune
  if (is.null(input$sFilterCommune) || input$sFilterCommune == "") return()
  
  # =========================
  # CAS CSB
  # =========================
  if (isTRUE(as.numeric(input$sFilterLocalFormation) == 1)) {
    
    output$sCsbSiteUi <- renderUI({
      
      csbTemp <- subset(
        csbData,
        commune == as.character(input$sFilterCommune)
      )
      csbList <- unique(as.character(csbTemp$csb))
      csbList <- csbList[!is.na(csbList)]
      
      selectInput(
        "sCsbSite",
        label = i18n()$t("Nom de formation sanitaire"),
        choices = c(csbList, "Select" = ""),
        selected = ""
      )
    })
    
    # Temps
    if (identical(input$sInformationGeo, "tempsminmax")) {
      
      output$sInformationGeoDisTempsUi <- renderUI({
        selectInput(
          "sInformationGeoDisTemps",
          i18n()$t("Temps de parcours"),
          choices = c(
            "Select" = "",
            "0 - 1 heure" = 60,
            "1 - 2 heures" = 120,
            "2 - 4 heures" = 240,
            "4 - 6 heures" = 360,
            "6 - 10 heures" = 600
          ) %>%
            stats::setNames(c(
              i18n()$t("Select"),
              i18n()$t("0 - 1 heure"),
              i18n()$t("1 - 2 heures"),
              i18n()$t("2 - 4 heures"),
              i18n()$t("4 - 6 heures"),
              i18n()$t("6 - 10 heures")
            )),
          multiple = TRUE
        )
      })
      
    } else {
      
      output$sInformationGeoDisTempsUi <- renderUI({
        selectInput(
          "sInformationGeoDisTemps",
          i18n()$t("Distance de parcours"),
          choices = c(
            "Select" = "",
            "0 - 5 kilomètres" = 5,
            "5 - 10 kilomètres" = 10,
            "10 - 15 kilomètres" = 15,
            "15 - 20 kilomètres" = 20,
            "20 - 25 kilomètres" = 25,
            "25 - 30 kilomètres" = 30
          ) %>%
            stats::setNames(c(
              i18n()$t("Select"),
              i18n()$t("0 - 5 kilomètres"),
              i18n()$t("5 - 10 kilomètres"),
              i18n()$t("10 - 15 kilomètres"),
              i18n()$t("15 - 20 kilomètres"),
              i18n()$t("20 - 25 kilomètres"),
              i18n()$t("25 - 30 kilomètres")
            )),
          multiple = TRUE
        )
      })
    }
    
    # =========================
    # CAS SITE COMMUNAUTAIRE
    # =========================
  } else {
    
    output$sCsbSiteUi <- renderUI({
      
      siteTemp <- subset(
        siteData,
        commune == as.character(input$sFilterCommune)
      )
      siteList <- unique(as.character(siteTemp$site))
      siteList <- siteList[!is.na(siteList)]
      
      selectInput(
        "sCsbSite",
        label = i18n()$t("Nom de formation sanitaire"),
        choices = c(siteList, "Select" = ""),
        selected = ""
      )
    })
    
    # Temps
    if (identical(input$sInformationGeo, "tempsminmax")) {
      
      output$sInformationGeoDisTempsUi <- renderUI({
        selectInput(
          "sInformationGeoDisTemps",
          i18n()$t("Temps de parcours"),
          choices = c(
            "Select" = "",
            "0 - 30 minutes" = 30,
            "30min - 1 heure" = 60,
            "1 - 2 heures" = 120,
            "2 - 4 heures" = 240
          ) %>%
            stats::setNames(c(
              i18n()$t("Select"),
              i18n()$t("0 - 30 minutes"),
              i18n()$t("30min - 1 heure"),
              i18n()$t("1 - 2 heures"),
              i18n()$t("2 - 4 heures")
            )),
          multiple = TRUE
        )
      })
      
    } else {
      
      output$sInformationGeoDisTempsUi <- renderUI({
        selectInput(
          "sInformationGeoDisTemps",
          i18n()$t("Distance de parcours"),
          choices = c(
            "Select" = "",
            "0 - 1 kilomètre" = 1,
            "1 - 2 kilomètres" = 2,
            "2 - 5 kilomètres" = 5,
            "5 - 10 kilomètres" = 10,
            "10 - 15 kilomètres" = 15
          ) %>%
            stats::setNames(c(
              i18n()$t("Select"),
              i18n()$t("0 - 1 kilomètre"),
              i18n()$t("1 - 2 kilomètres"),
              i18n()$t("2 - 5 kilomètres"),
              i18n()$t("5 - 10 kilomètres"),
              i18n()$t("10 - 15 kilomètres")
            )),
          multiple = TRUE
        )
      })
    }
  }
})

# Activation selectInput info geographique filter
observeEvent(c(input$sInformationGeo, input$sFilterLocalFormation), {
  
  # sécurité
  if (is.null(input$sInformationGeo) || is.null(input$sFilterLocalFormation)) return()
  
  isTime <- input$sInformationGeo == "tempsminmax"
  isDist <- input$sInformationGeo == "distance"
  isCSB  <- input$sFilterLocalFormation == 1
  isSite <- input$sFilterLocalFormation == 2
  
  # =========================
  # TEMPS - CSB
  # =========================
  if (isTime && isCSB) {
    
    output$sInformationGeoDisTempsUi <- renderUI({
      selectInput(
        "sInformationGeoDisTemps",
        i18n()$t("Temps de parcours"),
        choices = c(
          "Select" = "",
          "0 - 1 heure" = 60,
          "1 - 2 heures" = 120,
          "2 - 4 heures" = 240,
          "4 - 6 heures" = 360,
          "6 - 10 heures" = 600
        ) %>% stats::setNames(c(
          i18n()$t("Select"),
          i18n()$t("0 - 1 heure"),
          i18n()$t("1 - 2 heures"),
          i18n()$t("2 - 4 heures"),
          i18n()$t("4 - 6 heures"),
          i18n()$t("6 - 10 heures")
        )),
        multiple = TRUE
      )
    })
    
    # =========================
    # TEMPS - SITE
    # =========================
  } else if (isTime && isSite) {
    
    output$sInformationGeoDisTempsUi <- renderUI({
      selectInput(
        "sInformationGeoDisTemps",
        i18n()$t("Temps de parcours"),
        choices = c(
          "Select" = "",
          "0 - 30 minutes" = 30,
          "30min - 1 heure" = 60,
          "1 - 2 heures" = 120,
          "2 - 4 heures" = 240
        ) %>% stats::setNames(c(
          i18n()$t("Select"),
          i18n()$t("0 - 30 minutes"),
          i18n()$t("30min - 1 heure"),
          i18n()$t("1 - 2 heures"),
          i18n()$t("2 - 4 heures")
        )),
        multiple = TRUE
      )
    })
    
    # =========================
    # DISTANCE - CSB
    # =========================
  } else if (isDist && isCSB) {
    
    output$sInformationGeoDisTempsUi <- renderUI({
      selectInput(
        "sInformationGeoDisTemps",
        i18n()$t("Distance de parcours"),
        choices = c(
          "Select" = "",
          "0 - 5 kilomètres" = 5,
          "5 - 10 kilomètres" = 10,
          "10 - 15 kilomètres" = 15,
          "15 - 20 kilomètres" = 20,
          "20 - 25 kilomètres" = 25,
          "25 - 30 kilomètres" = 30
        ) %>% stats::setNames(c(
          i18n()$t("Select"),
          i18n()$t("0 - 5 kilomètres"),
          i18n()$t("5 - 10 kilomètres"),
          i18n()$t("10 - 15 kilomètres"),
          i18n()$t("15 - 20 kilomètres"),
          i18n()$t("20 - 25 kilomètres"),
          i18n()$t("25 - 30 kilomètres")
        )),
        multiple = TRUE
      )
    })
    
    # =========================
    # DISTANCE - SITE
    # =========================
  } else if (isDist && isSite) {
    
    output$sInformationGeoDisTempsUi <- renderUI({
      selectInput(
        "sInformationGeoDisTemps",
        i18n()$t("Distance de parcours"),
        choices = c(
          "Select" = "",
          "0 - 1 kilomètre" = 1,
          "1 - 2 kilomètres" = 2,
          "2 - 5 kilomètres" = 5,
          "5 - 10 kilomètres" = 10,
          "10 - 15 kilomètres" = 15
        ) %>% stats::setNames(c(
          i18n()$t("Select"),
          i18n()$t("0 - 1 kilomètre"),
          i18n()$t("1 - 2 kilomètres"),
          i18n()$t("2 - 5 kilomètres"),
          i18n()$t("5 - 10 kilomètres"),
          i18n()$t("10 - 15 kilomètres")
        )),
        multiple = TRUE
      )
    })
  }
})


# Afficher les données filter par la formulaire dans le table filtre
output$filterTable <- DT::renderDataTable({
  
  # Récupération des données (UNE seule fois)
  village <- fun.filterDataGeo()
  
  print(village)
  
  # Sécurité : éviter crash DT
  if (is.null(village) || nrow(village) == 0) {
    return(
      DT::datatable(
        data.frame(Message = i18n()$t("Aucune donnée disponible")),
        options = list(dom = "t"),
        rownames = FALSE
      )
    )
  }
  
  DT::datatable(
    village,
    extensions = "FixedColumns",
    options = list(
      
      scrollX = TRUE,
      
      fixedColumns = list(
        leftColumns = 2,
        rightColumns = 1
      ),
      
      initComplete = JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({",
        "'background-color': 'steelblue',",
        "'color': 'white'",
        "});",
        "}"
      ),
      
      language = fromJSON(i18n()$t("./data/other/French.json"))
    )
  )
})