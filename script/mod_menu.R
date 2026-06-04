# =========================
# APPLICATION SIDEBAR MENU
# =========================

output$pageSidebarMenu <- renderMenu({
  
  sidebarMenu(
    
    id = "tabs",
    
    br(),
    
    # About page
    menuItem(
      text = i18n()$t("Apropos"),
      tabName = "tabAbout",
      icon = icon("circle-info"),
      selected = TRUE
    ),
    
    # Route estimation
    menuItem(
      text = i18n()$t("Estimation d'itineraires"),
      tabName = "tabEstimate",
      icon = icon("route")
    ),
    
    # Geographic filter
    menuItem(
      text = i18n()$t("Filtre géographique"),
      tabName = "tabFilter",
      icon = icon("filter")
    ),
    
    # Distribution map
    menuItem(
      text = i18n()$t("Distribution"),
      tabName = "tabDistribution",
      icon = icon("chart-area")
    )
    
  )
  
})