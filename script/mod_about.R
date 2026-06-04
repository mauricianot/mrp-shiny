# =========================
# ABOUT PAGE
# =========================

output$tabAboutPage <- renderUI({
  
  fluidRow(
    column(width = 12,
           box(
             width = 12, 
             id = 'about',
             title = NULL,
             headerBorder = FALSE,
             h1(i18n()$t("Bienvenue sur LALANA!"), class = "text-center"),
             hr(),
             tags$p(HTML(i18n()$t("Cette application a été développée dans le cadre du projet «Populations enclavées et paludisme (MRP) : mise à l’échelle d’outils pour la modélisation de l’accessibilité géographique à petites échelles dans le sud-est de Madagascar» mis en place conjointement par l’<a href='https://www.pasteur.mg/' target='_blank'>IPM</a>, l’<a href='https://www.ird.fr/' target='_blank'>IRD</a> et l'équipe de <a href='https://pivotworks.org/' target='_blank'>PIVOT Science</a> et financé par <a href='https://www.pmi.gov/' target='_blank'>USAID-PMI</a>. Elle a pour but de renforcer la capacité des programmes de santé primaire et communautaire à fournir des soins aux populations enclavés dans les Districts d’Ifanadiana, Manakara, Vohipeno et Farafangana. Les estimations fournies dans cette application sont basées sur une cartographie participative de plus de 500,000 bâtiments connectés par un réseau de plus de 70,000km de chemins, réalisée sur <a href='https://www.openstreetmap.org/' target='_blank'>OpenStreetMap</a> en 2018/2019 pour Ifanadiana et 2021/2022 pour les autres Districts."))),
             tags$p(HTML(i18n()$t('Plusieurs fonctionnalités sont disponibles :'))),
             tags$ul(
               tags$li(HTML("<p>", i18n()$t('<b>Estimation d’itinéraires : </b> permet de calculer le chemin le plus court entre deux points du district d’Ifanadiana (structure de soins ou village), avec des informations précises sur la distance et le temps de trajet selon la géographie du district et les conditions climatiques. Ces itinéraires peuvent être sauvegardés en format PDF, Word ou HTML pour utilisation ultérieure lors des missions sur le terrain.'),"</p>")),
               tags$li(HTML("<p>", i18n()$t('<b>Filtre géographique : </b> permet d’identifier les ménages et zones résidentielles qui sont à une distance choisie ou temps de trajet des structures de soins de santé primaire (Site communautaire ou Centre de Santé). Ces ménages sont identifiés à la fois sous  forme de tableau et sur une carte dynamique.'),"</p>")),
               tags$li(HTML("<p>", i18n()$t('<b>Distribution : </b> montre sur une carte la distribution géographique de l’accès aux Centres de Santé et Sites communautaire pour la population du district d’Ifanadiana (distance et temps de trajet). Cet outil permet de visualiser les zones du district les plus vulnérables en termes d’accessibilité géographique. Il permet également à l’utilisateur de connaitre avec précision la proportion de la population avec un bon ou mauves accès géographique aux soins dans chaque commune et Fokontany du district.'),"</p>"))
             ),
             tags$p(HTML(i18n()$t("Pour obtenir plus d'informations sur l'utilisation des applications LALANA, OSMAnd et QuickOSM, veuillez télécharger le guide d'utilisation en cliquant sur ce <a href='users_guide_mrp.pdf' target='_blank' download = 'users_guide_mrp.pdf'>LIEN</a>."))),
             fluidRow(
               p(class = 'text-center', 
                 column(width = 4, 
                        tags$img(src = "img_apropos3.jpeg", 
                                 width = "100%", 
                                 title=i18n()$t("Institut Pasteur Visitez les ménages"), 
                                 style="padding:2px;")),
                 column(width = 4, 
                        tags$img(src = "img_apropos1.png", 
                                 width = "100%", 
                                 title=i18n()$t("Planet Wheeler Visitez le site communautaire Ambalapaiso Nord (Kelilalina)"), style="padding:2px;")),
                 column(width = 4, 
                        tags$img(src = "img_apropos2.png", 
                                 width = "100%", 
                                 title=i18n()$t("Visite du site communautaire avec Peter Harris"), style="padding:2px;"))
               )
             ),
             hr(),
             fluidRow(
               p(class = 'text-center',
                 tags$img(src = "logo_mbs.png"),
                 tags$img(src = "logo_ipm.png"),
                 tags$img(src = "logoIRD.png", 
                          style="margin: 10px;"),
                 tags$img(src = "logoPivot.png", 
                          style="margin: 10px;")
               )
             )
           )
    )
  )
  
})