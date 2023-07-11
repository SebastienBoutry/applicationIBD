source("Global.R")

# prefixer()
ui <- shiny::fluidPage(
  
  shiny::tags$head(
    shiny::tags$style(
      
      "
  .custom-table {
    border: 1px solid black;
  }
  
  .custom-table th {
    background-color: #f2f2f2;  
    font-weight: bold;          
    text-align: center;         
  }
  
  .custom-table td {
    text-align: center;         
  }
",
      ".custom-plot {",
      "  width: 800px;",
      # "  height: 400px;",
      "  margin-top: 40px;",  # Définir la marge supérieure personnalisée en pixels
      "}",
      ".custom-notification {",
      "  font-size: 40px;",
      "  text-align: center;",
      "  width: 300px;",
      "  position: fixed;",
      "  top: 50%;",
      "  left: 50%;",
      "  transform: translate(-50%, -50%);",
      "  color: pink;",
      "}",
      ".custom-notification2 {",
      "  font-size: 40px;",
      "  text-align: center;",
      "  width: 300px;",
      "  position: fixed;",
      "  top: 50%;",
      "  left: 50%;",
      "  transform: translate(-50%, -50%);",
      "  color: pink;",
      "}",
      ".custom-text {
        font-size: 18px;
        margin-bottom: 10px;
      }",
      ".custom-heading {
        font-size: 24px;
        font-weight: bold;
        margin-bottom: 15px;
      }",
      
      "@media only screen and (max-width: 768px) {
        .custom-plot {
          height: 300px;
          margin-top: 20px;
        }
        .custom-notification {
          font-size: 30px;
        }
        .custom-text {
          font-size: 16px;
        }
        .custom-heading {
          font-size: 20px;
        }
      }",
      
      HTML("
      
      .navbar-default .navbar-brand {color: black;font-size: 30px;}
        .navbar-default .navbar-brand:hover {color: black;}
        .navbar { background-color: #66C1BF;}
        .navbar-default .navbar-nav > li > a {color:black;}
        .navbar-default .navbar-nav > .active > a,
        .navbar-default .navbar-nav > .active > a:focus,
        .navbar-default .navbar-nav > .active > a:hover {color: white;background-color: #66C1BF;font-size: 25px;}
        .navbar-default .navbar-nav > li > a:hover {color: black;background-color:#66C1BF;text-decoration:underline;}
        .navbar-default .navbar-nav > li > a[data-value='Données Brutes'] {color: white;background-color: #66C1BF;font-size: 25px;}
        .navbar-default .navbar-nav > li > a[data-value='Visualisation'] {color: white;background-color: #66C1BF;font-size: 25px;}
        .navbar-default .navbar-nav > li > a[data-value='Profiles'] {color: white;background-color: #66C1BF;font-size: 25px;}
        
    
")
    ),
  ),
  shinyjs::useShinyjs(),
  
  div(
    id = "welcome_page",
    style = "position: fixed; top: 0; left: 0; width: 100%; height: 100%; background-color: #fff; z-index: 9999; text-align: center;",
    
    img(
      src = "VisualDiatom.gif",
      style = "width: 1500px; cursor: pointer; position: absolute; top: 50%; left: 50%; transform: translate(-50%, -50%);",
      onclick = "shinyjs.toggle('welcome_page'); shinyjs.toggle('app_content');"
    )
  ),
  
  
  div(
    id = "app_content",
    style = "display: none;",
    
  shiny::navbarPage(
    title = "",
    shiny::tabPanel(
      title = "Accueil",
      shiny::fluidRow(
        shiny::column(
          width = 12,
          
          div(class = "container",
              style = "display: flex; justify-content: center; align-items: center; margin-top: 15px; background-color: white; padding: 20px;",
              div(
                style = "margin-right: 40px;",
                img(src = "VisualDiatoms Logo.png", width = "200px")
              )),
    
          div(
            class = "container",
            style = "display: flex; justify-content: center; align-items: center; margin-top: 2px; background-color: white; padding: 20px;",
            div(
              style = "margin-right: 60px;",
              img(src = "INRAE.png", width = "100px")
            ),
            div(
              style = "margin-right: 60px;",
              img(src = "ECOVEA.png", width = "100px")
            ),
            div(
              style = "margin-right: 60px;",
              img(src = "logo_Aquaref.png", width = "100px")
            )
          ),
          
          div(
            class = "container",
            style = "display: flex; justify-content: center; align-items: center; margin-top: 2px; background-color: white; padding: 5px;",
            div(
              style = "margin-right: 10px;",
              "Auteur Principal: Léonard Heinry, Co-Auteurs: Sebastien Boutry, Juliette Rosebery"
            )),

          
          shiny::div(style = "border: 2px solid #66C1BF; border-radius: 10px; padding: 10px; margin-top: 20px; margin-bottom: 10px; background-color: #f2f2f2;",
          
          shiny::h1("Bienvenue dans l'application VisualDiatoms !"),
          
          shiny::p(
            class = "custom-text",
            paste0("Cette application vous permet de visualiser et de comparer les données de prélèvements diatomiques présents sur la base publique NAIADES.
            Si c'est la première fois que vous entrez sur l'application, une notice d'utilisation est à votre disposition ci-dessous. Bonne visite !"
          )),
          
          
          shiny::div(style = "border: 4px solid #66C1BF; border-radius: 10px; padding: 10px; margin-bottom: 10px; background-color: #f2f2f2;",
                     
                     shiny::h3(class = "custom-heading", "Introduction"),
                     
                     shiny::p(
                       class = "custom-text",
                       "L'objectif de cette application est d'améliorer la compréhension de la donnée diatomique, en proposant à l'utilisateur de visualiser 
                       les taxons enregistrés sur la base de données publique NAIADES. Grâce à l'interface interactive, vous allez pouvoir sélectionner, télécharger et 
                       même comparer les données des prélèvements floristiques opérés par les agences de l'eau depuis 2007. Notre application se décompose en plusieurs
                       onlget qui rendent la navigation simple et intuitive. Pour être certains que vous profiterez pleinement de l'expérience, la suite des éléments des cet
                       onglet décrit de manière synthétique les fonctionnalités que vous retrouverez dans l'application."
                       
                     )),
          
          shiny::div(style = "border: 4px solid #66C1BF; border-radius: 10px; padding: 10px; margin-bottom: 10px; background-color: #f2f2f2;",
                     
                     shiny::h3(class = "custom-heading", "L'onglet Données Brutes"),
                     
                     shiny::p(
                       class = "custom-text",
                       "En cliquant sur cet onglet, vous avez directement accès à la dernière version des données brutes extraites et formatées depuis NAIADES. 
            Servez-vous du panneau de gauche pour naviguer entre les années de prélèvements. 
            Chaque tableau d'une année est construit de la même façon, les colonnes sont requêtables pour celles présentant des caractères. 
            Les colonnes numérique peuvent quand à elles être filtrées à l'aide d'un curseur qui apparaît lorsque vous cliquez dans la barre de recherche de la colonne.
            Enfin, si vous en avez besoin, vous pouvez télécharger les données d'une année au format CSV à l'aide du bouton 'Download' situé en bas de chaque tableau.
            Veuillez noter que le volume des données est assez important et peut donc prendre plusieurs secondes voir quelques minutes !"
                       
                     )),
          
          shiny::div(style = "border: 4px solid #66C1BF; border-radius: 10px; padding: 10px; margin-bottom: 10px; background-color: #f2f2f2;",
                     shiny::h3(class = "custom-heading", "L'onglet Visualisation"),
                     shiny::p(
                       class = "custom-text",
                       "Cet onglet est composé de plusieurs éléments. Le panneau 'Chorologie' vous permet de visualiser les données de la base brute avec une vue centrée sur les taxons. 
            Sélectionnez le ou les taxons à afficher à l'aide de la liste déroulante 'Liste des taxons disponibles'. 
            La carte de l'onglet affichera ensuite les emplacements géographiques des taxons sélectionnés, et un menu vous permettra de cocher les années que vous souhaitez observer. 
            Vous pouvez sélectionner jusqu'à deux taxons. Chaque point de la carte est cliquable et affichera les informations du prélèvement que vous regardez.
            En dessous de 'Liste des taxons disponibles', les taxons appariés à celui ou ceux que vous avez choisit vous sont précisés.
            Enfin, deux histogrammes interactifs sont activables via les boutons 'Afficher les Abondances Moyennes' et 'Afficher les occurences'. Ils présentent l'évolution de l'abondance relative (en pour 1000) et 
            l'évolution du nombre d'occurences du ou des taxons dans les relevés des années ou il(s) est/sont vu, ce qui vous permet d'avoir une idée de son/leur importance.
            Lorsque vous sélectionnez un deuxième taxon, les représentations graphiques de chacun (Carte et Plot) se superposent pour vous permettre de comparer 
            l'évolution des deux.
            "
                     ),
                     
                     shiny::div(style = "border: 4px solid #66C1BF; border-radius: 10px; padding: 10px; margin-bottom: 10px; background-color: #f2f2f2;",
                                shiny::h3(class = "custom-heading", "Le sous-Onglet Données"),
                                shiny::p(
                                  class = "custom-text",
                                  "Dans ce panneau, 
            vous pouvez afficher et télécharger les données des taxons que vous avez sélectionnés depuis le panneau Chorologie. 
            Utilisez le bouton 'Download' pour télécharger les données au format CSV."
                                )),
                     
                     
                     shiny::div(style = "border: 4px solid #66C1BF; border-radius: 10px; padding: 10px; margin-bottom: 10px; background-color: #f2f2f2;",
                                shiny::h3(class = "custom-heading", "Le sous-Onglet Profile"),
                                shiny::p(
                                  class = "custom-text",
                                  "Ce panneau comporte deux onglets: 'Trophique' et 'Écologique'."
                                ),
                                shiny::p(class = "custom-text",
                                         "Dans l'onglet 'Trophique', vous pouvez afficher les informations concernant les paramètres pyshico-chimiques des milieux dans lesquels le taxon que 
                   vous avez sélectionné est vu. Ces informations ont été récupérées grâce aux travaux de David Carayon en 2019 qui a dréssé un tableau 
                   de profils physico-chimiques d'un grand nombre de taxons. Si un taxon n'est pas présent dans la base de David Carayon,
                   alors un message apparaîtra vous le précisant. Les valeurs des paramètres se superposent la encore si vous sélectionnez deux taxons."),
                                shiny::p(class = "custom-text",
                                         "Dans l'onglet 'Écologique', vous pouvez voir si le taxon sélectionné est indicateur ou pas de l'Indice Biologique Diatomées (IBD).
                   Si il l'est, vous verrez le profil écologique du taxon en question s'afficher à l'écran, sinon un message apparaîtra, comme pour l'onglet Trophique"))),
          
          shiny::p(class = "custom-text",
                   "A présent je vous laisse profiter de l'application VisualDiatoms!"),
          
          div(
            class = "container",
            style = "display: flex; justify-content: center; align-items: center; margin-top: 30px; background-color: white; padding: 20px; border: 2px solid black;",
            div(
              style = "margin-right: 20px;",
              HTML("<p>Références bibliograpiques:</p>"),
              
              HTML("<p>Carayon, D., Tison-Rosebery, J., & Delmas, F., 2019. Defining a new autoecological trait matrix for French stream benthic diatoms. Ecological Indicators, 103, 650-658.</p>"),
              
              HTML("<p>Coste, M., Boutry, S., Tison-Rosebery, J., Delmas, F., 2009. Improvements of the Biological Diatom Index (IBD): description and efficiency of the new version (IBD-2006). Ecological Indicators, 9, 621–650. https://doi.org/10.1016/j.ecolind.2008.06.003.</p>"),
              
              uiOutput("link")
              
              ))
        )
          
        )
      )
    ),
    shiny::tabPanel(
      title = "Données Brutes",
      shiny::sidebarLayout(
        shiny::sidebarPanel(
          shiny::radioButtons(
            inputId = "radio",
            label = "Visualisation des données brutes",
            choices = c("Nothing Selected" = ""),
            selected = NULL
          ),
          width = 3
        ),
        shiny::mainPanel(
          DT::dataTableOutput("tab"),
          shiny::downloadButton("downloadData", "Download")
        )
      )
    ),
    shiny::tabPanel(
      title = "Visualisation",
      shiny::sidebarLayout(
        shiny::sidebarPanel(
          shiny::selectizeInput("taxons", "Liste des taxons disponibles: ", "", multiple = TRUE),
          shiny::code(
            "Espèces comprises dans cette appelation: ",
            style = "font-size:15px;"
          ),
          shiny::p(
            shiny::textOutput("name_list"),
            style = "font-size:15px;color:black;"
          ),
          shiny::p(
            shiny::textOutput("name_list2"),
            style = "font-size:15px;color:black;"
          ),
          checkboxInput("toggleMaps", "Afficher les Abondances Moyennes"),
          checkboxInput("toggleMaps2", "Afficher les Occurences"),
          plotly::plotlyOutput("Plot1"),
          plotly::plotlyOutput("Plot2"),
          width = 4
        ),
        shiny::mainPanel(
          shiny::tabsetPanel(
            id = "tabs",
            shiny::tabPanel(
              "Chorologie",
              fluidRow(
                leafletOutput("mapFiltered", width = "100%", height = "800px")
              )
              # fluidRow(
              #   leafletOutput("mapFiltered2", width = "100%", height = "800px")
              # )
            ),
            shiny::tabPanel(
              "Données",
              shiny::fluidRow(
                shiny::mainPanel(
                  DT::dataTableOutput("Donnees2", width = "100%"),
                  shiny::downloadButton("downloadData2", "Download")
                )
              )
            ), 
            shiny::navbarMenu(
              "Profile",
              shiny::tabPanel(
                "Trophique",
                shiny::mainPanel(
                  shiny::plotOutput("Trophie", width = "100%")
                )
              ), 
              shiny::tabPanel(
                "Écologique",
                shiny::plotOutput("Profil", width = "100%")
              )
            )
          )
        )
      )
    )
  )
))

server <- function(input, output, session) {
  
  shinyjs::hide("app_content")
  
  observeEvent(input$welcome_page, {
    shinyjs::toggle("welcome_page")
    shinyjs::toggle("app_content")

  })
  
  url <- a("https://naiades.eaufrance.fr", href="https://naiades.eaufrance.fr", 
           style = "color:#423089;font-size:18px", target="_blank")
  output$link <- renderUI({
    tagList("Lien vers NAIADES:", url)
  })
 
  observeEvent(input$toggleMaps, {
    if (input$toggleMaps) {
      shinyjs::show("Plot1")
      shinyjs::hide("Plot2")
    } else {
      shinyjs::hide("Plot1")
    }
  })
  
  observeEvent(input$toggleMaps2, {
    if (input$toggleMaps2) {
      shinyjs::hide("Plot1")
      shinyjs::show("Plot2")
    } else {
      shinyjs::hide("Plot2")
    }
  })
  
  # observeEvent(input$toggleMaps, {
  #   if (input$toggleMaps) {
  #     shinyjs::hide("mapFiltered")
  #     shinyjs::show("mapFiltered2")
  #   } else {
  #     shinyjs::hide("mapFiltered2")
  #     shinyjs::show("mapFiltered")
  #   }
  # })
  
  # Observer pour surveiller les changements dans l'input taxons
  observe({
    # Obtenir le nombre d'éléments sélectionnés
    selected <- length(input$taxons)
    
    # Si plus de deux éléments sont sélectionnés, mettre à jour le selectInput pour n'afficher que les deux premiers éléments sélectionnés
    if (selected > 2) {
      updateSelectizeInput(session, "taxons", selected = input$taxons[1:2])
      
      showNotification(
        "Vous ne pouvez pas comparer plus de deux taxons",
        type = "warning",
        duration = 20
      )
    }
  })
  
  
  # shiny::observe({
  #   if (is.null(input$upload)) {
  #     return()
  #   } else {
  #     var_example <- 1
  #   }
  # })
  # output$filesUploaded <- shiny::reactive({
  #   val <- !(is.null(input$upload))
  #   print(val)
  # })
  # 
  # shiny::outputOptions(output, "filesUploaded", suspendWhenHidden = FALSE)
  
  # ---------------------------- Pre-traitement des données
  
  # Charger la dernière version des données NAIADES -------------------------
  
  
  # observeEvent(input$do, {
  #   show_modal_spinner(spin = "cube-grid",
  #                      color = "#009999",
  #                      text = "Téléchargement des données depuis le site NAIADES France, merci de patienter")
  
  
  
  # En mode SQL
  
  # Lien avec la base postgreSQL
  
  # library(RMySQL)
  # library(RPostgreSQL)
  #
  # # Établir la connexion à la base de données
  # mysqlconnection <- dbConnect(PostgreSQL(),
  #   dbname = "diatom",
  #   host = "",
  #   port = ,
  #   user = "",
  #   password = ""
  # )
  
  
  # Exécuter la requête SQL et stocker les résultats dans un data frame
  
  # query <- "SELECT abundance
  #     FROM adne.inventory_taxon"
  #
  #     query <- "
  # SELECT taxon_id, taxon_name, code, abundance, inventory_date, inventory_id, station_code, commune, station_id, x, y
  # FROM public.taxon
  # JOIN adne.inventory_taxon USING (taxon_id)
  # JOIN adne.inventory USING (inventory_id)
  # JOIN station USING (station_id)
  # JOIN station_projection USING (station_id)
  # JOIN public.taxon_identifier USING (taxon_id)
  # WHERE identifier_type_id = 1
  # "
  
  #   france_metropolitaine <- st_read("data/FRA_adm0.shp")
  #
  #   query <- "
  # SELECT t.taxon_id, t.taxon_name, t.code, t.abundance, t.inventory_date, t.inventory_id, t.identifier_type_id, t.station_code, t.commune, t.station_id, t.x, t.y, s.total_abundance
  # FROM (
  #   SELECT station_code, inventory_date, SUM(abundance) AS total_abundance
  #   FROM public.taxon
  #   JOIN adne.inventory_taxon USING (taxon_id)
  #   JOIN adne.inventory USING (inventory_id)
  #   JOIN station USING (station_id)
  #   JOIN station_projection USING (station_id)
  #   JOIN public.taxon_identifier USING (taxon_id)
  #   WHERE inventory_type_id = 1
  #   GROUP BY station_code, inventory_date
  # ) AS s
  # JOIN (
  #   SELECT taxon_id, taxon_name, code, abundance, inventory_date, inventory_id, identifier_type_id, station_code, commune, station_id, x, y
  #   FROM public.taxon
  #   JOIN adne.inventory_taxon USING (taxon_id)
  #   JOIN adne.inventory USING (inventory_id)
  #   JOIN station USING (station_id)
  #   JOIN station_projection USING (station_id)
  #   JOIN public.taxon_identifier USING (taxon_id)
  #   WHERE inventory_type_id = 1
  # ) AS t
  # ON t.station_code = s.station_code AND t.inventory_date = s.inventory_date
  # "
  #   result <- dbGetQuery(mysqlconnection, query)
  #
  #
  #   # Fermer la connexion à la base de données
  #   dbDisconnect(mysqlconnection)
  #
  #   Diatom <- as_tibble(result) %>%
  #     mutate(
  #       SANDRE = ifelse(identifier_type_id == 2, code, NA),
  #       taxon = ifelse(identifier_type_id == 1, code, NA)
  #     ) %>%
  #     fill(taxon, .direction = "up") %>%
  #     select(-code) %>%
  #     mutate(full_name = paste0(taxon_name, " ", "(", taxon, ")")) %>%
  #     dplyr::select(
  #       "CODE_STATION" = station_code,
  #       "DATE" = inventory_date,
  #       commune,
  #       x,
  #       y,
  #       "Nom_latin_taxon" = taxon_name,
  #       SANDRE,
  #       "RESULTAT" = abundance,
  #       total_abundance,
  #       taxon
  #     ) %>%
  #     filter(!is.na(SANDRE)) %>%
  #     mutate(SANDRE = as.integer(SANDRE)) %>%
  #     dplyr::mutate(RESULTAT = (RESULTAT / total_abundance) * 1000) %>%
  #     dplyr::mutate(RESULTAT = round(RESULTAT, 2)) %>% # Passage des abondances en relative pour 1000
  #     dplyr::select(-total_abundance) %>%
  #     sf::st_as_sf(coords = c("x", "y"), crs = 2154) %>%
  #     st_transform(geometry, crs = 4326) %>%
  #     st_intersection(france_metropolitaine) %>%
  #     dplyr::select(CODE_STATION, DATE, SANDRE, Nom_latin_taxon, RESULTAT, taxon, commune) %>%
  #     tidyr::extract(geometry, c("long", "lat"), "\\((.*), (.*)\\)", convert = TRUE) %>%
  #     left_join(as_tibble(read.csv2("data/table_transcodage.csv", stringsAsFactors = FALSE)) %>%
  #       dplyr::select(taxon = "abre", True_name = "CodeValid"), by = "taxon") %>%
  #     mutate(taxon = if_else(is.na(True_name) == T, taxon, True_name)) %>%
  #     dplyr::select(-True_name) %>%
  #     filter(!is.na(taxon))
  
  # Direct depuis NAIADES
  
  #
  #   download(
  #     "https://naiades.eaufrance.fr/reports/reportsperyear/HB/Naiades_Export_France_Entiere_HB.zip",
  #      # "https://naiades.eaufrance.fr/reports/reportsperyear/HM/Naiades_Export_France_Entiere_HM.zip",
  #     dest="data/dataset.zip",
  #     mode="wb")
  #   unzip ("data/dataset.zip"
  #          ,exdir = "./data"
  #          )
  #   file.remove('data/cep.csv')
  #   file.remove('data/operation.csv')
  #   file.remove('data/resultat.csv')
  #   file.remove('data/DescriptionDonneesHB.pdf')
  #
  #   remove_modal_spinner()
  #
  #   show_modal_spinner(spin = "cube-grid",
  #                      color = "#009999",
  #                      text = "Transcodage et compilation des données: Encore quelques instants")
  #
  #
  #   # france_metropolitaine <- st_read("data/FRA_adm0.shp")
  #
  #   Diatom <- as_tibble(fread("data/fauneflore.csv")) %>%
  #     dplyr::select("CODE_STATION" = CdStationMesureEauxSurface,
  #                   "Nom_groupe_taxo" = LbSupport,
  #                   "DATE" = DateDebutOperationPrelBio,
  #                   "SANDRE" = CdAppelTaxon,
  #                   "Nom_latin_taxon" = NomLatinAppelTaxon,
  #                   "RESULTAT" = RsTaxRep,
  #                   "Code_groupe_taxo" = CdSupport) %>%
  #     filter(Code_groupe_taxo == 10) %>%
  #     dplyr::select(-Code_groupe_taxo) %>%
  #     distinct(CODE_STATION, Nom_groupe_taxo, DATE, SANDRE, Nom_latin_taxon, RESULTAT) %>%
  #     dplyr::select(-Nom_groupe_taxo) %>%
  #     arrange(DATE,
  #             CODE_STATION,
  #             Nom_latin_taxon, RESULTAT) %>%
  #     filter(RESULTAT != 0) %>%
  #     mutate(DATE = as.Date(DATE)) %>%
  #     arrange(DATE) %>%
  #     group_by(CODE_STATION, DATE) %>%
  #     dplyr::mutate(tot=sum(RESULTAT)) %>%
  #     ungroup() %>%
  #     dplyr::mutate(RESULTAT = (RESULTAT/tot)*1000) %>%
  #     dplyr::mutate(RESULTAT = round(RESULTAT,2)) %>% # Passage des abondances en relative pour 1000
  #     dplyr::select(-tot) %>%
  #     left_join(read.csv2("data/CODE_OMNIDIA_traites.csv", stringsAsFactors = FALSE), by = "SANDRE") %>%
  #     filter(SANDRE != 0) %>%
  #     rename(taxon = code_omnidia) %>%
  #     mutate(CODE_STATION =  str_remove(CODE_STATION, "^0+")) %>%
  #     left_join(as_tibble(
  #       read.csv2("data/stations.csv", stringsAsFactors = FALSE,
  #                 quote = "", na.strings=c("","NA"))
  #     ) %>% select(CODE_STATION = CdStationMesureEauxSurface, commune = LbCommune,
  #                  longitude = CoordXStationMesureEauxSurface, latitude = CoordYStationMesureEauxSurface) %>%
  #       mutate(longitude = as.numeric(longitude),
  #              latitude = as.numeric(latitude)) %>%
  #       filter(!is.na(longitude)) %>%
  #       filter(longitude > 0) %>%
  #       mutate(CODE_STATION =  str_remove(CODE_STATION, "^0+")), by = "CODE_STATION") %>%
  #     drop_na() %>%
  #     sf::st_as_sf(coords = c("longitude", "latitude"), crs = 2154) %>%
  #     st_transform(geometry, crs = 4326) %>%
  #      st_intersection(france_metropolitaine) %>%
  # dplyr::select(CODE_STATION,DATE,SANDRE,Nom_latin_taxon,RESULTAT,taxon, commune) %>%
  # tidyr::extract(geometry, c("long", "lat"), "\\((.*), (.*)\\)", convert = TRUE) %>%
  # left_join(as_tibble(read.csv2("data/table_transcodage.csv", stringsAsFactors = FALSE)) %>%
  #             dplyr::select(taxon = "abre", True_name = "CodeValid"), by = "taxon") %>%
  # mutate(taxon = if_else(is.na(True_name) == T, taxon, True_name)) %>%
  # dplyr::select(-True_name) %>% filter(!is.na(taxon)) %>%
  # left_join(read.csv2("data/table_transcodage.csv", stringsAsFactors = FALSE) %>%
  #                 select(taxon = CodeValid, full_name = name_valid) %>% distinct() %>%
  #                 mutate(full_name = sub("\\_g.*", "", full_name)), by = "taxon") %>%
  #     mutate(full_name = str_replace_all(full_name, "[^[:alnum:]]", " ")) %>%
  #     mutate(full_name = paste0(full_name, " ", "(", taxon, ")")) %>%
  #     filter(commune %notin% Communes) %>%
  #     mutate(lon = round(long,2), lat = round(lat,2)) %>%
  #     left_join(read.csv2("data/table_transcodage.csv", stringsAsFactors = FALSE) %>%
  #                 select(abre, name, taxon = CodeValid) %>% unique() %>%
  #                 group_by(taxon) %>% filter(abre %notin% taxon) %>% mutate(list = paste0(abre, " ", sub("\\_g.*", "", name))) %>%
  #                 mutate(taxons_apparies = paste(list, collapse = " / ")) %>%
  #                 select(-abre, -name, -list) %>% distinct(), by = "taxon")
  
  # save(Diatom, file = "data/Donnees_compiles.RData")
  #
  #   remove_modal_spinner()
  #
  # },priority = 1, ignoreInit = TRUE)
  
  
  # ---------------------------- Fin Pre-traitement des données
  
  data <- shiny::reactive({
    # req(input$upload)
    
    shinybusy::show_modal_spinner(
      spin = "cube-grid",
      color = "#66C1BF",
      text = "Chargement des données NAIADES: Cette opération peut prendre quelques minutes"
    )
    
    # load(input$upload$datapath)
    
    githubURL <- paste0("https://github.com/leolea12/NAIDESexport/raw/main/data_raw/", fichier_plus_recent)
    
    # tempfile <- tempfile()  # Crée un fichier temporaire pour stocker le fichier téléchargé
    # download.file(url = githubURL, destfile = tempfile, mode = "wb")  # Télécharge le fichier à partir de l'URL
    # 
    # load(tempfile)
    
    load(url(githubURL))
    
    # load("data/Diatom.RData")
    
    
    shiny::updateRadioButtons(
      session = session,
      inputId = "radio",
      choiceNames = seq(min(lubridate::year(Diatom$DATE)), max(lubridate::year(Diatom$DATE))),
      choiceValues = seq(min(lubridate::year(Diatom$DATE)), max(lubridate::year(Diatom$DATE)))
    )
    
    shinyWidgets::show_alert(
      title = "Chargement terminé",
      text = "Les données NAIADES ont été chargées avec succès !",
      type = "success"
    )
    
    shinybusy::remove_modal_spinner()
    
    
    Diatom 
    # %>% mutate(full_name = ifelse(full_name == "Aucun", paste0(Nom_latin_taxon, " (", taxon,")"), full_name))
    
  })
  
  
  selection_taxon <- shiny::reactive({
    # req(input$upload)
    
    data() %>%
      dplyr::select(full_name) %>%
      dplyr::arrange(full_name) %>%
      dplyr::pull(full_name) %>%
      unique()
  })
  
  shiny::observe({
    shiny::updateSelectizeInput(session, "taxons",
                                choices = selection_taxon()
    )
  })
  
  output$name_list <- shiny::renderText({
    as.character(data() %>%
                   dplyr::filter(full_name %in% input$taxons[1]) %>%
                   dplyr::select(taxons_apparies, CodeValid) %>%
                   unique() %>%
                   dplyr::mutate(taxons_apparies = dplyr::if_else(taxons_apparies == "Aucun", paste0("Pour ",str_sub(input$taxons[1],  start = -5, end = -2),": Aucun"), 
                                                                  paste0("Pour ",str_sub(input$taxons[1],  start = -5, end = -2),": Taxons compris: ", taxons_apparies,", Code Valide = ",CodeValid))))[1]
  })
  
  output$name_list2 <- shiny::renderText({
    if(length(input$taxons) == 2){
      as.character(data() %>%
                     dplyr::filter(full_name %in% input$taxons[2]) %>%
                     dplyr::select(taxons_apparies, CodeValid) %>%
                     unique() %>%
                     dplyr::mutate(taxons_apparies = dplyr::if_else(taxons_apparies == "Aucun", paste0("Pour ",str_sub(input$taxons[2],  start = -5, end = -2),": Aucun"), 
                                                                    paste0("Pour ",str_sub(input$taxons[2],  start = -5, end = -2),": Taxons compris: ", taxons_apparies,", Code Valide = ",CodeValid))))[1]
    }else{""}
    
  })
  
  
  
  
  output$tab <- DT::renderDataTable({
    # req(input$upload)
    shiny::req(input$radio)
    
    
    DT::datatable(
      data() %>% 
        mutate(lon = round(lon, 2), lat = round(lat, 2)) %>%
        distinct(taxon, CODE_STATION, DATE, .keep_all = TRUE) %>%
        dplyr::filter(lubridate::year(DATE) == input$radio) %>%
        dplyr::select(
          date = DATE,
          "n°station" = CODE_STATION,
          commune,
          longitude = lon,
          latitude = lat,
          taxon = full_name,
          "Abondance relative (‰)" = RESULTAT
        ),
      extensions = "Buttons",
      options = list(
        pageLength = 20,
        scroller = TRUE
      ),
      filter = "top", selection = "multiple", escape = FALSE,
      class = 'custom-table'
    )
    
  })
  
  output$radio <- shiny::renderText({
    return(paste("RadioButtons :", input$radio))
  })
  
  
  output$downloadData <- shiny::downloadHandler(
    filename = function() {
      paste("Données pour l'année ", as.character(unique(lubridate::year(data()$DATE))), ".csv", sep = ".")
    },
    content = function(file) {
      shinybusy::show_modal_spinner(
        spin = "cube-grid",
        color = "#66C1BF",
        text = "Téléchargement des données, cette opération peut prendre quelques minutes"
      )
      utils::write.csv2(data() %>% 
                          mutate(lon = round(lon, 2), lat = round(lat, 2)) %>%
                          distinct(taxon, CODE_STATION, DATE, .keep_all = TRUE) %>%
                          dplyr::filter(lubridate::year(DATE) == input$radio) %>%
                          dplyr::select(
                            Date = DATE,
                            "n°station" = CODE_STATION,
                            Commune = commune,
                            Longitude = lon,
                            Latitude = lat,
                            Taxon = full_name,
                            Abondance = RESULTAT
                          ), file)
      shinybusy::remove_modal_spinner()
      shinyWidgets::show_alert(
        title = "Chargement terminé",
        text = "Vos données ont été téléchargées avec succès !",
        type = "success"
      )
    }
  )
  
  
  # Définir une palette de couleurs fixe
  
  
  output$Plot1 <- plotly::renderPlotly({
    
    req(length(input$taxons) <= 2)
    
    Code_Valid <- data() %>%
      dplyr::filter(full_name %in% input$taxons) %>%
      dplyr::pull(CodeValid) %>% unique()
    
    data() %>%
      dplyr::filter(full_name %in% Code_Valid) %>%
      dplyr::mutate(annee = as.factor(lubridate::year(DATE))) %>%
      dplyr::group_by(annee, full_name) %>%
      dplyr::summarise(Abondance_moyenne = mean(RESULTAT, na.rm = TRUE)) %>%
      
      plot_ly(x = ~annee, y = ~round(Abondance_moyenne, 0), color = ~str_sub(full_name, -6), type = "bar",
              colors = c("#66C1BF", "#423089")) %>%
      plotly::layout(
        title = "",
        xaxis = list(title = "", tickangle = 45, tickfont = list(size = 10)),
        yaxis = list(title = "", tickfont = list(size = 10)),
        barmode = "group",
        legend = list(orientation = "h", x = 0.5, y = -0.15),
        showlegend = length(input$taxons) > 1,
        font = list(size = 15)
      )
  
  })
  
  output$Plot2 <- plotly::renderPlotly({
    req(length(input$taxons) <= 2)
    
    Code_Valid <- data() %>%
      dplyr::filter(full_name %in% input$taxons) %>%
      dplyr::pull(CodeValid) %>% unique()

    data() %>%
      dplyr::filter(full_name %in% Code_Valid) %>%
      dplyr::mutate(annee = as.factor(lubridate::year(DATE))) %>%
      dplyr::group_by(annee, full_name) %>%
      dplyr::summarise(Occurence = dplyr::n()) %>%
      plot_ly(x = ~annee, y = ~Occurence, color = ~str_sub(full_name, -6), type = "bar",
              colors = c("#66C1BF", "#423089")) %>%
      # add_trace(position = "stack", hoverinfo = "text",
      #           text = ~paste("Espèce: ", str_sub(full_name, -6), "<br>Annee: ", annee, "<br>Occurence: ", Occurence),
      #           marker = list(color = ~str_sub(full_name, -6))) %>%
      plotly::layout(
        title = "",
        xaxis = list(title = "", tickangle = 45, tickfont = list(size = 10)),
        yaxis = list(title = "", tickfont = list(size = 10)),
        barmode = "group",
        legend = list(orientation = "h", x = 0.5, y = -0.15),
        showlegend = length(input$taxons) > 1,
        font = list(size = 15)
      )
  })
  
  
  mapFiltered <- shiny::reactive({
    shiny::req(input$taxons)
    
    Code_Valid <- data() %>%
      dplyr::filter(full_name %in% input$taxons) %>%
      dplyr::pull(CodeValid) %>% unique()
    
    leaflet_data <- data() %>%
      dplyr::filter(full_name %in% Code_Valid) %>%
      mutate(full_name = CodeValid) %>%
      dplyr::mutate(annee = as.character(lubridate::year(DATE))) %>%
      mutate(grp_color = str_sub(CodeValid, -6)) %>%
      dplyr::group_by(grp_color) %>%
      dplyr::mutate(label = dplyr::cur_group_id()) %>%
      dplyr::distinct() %>%
      dplyr::ungroup() %>%
      dplyr::mutate(Location = paste0(
        CODE_STATION, "_",
        lubridate::year(DATE), "_0",
        lubridate::month(DATE), "_0",
        lubridate::day(DATE)
      ),
      color = color_palette[label]) %>%
      ungroup()
    
    # points <- leaflet_data %>%
    #   sf::st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
    #   dplyr::select(taxon, geometry)
    
    polygones <- hydro %>% st_transform(geometry, crs = 4326)
    
    # points <- st_make_valid(points)
    polygones <- st_make_valid(polygones)
    
    # intersection_tab <- st_intersection(points, polygones) %>%
    #   dplyr::select(taxon, NomHER1) %>%
    #   group_by(NomHER1) %>%
    #   summarise(count = n()) %>%
    #   st_drop_geometry()
    # 
    # intersection_tab_final <- data.frame(hydro) %>% left_join(intersection_tab, by = "NomHER1") %>%
    #   st_as_sf() %>% mutate(count = ifelse(is.na(count) == TRUE, 0, count))
    map_base %>%
      leaflet::addCircleMarkers(
        data = leaflet_data,
        lng = ~long, 
        lat = ~lat,
        group = ~annee,
        color = ~color,
        fillOpacity = 1,
        weight = 1,
        radius = 5,
        # icon = leafIcons,
        popup = ~ paste(
          "1- Année: ", annee, "<br>",
          "2- Taxon: ", CodeValid, "<br>",
          "3- Commune: ", commune, "<br>",
          "4- Longitude: ", round(long, 2), "<br>",
          "5- Latitude: ", round(lat, 2), "<br>",
          "5- Abondance relative (‰):", round(RESULTAT, 2), "<br>"
        ), 
        # clusterOptions = leaflet::markerClusterOptions(removeOutsideVisibleBounds = F),
        labelOptions = leaflet::labelOptions(
          noHide = F,
          direction = "auto"
        )
      ) %>%
      
      addPolygons(data = 
                    polygones,
                  # intersection_tab_final,
                  label = 
                    # paste0(intersection_tab_final$NomHER1, " | Total: ",
                    #              intersection_tab_final$count),
                    polygones$NomHER1,
                  
                  color = "black",
                  fill = "lightgrey",
                  group = "Hydro écorégions",
                  labelOptions =  labelOptions(textsize = "15px")) %>%
      
      leaflet::addLayersControl(
        position = "topleft",
        baseGroups = c(
          "Fond satellite",
          "Fond clair",
          "Fond noir",
          "Open Street Map"
        ),
        overlayGroups = c(unique(leaflet_data$annee), "Hydro écorégions"),
        options = leaflet::layersControlOptions(collapsed = FALSE)
      ) %>%
      leaflet::hideGroup(group = unique(leaflet_data$annee)) %>%
      leaflet::setView(
        lng = 2,
        lat = 47,
        zoom = 6) %>%
      addControl(html = "<div id='custom-legend'>Référence Données: https://naiades.eaufrance.fr/ .</div>",
                 position = "bottomleft")
    
    
  })
  
  
  # mapFiltered2 <- shiny::reactive({
  #   
  #   shiny::req(input$taxons)
  #   
  #   Code_Valid <- data() %>% 
  #     dplyr::filter(full_name %in% input$taxons[1]) %>% 
  #     dplyr::pull(CodeValid) %>% unique()
  #   
  #   leaflet_data <- data() %>%
  #     dplyr::filter(full_name %in% Code_Valid) %>%
  #     mutate(grp = str_sub(full_name, -6)) %>%
  #     dplyr::group_by(grp) %>%
  #     dplyr::mutate(label = dplyr::cur_group_id()) %>%
  #     dplyr::distinct() %>%
  #     dplyr::mutate(annee = lubridate::year(DATE))
  #   
  #   years <- as.character(unique(leaflet_data$annee))
  #   
  #   map_base %>%
  #     
  #     addHeatmap(data = leaflet_data %>% dplyr::filter(annee == years[1]), lng = ~long, lat = ~lat, blur = 5, max = 0.5, radius = 25, group = years[1],
  #                gradient = brewer.pal(9, "YlGnBu")) %>%
  #     addHeatmap(data = leaflet_data %>% dplyr::filter(annee == years[2]), lng = ~long, lat = ~lat, blur = 5, max = 0.5, radius = 25, group = years[2],
  #                gradient = brewer.pal(9, "YlGnBu")) %>%
  #     addHeatmap(data = leaflet_data %>% dplyr::filter(annee == years[3]), lng = ~long, lat = ~lat, blur = 5, max = 0.5, radius = 25, group = years[3],
  #                gradient = brewer.pal(9, "YlGnBu")) %>%
  #     addHeatmap(data = leaflet_data %>% dplyr::filter(annee == years[4]), lng = ~long, lat = ~lat, blur = 5, max = 0.5, radius = 25, group = years[4],
  #                gradient = brewer.pal(9, "YlGnBu")) %>%
  #     addHeatmap(data = leaflet_data %>% dplyr::filter(annee == years[5]), lng = ~long, lat = ~lat, blur = 5, max = 0.5, radius = 25, group = years[5],
  #                gradient = brewer.pal(9, "YlGnBu")) %>%
  #     addHeatmap(data = leaflet_data %>% dplyr::filter(annee == years[6]), lng = ~long, lat = ~lat, blur = 5, max = 0.5, radius = 25, group = years[6],
  #                gradient = brewer.pal(9, "YlGnBu")) %>%
  #     addHeatmap(data = leaflet_data %>% dplyr::filter(annee == years[7]), lng = ~long, lat = ~lat, blur = 5, max = 0.5, radius = 25, group = years[7],
  #                gradient = brewer.pal(9, "YlGnBu")) %>%
  #     addHeatmap(data = leaflet_data %>% dplyr::filter(annee == years[8]), lng = ~long, lat = ~lat, blur = 5, max = 0.5, radius = 25, group = years[8],
  #                gradient = brewer.pal(9, "YlGnBu")) %>%
  #     addHeatmap(data = leaflet_data %>% dplyr::filter(annee == years[9]), lng = ~long, lat = ~lat, blur = 5, max = 0.5, radius = 25, group = years[9],
  #                gradient = brewer.pal(9, "YlGnBu")) %>%
  #     addHeatmap(data = leaflet_data %>% dplyr::filter(annee == years[10]), lng = ~long, lat = ~lat, blur = 5, max = 0.5, radius = 25, group = years[10],
  #                gradient = brewer.pal(9, "YlGnBu")) %>%
  #     addHeatmap(data = leaflet_data %>% dplyr::filter(annee == years[11]), lng = ~long, lat = ~lat, blur = 5, max = 0.5, radius = 25, group = years[11],
  #                gradient = brewer.pal(9, "YlGnBu")) %>%
  #     addHeatmap(data = leaflet_data %>% dplyr::filter(annee == years[12]), lng = ~long, lat = ~lat, blur = 5, max = 0.5, radius = 25, group = years[12],
  #                gradient = brewer.pal(9, "YlGnBu")) %>%
  #     addHeatmap(data = leaflet_data %>% dplyr::filter(annee == years[13]), lng = ~long, lat = ~lat, blur = 5, max = 0.5, radius = 25, group = years[13],
  #                gradient = brewer.pal(9, "YlGnBu")) %>%
  #     addHeatmap(data = leaflet_data %>% dplyr::filter(annee == years[14]), lng = ~long, lat = ~lat, blur = 5, max = 0.5, radius = 25, group = years[14],
  #                gradient = brewer.pal(9, "YlGnBu")) %>%
  #     addHeatmap(data = leaflet_data %>% dplyr::filter(annee == years[15]), lng = ~long, lat = ~lat, blur = 5, max = 0.5, radius = 25, group = years[15],
  #                gradient = brewer.pal(9, "YlGnBu")) %>%
  #     addHeatmap(data = leaflet_data %>% dplyr::filter(annee == years[16]), lng = ~long, lat = ~lat, blur = 5, max = 0.5, radius = 25, group = years[16],
  #                gradient = brewer.pal(9, "YlGnBu")) %>%
  #     addHeatmap(data = leaflet_data %>% dplyr::filter(annee == years[17]), lng = ~long, lat = ~lat, blur = 5, max = 0.5, radius = 25, group = years[17],
  #                gradient = brewer.pal(9, "YlGnBu")) %>%
  #     addHeatmap(data = leaflet_data %>% dplyr::filter(annee == years[18]), lng = ~long, lat = ~lat, blur = 5, max = 0.5, radius = 25, group = years[18],
  #                gradient = brewer.pal(9, "YlGnBu")) %>%
  #     addHeatmap(data = leaflet_data %>% dplyr::filter(annee == years[19]), lng = ~long, lat = ~lat, blur = 5, max = 0.5, radius = 25, group = years[19],
  #                gradient = brewer.pal(9, "YlGnBu")) %>%
  #     addHeatmap(data = leaflet_data %>% dplyr::filter(annee == years[20]), lng = ~long, lat = ~lat, blur = 5, max = 0.5, radius = 25, group = years[20],
  #                gradient = brewer.pal(9, "YlGnBu")) %>%
  #     addHeatmap(data = leaflet_data %>% dplyr::filter(annee == years[21]), lng = ~long, lat = ~lat, blur = 5, max = 0.5, radius = 25, group = years[21],
  #                gradient = brewer.pal(9, "YlGnBu")) %>%
  #     addHeatmap(data = leaflet_data %>% dplyr::filter(annee == years[22]), lng = ~long, lat = ~lat, blur = 5, max = 0.5, radius = 25, group = years[22],
  #                gradient = brewer.pal(9, "YlGnBu")) %>%
  #     addHeatmap(data = leaflet_data %>% dplyr::filter(annee == years[23]), lng = ~long, lat = ~lat, blur = 5, max = 0.5, radius = 25, group = years[23],
  #                gradient = brewer.pal(9, "YlGnBu")) %>%
  #     addHeatmap(data = leaflet_data %>% dplyr::filter(annee == years[24]), lng = ~long, lat = ~lat, blur = 5, max = 0.5, radius = 25, group = years[24],
  #                gradient = brewer.pal(9, "YlGnBu")) %>%
  #     addHeatmap(data = leaflet_data %>% dplyr::filter(annee == years[25]), lng = ~long, lat = ~lat, blur = 5, max = 0.5, radius = 25, group = years[25],
  #                gradient = brewer.pal(9, "YlGnBu")) %>%
  #     addHeatmap(data = leaflet_data %>% dplyr::filter(annee == years[26]), lng = ~long, lat = ~lat, blur = 5, max = 0.5, radius = 25, group = years[26],
  #                gradient = brewer.pal(9, "YlGnBu")) %>%
  #     addHeatmap(data = leaflet_data %>% dplyr::filter(annee == years[27]), lng = ~long, lat = ~lat, blur = 5, max = 0.5, radius = 25, group = years[27],
  #                gradient = brewer.pal(9, "YlGnBu")) %>%
  #     addHeatmap(data = leaflet_data %>% dplyr::filter(annee == years[28]), lng = ~long, lat = ~lat, blur = 5, max = 0.5, radius = 25, group = years[28],
  #                gradient = brewer.pal(9, "YlGnBu")) %>%
  #     addHeatmap(data = leaflet_data %>% dplyr::filter(annee == years[29]), lng = ~long, lat = ~lat, blur = 5, max = 0.5, radius = 25, group = years[29],
  #                gradient = brewer.pal(9, "YlGnBu")) %>%
  #     addHeatmap(data = leaflet_data %>% dplyr::filter(annee == years[30]), lng = ~long, lat = ~lat, blur = 5, max = 0.5, radius = 25, group = years[30],
  #                gradient = brewer.pal(9, "YlGnBu")) %>%
  #     addHeatmap(data = leaflet_data %>% dplyr::filter(annee == years[31]), lng = ~long, lat = ~lat, blur = 5, max = 0.5, radius = 25, group = years[31],
  #                gradient = brewer.pal(9, "YlGnBu")) %>%
  #     addHeatmap(data = leaflet_data %>% dplyr::filter(annee == years[32]), lng = ~long, lat = ~lat, blur = 5, max = 0.5, radius = 25, group = years[32],
  #                gradient = brewer.pal(9, "YlGnBu")) %>%
  #     addHeatmap(data = leaflet_data %>% dplyr::filter(annee == years[33]), lng = ~long, lat = ~lat, blur = 5, max = 0.5, radius = 25, group = years[33],
  #                gradient = brewer.pal(9, "YlGnBu")) %>%
  #     addHeatmap(data = leaflet_data %>% dplyr::filter(annee == years[34]), lng = ~long, lat = ~lat, blur = 5, max = 0.5, radius = 25, group = years[34],
  #                gradient = brewer.pal(9, "YlGnBu")) %>%
  #     addHeatmap(data = leaflet_data %>% dplyr::filter(annee == years[35]), lng = ~long, lat = ~lat, blur = 5, max = 0.5, radius = 25, group = years[35],
  #                gradient = brewer.pal(9, "YlGnBu")) %>%
  #     addHeatmap(data = leaflet_data %>% dplyr::filter(annee == years[36]), lng = ~long, lat = ~lat, blur = 5, max = 0.5, radius = 25, group = years[36],
  #                gradient = brewer.pal(9, "YlGnBu")) %>%
  #     addHeatmap(data = leaflet_data %>% dplyr::filter(annee == years[37]), lng = ~long, lat = ~lat, blur = 5, max = 0.5, radius = 25, group = years[37],
  #                gradient = brewer.pal(9, "YlGnBu")) %>%
  #     addHeatmap(data = leaflet_data %>% dplyr::filter(annee == years[38]), lng = ~long, lat = ~lat, blur = 5, max = 0.5, radius = 25, group = years[38],
  #                gradient = brewer.pal(9, "YlGnBu")) %>%
  #     addHeatmap(data = leaflet_data %>% dplyr::filter(annee == years[39]), lng = ~long, lat = ~lat, blur = 5, max = 0.5, radius = 25, group = years[39],
  #                gradient = brewer.pal(9, "YlGnBu")) %>%
  #     addHeatmap(data = leaflet_data %>% dplyr::filter(annee == years[40]), lng = ~long, lat = ~lat, blur = 5, max = 0.5, radius = 25, group = years[40],
  #                gradient = brewer.pal(9, "YlGnBu")) %>%
  #     addHeatmap(data = leaflet_data %>% dplyr::filter(annee == years[41]), lng = ~long, lat = ~lat, blur = 5, max = 0.5, radius = 25, group = years[41],
  #                gradient = brewer.pal(9, "YlGnBu")) %>%
  #     addHeatmap(data = leaflet_data %>% dplyr::filter(annee == years[42]), lng = ~long, lat = ~lat, blur = 5, max = 0.5, radius = 25, group = years[42],
  #                gradient = brewer.pal(9, "YlGnBu")) %>%
  #     addHeatmap(data = leaflet_data %>% dplyr::filter(annee == years[43]), lng = ~long, lat = ~lat, blur = 5, max = 0.5, radius = 25, group = years[43],
  #                gradient = brewer.pal(9, "YlGnBu")) %>%
  #     
  #     leaflet::addLayersControl(
  #       position = "topleft",
  #       baseGroups = c(
  #         "Fond satellite",
  #         "Fond clair",
  #         "Fond noir",
  #         "Open Street Map"
  #       ),
  #       overlayGroups = leaflet_data$annee,
  #       options = leaflet::layersControlOptions(collapsed = FALSE)) %>%
  #     leaflet::hideGroup(group = unique(leaflet_data$annee)) %>%
  #     leaflet::setView(
  #       lng = 2,
  #       lat = 47,
  #       zoom = 6)
  #   
  #   
  # })
  
  
  output$mapFiltered <- leaflet::renderLeaflet({
    mapFiltered()
  })
  
  # output$mapFiltered2 <- leaflet::renderLeaflet({
  #   mapFiltered2()
  # })
  
  output$Donnees2 <- DT::renderDataTable({
    
    shiny::req(input$taxons)
    
    Code_valid <- data() %>% 
      dplyr::filter(full_name %in% input$taxons) %>%
      dplyr::pull(CodeValid) %>% unique()
    
    DT::datatable(
      data() %>% 
        dplyr::filter(full_name %in% Code_valid) %>%
        dplyr::mutate(RESULTAT = round(RESULTAT, 2)) %>%
        dplyr::select(
          date = DATE, station = CODE_STATION, commune,
          taxon = CodeValid, Sandre = SANDRE,
          longitude = lon, latitude = lat,
          "ABONDANCE relative (‰)" = RESULTAT
        ),
      extensions = "Buttons",
      options = list(
        pageLength = 10,
        scroller = TRUE
      ),
      filter = "top", selection = "multiple", escape = FALSE,
      class = 'custom-table'
    )
  })
  
  
  # Combinaison des valeurs des colonnes optima, tolerance, range_min, range_max
  output$Trophie <- renderPlot({
    
    shiny::req(input$taxons)
    
    # num_taxons <- length(input$taxons)
    # 
    # if (length(input$taxons) == 1) {
    #   
    #   facet <- "parameter"
    #   x <- "variable"
    #   
    # } else {
    #   
    #   facet <- "variable"
    #   x <- "parameter"
    # 
    # }
    
    # num_taxons <- length(input$taxons)
    
    Code_Valid <- data() %>% 
      dplyr::filter(full_name %in% input$taxons) %>%
      dplyr::pull(CodeValid) %>% unique()
    
    data <- reshape2::melt(trophie %>% 
                             dplyr::filter(full_name %in% Code_Valid) %>%
                             rename(Optimum = optima, Tolerance = tolerance, Seuil_minimum = range_min, Seuil_maximum = range_max), 
                           id.vars = c("full_name", "parameter_full"), 
                           measure.vars = c("Optimum", "Tolerance", "Seuil_minimum", "Seuil_maximum"))  %>%
      group_by(parameter_full) %>%
      mutate(group = cur_group_id()) %>%
      ungroup() %>%
      mutate(code = str_sub(full_name, start = -5, end = -2))
    
    
    if (nrow(data) == 0) {
      showNotification(
        "Ce taxon ne possède pas de profil trophique défini d'après l'étude de Carayon et al 2019",
        type = "warning",
        duration = 20
      )
      return(NULL)
    }else{
      
      data2 <- data %>% group_by(full_name, parameter_full) %>%
        pivot_wider(names_from = variable, values_from = value) %>%
        rowwise() %>%
        mutate(Distribution = list(rtruncnorm(n = 500, a = 0, b = Inf, mean = Optimum, sd = Tolerance))) %>%
        ungroup()
      
      p <- data2 %>%
        unnest(Distribution) %>%
        ggplot(aes(x = code, y = Distribution, color = parameter_full, fill = full_name)) +
        geom_violinhalf() +
        facet_wrap(~ parameter_full, scales = "free", ncol = 4)+
        labs(x = "", y = "Valeurs") +
        theme_bw() +
        theme(
          panel.spacing = unit(2, "lines"),
          text = element_text(size = 12),
          strip.placement = "outside",
          strip.background = element_blank()
          # axis.text.x = element_text(angle = 45, hjust = 1)
        ) +
        scale_fill_manual(values = c("#9ED6E3", "#423089"))+
        scale_color_manual(values = rep("#000000", length(unique(data2$parameter_full))))+
        guides(color = FALSE)
      
      
      
      
      # ggplot(data, aes(x = code, y = value, fill = variable, group = group, text = variable)) +
      #   geom_violin() +
      #   # geom_line(color = "black", size = 0.1, aes(group = parameter_full)) +
      #   # scale_color_manual(values = c("Optimum" = "red", "Tolerance" = "blue", "Seuil_minimum" = "green", "Seuil_maximum" = "orange")) +
      #   labs(x = "", y = "Valeurs") +
      #   theme_bw() +
      #   theme(
      #     panel.spacing = unit(2, "lines"),
      #     text = element_text(size = 10),
      #     strip.placement = "outside",
      #     strip.background = element_blank()
      #     # axis.text.x = element_text(angle = 45, hjust = 1)
      #   ) +
      #   facet_wrap(~ parameter_full, scales = "free", ncol = 4)
      
    }
    
    
    # dplyr::filter(parameter %in% input$param) %>%
    # ggplot(aes(x = get(x), y = value, fill = str_sub(full_name, -6))) +
    # geom_bar(stat = "identity", position = "dodge") +
    # ggplot2::labs(
    #   title = "Profil Physico-chimique",
    #   fill = "Espèce"
    # ) +
    # ggplot2::scale_y_continuous(breaks = scales::pretty_breaks()) +
    # scale_fill_manual(values = c("blue", "red")) +
    # scale_alpha_manual(values = c(0.6, 0.6)) +
    # facet_wrap(~get(facet), scales = "free") +
    # theme_classic() +
    # theme(text = element_text(size = 20), 
    #       axis.title.x = element_blank(),
    #       axis.text.x = if (num_taxons == 1) ggplot2::element_text(angle = 45, vjust = 1, hjust = 1) else ggplot2::element_blank(),
    #       strip.text = if (num_taxons == 1) ggplot2::element_blank() else ggplot2::element_text(size = 12))
    
    grid.draw(shift_legend(p))
    
  }, height = 800, width = 800)
  
  
  output$Profil <- renderPlot({
    
    req(input$taxons)
    
    Code_Valid <- data() %>% 
      dplyr::filter(full_name %in% input$taxons) %>%
      dplyr::pull(CodeValid) %>% unique()
    
    tab <- profiles %>%
      dplyr::filter(full_name %in% Code_Valid)
    
    
    if (nrow(tab) == 0) {
      showNotification(
        "Ce taxon ne possède pas de profil écologique",
        type = "warning",
        duration = 20
      )
      return(NULL)}else{
        
        num_colors <- 7  # Nombre de couleurs dans le gradient
        
        colories <- colorRampPalette(c("red", "orange", "green"))(num_colors)
        
        p <- tab %>%
          pivot_longer(
            cols = starts_with("CL"),  
            names_to = "Classe",  
            values_to = "Valeur"  
          ) %>%
          mutate(Valeur = as.numeric(Valeur)) %>%
          ggplot(aes(x = factor(Classe), y = Valeur,
                     group = full_name)) +
          geom_point(size = 6, aes(shape = full_name, 
                                   color = factor(Classe))) +
          geom_smooth(se = FALSE, size = 0.5, method = "loess",span = 0.3, color = "black") +
          ggplot2::labs(
            title = "Probabilité de présence le long des 7 classes de qualité"
          ) +
          ggplot2::scale_y_continuous(breaks = scales::pretty_breaks()) +
          scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1), breaks = seq(0.1, 1, by = 0.1)) +
          scale_color_manual(values = colories,
                             name = "") +  # Utilisation d'une échelle de couleur discrète
          theme_classic() +
          theme(text = element_text(size = 20),
                legend.title = element_blank(),
                axis.title.x = element_blank(),
                axis.title.y = element_blank(),
                plot.margin = margin(t = 20, unit = "pt"))
        
      }
    
    p
    
  }, height = 600, width = 800)
  
  output$downloadData2 <- shiny::downloadHandler(
    
    filename = function() {
      paste(as.character(data() %>%
                           dplyr::filter(full_name %in% input$taxons[1]) %>%
                           unique() %>%
                           dplyr::pull(CodeValid) %>%
                           unique()),
            as.character(data() %>%
                           dplyr::filter(full_name %in% input$taxons[2]) %>%
                           unique() %>%
                           dplyr::pull(CodeValid) %>%
                           unique()),
            Sys.Date(), ".csv", sep = ".")
    },
    content = function(file) {
      shinybusy::show_modal_spinner(
        spin = "cube-grid",
        color = "#009999",
        text = "Téléchargement des données, cette opération peut prendre quelques minutes"
      )
      
      utils::write.csv2(data() %>% dplyr::filter(full_name %in% input$taxons) %>%
                          dplyr::mutate(full_name = CodeValid) %>%
                          dplyr::mutate(RESULTAT = round(RESULTAT, 2)) %>%
                          dplyr::select(
                            Date = DATE, Station = CODE_STATION, Commune = commune,
                            Taxon = CodeValid, Sandre = SANDRE,
                            Longitude = lon, Latitude = lat,
                            "ABONDANCE relative (‰)" = RESULTAT
                          ), file)
      
      shinybusy::remove_modal_spinner()
      
      shinyWidgets::show_alert(
        title = "Chargement terminé",
        text = "Les données du taxon ont été téléchargées avec succès !",
        type = "success"
      )
      
    }
    
  )
}

# Run the app ----
shiny::shinyApp(ui = ui, server = server)







