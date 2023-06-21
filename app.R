source("Global.R")

# prefixer()
ui <- shiny::fluidPage(
  shinyjs::useShinyjs(),
  theme = shinythemes::shinytheme("cerulean"),
  shiny::navbarPage(
    title = "IBD 2023",
    shiny::tabPanel(
      title = "Chargement des données",
      shiny::sidebarLayout(
        shiny::sidebarPanel(
          # actionButton("do", "Accès aux données NAIADES"),
          # fileInput(
          #   inputId = "upload",
          #   label = "",
          #   accept = c(".RData"),
          #   buttonLabel = "Charger",
          #   placeholder = "Insérer les données",
          #   multiple = FALSE
          # ),
          shiny::radioButtons(
            inputId = "radio",
            label = "Visualisation des données brutes",
            choices = c("Nothing Selected" = ""),
            selected = NULL
          ),
          # textOutput("radio"),
          width = 3
        ),
        shiny::mainPanel(
          "Données",
          DT::dataTableOutput("tab"),
          shiny::downloadButton("downloadData", "Download")
        )
      )
    ),
    shiny::tabPanel(
      "Carte",
      # conditionalPanel(
      # condition = "output.filesUploaded",
      shiny::sidebarLayout(
        shiny::sidebarPanel(
          shiny::selectInput("taxons", "Liste des taxons disponibles: ", ""),
          shiny::code(
            "Espèces comprises dans cette appelation: ",
            style = "font-size:15px;color:#00a3a6"
          ),
          shiny::p(
            shiny::textOutput("name_list"),
            style = "font-size:15px;color:black;"
          ),
          shiny::plotOutput("Plot1"),
          shiny::plotOutput("Plot2"),
          width = 4
        ),
        shiny::mainPanel(
          shiny::tabsetPanel(
            id = "tabs",
            shiny::tabPanel(
              "Chorologie",
              shiny::fluidRow(leaflet::leafletOutput("mapFiltered", width = "100%", height = "800px"))
            ),
            shiny::tabPanel(
              "Données",
              shiny::fluidRow(
                shiny::mainPanel(DT::dataTableOutput("Donnees2",
                  width = "100%"
                ), shiny::downloadButton("downloadData2", "Download"))
              )
            )
          )
        )
      )
      # )
    )
  )
)


server <- function(input, output, session) {
  library(openxlsx)
  library(lubridate)



  shiny::observe({
    if (is.null(input$upload)) {
      return()
    } else {
      var_example <- 1
    }
  })
  output$filesUploaded <- shiny::reactive({
    val <- !(is.null(input$upload))
    print(val)
  })

  shiny::outputOptions(output, "filesUploaded", suspendWhenHidden = FALSE)



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
      color = "#009999",
      text = "Chargement des données"
    )

    # load(input$upload$datapath)

    load("data/Donnees_compiles.RData")

    shiny::updateRadioButtons(
      session = session,
      inputId = "radio",
      choiceNames = seq(min(lubridate::year(Diatom$DATE)), max(lubridate::year(Diatom$DATE))),
      choiceValues = seq(min(lubridate::year(Diatom$DATE)), max(lubridate::year(Diatom$DATE)))
    )

    shinybusy::remove_modal_spinner()

    Diatom
  })


  selection_taxon <- shiny::reactive({
    # req(input$upload)

    data() %>%
      dplyr::select(full_name) %>%
      unique() %>%
      dplyr::arrange(full_name) %>%
      dplyr::pull(full_name)
  })

  output$name_list <- shiny::renderText({
    as.character(data() %>%
      dplyr::filter(full_name %in% input$taxons) %>%
      dplyr::select(taxons_apparies) %>%
      unique() %>%
      dplyr::mutate(taxons_apparies = dplyr::if_else(is.na(taxons_apparies) == T, "Aucun", taxons_apparies)))
  })

  shiny::observe({
    shiny::updateSelectInput(session, "taxons",
      choices = c("", selection_taxon())
    )
  })


  output$tab <- DT::renderDataTable({
    # req(input$upload)
    shiny::req(input$radio)


    DT::datatable(
      data() %>% dplyr::filter(lubridate::year(DATE) == input$radio) %>%
        dplyr::select(
          date = DATE,
          "n°station" = CODE_STATION,
          commune,
          longitude = lon,
          latitude = lat,
          taxon = full_name,
          Abondance = RESULTAT
        ),
      extensions = "Buttons",
      options = list(
        pageLength = 20,
        scroller = TRUE
      ),
      filter = "top", selection = "multiple", escape = FALSE
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
      shinybusy::show_modal_spinner()
      utils::write.csv2(data(), file)
      shinybusy::remove_modal_spinner()
    }
  )


  mapFiltered <- shiny::reactive({
    shiny::req(input$taxons)
    leaflet_data <- data() %>%
      dplyr::filter(full_name %in% input$taxons) %>%
      dplyr::mutate(annee = as.factor(lubridate::year(DATE))) %>%
      dplyr::group_by(full_name) %>%
      dplyr::mutate(label = dplyr::cur_group_id()) %>%
      dplyr::distinct() %>%
      dplyr::ungroup() %>%
      dplyr::mutate(Location = paste0(
        CODE_STATION, "_",
        lubridate::year(DATE), "_0",
        lubridate::month(DATE), "_0",
        lubridate::day(DATE)
      ))

    map_tax <- split(leaflet_data, leaflet_data$annee)


    l <- leaflet::leaflet() %>% leaflet::addTiles()

    names(map_tax) %>%
      purrr::walk(function(df) {
        l <<- l %>%
          leaflet::addMarkers(
            data = map_tax[[df]],
            lng = ~long, lat = ~lat,
            group = df,
            popup = ~ paste(
              "1- Année: ", annee, "<br>",
              "2- Taxon: ", full_name, "<br>",
              "3- Commune: ", commune, "<br>",
              "4- Longitude: ", round(long, 2), "<br>",
              "5- Latitude: ", round(lat, 2), "<br>",
              "5- Abondance relative pour 1000:", round(RESULTAT, 2), "<br>"
            ),
            clusterOptions = leaflet::markerClusterOptions(removeOutsideVisibleBounds = F),
            labelOptions = leaflet::labelOptions(
              noHide = F,
              direction = "auto"
            )
          )
      })

    maps <- l %>%
      leaflet::addLayersControl(
        position = "topleft",
        baseGroups = c(
          "Fond satellite",
          "Fond clair",
          "Fond noir",
          "Open Street Map"
        ),
        overlayGroups = names(map_tax),
        options = leaflet::layersControlOptions(collapsed = FALSE)
      ) %>%
      # addControl(title, position = "topleft", className="map-title") %>%
      leaflet::hideGroup(group = unique(leaflet_data$annee)) %>%
      leaflet::setView(
        lng = 2,
        lat = 47,
        zoom = 6
      ) %>%
      leaflet::addProviderTiles(providers$Esri.WorldGrayCanvas,
        group = "Fond clair"
      ) %>%
      leaflet::addProviderTiles(providers$CartoDB.DarkMatter,
        group = "Fond noir"
      ) %>%
      leaflet::addProviderTiles(providers$GeoportailFrance.orthos,
        group = "Fond satellite"
      ) %>%
      leaflet::addProviderTiles(providers$OpenStreetMap.Mapnik,
        group = "Open Street Map"
      )
  })

  output$mapFiltered <- leaflet::renderLeaflet({
    mapFiltered()
  })

  output$Plot1 <- shiny::renderPlot({
    data() %>%
      dplyr::filter(full_name %in% input$taxons) %>%
      dplyr::mutate(annee = as.factor(lubridate::year(DATE))) %>%
      dplyr::group_by(annee) %>%
      dplyr::summarise(Abondance_moyenne = mean(RESULTAT, na.rm = TRUE)) %>%
      ggplot2::ggplot(ggplot2::aes(y = Abondance_moyenne, x = annee, group = 1)) +
      ggplot2::geom_bar(stat = "identity") +
      ggplot2::labs(
        title = "Abondance relative moyenne",
        x = "Année", y = "Abondance relative ‰"
      ) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, vjust = 1, hjust = 1)) +
      ggplot2::scale_y_continuous(breaks = scales::pretty_breaks())
  })


  output$Plot2 <- shiny::renderPlot({
    data() %>%
      dplyr::filter(full_name %in% input$taxons) %>%
      dplyr::mutate(annee = as.factor(lubridate::year(DATE))) %>%
      dplyr::group_by(annee, taxon) %>%
      dplyr::summarise(Occurence = dplyr::n()) %>%
      ggplot2::ggplot(ggplot2::aes(y = Occurence, x = annee)) +
      ggplot2::geom_bar(stat = "identity") +
      ggplot2::labs(
        title = "Nombre de recensement",
        x = "Année", y = "Nombre de stations"
      ) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, vjust = 1, hjust = 1)) +
      ggplot2::scale_y_continuous(breaks = scales::pretty_breaks())
  })

  output$Donnees2 <- DT::renderDataTable({
    shiny::req(input$taxons)
    DT::datatable(
      data() %>%
        dplyr::filter(full_name %in% input$taxons) %>%
        dplyr::mutate(RESULTAT = round(RESULTAT, 2)) %>%
        dplyr::select(
          date = DATE, station = CODE_STATION, commune,
          taxon = full_name, Sandre = SANDRE,
          longitude = lon, latitude = lat,
          "ABONDANCE relative (‰)" = RESULTAT
        ),
      extensions = "Buttons",
      options = list(
        pageLength = 10,
        scroller = TRUE
      ),
      filter = "top", selection = "multiple", escape = FALSE
    )
  })

  output$downloadData2 <- shiny::downloadHandler(
    filename = function() {
      paste(as.character(data() %>%
        dplyr::filter(full_name %in% input$taxons) %>%
        unique() %>%
        dplyr::pull(full_name)), Sys.Date(), ".csv", sep = ".")
    },
    content = function(file) {
      utils::write.csv2(data() %>% dplyr::filter(full_name %in% input$taxons), file)
    }
  )
}

# Run the app ----
shiny::shinyApp(ui = ui, server = server)
