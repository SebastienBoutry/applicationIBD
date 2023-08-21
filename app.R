source("Global.R")

ui <- shiny::fluidPage(
  shiny::tags$head(
    shiny::tags$style(
      
      ".jhr{
      display: inline;
      vertical-align: middle;
      padding-left: 10px;
      }",
    
    ".navbar{margin-right:auto; margin-left:auto;}",
      
  ".custom-table {",
    "border: 1px solid black;",
    "margin-top: 40px; ",
  "}",

      ".custom-plot {",
      "  width: 800px;",
      # "  height: 400px;",
      "  margin-top: 40px;",  # Définir la marge supérieure personnalisée en pixels
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
        .navbar-default .navbar-nav > li > a {color:white;}
        .navbar-default .navbar-nav > .active > a,
        .navbar-default .navbar-nav > .active > a:focus,
        .navbar-default .navbar-nav > .active > a:hover {color: white;background-color: #66C1BF;font-size: 25px;}
        .navbar-default .navbar-nav > li > a:hover {color: white;background-color:#66C1BF;text-decoration:underline; font-size: 25px;}
        .navbar-default .navbar-nav > li > a[data-value='Données Brutes'] {color: white;background-color: #66C1BF;font-size: 25px;}
        .navbar-default .navbar-nav > li > a[data-value='Visualisation'] {color: white;background-color: #66C1BF;font-size: 25px;}
        .navbar-default .navbar-nav > li > a[data-value='Profiles'] {color: white;background-color: #66C1BF;font-size: 25px;}
        .nowrap {white-space: nowrap;}
        
    
")
    ),
  ),
  shinyjs::useShinyjs(),
  shiny.i18n::usei18n(i18n),
  shiny::div(
    id = "welcome_page",
    style = "position: fixed; top: 0; left: 0; width: 100%; height: 100%; background-color: #fff; z-index: 9999; text-align: center;",

    img(
      src = "VisualDiatoms.gif",
      style = "width: 800px; cursor: pointer; position: absolute; top: 50%; left: 50%; transform: translate(-50%, -50%);",
      onclick = "shinyjs.toggle('welcome_page'); shinyjs.toggle('app_content');"
    )
  ),
  
  
  shiny::div(
    id = "app_content",
    style = "display: none;",
    
    shiny::navbarPage(
      title = "",
      shiny::tabPanel(
        title = i18n$t("Welcoming page"),
        
        pickerInput(inputId = "selected_language",
                    label = i18n$t('Change language'),
                    choices = df$val,
                    choicesOpt = list(content = df$img),
                    selected = 'fr'),
        shiny::fluidRow(
          shiny::column(
            width = 12,
            
            shiny::div(
              class = "container",
              style = "display: flex; justify-content: center; align-items: center; margin-top: 2px; background-color: white; padding: 20px;",
              shiny::div(class = "container",
                         style = "display: flex; justify-content: center; align-items: center; margin-top: 15px; background-color: white; padding: 20px;",
                         div(
                           style = "margin-right: 20px;",
                           img(src = "LOGO.png", width = "400px")
                         )),
              shiny::div(
                style = "margin-right: 60px;",
                img(src = "EABX.png", width = "200px")
              ),
              shiny::div(
                style = "margin-right: 60px;",
                img(src = "OFB.png", width = "200px")
              ),
              shiny::div(
                style = "margin-right: 60px;",
                img(src = "logo_Aquaref.png", width = "200px")
              )
            ),
          
              shiny::div(
                style = "margin-right: 10px;",
                i18n$t("Main Author: Léonard Heinry, Co-Authors: Sebastien Boutry, Juliette Rosebery")
              ),

              shiny::div(
                style = "border: 2px solid #66C1BF; border-radius: 10px; padding: 10px; margin-top: 20px; margin-bottom: 10px; background-color: #f2f2f2;",
                shiny::h1(i18n$t("Welcome to the VisualDiatoms application!")),
                shiny::p(
                  class = "custom-text",
                i18n$t("This application allows you to visualize and compare diatom sampling data from the NAIADES public database. If it is your first time using the application, there is a user guide available below. Enjoy your visit!")
                )
              ),

              shiny::div(
                style = "border: 4px solid #66C1BF; border-radius: 10px; padding: 10px; margin-bottom: 10px; background-color: #f2f2f2;",
                shiny::h3(class = "custom-heading", "Introduction"),
                shiny::p(
                  class = "custom-text",
                  i18n$t("The goal of this application is to improve the understanding of diatom data by allowing users to visualize the taxa recorded in the NAIADES public database. Through the interactive interface, you can select, download, and even compare floristic sampling data collected by water agencies since 2007. Our application is divided into several tabs, making navigation simple and intuitive. \nTo ensure you fully enjoy the experience, the rest of this page provides a brief description of the features you'll find in the application.")
                )
              ),

              shiny::div(
                style = "border: 4px solid #66C1BF; border-radius: 10px; padding: 10px; margin-bottom: 10px; background-color: #f2f2f2;",
                shiny::h3(class = "custom-heading", i18n$t("Raw Data Tab")),
                shiny::p(
                  class = "custom-text",
                  i18n$t("By clicking on this tab, you have direct access to the latest version of the raw data extracted and formatted from NAIADES. Use the left panel to navigate between the years of sampling. Each table for a year is constructed in the same way, with columns that can be queried for those containing characters. Numeric columns can be filtered using a slider that appears when you click in the search bar of the column. Finally, if needed, you can download the data for a year in CSV format using the 'Download' button located at the bottom of each table. Please note that the volume of data is quite large and may take several seconds or a few minutes to load!"),
                )
              ),
              
              shiny::div(
                style = "border: 4px solid #66C1BF; border-radius: 10px; padding: 10px; margin-bottom: 10px; background-color: #f2f2f2;",
                shiny::h3(class = "custom-heading", i18n$t("Visualization Tab")),
                shiny::p(
                  class = "custom-text",
                  i18n$t("This tab consists of several elements. In the taxonomy tab, you will get the informations concerning the taxa included in your selection, as well as the actual valid appelation. The 'Chorology' panel allows you to visualize the data from the raw database with a focus on taxa. Select the taxa you want to display using the 'List of available taxa' dropdown. A map will then appear on which you can choose a year range and display the corresponding geographic locations of the selected taxa, according to its typology. You will also be able to display density of the taxa location depending on the year range you have selected. You can select up to two taxa. Each point on the map is clickable and will display the information of the sampling you are looking at. Hydro-ecoregions can also be displayed on the map. Finally, three plots can be shown using the 'Display Average Abundance', 'Display Occurrences' and 'Display Typology' buttons. They respectively show the evolution of the relative abundance (per 1000), the number of occurrences of the taxa in the surveys of the years in which they were observed and the percentage of sampled that were recorded in water body and in watercourse. When you select a second taxon, the graphical representations of each taxon (Map and Plots) are superimposed for comparison. On the plots, red crosses appear if two taxa are sampled in the same year.")
                )
              ),
              
              shiny::div(
                style = "border: 4px solid #66C1BF; border-radius: 10px; padding: 10px; margin-bottom: 10px; background-color: #f2f2f2;",
                shiny::h3(class = "custom-heading", i18n$t("Profile Sub-Tab")),
                shiny::p(
                  class = "custom-text",
                  i18n$t("This panel has two tabs: Trophic and Ecological")
                ),
                shiny::p(
                  class = "custom-text",
                  i18n$t("In the 'Trophic' tab, you can view information about the physico-chemical preferences of the taxa you have selected. This information was obtained through the work of David Carayon in 2019, who compiled a table of physico-chemical profiles for a large number of taxa. If a taxon is not present in David Carayon's database, a message will appear to indicate this. Parameter values also overlap if you select two taxa.")
                ),
                shiny::p(
                  class = "custom-text",
                  i18n$t("In the 'Ecological' tab, you can see if the selected taxon is an indicator of the Diatom Biological Index (IBD). If it is, the ecological profile of the selected taxon will be displayed on the screen; otherwise, a message will appear, as in the Trophic tab."
                ))
              ),
            
            shiny::div(
              style = "border: 4px solid #66C1BF; border-radius: 10px; padding: 10px; margin-bottom: 10px; background-color: #f2f2f2;",
              shiny::h3(class = "custom-heading", i18n$t("Data Sub-Tab")),
              shiny::p(
                class = "custom-text",
                i18n$t("In this panel, you can display and download the data of the taxa you have selected from the Chorology panel. Use the 'Download' button to download the data in CSV format.")
              )
            ),
                       
                       
                       shiny::div(
                         class = "container",
                         style = "display: flex; justify-content: center; align-items: center; margin-top: 25px; background-color: white; padding: 20px; border: 2px solid white;",
                         shiny::div(
                           style = "margin-right: 30px;",
                           
                           shiny::p(
                             class = "custom-text",
                             i18n$t("Bibliography")),
                           HTML("<p>Carayon, D., Tison-Rosebery, J., & Delmas, F., 2019. Defining a new autoecological trait matrix for French stream benthic diatoms. Ecological Indicators, 103, 650-658.</p>"),
                           
                           HTML("<p>Coste, M., Boutry, S., Tison-Rosebery, J., Delmas, F., 2009. Improvements of the Biological Diatom Index (IBD): description and efficiency of the new version (IBD-2006). Ecological Indicators, 9, 621–650. https://doi.org/10.1016/j.ecolind.2008.06.003.</p>"),
                           
                           uiOutput("link")
                           
                         )
            )
            
          )
        )
      ),
      shiny::tabPanel(
        title = i18n$t("Raw data"),
        shiny::sidebarLayout(
          shiny::sidebarPanel(
            shiny::radioButtons(
              inputId = "radio",
              label = i18n$t("Data raw visualization"),
              choices = c(i18n$t("Nothing Selected"), ""),
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
        title = i18n$t("Visualization"),
        shiny::sidebarLayout(
          shiny::sidebarPanel(
            shiny::selectizeInput("taxons", i18n$t("List of available taxa: "), "", multiple = TRUE,
                                  options = list(maxOptions = 10000)),
            
             
            shiny::checkboxInput("toggleMaps", i18n$t("Display Average Abundance")),
            shiny::checkboxInput("toggleMaps2", i18n$t("Display Occurrences")),
            shiny::checkboxInput("toggleMaps3", i18n$t("Display typology")),
            
          shinycssloaders::withSpinner(
            plotOutput("Plot1", width = "100%", height = "400px")
            , type = 1, id = "spinnerPlot1"),

          shinycssloaders::withSpinner(
            plotOutput("Plot2", width = "100%", height = "400px")
            , type = 1, id = "spinnerPlot2"),
          
          shinycssloaders::withSpinner(
            plotOutput("Circular_plot", width = "100%", height = "400px")
            , type = 1, id = "spinnerPlot3"),
          
           width = 4),
          shiny::mainPanel(
            shiny::tabsetPanel(
              id = "tabs",
              
              shiny::tabPanel(i18n$t("Taxonomy"),
                              
                              shiny::tags$style(HTML("
    .custom-div {
      margin-top: 10px;
      margin-bottom: 10px;
    }
  ")),
                              
                              shiny::div(
                                shiny::p(
                                  i18n$t("Species included in your selection: "),
                                  style = "font-size:20px;color:red;"
                                ),
                                class = "custom-div"
                              ),
                              
                              shiny::div(
                                shiny::p(
                                  shiny::textOutput("name_list"),
                                  style = "font-size:15px;color:black;"
                                ),
                                class = "custom-div"
                              ),
                              
                              shiny::div(
                                shiny::p(
                                  shiny::textOutput("name_list1"),
                                  style = "font-size:15px;color:black;"
                                ),
                                class = "custom-div"
                              ),
                              
                              shiny::div(
                                shiny::p(
                                  shiny::textOutput("name_list2"),
                                  style = "font-size:15px;color:black;"
                                ),
                                class = "custom-div"
                              ),
                              
                              shiny::div(
                                shiny::p(
                                  shiny::textOutput("name_list3"),
                                  style = "font-size:15px;color:black;"
                                ),
                                class = "custom-div"
                              )
                              
              ),
              
              shiny::tabPanel(
                i18n$t("Chorology"),
                dateRangeInput("date", i18n$t("Choose a year range:"),
                               start = as.Date("2007-01-01"),
                               end = as.Date("2007-01-01"),
                               format = "yyyy-mm-dd",
                               min = as.Date("2007-01-01")),
                fluidRow(
                  shinycssloaders::withSpinner(leafletOutput("mapFiltered", width = "100%", height = "800px"))
                )
              ),
              # shiny::tabPanel(
              # "Chorologie", 
              # shiny::checkboxInput("Animation1", "Carte de densité Taxon 1"),
              # shiny::checkboxInput("Animation2", "Carte de densité Taxon 2"),
              # leafletOutput("mapFiltered2", width = "100%", height = "800px"),
              # fluidRow(
              #   leafletOutput("mapFiltered3", width = "100%", height = "800px")
              # )
              # ),
              shiny::navbarMenu(
                i18n$t("Profile"),
                shiny::tabPanel(
                  i18n$t("Trophic"),
                  shiny::mainPanel(
                    shiny::div(class = "custom-plot",
                        shinycssloaders::withSpinner(shiny::plotOutput("Trophie", width = "100%")))
                  )
                ), 
                shiny::tabPanel(
                  i18n$t("Ecological"),
                  shiny::mainPanel(
                    shiny::tags$div(
                      shiny::h3(i18n$t("Information about the ecological profile")),
                      shiny::br(),
                      shiny::p(
                        i18n$t("The ecological profile of a taxon is defined based on a percentage probability of presence along a gradient of 7 classes of water quality. This gradient is based on a list of physico-chemical parameters including pH, conductivity, dissolved oxygen, biological oxygen demand, ammonium, orthophosphates, and nitrates. It is calibrated at the European scale."),
                        p(i18n$t("The graph below shows the ecological profile(s) of the taxon(s) you have selected."
                      )))
                    ),
                    shinycssloaders::withSpinner(shiny::plotOutput("Profil", width = "100%"))
                  )
                )
              ),
              shiny::tabPanel(
                title = i18n$t("Data"),
                    DT::dataTableOutput("Donnees2"),
                    shiny::downloadButton("downloadData2", "Download")
                  
              )
            )
          )
        )
      )
    )
  ))


server <- function(input, output, session) {
  
  observeEvent(input$selected_language, {
    update_lang(input$selected_language)
  })
  
  shinyjs::hide("app_content")
  
  shiny::observeEvent(input$welcome_page, {
    shinyjs::toggle("welcome_page")
    shinyjs::toggle("app_content")
    
  })

  shiny::observeEvent(input$taxons, {
    
    data_filtered <- data() %>% dplyr::filter(full_name %in% input$taxons)
    updateDateRangeInput(session, "date", 
                         start = min(data_filtered$DATE),
                         end = min(data_filtered$DATE),
                         min = min(data_filtered$DATE),
                         max = max(data_filtered$DATE))
  })
  
  url <- a("https://naiades.eaufrance.fr", href="https://naiades.eaufrance.fr", 
           style = "color:#423089;font-size:18px", target="_blank")
  output$link <- renderUI({
    tagList(i18n$t("Link to NAIADES:"), url)
  })
  
  shiny::observeEvent(input$toggleMaps, {
    if (input$toggleMaps) {
      shinyjs::show("Plot1")
      shinyjs::show("spinnerPlot1")
    } else {
      shinyjs::hide("Plot1")
      shinyjs::hide("spinnerPlot1")
    }
  })
  
  shiny::observeEvent(input$toggleMaps2, {
    if (input$toggleMaps2) {
      shinyjs::show("Plot2")
      shinyjs::show("spinnerPlot2")
    } else {
      shinyjs::hide("Plot2")
      shinyjs::hide("spinnerPlot2")
    }
  })
 
  shiny::observeEvent(input$toggleMaps3, {
    if (input$toggleMaps3) {
      shinyjs::show("Circular_plot")
      shinyjs::show("spinnerPlot3")
    } else {
      shinyjs::hide("Circular_plot")
      shinyjs::hide("spinnerPlot3")
    }
  })
  
  shiny::observeEvent(input$taxons, {
    shiny::updateCheckboxInput(session, "toggleMaps", value = FALSE)
    shiny::updateCheckboxInput(session, "toggleMaps2", value = FALSE)
    shiny::updateCheckboxInput(session, "toggleMaps3", value = FALSE)
  })
  
  shiny::observeEvent(input$taxons, {
      shinyjs::show("name_list")
      shinyjs::show("spinnerPlot4")
  })
  
  shiny::observeEvent(input$taxons, {
    if (length(input$taxons) == 2) {
      shinyjs::show("name_list2")
      shinyjs::show("spinnerPlot5")
    } else {
      shinyjs::hide("")
    }
  })

  # observeEvent(input$Animation1, {
  #   if (input$Animation1) {
  #     shinyjs::hide("mapFiltered")
  #     # shinyjs::hide("mapFiltered3")
  #     shinyjs::show("mapFiltered2")
  #   } else {
  #     shinyjs::hide("mapFiltered2")
  #     # shinyjs::hide("mapFiltered3")
  #     shinyjs::show("mapFiltered")
  #   }
  # })
  
  # observeEvent(input$Animation2, {
  #   if (input$Animation2) {
  #     shinyjs::hide("mapFiltered")
  #     shinyjs::hide("mapFiltered2")
  #     shinyjs::show("mapFiltered3")
  #   } else {
  #     shinyjs::hide("mapFiltered2")
  #     shinyjs::hide("mapFiltered3")
  #     shinyjs::show("mapFiltered")
  #   }
  # })
  
  # Observer pour surveiller les changements dans l'input taxons
  shiny::observe({
    # Obtenir le nombre d'éléments sélectionnés
    selected <- base::length(input$taxons)
    
    # Si plus de deux éléments sont sélectionnés, mettre à jour le selectInput pour n'afficher que les deux premiers éléments sélectionnés
    if (selected > 2) {
      shiny::updateSelectizeInput(session, "taxons", selected = input$taxons[1:2])
      
      shiny::showNotification(
        i18n$t("You can't compare more than two taxons"),
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
      text = " Chargement des données NAIADES | Loading NAIADES data | Cargando datos de NAIADES | Laden von NAIADES-Daten "
    )
    
    # load(input$upload$datapath)
    
    githubURL <- base::paste0("https://github.com/leolea12/NAIDESexport/raw/main/data_raw/", fichier_plus_recent)
    
    # tempfile <- tempfile()  # Crée un fichier temporaire pour stocker le fichier téléchargé
    # download.file(url = githubURL, destfile = tempfile, mode = "wb")  # Télécharge le fichier à partir de l'URL
    # 
    # load(tempfile)
    
    base::load(url(githubURL))
    # base::load("data/test.RData")
    # load("data/Diatom.RData")
    
    
    shiny::updateRadioButtons(
      session = session,
      inputId = "radio",
      choiceNames = seq(min(lubridate::year(Diatom$DATE)), max(lubridate::year(Diatom$DATE))),
      choiceValues = seq(min(lubridate::year(Diatom$DATE)), max(lubridate::year(Diatom$DATE)))
    )
    
    shinyWidgets::show_alert(
      title = "",
      text = " NAIADES data successfully loaded! | Les données NAIADES ont été chargées avec succès | ¡Datos de NAIADES cargados con éxito! | NAIADES-Daten erfolgreich geladen!",
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
      base::unique()
  })
  
  shiny::observe({
    shiny::updateSelectizeInput(session, "taxons",
                                choices = selection_taxon()
    )
  })
  
  output$name_list <- shiny::renderText({

    req(input$taxons)
    
    as.character(data() %>%
                   dplyr::filter(full_name %in% input$taxons[1]) %>%
                   dplyr::select(taxons_apparies, CodeValid) %>%
                   unique() %>%
                   dplyr::mutate(taxons_apparies = dplyr::if_else(taxons_apparies == "Aucun", base::paste0(i18n$t("For "),str_sub(input$taxons[1],  start = -5, end = -2),i18n$t(": None")),
                                                                  base::paste0(i18n$t("For "),str_sub(input$taxons[1],  start = -5, end = -2),i18n$t(": Taxa included = "), taxons_apparies))))[1]
  })
  
  output$name_list1 <- shiny::renderText({
    
    req(input$taxons)
    
    as.character(data() %>%
                   dplyr::filter(full_name %in% input$taxons[1]) %>%
                   dplyr::select(taxons_apparies, CodeValid) %>%
                   unique() %>%
                   dplyr::mutate(CodeValid = base::paste0(i18n$t("Valid Code = "),CodeValid)))[2]
  })
  
  output$name_list2 <- shiny::renderText({
    
    if(length(input$taxons) == 2){
    as.character(data() %>%
                     dplyr::filter(full_name %in% input$taxons[2]) %>%
                     dplyr::select(taxons_apparies, CodeValid) %>%
                     unique() %>%
                     dplyr::mutate(taxons_apparies = dplyr::if_else(taxons_apparies == "Aucun", base::paste0(i18n$t("For "),str_sub(input$taxons[2],  start = -5, end = -2),i18n$t(": None")), 
                                                                    base::paste0(i18n$t("For "), str_sub(input$taxons[2],  start = -5, end = -2),i18n$t(": Taxa included = "), taxons_apparies))))[1]
    }else{""}
    
  })
  
  output$name_list3 <- shiny::renderText({
    
    if(length(input$taxons) == 2){
      as.character(data() %>%
                     dplyr::filter(full_name %in% input$taxons[2]) %>%
                     dplyr::select(taxons_apparies, CodeValid) %>%
                     unique() %>%
                     dplyr::mutate(CodeValid = base::paste0(i18n$t("Valid Code = "),CodeValid)))[2]
    }else{""}
    
  })
  
  
  
  
  output$tab <- DT::renderDataTable({
    # req(input$upload)
    shiny::req(input$radio)
    
    langue <- i18n$get_key_translation()
    

    RESULTS <- i18n$t("Relative abundance (‰)")
    Commune <- i18n$t("Municipality")
    Latitude <- i18n$t("Latitude")
    Longitude <- i18n$t("Longitude")
    Station <-  i18n$t("Station")
    Date <- i18n$t("Date")
    taxon <- i18n$t("Taxa")
                             
    DT::datatable(
      data() %>% 
        dplyr::mutate(lon = round(long, 2), lat = round(lat, 2)) %>%
        distinct(taxon, CODE_STATION, DATE, .keep_all = TRUE) %>%
        dplyr::filter(lubridate::year(DATE) == input$radio) %>%
        mutate(lon = as.character(lon),
               lat = as.character(lat),
               DATE = as.character(DATE),
               full_name = as.factor(full_name),
               commune = as.factor(commune)) %>%
        dplyr::select(
          !!Date := DATE,
          !!Station := CODE_STATION,
          !!Commune := commune,
          !!Longitude := lon,
          !!Latitude := lat,
          !!taxon := full_name,
          !!RESULTS := RESULTAT
        ),
      extensions = "Buttons",
      options = list(
        pageLength = 10,
        autowidth = TRUE,
        columnDefs = list(
          list(className = "nowrap", targets = "_all"),
          list(targets = c(2,3,6,7), width = '150px')
        ),
        scrollX = TRUE
      ),
      filter = "top", escape = FALSE,
      class = 'custom-table'
    )
    
  })
  
  output$radio <- shiny::renderText({
    return(paste("RadioButtons :", input$radio))
  })
  
  
  output$downloadData <- shiny::downloadHandler(
    filename = function() {
      base::paste(i18n$t("Data for "), as.character(unique(lubridate::year(data()$DATE))), ".csv", sep = ".")
    },
    content = function(file) {
      shinybusy::show_modal_spinner(
        spin = "cube-grid",
        color = "#66C1BF",
        text = i18n$t("Downloading data may take a few minutes")
      )
      utils::write.csv2(data() %>% 
                          mutate(lon = round(lon, 2), lat = round(lat, 2)) %>%
                          distinct(taxon, CODE_STATION, DATE, .keep_all = TRUE) %>%
                          dplyr::filter(lubridate::year(DATE) == input$radio) %>%
                          dplyr::select(
                            Date = DATE,
                            "n°station" = CODE_STATION,
                            Municipality = commune,
                            Longitude = lon,
                            Latitude = lat,
                            Taxon = full_name,
                            Abundance = RESULTAT
                          ), file)
      shinybusy::remove_modal_spinner()
      shinyWidgets::show_alert(
        title = i18n$t("Loading successful"),
        text = i18n$t("Data successfully downloaded !"),
        type = "success"
      )
    }
  )
  
  
  # Définir une palette de couleurs fixe
  
  
  output$Plot1 <- shiny::renderPlot({
    
    req(input$toggleMaps)
    
    Code_Valid <- data() %>%
      dplyr::filter(full_name %in% input$taxons) %>%
      dplyr::pull(CodeValid) %>% base::unique()
    
    Code_Valid1 <- data() %>% 
      dplyr::filter(full_name %in% input$taxons[1]) %>%
      dplyr::pull(CodeValid) %>% base::unique()
    
    Code_Valid2 <- data() %>% 
      dplyr::filter(full_name %in% input$taxons[2]) %>%
      dplyr::pull(CodeValid) %>% base::unique()
    
    plot_data <- data() %>% 
      dplyr::filter(full_name %in% Code_Valid) %>%
      dplyr::mutate(
        annee = as.factor(lubridate::year(DATE)),
        Code = ifelse(full_name == Code_Valid1, "first", "second"),
        Code  = factor(Code, levels = c("first", "second"))) %>%
      dplyr::group_by(annee, full_name, Code) %>%
      dplyr::summarise(Abondance_moyenne = mean(RESULTAT, na.rm = TRUE)) %>%
      ungroup()
    
    levels_order <- c(Code_Valid1, Code_Valid2)
    
    plot_data %>%
      ggplot2::ggplot(aes(x = annee, y = Abondance_moyenne)) +
      ggplot2::geom_point(aes(color = full_name), size = 5) +
      ggplot2::geom_line(aes(group = full_name, color = full_name), linewidth = 3) +
      ggplot2::theme_classic() +
      ggplot2::scale_color_manual(values = c("#66C1BF", "#423089"), 
                                  labels = c(stringr::str_sub(Code_Valid1, -6), stringr::str_sub(Code_Valid2, -6)),
                                  breaks = levels_order) +
      labs(tag =ifelse(length(input$taxons) == 2 & Code_Valid1 != Code_Valid2,"* = both\nspecies","")) +
      ggplot2::theme(
        legend.text = element_text(size = 12),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
        plot.tag.position = c(.87,.4),
        plot.tag = element_text(hjust =0, size=12, color = "red")
      ) + 
      labs(
        y = i18n$t("Abundance")
      ) +
      ggplot2::guides(fill = guide_legend(title = NULL)) +
      ggplot2::geom_text(
        aes(label = ifelse(duplicated(annee), "*", ""), y = 0),
        color = "red",
        size = 8
      ) 

    
  })
  
  output$Plot2 <- shiny::renderPlot({
    
    req(input$toggleMaps2)
    
    Code_Valid <- data() %>%
      dplyr::filter(full_name %in% input$taxons) %>%
      dplyr::pull(CodeValid) %>% base::unique()
    
    Code_Valid1 <- data() %>% 
      dplyr::filter(full_name %in% input$taxons[1]) %>%
      dplyr::pull(CodeValid) %>% base::unique()
    
    Code_Valid2 <- data() %>% 
      dplyr::filter(full_name %in% input$taxons[2]) %>%
      dplyr::pull(CodeValid) %>% base::unique()
    
    plot_data2 <- data() %>%
      dplyr::filter(full_name %in% Code_Valid) %>%
      dplyr::mutate(
        annee = as.factor(lubridate::year(DATE)),
        Code = ifelse(full_name == Code_Valid1, "first", "second"),
        Code  = factor(Code, levels = c("first", "second"))) %>%
      dplyr::group_by(annee, full_name, Code) %>%
      dplyr::group_by(annee, full_name) %>%
      dplyr::summarise(Occurence = dplyr::n())
    
    levels_order <- c(Code_Valid1, Code_Valid2)
    
    
    plot_data2 %>%
      ggplot2::ggplot(aes(x = annee, y = Occurence)) +
      ggplot2::geom_point(aes(color = full_name), size = 5) +
      ggplot2::geom_line(aes(group = full_name, color = full_name), linewidth = 3) +
      ggplot2::theme_classic() +
      ggplot2::scale_color_manual(values = c("#66C1BF", "#423089"), 
                                  labels = c(stringr::str_sub(Code_Valid1, -6), stringr::str_sub(Code_Valid2, -6)),
                                  breaks = levels_order) +
      labs(tag =ifelse(length(input$taxons) == 2 & Code_Valid1 != Code_Valid2,"* = both\nspecies","")) +
      ggplot2::theme(
        legend.text = element_text(size = 12),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
        plot.tag.position = c(.87,.4),
        plot.tag = element_text(hjust =0, size=12, color = "red")
      ) + 
      labs(
        y = i18n$t("Occurrence")
      ) +
      ggplot2::guides(fill = guide_legend(title = NULL)) +
      ggplot2::geom_text(
        aes(label = ifelse(duplicated(annee), "*", ""), y = 0),
        color = "red",
        size = 8
      ) 
  })
  
  
  
  output$Circular_plot <- shiny::renderPlot({
    
    req(input$taxons)
    req(input$toggleMaps3)
    
    Code_Valid <- data() %>%
      dplyr::filter(full_name %in% input$taxons) %>%
      dplyr::pull(CodeValid) %>% base::unique()
    
    Circular_data <- data() %>%
      dplyr::filter(full_name %in% Code_Valid) %>%
      dplyr::select(full_name, type) %>%
      dplyr::group_by(full_name) %>%
      dplyr::summarise(
        "cours d'eau" = sum(type == "cours d'eau"),
        "plan d'eau" = sum(type == "plan d'eau")
      ) %>%
      ungroup() %>%
      pivot_longer(
        cols = c("cours d'eau", "plan d'eau"),
        names_to = "group",
        values_to = "values"
      ) %>%
      group_by(full_name) %>%
      mutate(percentage = ifelse(values > 0, values/sum(values) * 100, 0)) %>%
      ungroup()
    
    blank_theme <- theme_minimal()+
      theme(
        axis.text.x=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.border = element_blank(),
        panel.grid=element_blank(),
        axis.ticks = element_blank(),
        plot.title=element_text(size=14, face="bold"),
        strip.text = element_text(size = 15, face = "bold"),
        legend.text = element_text(size = 12)
      )

    pie_plot <- ggplot(Circular_data, aes(x = "", y = percentage, fill = group)) +
      geom_bar(width = 1, stat = "identity", color = "black") +
      
      ggplot2::scale_fill_manual(values = c("#3399FF", "#339900"), labels = c("cours d'eau", 
                                                                              "plan d'eau")) +
      coord_polar("y", start = 0) +
      blank_theme +
      geom_text(data = subset(Circular_data, percentage > 0),
                aes(label = paste(round(percentage, 1), "%")),
                position = position_stack(vjust = 0.5),
                size = 5) +
      facet_wrap(~str_sub(full_name, -6), ncol = 2) +
      labs(fill = "Typologie")
    
    pie_plot
    
  })
  
  mapFiltered <- shiny::reactive({
    shiny::req(input$taxons)
    
    Code_Valid <- data() %>%
      dplyr::filter(full_name %in% input$taxons) %>%
      dplyr::pull(CodeValid) %>% unique()
    
    Code_Valid1 <- data() %>% 
      dplyr::filter(full_name %in% input$taxons[1]) %>%
      dplyr::pull(CodeValid) %>% unique()
    
    Code_Valid2 <- data() %>% 
      dplyr::filter(full_name %in% input$taxons[2]) %>%
      dplyr::pull(CodeValid) %>% unique()
    
    leaflet_data <- data() %>%
      dplyr::filter(type == "cours d'eau") %>%
      dplyr::filter(full_name %in% Code_Valid) %>%
      dplyr::mutate(full_name = CodeValid) %>%
      dplyr::mutate(annee = as.character(lubridate::year(DATE))) %>%
      dplyr::mutate(grp_color = str_sub(CodeValid, -6)) %>%
      dplyr::group_by(grp_color) %>%
      dplyr::mutate(label = dplyr::cur_group_id()) %>%
      dplyr::distinct() %>%
      dplyr::ungroup() %>%
      dplyr::mutate(Location = paste0(
        CODE_STATION, "_",
        lubridate::year(DATE), "_0",
        lubridate::month(DATE), "_0",
        lubridate::day(DATE)
      )) %>%
      ungroup()
    
    leaflet_data2 <- data() %>%
      dplyr::filter(type == "plan d'eau") %>%
      dplyr::filter(full_name %in% Code_Valid) %>%
      dplyr::mutate(full_name = CodeValid) %>%
      dplyr::mutate(annee = as.character(lubridate::year(DATE))) %>%
      dplyr::mutate(grp_color = str_sub(CodeValid, -6)) %>%
      dplyr::group_by(grp_color) %>%
      dplyr::mutate(label = dplyr::cur_group_id()) %>%
      dplyr::distinct() %>%
      dplyr::ungroup() %>%
      dplyr::mutate(Location = paste0(
        CODE_STATION, "_",
        lubridate::year(DATE), "_0",
        lubridate::month(DATE), "_0",
        lubridate::day(DATE)
      )) %>%
      ungroup()
    
    leaflet_data <- leaflet_data %>% dplyr::filter(DATE >= input$date[1] & DATE <= input$date[2])
    leaflet_data2 <- leaflet_data2 %>% dplyr::filter(DATE >= input$date[1] & DATE <= input$date[2])

    
    # Fonction de correspondance pour les couleurs
    color_mapping <- function(value) {
      base::ifelse(value == Code_Valid1, "#66C1BF", "#423089")
    }
    
    
    # pal <- colorFactor(
    #   palette = c("#66C1BF", "#423089"),
    #   domain = unique(leaflet_data$CodeValid)
    # )
    
    # points <- leaflet_data %>%
    #   sf::st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
    #   dplyr::select(taxon, geometry)

    polygones <- hydro %>% sf::st_transform(geometry, crs = 4326)
    
    # points <- st_make_valid(points)
    polygones <- sf::st_make_valid(polygones)
    
    # intersection_tab <- st_intersection(points, polygones) %>%
    #   dplyr::select(taxon, NomHER1) %>%
    #   group_by(NomHER1) %>%
    #   summarise(count = n()) %>%
    #   st_drop_geometry()
    # 
    # intersection_tab_final <- data.frame(hydro) %>% left_join(intersection_tab, by = "NomHER1") %>%
    #   st_as_sf() %>% mutate(count = ifelse(is.na(count) == TRUE, 0, count))
    
    bins <- c(unique(polygones$CdHER1))
    pal <- colorBin("YlOrRd", domain = polygones$NomHER1, bins = bins)
    
    leaflet::leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
      leaflet::addProviderTiles(providers$Esri.WorldGrayCanvas,
                                group = i18n$t("White background")
      ) %>%
      leaflet::addProviderTiles(providers$CartoDB.DarkMatter,
                                group = i18n$t("Black background")
      ) %>%
      leaflet::addProviderTiles(providers$GeoportailFrance.orthos,
                                group = i18n$t("Satellite background")
      ) %>%
      leaflet::addProviderTiles("OpenStreetMap",
                                group = "Open Street Map"
      ) %>%
      htmlwidgets::onRender(
        "function(el, x) {
          L.control.zoom({position:''}).addTo(this);
        }") %>%
      leaflet::addCircleMarkers(
        data = leaflet_data,
        lng = ~long, 
        lat = ~lat,
        group = "cours d'eau",
        color = ~color_mapping(CodeValid),
        fillOpacity = 0.8,
        weight = 1,
        radius = 5,
        # icon = leafIcons,
        popup = ~ paste(
          i18n$t("1- Year: "), annee, "<br>",
          i18n$t("2- Taxa: "), CodeValid, "<br>",
                 i18n$t("3- Municipality: "), commune, "<br>",
          i18n$t("4- Longitude: "), round(long, 2), "<br>",
          i18n$t("5- Latitude: "), round(lat, 2), "<br>",
          i18n$t("6- Relative abundance (‰):"), round(RESULTAT, 2), "<br>"
        ), 
        # clusterOptions = leaflet::markerClusterOptions(removeOutsideVisibleBounds = F),
        labelOptions = leaflet::labelOptions(
          noHide = F,
          direction = "auto"
        )
      ) %>% 
      leaflet::addCircleMarkers(
        data = leaflet_data2,
        lng = ~long, 
        lat = ~lat,
        group = "plan d'eau",
        color = ~color_mapping(CodeValid),
        fillOpacity = 0.8,
        weight = 1,
        radius = 5,
        # icon = leafIcons,
        popup = ~ paste(
          i18n$t("1- Year: "), annee, "<br>",
          i18n$t("2- Taxa: "), CodeValid, "<br>",
          i18n$t("3- Municipality: "), commune, "<br>",
          i18n$t("4- Longitude: "), round(long, 2), "<br>",
          i18n$t("5- Latitude: "), round(lat, 2), "<br>",
          i18n$t("6- Relative abundance (‰):"), round(RESULTAT, 2), "<br>"
        ), 
        # clusterOptions = leaflet::markerClusterOptions(removeOutsideVisibleBounds = F),
        labelOptions = leaflet::labelOptions(
          noHide = F,
          direction = "auto"
        )
      ) %>%
       addHeatmap(
        data = leaflet_data %>% dplyr::filter(full_name %in% Code_Valid1),
        lng = ~long,
        lat = ~lat,
        group = paste0(i18n$t("density: "),Code_Valid1),
        blur = 15,     # Adjust the blur radius for smoothing the heatmap
        max = 0.6,     # Adjust the maximum intensity value
        radius = 10 # Adjust the radius of influence for each point
      ) %>%
      
      addHeatmap(
        data = leaflet_data %>% dplyr::filter(full_name %in% Code_Valid2),
        lng = ~long,
        lat = ~lat,
        group = ifelse(length(input$taxons) == 2, 
                       paste0(i18n$t("density: "),Code_Valid2),
                       i18n$t("Only one taxon selected")),
        blur = 15,     # Adjust the blur radius for smoothing the heatmap
        max = 0.6,     # Adjust the maximum intensity value
        radius = 10 # Adjust the radius of influence for each point
      ) %>%
      
      leaflet::addPolygons(
        data = polygones,
        label = polygones$NomHER1,
        color = "lightgrey",
        fillColor = 
          # "white",
          ~pal(CdHER1),
        dashArray = "3",
        fillOpacity = 0.1,
        group = i18n$t("Hydro ecoregions"),
        labelOptions = labelOptions(textsize = "15px"),
        highlightOptions = highlightOptions(
          weight = 5,
          color = "#666",
          # fillColor = ~pal(CdHER1),
          dashArray = "3",
          fillOpacity = 0.1,
          bringToFront = TRUE
        )
      ) %>%
      
      
      leaflet::addLayersControl(
        position = "topright",
        baseGroups = c(
          i18n$t("White background"),
          i18n$t("Black background"),
          i18n$t("Satellite background"),
          "Open Street Map"
        ),
        overlayGroups = c("cours d'eau", "plan d'eau",  
                          paste0(i18n$t("density: "),Code_Valid1), 
                          ifelse(length(input$taxons) == 2, 
                                 paste0(i18n$t("density: "),Code_Valid2),
                                 i18n$t("Only one taxon selected")),
                          i18n$t("Hydro ecoregions")),
        options = leaflet::layersControlOptions(collapsed = TRUE)
      ) %>%
      leaflet::hideGroup(group = c("cours d'eau", "plan d'eau", 
                                   paste0(i18n$t("density: "),Code_Valid1), 
                                   ifelse(length(input$taxons) == 2, 
                                          paste0(i18n$t("density: "),Code_Valid2),
                                          i18n$t("Only one taxon selected")),
                                   i18n$t("Hydro ecoregions"))) %>%
      leaflet::setView(
        lng = 2,
        lat = 47,
        zoom = 6) %>%
      leaflet::addControl(html = "<div id='custom-legend'> https://naiades.eaufrance.fr </div>",
                 position = "bottomleft")
    
    
  })
  
  
  # mapFiltered2 <- shiny::reactive({
  # 
  #   shiny::req(input$taxons)
  # 
  #   Code_Valid <- data() %>%
  #     dplyr::filter(full_name %in% input$taxons) %>%
  #     dplyr::pull(CodeValid) %>% unique()
  #   
  #   Code_Valid1 <- data() %>% 
  #     dplyr::filter(full_name %in% input$taxons[1]) %>%
  #     dplyr::pull(CodeValid) %>% unique()
  #   
  #   leaflet_data1 <- data() %>%
  #     dplyr::filter(full_name %in% Code_Valid1) %>%
  #     dplyr::mutate(full_name = CodeValid) %>%
  #     dplyr::mutate(annee = as.character(lubridate::year(DATE))) %>%
  #     dplyr::mutate(grp_color = str_sub(CodeValid, -6)) %>%
  #     dplyr::group_by(grp_color) %>%
  #     dplyr::mutate(label = dplyr::cur_group_id()) %>%
  #     dplyr::distinct() %>%
  #     dplyr::ungroup() %>%
  #     dplyr::mutate(Location = paste0(
  #       CODE_STATION, "_",
  #       lubridate::year(DATE), "_0",
  #       lubridate::month(DATE), "_0",
  #       lubridate::day(DATE)
  #     )) %>%
  #     ungroup()
  #   
  #   leaflet_data1 <- leaflet_data1[leaflet_data1$DATE >= input$date[1] & leaflet_data1$DATE <= input$date[2], ]
  # 
  #   # power1 <- data.frame(
  #   #   "Latitude" = leaflet_data1$lat,
  #   #   "Longitude" = leaflet_data1$long,
  #   #   "start" = leaflet_data1$DATE,
  #   #   "end" = leaflet_data1$DATE + 100
  #   # )
  #   # 
  #   # 
  #   # power_geo1 <- geojsonio::geojson_json(power1,lat="Latitude",lon="Longitude")
  # 
  #   map_base %>%
  #     addHeatmap(
  #       data = leaflet_data1,
  #       lng = ~long,
  #       lat = ~lat,
  #       group = unique(leaflet_data1$CodeValid),
  #       blur = 15,     # Adjust the blur radius for smoothing the heatmap
  #       max = 0.6,     # Adjust the maximum intensity value
  #       radius = 10 # Adjust the radius of influence for each point
  #     ) %>% 
  #     addCircleMarkers(data = leaflet_data1,
  #                      lng = ~long, 
  #                      lat = ~lat,
  #                popup = ~paste("Date: ", as.character(DATE), "<br>",
  #                               "Longitude: ", long, "<br>",
  #                               "Latitude: ", lat)) %>%
  #     # addTimeline(
  #     #   data = power_geo1,
  #     #   timelineOpts = timelineOptions(
  #     #     pointToLayer = htmlwidgets::JS(
  #     #       "
  #     #   function(data, latlng) {
  #     #     return L.circleMarker(latlng, {
  #     #       color: '#66C1BF',
  #     #       fillColor: '#66C1BF',
  #     #       radius: 1
  #     #     });
  #     #   }
  #     #   "
  #     #     ),
  #     #     style = NULL
  #     #   ),
  #     #   sliderOpts = sliderOptions(
  #     #     formatOutput = htmlwidgets::JS(
  #     #       "function(date) {return ''}"
  #     #     ),
  #     #     position = "bottomright",
  #     #     step = length(unique(leaflet_data1$annee)) + 100,
  #     #     duration = 10000,
  #     #     showTicks = TRUE
  #     #   ),
  #     #   width = "50%"
  #     # ) %>%
  #     leaflet::addLayersControl(
  #       position = "topleft",
  #       baseGroups = c(
  #         "Fond satellite",
  #         "Fond clair",
  #         "Fond noir",
  #         "Open Street Map"
  #       ),
  #       overlayGroups = c(unique(leaflet_data1$CodeValid)),
  #       options = leaflet::layersControlOptions(collapsed = FALSE)
  #     ) %>%
  #     leaflet::hideGroup(group = c(unique(leaflet_data1$CodeValid))) %>%
  #     leaflet::setView(
  #       lng = 2,
  #       lat = 47,
  #       zoom = 6)
  # 
  # 
  # })
  
  # mapFiltered3 <- shiny::reactive({
  #   
  #   shiny::req(input$taxons)
  #   shiny::req(length(input$taxons) == 2)
  #   
  #   Code_Valid <- data() %>%
  #     dplyr::filter(full_name %in% input$taxons) %>%
  #     dplyr::pull(CodeValid) %>% unique()
  #   
  #   Code_Valid2 <- data() %>% 
  #     dplyr::filter(full_name %in% input$taxons[2]) %>%
  #     dplyr::pull(CodeValid) %>% unique()
  #   
  #   leaflet_data2 <- data() %>%
  #     dplyr::filter(full_name %in% Code_Valid2) %>%
  #     dplyr::mutate(full_name = CodeValid) %>%
  #     dplyr::mutate(annee = as.character(lubridate::year(DATE))) %>%
  #     dplyr::mutate(grp_color = str_sub(CodeValid, -6)) %>%
  #     dplyr::group_by(grp_color) %>%
  #     dplyr::mutate(label = dplyr::cur_group_id()) %>%
  #     dplyr::distinct() %>%
  #     dplyr::ungroup() %>%
  #     dplyr::mutate(Location = paste0(
  #       CODE_STATION, "_",
  #       lubridate::year(DATE), "_0",
  #       lubridate::month(DATE), "_0",
  #       lubridate::day(DATE)
  #     )) %>%
  #     ungroup()
  #   
  #   power2 <- data.frame(
  #     "Latitude" = leaflet_data2$lat,
  #     "Longitude" = leaflet_data2$long,
  #     "start" = leaflet_data2$DATE,
  #     "end" = leaflet_data2$DATE + 100
  #   )
  #   
  #   
  #   power_geo2 <- geojsonio::geojson_json(power2,lat="Latitude",lon="Longitude")
  #   
  #   map_base %>%
  #     addHeatmap(
  #       data = leaflet_data2,
  #       lng = ~long,
  #       lat = ~lat,
  #       group = unique(leaflet_data2$CodeValid),
  #       blur = 15,     # Adjust the blur radius for smoothing the heatmap
  #       max = 0.6,     # Adjust the maximum intensity value
  #       radius = 10 # Adjust the radius of influence for each point
  #     ) %>%
  #     addTimeline(
  #       data = power_geo2,
  #       timelineOpts = timelineOptions(
  #         pointToLayer = htmlwidgets::JS(
  #           "
  #       function(data, latlng) {
  #         return L.circleMarker(latlng, {
  #           color: '#66C1BF',
  #           fillColor: '#66C1BF',
  #           radius: 1
  #         });
  #       }
  #       "
  #         ),
  #         style = NULL
  #       ),
  #       sliderOpts = sliderOptions(
  #         formatOutput = htmlwidgets::JS(
  #           "function(date) {return ''}"
  #         ),
  #         position = "bottomright",
  #         step = length(unique(leaflet_data2$annee)) + 100,
  #         duration = 10000,
  #         showTicks = TRUE
  #       ),
  #       width = "50%"
  #     ) %>%
  #     leaflet::addLayersControl(
  #       position = "topleft",
  #       baseGroups = c(
  #         "Fond satellite",
  #         "Fond clair",
  #         "Fond noir",
  #         "Open Street Map"
  #       ),
  #       overlayGroups = c(unique(leaflet_data2$CodeValid)),
  #       options = leaflet::layersControlOptions(collapsed = FALSE)
  #     ) %>%
  #     leaflet::hideGroup(group = c(unique(leaflet_data2$CodeValid))) %>%
  #     leaflet::setView(
  #       lng = 2,
  #       lat = 47,
  #       zoom = 6)
  #   
  # })
  
  
  output$mapFiltered <- leaflet::renderLeaflet({
    mapFiltered()
  })
  
  # output$mapFiltered2 <- leaflet::renderLeaflet({
  #   mapFiltered2()
  # })
  # 
  # output$mapFiltered3 <- leaflet::renderLeaflet({
  #   mapFiltered3()
  # })
  
  output$Donnees2 <- DT::renderDataTable({
    
    shiny::req(input$taxons)
    
    Code_valid <- data() %>% 
      dplyr::filter(full_name %in% input$taxons) %>%
      dplyr::pull(CodeValid) %>% unique()

      Commune <- i18n$t("Municipality")
      Abondance <- i18n$t("Relative abundance (‰)")
      Latitude <- i18n$t("Latitude")
      Longitude <- i18n$t("Longitude")
      Station <-  i18n$t("Station")
      Date <- i18n$t("Date")
      taxon <- i18n$t("Taxa")
      
    DT::datatable(
      data() %>% 
        dplyr::filter(full_name %in% Code_valid) %>%
        dplyr::mutate(RESULTAT = round(RESULTAT, 2),
                      lat = round(lat,2),
                      lon = round(lon,2)) %>%
        mutate(SANDRE = as.character(SANDRE),
               lon = as.character(lon),
               lat = as.character(lat),
               DATE = as.character(DATE),
               full_name = as.factor(full_name),
               commune = as.factor(commune)) %>%
        dplyr::select(
          !!Date := DATE, !!Station := CODE_STATION, !!Commune := commune,
          !!taxon := CodeValid, Sandre = SANDRE,
          !!Latitude := lat, !!Longitude := lon,
          !!Abondance := RESULTAT
        ),
      extensions = "Buttons",
      options = list(
        pageLength = 10,
        autowidth = TRUE,
        columnDefs = list(
          list(className = "nowrap", targets = "_all"),
          list(targets = c(2,3,6,7), width = '150px')
        ),
        scrollX = TRUE
      ),
      filter = "top",
      class = 'custom-table'
    )
  })
  
  
  # Combinaison des valeurs des colonnes optima, tolerance, range_min, range_max
  output$Trophie <- renderPlot({
    
    shiny::req(input$taxons)
    
    correspondance <- c(
      "COND" = i18n$t("Conductivity (μS/cm)"),
      "DBO5" = i18n$t("Oxygen demand (mg/L)"),
      "NO3" = i18n$t("Nitrates (mg/L)"),
      "PO4" = i18n$t("Phosphates (mg/L)"),
      "SAT" = i18n$t("Oxygen saturation (%)"),
      "NORG" = i18n$t("Organic nitrogen (mg/L)"),
      "PH" = "pH"
    )
    
    trophie <- trophie %>% dplyr::mutate(parameter_full = correspondance[parameter])
    
    Code_Valid <- data() %>% 
      dplyr::filter(full_name %in% input$taxons) %>%
      dplyr::pull(CodeValid) %>% base::unique()
    
    Code_Valid1 <- data() %>% 
      dplyr::filter(full_name %in% input$taxons[1]) %>%
      dplyr::pull(CodeValid) %>% base::unique()
    
    Code_Valid2 <- data() %>% 
      dplyr::filter(full_name %in% input$taxons[2]) %>%
      dplyr::pull(CodeValid) %>% base::unique()
    
    data <- reshape2::melt(trophie %>% 
                             dplyr::filter(full_name %in% Code_Valid) %>%
                             dplyr::rename(Optimum = optima, Tolerance = tolerance, Seuil_minimum = range_min, Seuil_maximum = range_max), 
                           id.vars = c("full_name", "parameter_full"), 
                           measure.vars = c("Optimum", "Tolerance", "Seuil_minimum", "Seuil_maximum"))  %>%
      dplyr::group_by(parameter_full) %>%
      dplyr::mutate(group = cur_group_id()) %>%
      ungroup() %>%
      dplyr::mutate(code = str_sub(full_name, start = -5, end = -2))
    
    
    if (nrow(data) == 0) {
      shiny::showNotification(
        i18n$t("This taxon does not have a defined trophic profile according to the Carayon et al 2019 study."),
        type = "warning",
        duration = 20
      )
      return(NULL)
    }else{
      
      data2 <- data %>% dplyr::group_by(full_name, parameter_full) %>%
        tidyr::pivot_wider(names_from = variable, values_from = value) %>%
        dplyr::rowwise() %>%
        dplyr::mutate(Distribution = list(rtruncnorm(n = 2000, a = 0, b = Inf, mean = Optimum, sd = Tolerance))) %>%
        ungroup()
    
      p <- data2 %>%
        tidyr::unnest(Distribution) %>%
        ggplot2::ggplot(aes(x = code, y = Distribution, color = parameter_full, fill = full_name)) +
        see::geom_violinhalf() +
        ggplot2::facet_wrap(~ parameter_full, scales = "free", ncol = 4)+
        ggplot2::labs(x = "", y = "") +
        ggplot2::theme_bw() +
        ggplot2::theme(
          panel.spacing = unit(2, "lines"),
          legend.text = element_text(size = 12),
          axis.text.x = element_text(size = 15),
          axis.text.y = element_text(size = 15),
          strip.placement = "outside",
          strip.background = element_blank(),
          strip.text = element_text(size = 15)
          # axis.text.x = element_text(angle = 45, hjust = 1)
        ) +
        ggplot2::scale_fill_manual(breaks = c(Code_Valid1, Code_Valid2),
          values = c("#66C1BF", "#423089"))+
        
        ggplot2::scale_color_manual(values = rep("#000000", length(unique(data2$parameter_full))))+
        ggplot2::guides(color = FALSE, fill = guide_legend(title = NULL))
      
      
      
      
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
    
  }, height = 800, width = 1000)
  
  
  output$Profil <- renderPlot({
  
    shiny::req(input$taxons)
    
    Code_Valid <- data() %>% 
      dplyr::filter(full_name %in% input$taxons) %>%
      dplyr::pull(CodeValid) %>% unique()
    
    Code_Valid1 <- data() %>% 
      dplyr::filter(full_name %in% input$taxons[1]) %>%
      dplyr::pull(CodeValid) %>% base::unique()
    
    Code_Valid2 <- data() %>% 
      dplyr::filter(full_name %in% input$taxons[2]) %>%
      dplyr::pull(CodeValid) %>% base::unique()
    
    levels_order <- c(Code_Valid1, Code_Valid2)
    
    tab <- profiles %>%
      dplyr::filter(full_name %in% Code_Valid)
    
    
    if (nrow(tab) == 0) {
      showNotification(
        i18n$t("This taxon does not have a defined ecological profile"),
        type = "warning",
        duration = 20
      )
      return(NULL)}else{
        
        # num_colors <- 7  # Nombre de couleurs dans le gradient
        # 
        # colories <- grDevices::colorRampPalette(c("red", "orange", "green"))(num_colors)
        
        p <- tab %>%
          tidyr::pivot_longer(
            cols = starts_with("CL"),  
            names_to = "Classe",  
            values_to = "Valeur"  
          ) %>%
          dplyr::mutate(Valeur = as.numeric(Valeur)) %>%
          ggplot2::ggplot(aes(x = factor(Classe), y = Valeur,
                     group = full_name, color = full_name)) +
          ggplot2::geom_point(size = 6, aes(shape = full_name)) +
          ggplot2::scale_color_manual(values = c("#66C1BF", "#423089"), labels = c(stringr::str_sub(Code_Valid1,-6), 
                                                                                  stringr::str_sub(Code_Valid2, -6)),
                                     breaks = levels_order) +
        
          ggplot2::geom_smooth(se = FALSE, size = 0.5, method = "loess",span = 0.3, color = "black") +
          ggplot2::labs(
            title = ""
          ) +
          ggplot2::scale_y_continuous(breaks = scales::pretty_breaks()) +
          ggplot2::scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1), breaks = seq(0.1, 1, by = 0.1)) +
          # ggplot2::scale_color_manual(values = colories,
          #                     name = "") +  # Utilisation d'une échelle de couleur discrète
          ggplot2::theme_classic() +
          ggplot2::theme(text = element_text(size = 20),
                legend.title = element_blank(),
                axis.title.x = element_blank(),
                axis.title.y = element_blank(),
                plot.margin = margin(t = 20, unit = "pt")) +
          ggplot2::guides(shape = "none")
        
      }
    
    p
    
  }, height = 600, width = 800)
  
  output$downloadData2 <- shiny::downloadHandler(
    
    Abondance <- i18n$t("Relative ABUNDANCE (‰)"),
    
    filename = function() {
      base::paste(as.character(data() %>%
                           dplyr::filter(full_name %in% input$taxons[1]) %>%
                             base::unique() %>%
                           dplyr::pull(CodeValid) %>%
                             base::unique()),
            as.character(data() %>%
                           dplyr::filter(full_name %in% input$taxons[2]) %>%
                           base::unique() %>%
                           dplyr::pull(CodeValid) %>%
                           base::unique()),
            Sys.Date(), ".csv", sep = ".")
    },
    content = function(file) {
      shinybusy::show_modal_spinner(
        spin = "cube-grid",
        color = "#009999",
        text = i18n$t("Downloading data may take a few minutes")
      )
      
      utils::write.csv2(data() %>% dplyr::filter(full_name %in% input$taxons) %>%
                          dplyr::mutate(full_name = CodeValid) %>%
                          dplyr::mutate(RESULTAT = round(RESULTAT, 2)) %>%
                          dplyr::select(
                            Date = DATE, Station = CODE_STATION, Commune = commune,
                            Taxon = CodeValid, Sandre = SANDRE,
                            Longitude = lon, Latitude = lat,
                            Abondance = RESULTAT
                          ), file)
      
      shinybusy::remove_modal_spinner()
      
      shinyWidgets::show_alert(
        title = i18n$t("Loading successful"),
        text = i18n$t("Data successfully downloaded !"),
        type = "success"
      )
      
    }
    
  )
}

# Run the app ----
shiny::shinyApp(ui = ui, server = server)







