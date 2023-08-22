# Chargement du fichier source contenant les packages et les fonctions 

source("Global.R")

# ---------------------- Mise en page générale de l'application (partie UI)

ui <- shiny::fluidPage(
  shiny::tags$head(
    shiny::tags$style(
      ".jhr{
      display: inline;
      vertical-align: middle;
      padding-left: 10px;
      }",
      ".navbar{margin-right:auto; margin-left:auto;}",

      # Customisation des tableaux et plots

      ".custom-table {",
      "border: 1px solid black;",
      "margin-top: 40px; ",
      "}",
      ".custom-plot {",
      "  width: 800px;",
      "  margin-top: 40px;",
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

      # Mise en page HTML

      shiny::HTML("
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
        .nowrap {white-space: nowrap;}")
    ),
  ),
  shinyjs::useShinyjs(),
  shiny.i18n::usei18n(i18n),
  
  # Page d'Accueil
  
  shiny::div(
    id = "welcome_page",
    style = "position: fixed; top: 0; left: 0; width: 100%; height: 100%; background-color: #fff; z-index: 9999; text-align: center;",
    shiny::img(
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
        shinyWidgets::pickerInput(
          inputId = "selected_language",
          label = i18n$t("Change language"),
          choices = df$val,
          choicesOpt = list(content = df$img),
          selected = "fr"
        ),
        shiny::fluidRow(
          shiny::column(
            width = 12,
            shiny::div(
              class = "container",
              style = "display: flex; justify-content: center; align-items: center; margin-top: 2px; background-color: white; padding: 20px;",
              shiny::div(
                class = "container",
                style = "display: flex; justify-content: center; align-items: center; margin-top: 15px; background-color: white; padding: 20px;",
                shiny::div(
                  style = "margin-right: 20px;",
                  shiny::img(src = "LOGO.png", width = "400px")
                )
              ),
              shiny::div(
                style = "margin-right: 60px;",
                shiny::img(src = "EABX.png", width = "200px")
              ),
              shiny::div(
                style = "margin-right: 60px;",
                shiny::img(src = "OFB.png", width = "200px")
              ),
              shiny::div(
                style = "margin-right: 60px;",
                shiny::img(src = "logo_Aquaref.png", width = "200px")
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
                i18n$t("In the 'Ecological' tab, you can see if the selected taxon is an indicator of the Diatom Biological Index (IBD). If it is, the ecological profile of the selected taxon will be displayed on the screen; otherwise, a message will appear, as in the Trophic tab.")
              )
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
                  i18n$t("Bibliography")
                ),
                shiny::HTML("<p>Carayon, D., Tison-Rosebery, J., & Delmas, F., 2019. Defining a new autoecological trait matrix for French stream benthic diatoms. Ecological Indicators, 103, 650-658.</p>"),
                shiny::HTML("<p>Coste, M., Boutry, S., Tison-Rosebery, J., Delmas, F., 2009. Improvements of the Biological Diatom Index (IBD): description and efficiency of the new version (IBD-2006). Ecological Indicators, 9, 621–650. https://doi.org/10.1016/j.ecolind.2008.06.003.</p>"),
                shiny::uiOutput("link")
              )
            )
          )
        )
      ),
      
      # Panneau Données Brutes 
      
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
      
      # Panneau Visualisation
      
      shiny::tabPanel(
        title = i18n$t("Visualization"),
        shiny::sidebarLayout(
          shiny::sidebarPanel(
            shiny::selectizeInput("taxons", i18n$t("List of available taxa: "), "",
              multiple = TRUE,
              options = list(maxOptions = 10000)
            ),
            shiny::checkboxInput("toggleMaps", i18n$t("Display Average Abundance")),
            shiny::checkboxInput("toggleMaps2", i18n$t("Display Occurrences")),
            shiny::checkboxInput("toggleMaps3", i18n$t("Display typology")),
            shinycssloaders::withSpinner(
              shiny::plotOutput("Plot1", width = "100%", height = "400px"),
              type = 1, id = "spinnerPlot1"
            ),
            shinycssloaders::withSpinner(
              shiny::plotOutput("Plot2", width = "100%", height = "400px"),
              type = 1, id = "spinnerPlot2"
            ),
            shinycssloaders::withSpinner(
              shiny::plotOutput("Circular_plot", width = "100%", height = "400px"),
              type = 1, id = "spinnerPlot3"
            ),
            width = 4
          ),
          shiny::mainPanel(
            shiny::tabsetPanel(
              id = "tabs",
              shiny::tabPanel(
                i18n$t("Taxonomy"),
                shiny::tags$style(shiny::HTML("
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
              
              # Panneau Chorologie 
              
              shiny::tabPanel(
                i18n$t("Chorology"),
                shiny::dateRangeInput("date", i18n$t("Choose a year range:"),
                  start = as.Date("2007-01-01"),
                  end = as.Date("2007-01-01"),
                  format = "yyyy-mm-dd",
                  min = as.Date("2007-01-01")
                ),
                shiny::fluidRow(
                  shinycssloaders::withSpinner(leaflet::leafletOutput("mapFiltered", width = "100%", height = "800px"))
                )
              ),
              shiny::navbarMenu(
                i18n$t("Profile"),
                shiny::tabPanel(
                  i18n$t("Trophic"),
                  shiny::mainPanel(
                    shiny::div(
                      class = "custom-plot",
                      shinycssloaders::withSpinner(shiny::plotOutput("Trophie", width = "100%"))
                    )
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
                        shiny::p(i18n$t("The graph below shows the ecological profile(s) of the taxon(s) you have selected."))
                      )
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
  )
)


# ---------------------- Fonctionnalités de l'application et éléments réactifs (partie serveur)


server <- function(input, output, session) {
  shiny::observeEvent(input$selected_language, {
    shiny.i18n::update_lang(input$selected_language)
  })

  shinyjs::hide("app_content")

  shiny::observeEvent(input$welcome_page, {
    shinyjs::toggle("welcome_page")
    shinyjs::toggle("app_content")
  })

  shiny::observeEvent(input$taxons, {
    data_filtered <- data() %>% dplyr::filter(full_name %in% input$taxons)
    shiny::updateDateRangeInput(session, "date",
      start = min(data_filtered$DATE),
      end = min(data_filtered$DATE),
      min = min(data_filtered$DATE),
      max = max(data_filtered$DATE)
    )
  })

  url <- a("https://naiades.eaufrance.fr",
    href = "https://naiades.eaufrance.fr",
    style = "color:#423089;font-size:18px", target = "_blank"
  )
  output$link <- shiny::renderUI({
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


  shiny::observe({
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


  # Chargement des données 

  data <- shiny::reactive({
    shinybusy::show_modal_spinner(
      spin = "cube-grid",
      color = "#66C1BF",
      text = " Chargement des données NAIADES | Loading NAIADES data | Cargando datos de NAIADES | Laden von NAIADES-Daten "
    )

    githubURL <- base::paste0("https://github.com/leolea12/NAIDESexport/raw/main/data_raw/", fichier_plus_recent)

    base::load(url(githubURL))
    # base::load("data/test.RData")


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

  # Listes taxonomiques
  
  output$name_list <- shiny::renderText({
    shiny::req(input$taxons)

    as.character(data() %>%
      dplyr::filter(full_name %in% input$taxons[1]) %>%
      dplyr::select(taxons_apparies, CodeValid) %>%
      unique() %>%
      dplyr::mutate(taxons_apparies = dplyr::if_else(taxons_apparies == "Aucun", base::paste0(i18n$t("For "), stringr::str_sub(input$taxons[1], start = -5, end = -2), i18n$t(": None")),
        base::paste0(i18n$t("For "), stringr::str_sub(input$taxons[1], start = -5, end = -2), i18n$t(": Taxa included = "), taxons_apparies)
      )))[1]
  })

  output$name_list1 <- shiny::renderText({
    shiny::req(input$taxons)

    as.character(data() %>%
      dplyr::filter(full_name %in% input$taxons[1]) %>%
      dplyr::select(taxons_apparies, CodeValid) %>%
      unique() %>%
      dplyr::mutate(CodeValid = base::paste0(i18n$t("Valid Code = "), CodeValid)))[2]
  })

  output$name_list2 <- shiny::renderText({
    if (length(input$taxons) == 2) {
      as.character(data() %>%
        dplyr::filter(full_name %in% input$taxons[2]) %>%
        dplyr::select(taxons_apparies, CodeValid) %>%
        unique() %>%
        dplyr::mutate(taxons_apparies = dplyr::if_else(taxons_apparies == "Aucun", base::paste0(i18n$t("For "), stringr::str_sub(input$taxons[2], start = -5, end = -2), i18n$t(": None")),
          base::paste0(i18n$t("For "), stringr::str_sub(input$taxons[2], start = -5, end = -2), i18n$t(": Taxa included = "), taxons_apparies)
        )))[1]
    } else {
      ""
    }
  })

  output$name_list3 <- shiny::renderText({
    if (length(input$taxons) == 2) {
      as.character(data() %>%
        dplyr::filter(full_name %in% input$taxons[2]) %>%
        dplyr::select(taxons_apparies, CodeValid) %>%
        unique() %>%
        dplyr::mutate(CodeValid = base::paste0(i18n$t("Valid Code = "), CodeValid)))[2]
    } else {
      ""
    }
  })



  # Tableau des données Brutes 
  
  output$tab <- DT::renderDataTable({
    # req(input$upload)
    shiny::req(input$radio)

    langue <- i18n$get_key_translation()


    RESULTS <- i18n$t("Relative abundance (‰)")
    Commune <- i18n$t("Municipality")
    Latitude <- i18n$t("Latitude")
    Longitude <- i18n$t("Longitude")
    Station <- i18n$t("Station")
    Date <- i18n$t("Date")
    taxon <- i18n$t("Taxa")

    DT::datatable(
      data() %>%
        dplyr::mutate(lon = round(long, 2), lat = round(lat, 2)) %>%
        dplyr::distinct(taxon, CODE_STATION, DATE, .keep_all = TRUE) %>%
        dplyr::filter(lubridate::year(DATE) == input$radio) %>%
        dplyr::mutate(
          lon = as.character(lon),
          lat = as.character(lat),
          DATE = as.character(DATE),
          full_name = as.factor(full_name),
          commune = as.factor(commune)
        ) %>%
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
          list(targets = c(2, 3, 6, 7), width = "150px")
        ),
        scrollX = TRUE
      ),
      filter = "top", escape = FALSE,
      class = "custom-table"
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
        dplyr::mutate(lon = round(lon, 2), lat = round(lat, 2)) %>%
        dplyr::distinct(taxon, CODE_STATION, DATE, .keep_all = TRUE) %>%
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

  # Plots des Abondances
  
  output$Plot1 <- shiny::renderPlot({
    shiny::req(input$toggleMaps)

    Code_Valid <- data() %>%
      dplyr::filter(full_name %in% input$taxons) %>%
      dplyr::pull(CodeValid) %>%
      base::unique()

    Code_Valid1 <- data() %>%
      dplyr::filter(full_name %in% input$taxons[1]) %>%
      dplyr::pull(CodeValid) %>%
      base::unique()

    Code_Valid2 <- data() %>%
      dplyr::filter(full_name %in% input$taxons[2]) %>%
      dplyr::pull(CodeValid) %>%
      base::unique()

    plot_data <- data() %>%
      dplyr::filter(full_name %in% Code_Valid) %>%
      dplyr::mutate(
        annee = as.factor(lubridate::year(DATE)),
        Code = ifelse(full_name == Code_Valid1, "first", "second"),
        Code = factor(Code, levels = c("first", "second"))
      ) %>%
      dplyr::group_by(annee, full_name, Code) %>%
      dplyr::summarise(Abondance_moyenne = mean(RESULTAT, na.rm = TRUE)) %>%
      dplyr::ungroup()

    levels_order <- c(Code_Valid1, Code_Valid2)

    plot_data %>%
      ggplot2::ggplot(ggplot2::aes(x = annee, y = Abondance_moyenne)) +
      ggplot2::geom_point(ggplot2::aes(color = full_name), size = 5) +
      ggplot2::geom_line(ggplot2::aes(group = full_name, color = full_name), linewidth = 3) +
      ggplot2::theme_classic() +
      ggplot2::scale_color_manual(
        values = c("#66C1BF", "#423089"),
        labels = c(stringr::str_sub(Code_Valid1, -6), stringr::str_sub(Code_Valid2, -6)),
        breaks = levels_order
      ) +
      ggplot2::labs(tag = ifelse(length(input$taxons) == 2 & Code_Valid1 != Code_Valid2, "* = both\nspecies", "")) +
      ggplot2::theme(
        legend.text = ggplot2::element_text(size = 12),
        axis.title.x = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_text(size = 12),
        axis.text.x = ggplot2::element_text(size = 12, angle = 45, hjust = 1),
        plot.tag.position = c(.87, .4),
        plot.tag = ggplot2::element_text(hjust = 0, size = 12, color = "red")
      ) +
      ggplot2::labs(
        y = i18n$t("Abundance")
      ) +
      ggplot2::guides(fill = ggplot2::guide_legend(title = NULL)) +
      ggplot2::geom_text(
        ggplot2::aes(label = ifelse(duplicated(annee), "*", ""), y = 0),
        color = "red",
        size = 8
      )
  })

  
  # Plots des Occurences
  
  output$Plot2 <- shiny::renderPlot({
    shiny::req(input$toggleMaps2)

    Code_Valid <- data() %>%
      dplyr::filter(full_name %in% input$taxons) %>%
      dplyr::pull(CodeValid) %>%
      base::unique()

    Code_Valid1 <- data() %>%
      dplyr::filter(full_name %in% input$taxons[1]) %>%
      dplyr::pull(CodeValid) %>%
      base::unique()

    Code_Valid2 <- data() %>%
      dplyr::filter(full_name %in% input$taxons[2]) %>%
      dplyr::pull(CodeValid) %>%
      base::unique()

    plot_data2 <- data() %>%
      dplyr::filter(full_name %in% Code_Valid) %>%
      dplyr::mutate(
        annee = as.factor(lubridate::year(DATE)),
        Code = ifelse(full_name == Code_Valid1, "first", "second"),
        Code = factor(Code, levels = c("first", "second"))
      ) %>%
      dplyr::group_by(annee, full_name, Code) %>%
      dplyr::group_by(annee, full_name) %>%
      dplyr::summarise(Occurence = dplyr::n())

    levels_order <- c(Code_Valid1, Code_Valid2)


    plot_data2 %>%
      ggplot2::ggplot(ggplot2::aes(x = annee, y = Occurence)) +
      ggplot2::geom_point(ggplot2::aes(color = full_name), size = 5) +
      ggplot2::geom_line(ggplot2::aes(group = full_name, color = full_name), linewidth = 3) +
      ggplot2::theme_classic() +
      ggplot2::scale_color_manual(
        values = c("#66C1BF", "#423089"),
        labels = c(stringr::str_sub(Code_Valid1, -6), stringr::str_sub(Code_Valid2, -6)),
        breaks = levels_order
      ) +
      ggplot2::labs(tag = ifelse(length(input$taxons) == 2 & Code_Valid1 != Code_Valid2, "* = both\nspecies", "")) +
      ggplot2::theme(
        legend.text = ggplot2::element_text(size = 12),
        axis.title.x = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_text(size = 12),
        axis.text.x = ggplot2::element_text(size = 12, angle = 45, hjust = 1),
        plot.tag.position = c(.87, .4),
        plot.tag = ggplot2::element_text(hjust = 0, size = 12, color = "red")
      ) +
      ggplot2::labs(
        y = i18n$t("Occurrence")
      ) +
      ggplot2::guides(fill = ggplot2::guide_legend(title = NULL)) +
      ggplot2::geom_text(
        ggplot2::aes(label = ifelse(duplicated(annee), "*", ""), y = 0),
        color = "red",
        size = 8
      )
  })

  # Plots des Typologies

  output$Circular_plot <- shiny::renderPlot({
    shiny::req(input$taxons)
    shiny::req(input$toggleMaps3)

    Code_Valid <- data() %>%
      dplyr::filter(full_name %in% input$taxons) %>%
      dplyr::pull(CodeValid) %>%
      base::unique()

    Circular_data <- data() %>%
      dplyr::filter(full_name %in% Code_Valid) %>%
      dplyr::select(full_name, type) %>%
      dplyr::group_by(full_name) %>%
      dplyr::summarise(
        "cours d'eau" = sum(type == "cours d'eau"),
        "plan d'eau" = sum(type == "plan d'eau")
      ) %>%
      dplyr::ungroup() %>%
      tidyr::pivot_longer(
        cols = c("cours d'eau", "plan d'eau"),
        names_to = "group",
        values_to = "values"
      ) %>%
      dplyr::group_by(full_name) %>%
      dplyr::mutate(percentage = ifelse(values > 0, values / sum(values) * 100, 0)) %>%
      dplyr::ungroup()

    blank_theme <- ggplot2::theme_minimal() +
      ggplot2::theme(
        axis.text.x = ggplot2::element_blank(),
        axis.title.x = ggplot2::element_blank(),
        axis.title.y = ggplot2::element_blank(),
        panel.border = ggplot2::element_blank(),
        panel.grid = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
        plot.title = ggplot2::element_text(size = 14, face = "bold"),
        strip.text = ggplot2::element_text(size = 15, face = "bold"),
        legend.text = ggplot2::element_text(size = 12)
      )

    pie_plot <- ggplot2::ggplot(Circular_data, ggplot2::aes(x = "", y = percentage, fill = group)) +
      ggplot2::geom_bar(width = 1, stat = "identity", color = "black") +
      ggplot2::scale_fill_manual(values = c("#3399FF", "#339900"), labels = c(
        "cours d'eau",
        "plan d'eau"
      )) +
      ggplot2::coord_polar("y", start = 0) +
      blank_theme +
      ggplot2::geom_text(
        data = subset(Circular_data, percentage > 0),
        ggplot2::aes(label = paste(round(percentage, 1), "%")),
        position = ggplot2::position_stack(vjust = 0.5),
        size = 5
      ) +
      ggplot2::facet_wrap(~ stringr::str_sub(full_name, -6), ncol = 2) +
      ggplot2::labs(fill = "Typologie")

    pie_plot
  })
  
  # Carte
  
  mapFiltered <- shiny::reactive({
    shiny::req(input$taxons)

    Code_Valid <- data() %>%
      dplyr::filter(full_name %in% input$taxons) %>%
      dplyr::pull(CodeValid) %>%
      unique()

    Code_Valid1 <- data() %>%
      dplyr::filter(full_name %in% input$taxons[1]) %>%
      dplyr::pull(CodeValid) %>%
      unique()

    Code_Valid2 <- data() %>%
      dplyr::filter(full_name %in% input$taxons[2]) %>%
      dplyr::pull(CodeValid) %>%
      unique()

    leaflet_data <- data() %>%
      dplyr::filter(type == "cours d'eau") %>%
      dplyr::filter(full_name %in% Code_Valid) %>%
      dplyr::mutate(full_name = CodeValid) %>%
      dplyr::mutate(annee = as.character(lubridate::year(DATE))) %>%
      dplyr::mutate(grp_color = stringr::str_sub(CodeValid, -6)) %>%
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
      dplyr::ungroup()

    leaflet_data2 <- data() %>%
      dplyr::filter(type == "plan d'eau") %>%
      dplyr::filter(full_name %in% Code_Valid) %>%
      dplyr::mutate(full_name = CodeValid) %>%
      dplyr::mutate(annee = as.character(lubridate::year(DATE))) %>%
      dplyr::mutate(grp_color = stringr::str_sub(CodeValid, -6)) %>%
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
      dplyr::ungroup()

    leaflet_data <- leaflet_data %>% dplyr::filter(DATE >= input$date[1] & DATE <= input$date[2])
    leaflet_data2 <- leaflet_data2 %>% dplyr::filter(DATE >= input$date[1] & DATE <= input$date[2])


    # Fonction de correspondance pour les couleurs
    color_mapping <- function(value) {
      base::ifelse(value == Code_Valid1, "#66C1BF", "#423089")
    }


    polygones <- hydro %>% sf::st_transform(sp::geometry, crs = 4326)

    polygones <- sf::st_make_valid(polygones)

    bins <- c(unique(polygones$CdHER1))
    pal <- leaflet::colorBin("YlOrRd", domain = polygones$NomHER1, bins = bins)

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
        }"
      ) %>%
      leaflet::addCircleMarkers(
        data = leaflet_data,
        lng = ~long,
        lat = ~lat,
        group = "cours d'eau",
        color = ~ color_mapping(CodeValid),
        fillOpacity = 0.8,
        weight = 1,
        radius = 5,
        popup = ~ paste(
          i18n$t("1- Year: "), annee, "<br>",
          i18n$t("2- Taxa: "), CodeValid, "<br>",
          i18n$t("3- Municipality: "), commune, "<br>",
          i18n$t("4- Longitude: "), round(long, 2), "<br>",
          i18n$t("5- Latitude: "), round(lat, 2), "<br>",
          i18n$t("6- Relative abundance (‰):"), round(RESULTAT, 2), "<br>"
        ),
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
        color = ~ color_mapping(CodeValid),
        fillOpacity = 0.8,
        weight = 1,
        radius = 5,
        popup = ~ paste(
          i18n$t("1- Year: "), annee, "<br>",
          i18n$t("2- Taxa: "), CodeValid, "<br>",
          i18n$t("3- Municipality: "), commune, "<br>",
          i18n$t("4- Longitude: "), round(long, 2), "<br>",
          i18n$t("5- Latitude: "), round(lat, 2), "<br>",
          i18n$t("6- Relative abundance (‰):"), round(RESULTAT, 2), "<br>"
        ),
        labelOptions = leaflet::labelOptions(
          noHide = F,
          direction = "auto"
        )
      ) %>%
      leaflet.extras::addHeatmap(
        data = leaflet_data %>% dplyr::filter(full_name %in% Code_Valid1),
        lng = ~long,
        lat = ~lat,
        group = paste0(i18n$t("density: "), Code_Valid1),
        blur = 15,
        max = 0.6,
        radius = 10
      ) %>%
      leaflet.extras::addHeatmap(
        data = leaflet_data %>% dplyr::filter(full_name %in% Code_Valid2),
        lng = ~long,
        lat = ~lat,
        group = ifelse(length(input$taxons) == 2,
          paste0(i18n$t("density: "), Code_Valid2),
          i18n$t("Only one taxon selected")
        ),
        blur = 15,
        max = 0.6,
        radius = 10
      ) %>%
      leaflet::addPolygons(
        data = polygones,
        label = polygones$NomHER1,
        color = "lightgrey",
        fillColor =
          ~ pal(CdHER1),
        dashArray = "3",
        fillOpacity = 0.1,
        group = i18n$t("Hydro ecoregions"),
        labelOptions = labelOptions(textsize = "15px"),
        highlightOptions = highlightOptions(
          weight = 5,
          color = "#666",
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
        overlayGroups = c(
          "cours d'eau", "plan d'eau",
          paste0(i18n$t("density: "), Code_Valid1),
          ifelse(length(input$taxons) == 2,
            paste0(i18n$t("density: "), Code_Valid2),
            i18n$t("Only one taxon selected")
          ),
          i18n$t("Hydro ecoregions")
        ),
        options = leaflet::layersControlOptions(collapsed = TRUE)
      ) %>%
      leaflet::hideGroup(group = c(
        "cours d'eau", "plan d'eau",
        paste0(i18n$t("density: "), Code_Valid1),
        ifelse(length(input$taxons) == 2,
          paste0(i18n$t("density: "), Code_Valid2),
          i18n$t("Only one taxon selected")
        ),
        i18n$t("Hydro ecoregions")
      )) %>%
      leaflet::setView(
        lng = 2,
        lat = 47,
        zoom = 6
      ) %>%
      leaflet::addControl(
        html = "<div id='custom-legend'> https://naiades.eaufrance.fr </div>",
        position = "bottomleft"
      )
  })

  output$mapFiltered <- leaflet::renderLeaflet({
    mapFiltered()
  })

  
  # Tableau des données filtrées par taxon
  
  output$Donnees2 <- DT::renderDataTable({
    shiny::req(input$taxons)

    Code_valid <- data() %>%
      dplyr::filter(full_name %in% input$taxons) %>%
      dplyr::pull(CodeValid) %>%
      unique()

    Commune <- i18n$t("Municipality")
    Abondance <- i18n$t("Relative abundance (‰)")
    Latitude <- i18n$t("Latitude")
    Longitude <- i18n$t("Longitude")
    Station <- i18n$t("Station")
    Date <- i18n$t("Date")
    taxon <- i18n$t("Taxa")

    DT::datatable(
      data() %>%
        dplyr::filter(full_name %in% Code_valid) %>%
        dplyr::mutate(
          RESULTAT = round(RESULTAT, 2),
          lat = round(lat, 2),
          lon = round(lon, 2)
        ) %>%
        dplyr::mutate(
          SANDRE = as.character(SANDRE),
          lon = as.character(lon),
          lat = as.character(lat),
          DATE = as.character(DATE),
          full_name = as.factor(full_name),
          commune = as.factor(commune)
        ) %>%
        dplyr::select(
          !!Date := DATE, !!Station := CODE_STATION, !!Commune := commune,
          !!taxon := CodeValid,
          Sandre = SANDRE,
          !!Latitude := lat, !!Longitude := lon,
          !!Abondance := RESULTAT
        ),
      extensions = "Buttons",
      options = list(
        pageLength = 10,
        autowidth = TRUE,
        columnDefs = list(
          list(className = "nowrap", targets = "_all"),
          list(targets = c(2, 3, 6, 7), width = "150px")
        ),
        scrollX = TRUE
      ),
      filter = "top",
      class = "custom-table"
    )
  })
  
    output$downloadData2 <- shiny::downloadHandler(

    Abondance <- i18n$t("Relative ABUNDANCE (‰)"),
    filename = function() {
      base::paste(
        as.character(data() %>%
          dplyr::filter(full_name %in% input$taxons[1]) %>%
          base::unique() %>%
          dplyr::pull(CodeValid) %>%
          base::unique()),
        as.character(data() %>%
          dplyr::filter(full_name %in% input$taxons[2]) %>%
          base::unique() %>%
          dplyr::pull(CodeValid) %>%
          base::unique()),
        Sys.Date(), ".csv",
        sep = "."
      )
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


  # Plots des Trophies 
  
  output$Trophie <- shiny::renderPlot(
    {
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
        dplyr::pull(CodeValid) %>%
        base::unique()

      Code_Valid1 <- data() %>%
        dplyr::filter(full_name %in% input$taxons[1]) %>%
        dplyr::pull(CodeValid) %>%
        base::unique()

      Code_Valid2 <- data() %>%
        dplyr::filter(full_name %in% input$taxons[2]) %>%
        dplyr::pull(CodeValid) %>%
        base::unique()

      data <- reshape2::melt(
        trophie %>%
          dplyr::filter(full_name %in% Code_Valid) %>%
          dplyr::rename(Optimum = optima, Tolerance = tolerance, Seuil_minimum = range_min, Seuil_maximum = range_max),
        id.vars = c("full_name", "parameter_full"),
        measure.vars = c("Optimum", "Tolerance", "Seuil_minimum", "Seuil_maximum")
      ) %>%
        dplyr::group_by(parameter_full) %>%
        dplyr::mutate(group = dplyr::cur_group_id()) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(code = stringr::str_sub(full_name, start = -5, end = -2))


      if (nrow(data) == 0) {
        shiny::showNotification(
          i18n$t("This taxon does not have a defined trophic profile according to the Carayon et al 2019 study."),
          type = "warning",
          duration = 20
        )
        return(NULL)
      } else {
        data2 <- data %>%
          dplyr::group_by(full_name, parameter_full) %>%
          tidyr::pivot_wider(names_from = variable, values_from = value) %>%
          dplyr::rowwise() %>%
          dplyr::mutate(Distribution = list(truncnorm::rtruncnorm(n = 2000, a = 0, b = Inf, mean = Optimum, sd = Tolerance))) %>%
          dplyr::ungroup()

        p <- data2 %>%
          tidyr::unnest(Distribution) %>%
          ggplot2::ggplot(ggplot2::aes(x = code, y = Distribution, color = parameter_full, fill = full_name)) +
          see::geom_violinhalf() +
          ggplot2::facet_wrap(~parameter_full, scales = "free", ncol = 4) +
          ggplot2::labs(x = "", y = "") +
          ggplot2::theme_bw() +
          ggplot2::theme(
            panel.spacing = unit(2, "lines"),
            legend.text = ggplot2::element_text(size = 12),
            axis.text.x = ggplot2::element_text(size = 15),
            axis.text.y = ggplot2::element_text(size = 15),
            strip.placement = "outside",
            strip.background = ggplot2::element_blank(),
            strip.text = ggplot2::element_text(size = 15)
          ) +
          ggplot2::scale_fill_manual(
            breaks = c(Code_Valid1, Code_Valid2),
            values = c("#66C1BF", "#423089")
          ) +
          ggplot2::scale_color_manual(values = rep("#000000", length(unique(data2$parameter_full)))) +
          ggplot2::guides(color = FALSE, fill = ggplot2::guide_legend(title = NULL))
      }

      grid::grid.draw(shift_legend(p))
    },
    height = 800,
    width = 1000
  )

  # Plots des profils écologiques
  
  output$Profil <- shiny::renderPlot(
    {
      shiny::req(input$taxons)

      Code_Valid <- data() %>%
        dplyr::filter(full_name %in% input$taxons) %>%
        dplyr::pull(CodeValid) %>%
        unique()

      Code_Valid1 <- data() %>%
        dplyr::filter(full_name %in% input$taxons[1]) %>%
        dplyr::pull(CodeValid) %>%
        base::unique()

      Code_Valid2 <- data() %>%
        dplyr::filter(full_name %in% input$taxons[2]) %>%
        dplyr::pull(CodeValid) %>%
        base::unique()

      levels_order <- c(Code_Valid1, Code_Valid2)

      tab <- profiles %>%
        dplyr::filter(full_name %in% Code_Valid)


      if (nrow(tab) == 0) {
        shiny::showNotification(
          i18n$t("This taxon does not have a defined ecological profile"),
          type = "warning",
          duration = 20
        )
        return(NULL)
      } else {
        p <- tab %>%
          tidyr::pivot_longer(
            cols = starts_with("CL"),
            names_to = "Classe",
            values_to = "Valeur"
          ) %>%
          dplyr::mutate(Valeur = as.numeric(Valeur)) %>%
          ggplot2::ggplot(ggplot2::aes(
            x = factor(Classe), y = Valeur,
            group = full_name, color = full_name
          )) +
          ggplot2::geom_point(size = 6, ggplot2::aes(shape = full_name)) +
          ggplot2::scale_color_manual(
            values = c("#66C1BF", "#423089"), labels = c(
              stringr::str_sub(Code_Valid1, -6),
              stringr::str_sub(Code_Valid2, -6)
            ),
            breaks = levels_order
          ) +
          ggplot2::geom_smooth(se = FALSE, size = 0.5, method = "loess", span = 0.3, color = "black") +
          ggplot2::labs(
            title = ""
          ) +
          ggplot2::scale_y_continuous(breaks = scales::pretty_breaks()) +
          ggplot2::scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1), breaks = seq(0.1, 1, by = 0.1)) +
          ggplot2::theme_classic() +
          ggplot2::theme(
            text = ggplot2::element_text(size = 20),
            legend.title = ggplot2::element_blank(),
            axis.title.x = ggplot2::element_blank(),
            axis.title.y = ggplot2::element_blank(),
            plot.margin = ggplot2::margin(t = 20, unit = "pt")
          ) +
          ggplot2::guides(shape = "none")
      }

      p
    },
    height = 600,
    width = 800
  )

}

# Run the app ----
shiny::shinyApp(ui = ui, server = server)
