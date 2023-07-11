getwd()

# getOption("repos")

options(shiny.http.response.timeout = 300)
options(encoding = "UTF-8")


library(httr)
library(gh)
library(RColorBrewer)
library(shinythemes)
library(shinybusy)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(quarto)
library(shiny)
library(sf)
library(leaflet)
library(plotly)
library(leaflet)
library(DT)
library(data.table)
library(stringr)
library(shinyWidgets)
library(conflicted)
library(readxl)
library(xlsx)
library(scales)
library(shinyjs)
library(fs)
library(git2r)
library(leaflet.extras)
library(openxlsx)
library(shinymaterial)
library(see)
library(truncnorm)
library(gtable)
library(cowplot)
library(grid)
library(fontawesome)



# cOuleur des plots 
color_palette <- c("#9ED6E3", "#423089")
color_palette2 <- c(brewer.pal(9, "Purples"), brewer.pal(9, "Reds"))

shift_legend <- function(p){
  
  # check if p is a valid object
  if(!"gtable" %in% class(p)){
    if("ggplot" %in% class(p)){
      gp <- ggplotGrob(p) # convert to grob
    } else {
      message("This is neither a ggplot object nor a grob generated from ggplotGrob. Returning original plot.")
      return(p)
    }
  } else {
    gp <- p
  }
  
  # check for unfilled facet panels
  facet.panels <- grep("^panel", gp[["layout"]][["name"]])
  empty.facet.panels <- sapply(facet.panels, function(i) "zeroGrob" %in% class(gp[["grobs"]][[i]]))
  empty.facet.panels <- facet.panels[empty.facet.panels]
  if(length(empty.facet.panels) == 0){
    message("There are no unfilled facet panels to shift legend into. Returning original plot.")
    return(p)
  }
  
  # establish extent of unfilled facet panels (including any axis cells in between)
  empty.facet.panels <- gp[["layout"]][empty.facet.panels, ]
  empty.facet.panels <- list(min(empty.facet.panels[["t"]]), min(empty.facet.panels[["l"]]),
                             max(empty.facet.panels[["b"]]), max(empty.facet.panels[["r"]]))
  names(empty.facet.panels) <- c("t", "l", "b", "r")
  
  # extract legend & copy over to location of unfilled facet panels
  guide.grob <- which(gp[["layout"]][["name"]] == "guide-box")
  if(length(guide.grob) == 0){
    message("There is no legend present. Returning original plot.")
    return(p)
  }
  gp <- gtable_add_grob(x = gp,
                        grobs = gp[["grobs"]][[guide.grob]],
                        t = empty.facet.panels[["t"]],
                        l = empty.facet.panels[["l"]],
                        b = empty.facet.panels[["b"]],
                        r = empty.facet.panels[["r"]],
                        name = "new-guide-box")
  
  # squash the original guide box's row / column (whichever applicable)
  # & empty its cell
  guide.grob <- gp[["layout"]][guide.grob, ]
  if(guide.grob[["l"]] == guide.grob[["r"]]){
    gp <- gtable_squash_cols(gp, cols = guide.grob[["l"]])
  }
  if(guide.grob[["t"]] == guide.grob[["b"]]){
    gp <- gtable_squash_rows(gp, rows = guide.grob[["t"]])
  }
  gp <- gtable_remove_grobs(gp, "guide-box")
  
  return(gp)
}


correspondance <- c("COND" = "Conductivité",
                    "DBO5" = "Demande en oxygène à 5 jours",
                    "NO3" = "Nitrates",
                    "PO4" = "Phosphates",
                    "SAT" = "Saturation en oxygène",
                    "NORG" = "Azote organique",
                    "PH" = "PH")

#Trophie
trophie <- as_tibble(read_excel("data/Sup_material_published.xlsx", sheet = "Numbers")) %>%
  dplyr::left_join(as_tibble(read_excel("data/Sup_material_published.xlsx", sheet = "database synthesis")) %>%
                     rename(code = "Code")) %>%
  left_join(as_tibble(utils::read.csv2("data/table_transcodage.csv", stringsAsFactors = FALSE)) %>%
              select(code = "abre", True_name = "CodeValid", full_name = name_valid), by = "code") %>%
  mutate(true_profile = if_else(code == True_name, 1,0)) %>%
  dplyr::filter(true_profile == 1) %>%
  mutate(code = if_else(is.na(True_name) == T, code, True_name)) %>%
  select(-True_name) %>% dplyr::filter(!is.na(code)) %>%
  mutate(full_name = sub("\\_g.*", "", full_name)) %>%
  mutate(full_name = str_replace_all(full_name, "[^[:alnum:]]", " ")) %>%
  mutate(full_name = paste0(full_name, " ", "(", code, ")")) %>%
  dplyr::select(full_name, code, parameter, 
                tolerance, 
                optima, range_min, range_max, Class) %>%
  mutate(parameter_full = correspondance[parameter])
  

# profiles <- as_tibble(read.csv2(file = "data/IBD_params.csv", sep = ";",
#                       dec = ",", header = TRUE, stringsAsFactors = FALSE)) %>%
#   select(-SANDRE, -DENOMINATION, -Origine) %>%
#   left_join(as_tibble(utils::read.csv2("data/table_transcodage.csv", stringsAsFactors = FALSE)) %>%
#               select(AFNOR = "abre", True_name = "CodeValid", full_name = name_valid), by = "AFNOR") %>%
#
#   mutate(true_profile = if_else(AFNOR == True_name, 1,0)) %>%
#   dplyr::filter(true_profile == 1) %>%
#   mutate(AFNOR = if_else(is.na(True_name) == T, AFNOR, True_name)) %>%
#   select(-True_name) %>% dplyr::filter(!is.na(AFNOR)) %>%
#
#   mutate(full_name = sub("\\_g.*", "", full_name)) %>%
#   mutate(full_name = str_replace_all(full_name, "[^[:alnum:]]", " ")) %>%
#   mutate(full_name = paste0(full_name, " ", "(", AFNOR, ")")) %>%
#   dplyr::select(-AFNOR, -true_profile, -Val.Ind.)

# save(profiles, file = "data/profiles.RData")

load("data/profiles.RData")


# Fond de carte
map_base <- leaflet::leaflet() %>%
  leaflet::addProviderTiles(providers$Esri.WorldGrayCanvas,
                            group = "Fond clair"
  ) %>%
  leaflet::addProviderTiles(providers$CartoDB.DarkMatter,
                            group = "Fond noir"
  ) %>%
  leaflet::addProviderTiles(providers$GeoportailFrance.orthos,
                            group = "Fond satellite"
  ) %>%
  leaflet::addProviderTiles("OpenStreetMap",
                            group = "Open Street Map")


#
`%notin%` <- Negate(`%in%`)

hydro <- st_read("data/Hydroecoregion1.shp")



# Chargement du fichier le plus récent ------------------------------------

# URL du référentiel et du dossier
repo_url <- "https://github.com/leolea12/NAIDESexport"
folder_path <- "data_raw"

# Obtenir le contenu du dossier depuis l'API GitHub
folder_content <- gh::gh("/repos/:owner/:repo/contents/:path",
                         owner = "leolea12",
                         repo = "NAIDESexport",
                         path = folder_path)


# Vecteur pour stocker les noms des fichiers .Rda
rda_file_names <- character()

# Parcourir les fichiers du dossier
for (file_info in folder_content) {
  # Vérifier si l'élément est un fichier .Rda
  if (file_info$type == "file" && grepl("\\.Rda$", file_info$name)) {
    # Ajouter le nom du fichier .Rda au vecteur
    rda_file_names <- c(rda_file_names, file_info$name)
  }
}

# Extract the dates from file names
dates <- sub(".*data_X([0-9.]+)\\..*", "\\1", basename(rda_file_names))

# Convert dates to proper format
dates <- as.POSIXct(dates, format = "%Y.%m.%d.%H.%M.%S")

# Find the index of the most recent date
index_plus_recent <- which.max(dates)

# Extract the file name corresponding to the most recent date
fichier_plus_recent <- basename(rda_file_names[index_plus_recent])



# Fermeture des données du GIT après arrêt du shiny


# onStop(function() {
#   tryCatch(
#     expr = {
#       rm(Diatom)
#     },
#     error = function(e) {
#       # Gérer l'erreur si la suppression du fichier échoue
#       # Vous pouvez afficher un message d'erreur ou effectuer d'autres actions ici
#       print("Erreur lors de la suppression du fichier")
#     }
#   )
# })



