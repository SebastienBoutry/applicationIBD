
getwd()

# getOption("repos")

options(shiny.http.response.timeout = 300)
options(encoding = "UTF-8")

library(shinythemes)
library(shinybusy)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(quarto)
library(shiny)
library(sf)
library(readr)
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
# library(downloader)

# options(shiny.maxRequestSize=10000*1024^2)

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
  )

`%notin%` <- Negate(`%in%`)
