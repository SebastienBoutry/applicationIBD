if (interactive()) {
  
  library("shiny")
  library("shi18ny")
  
  ui <- fluidPage(
    tags$h1("shiny internationalization"),
    
    br(),
    # Initialize shi18ny
    useShi18ny(),
    
    # Language selector input
    langSelectorInput("lang", position = "fixed"),
    
    # UI elements
    # Text can be translated directly from the UI, using the ui_ function:
    h1(ui_("hello")),
    # OR by using uiOutput:
    uiOutput("results")
    
  )
  
  server <- function(input, output, session) {
    # Configure shi18ny
    i18n <- list(
      defaultLang = "en",
      availableLangs = c("es", "en", "pt")
    )
    
    # Call language module to get currently selected language and save it in a reactive
    lang <- callModule(langSelector, "lang", i18n = i18n, showSelector = TRUE)
    
    # Update UI translation
    observeEvent(lang(),{
      uiLangUpdate(input$shi18ny_ui_classes, lang())
    })
    
    # Render translations by passing the text and the active (selected) language as the lang() parameter to the i_ function
    output$results <- renderUI({
      list(
        h1(i_("world", lang())), 
        br(), 
        h4(i_("language", lang()))
      )
    })
  }
  
  shinyApp(ui, server)
  
}