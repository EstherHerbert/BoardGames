ui <- bslib::page_sidebar(
  title = "Game Chooser",
  sidebar = bslib::sidebar(
    textInput("username", "Your BGG username"),
    actionButton("search", "Get Collection"),
    numericInput("players", "Number of Players", min = 1, max = 20,
                 value = 2),
    checkboxInput("time", label = "Is time limited?"),
    uiOutput("timeInput"),
    checkboxInput("young", label = "Are there young players?"),
    uiOutput("ageInput"),
    uiOutput("catInput"),
    actionButton("rand", "Random Game?", icon = icon("shuffle")),
    span(textOutput("random_game"), style = "font-size:22px"),
    textOutput("random_game_details"),
    uiOutput("random_game_image"),
    img(src = "BGGLogo.PNG", width = 200),
    width = 400,
    open = 'always'
  ),
  DT::dataTableOutput("games_filtered")
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  # bslib::bs_themer()

  games <- eventReactive(input$search, {

    df <- get_collection(input$username)

    df <- dplyr::filter(df, own == T)

    dplyr::select(df, -own, -objectid)

  })

  output$timeInput <- renderUI({
    if(input$time) {
      numericInput("minutes", "How long do you have? (minutes)", min = 10,
                   max = 120, value = 30, step = 5)
    }
  })

  output$ageInput <- renderUI({
    if(input$young) {
      numericInput("age", "How old is the youngest player?", min = 4,
                   max = 15, value = 4)
    }
  })

  output$catInput <- renderUI({
    selectizeInput(
      'cat', 'Choose a category (optional)',
      choices = sort(unique(unlist(
        stringr::str_split(games()$category[!games()$category==""], ", ")
      ))),
      multiple = TRUE
    )
  })

  games_filtered <- reactive({
    req(input$players)
    df <- dplyr::filter(games(), minplayers <= input$players,
                        maxplayers >= input$players)

    if(input$time) {
      req(input$minutes)
      df <- dplyr::filter(df, maxplaytime <= input$minutes)
    }

    if(input$young) {
      req(input$age)
      df <- dplyr::filter(df, as.numeric(gsub("\\D", "", minage)) <=
                            input$age)
    }

    if(!is.null(input$cat)) {
      temp <- paste0("(", input$cat, ")", collapse = "|")
      df <- dplyr::filter(df, stringr::str_detect(category, temp))
    }

    df

  })

  output$games_filtered <- DT::renderDataTable({
    dplyr::mutate(
      games_filtered(),
      image = paste0("<img src=\"", image, "\" height=\"30\">")
    ) %>%
      dplyr::rename(!!!games_labels) %>%
      dplyr::select(-Type, -description) %>%
      dplyr::relocate(` ` = image) %>%
      DT::datatable(options = list(paging = FALSE), escape = FALSE,
                    rownames = FALSE, selection = 'single')
  })

  description_row <- reactive({
    req(input$games_filtered_rows_selected)
    stringr::str_replace_all(
      games_filtered()[input$games_filtered_rows_selected,]$description,
      "\n", "<br>"
    ) %>%
      HTML()
  })

  observeEvent(input$games_filtered_rows_selected, {
    showModal(
      modalDialog(
        description_row(),
        title = 'Description',
        easyClose = TRUE,
        size = 'l'
      )
    )
  })

  random_game <- eventReactive(input$rand, {
    dplyr::slice_sample(games_filtered(), n = 1)
  })

  output$random_game <- renderText({
    glue::glue_data(random_game(), "Random game: {name}")
  })

  output$random_game_details <- renderText({
    df <- dplyr::select(random_game(), minplayers:minage, -image) %>%
      dplyr::rename(dplyr::any_of(games_labels))

    paste0(names(df), ": ", df[1,], collapse = "; ")
  })

  output$random_game_description <- renderUI({
    stringr::str_replace_all(random_game()$description, "&#10;", "<br>") %>%
      HTML()
  })

  output$random_game_image <- renderUI({
    tags$img(src = random_game()$image, width = 300)
  })

  session$onSessionEnded(stopApp)

}

# Run the application
shinyApp(ui = ui, server = server,
         options = list(launch.browser = TRUE, display.mode = 'normal'))
