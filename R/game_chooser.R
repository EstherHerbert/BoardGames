#' Shiny app to help choose a game to play
#'
#' This app uses a board game collection (as accessed from
#'   [boardgamegeek.com](https://boardgamegeek.com)) to help a user choose a game
#'   to play based on the number of players, the time available, and/or the age
#'   of the youngest player. There's also a random game selector if you're
#'   really indecisive!
#'
#' @param ... options passed to [shiny::shinyApp()]
#'
#' @export
game_chooser <- function(...) {

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
      actionButton("rand", "Random Game?", icon = icon("shuffle")),
      span(textOutput("random_game"), style = "font-size:22px"),
      textOutput("random_game_details"),
      uiOutput("random_game_image"),
      uiOutput("random_game_description"),
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
        games_filtered()[input$games_filtered_rows_selected,]$description
    })

    observe({
      req(input$games_filtered_rows_selected)
      showModal(
        modalDialog(
          description_row(),
          title = 'Description',
          easyClose = TRUE,
          size = 'l'
        )
      )
    })
    shiny::modalDialog()

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
           options = list(launch.browser = TRUE, display.mode = 'normal'), ...)

}
