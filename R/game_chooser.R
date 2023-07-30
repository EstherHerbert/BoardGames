#' Shiny app to help choose a game to play
#'
#' This app uses a board game collection (as accessed from
#'   [boardgamegeek.com](https://boardgamegeek.com) to help a user choose a game
#'   to play based on the number of players, the time available, and/or the age
#'   of the youngest player. There's also a random game selector if you're
#'   really indecisive!
#'
#' @param ... options passed to [shiny::shinyApp()]
#'
#' @export
game_chooser <- function(...) {

  ui <- fluidPage(

    # Application title
    titlePanel("Game Chooser"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
      sidebarPanel(
        fluidRow(
          column(width = 9, textInput("username", "Your BGG username")),
          column(width = 3, div(style = "margin-top: 25px",
                                actionButton("search", "Search")))
        ),
        numericInput("players", "Number of Players", min = 1, max = 20,
                     value = 2),
        shiny::checkboxInput("time", label = "Is time limited?"),
        uiOutput("timeInput"),
        checkboxInput("young", label = "Are there young players?"),
        uiOutput("ageInput"),
        actionButton("rand", "Random Game?", icon = icon("shuffle")),
        span(textOutput("random_game"), style = "font-size:22px"),
        textOutput("random_game_details"),
        uiOutput("random_game_image")
      ),

      # Show a plot of the generated distribution
      mainPanel(
        DT::dataTableOutput("games_filtered")
      )
    )
  )

  # Define server logic required to draw a histogram
  server <- function(input, output, session) {

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
      dplyr::rename(games_filtered(), games_labels) %>%
        DT::datatable(options = list(paging = FALSE)) %>%
        DT::formatRound(columns = 2:6, digits = 0)
    })

    random_game <- eventReactive(input$rand, {
      dplyr::slice_sample(games_filtered(), n = 1)
    })

    output$random_game <- renderText({
      glue::glue_data(random_game(), "Random game: {name}")
    })

    output$random_game_details <- renderText({
      df <- dplyr::select(random_game(), minplayers:minage, -image) %>%
        dplyr::rename(any_of(games_labels))

      paste0(names(df), ": ", df[1,], collapse = "; ")
    })

    output$random_game_image <- renderUI({
      tags$img(src = random_game()$image, width = 300)
    })

  }

  # Run the application
  shinyApp(ui = ui, server = server, ...)

}
