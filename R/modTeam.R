#' Team UI
#'
#' @param id  @param id A unique identifier string for the module’s UI.
#'
#' @return list of shiny reactive input and output.
#'
mod_team_ui <- function(id) {
  ns <- shiny::NS(id)

  list(
    select = shiny::selectizeInput(
      inputId = ns("team"),
      label = "Teams:",
      choices = team_data$name,
      selected = team_data$name[1]
    ),
    ui = shiny::uiOutput(ns("ui"))
  )
}

#' Team Server
#'
#' @param id An ID string that corresponds with the ID used to call the
#' module's UI function.
#' @param td reactive team_data object
#'
#' @return side effect is to render ui which consists of
#'  - team photo
#'  - team logo
#'  - team cap insignia
#'
mod_team_server <- function(id, td) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$ui <- shiny::renderUI({
      makeTeam(
        td,
        ns("teamPhoto"),
        ns("teamLogo"),
        ns("capInsignia")
      )
    })

    output$teamPhoto <- shiny::renderUI({
      if (input$team == "KT Wiz") {
        shiny::img(src = paste0("team_img/", td()$photo),style = "height: 600px;")
      } else {
        shiny::img(src = paste0("team_img/", td()$photo), height = "100%")
      }
    })

    output$teamLogo <- shiny::renderUI({
      shiny::img(src = paste0("team_logo/", td()$logo), height = "242px")
    })

    output$capInsignia <- shiny::renderUI({
      shiny::img(src = paste0("team_cap/", td()$cap), height = "242px")
    })
  })
}
