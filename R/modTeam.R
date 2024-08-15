
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
