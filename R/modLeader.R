
mod_leaderboard_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("leaders"))
  )
}

mod_leaderboard_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$leaders <- shiny::renderUI({
      makeLeaderboards(
        ns("leaderYT"),
        ns("leaderInst"),
        ns("leaderTT")
        )
    })

    output$leaderYT <- gt::render_gt({
      makegtYT(ultra_combo)
    })

    output$leaderInst <- gt::render_gt({
      makegtInst(ultra_combo)
    })

    output$leaderTT <- gt::render_gt({
      makegtTT(ultra_combo)
    })
  })
}
