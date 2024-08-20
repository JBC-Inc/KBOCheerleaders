
#' Leaderboard UI
#'
#' @param id A unique identifier string for the module’s UI.
#'
#' @return Shiny reactive UI output
#' @keywords internal
#'
mod_leaderboard_ui <- function(id) {
  ns <- shiny::NS(id)
  list(
    ui = shiny::uiOutput(ns("leaders"))
  )
}

#' Leaderboard Server
#'
#' Generate the leaderboard UI that holds the 3 cheerleader leaderboard
#' cards for
#'  - YouTube
#'  - Instagram
#'  - TikTok
#'
#' @param id An ID string that corresponds with the ID used to call the
#' module's UI function.
#'
#' @return Side effect is to render and update the three leaderboards:
#'  - leaderYT
#'  - leaderInst
#'  - leaderTT
#'  using gt tables and dynamically generate UI elements for display.
#' @keywords internal
#'
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
      makegtYT(ultra_combo, 20)
    })

    output$leaderInst <- gt::render_gt({
      makegtInst(ultra_combo, 20)
    })

    output$leaderTT <- gt::render_gt({
      makegtTT(ultra_combo, 10)
    })
  })
}
