
#' Reactable UI
#'
#' @param id A unique identifier string for the module’s UI.
#'
#' @return Shiny reactive output
#' @keywords internal
#'
mod_react_ui <- function(id) {
  ns <- shiny::NS(id)
  list(
    ui = shiny::uiOutput(ns("teamreact"))
  )
}

#' Reactable Server
#'
#' A reactable with expanding rows, for each team display all the cheerleaders
#' for that team, and their followers statistics.
#'
#' @param id An ID string that corresponds with the ID used to call the
#' module's UI function.
#' @param td reactive team_data
#'
#' @return side effect render the reactable
#' @keywords internal
#'
mod_react_server <- function(id, td) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    leader_data <- shiny::reactive(label = "Reactable Data", {

      teams <- ultra_combo |>
        dplyr::group_by(team) |>
        dplyr::summarize(members = dplyr::n_distinct(name),
                         .groups = 'drop')

      uc <- ultra_combo |>
        dplyr::group_by(team, name) |>
        dplyr::summarize(
          followers = sum(dplyr::across(c(subs, instagram_followers,
                                          tiktok_followers)), na.rm = TRUE),

          subs = sum(subs, na.rm = TRUE),
          instagram_followers = sum(instagram_followers, na.rm = TRUE),
          tiktok_followers = sum(tiktok_followers, na.rm = TRUE),

          .groups = "drop") |>
        dplyr::left_join(team_cheerleaders |>
                           dplyr::select(cheerleader, link),
                         by = c("name" = "cheerleader"))

      list(
        teams = teams,
        uc = uc
      )
    })

    output$rt <- reactable::renderReactable({

      makeReactable(leader_data)
    })

    output$teamreact <- shiny::renderUI({

      bslib::card(
        bslib::card_header(
          bslib::tooltip(
            shiny::span(
              "Cheerleader Social Media Statistics by Team",
              bsicons::bs_icon("question-circle-fill")
            ),
            "Click Cheerleader Image to view the page."),
          class = "bg-dark"
        ),
        bslib::card_body(
          reactable::reactableOutput(ns("rt"))
        )
      )
    })
  })
}
