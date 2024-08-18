#' User Interface module for team followers
#'
#' @param id  @param id A unique identifier string for the module’s UI.
#'
#' @return shiny::uiOutput
#'
mod_stats_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("stats"))
}

#' Team followers server
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function.
#' @param agg_follow reactive which sums/sorts total followers by
#'  - team
#'  - color
#'  - image
#'
#' @return ggplot2 with interactive team logo
#'
mod_stats_server <- function(id, agg_follow) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$stats <- shiny::renderUI({
      makeStatsPage(ns("fat"), ns("f1"), ns("f2"), ns("f3"))
    })

    output$fat <- shiny::renderPlot({

      agg_follow() |>
        ggplot2::ggplot(mapping = ggplot2::aes(
          x = reorder(team, -followers),
          y = followers,
          fill = color
        )) +
        ggplot2::geom_bar(stat = "identity") +
        ggimage::geom_image(
          mapping = ggplot2::aes(image = paste0("www/team_logo/", team_img)),
          size = 0.2) +
        ggplot2::scale_fill_identity() +
        ggplot2::scale_y_continuous(
          breaks = seq(0, 5000000, 500000),
          labels = scales::comma_format(),
          # scales::label_number(scale = 1e-6, suffix = "M")) +
          limits = c(0, 4000000)
        ) +
        ggplot2::labs(title = "", x = "", y = "") +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          axis.text.x = ggplot2::element_text(
            angle = 45,
            hjust = 1,
            size = 14
          ),
          axis.text.y = ggplot2::element_text(size = 14),
          plot.margin = ggplot2::margin(5, 5, 5, 5)
        )
    })

    output$f1 <- shiny::renderPlot({
      fat_distro_plot[[4]]
    })

    output$f2 <- shiny::renderPlot({
      fat_distro_plot[[2]]
    })

    output$f3 <- shiny::renderPlot({
      fat_distro_plot[[3]]
    })
  })
}
