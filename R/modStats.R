#' User Interface module for team followers
#'
#' @param id  @param id A unique identifier string for the module’s UI.
#'
#' @return shiny::uiOutput
#' @keywords internal
#'
mod_stats_ui <- function(id) {
  ns <- shiny::NS(id)
  list(
    ui = shiny::uiOutput(ns("stats"))
  )
}

#' Team followers server
#'
#' @param id An ID string that corresponds with the ID used to call the
#' module's UI function.
#'
#' @return ggplot2 with interactive team logo.
#' @keywords internal
#'
mod_stats_server <- function(id, parent) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    agg_follow <- shiny::reactive(label = "Followers Across Teams", {

      ultra_combo |>
        dplyr::rowwise() |>
        dplyr::mutate(
          followers = sum(c(subs, instagram_followers, tiktok_followers),
                          na.rm =TRUE)) |>
        dplyr::group_by(team, color, team_img) |>
        dplyr::summarize(followers = sum(followers), .groups = 'drop') |>
        dplyr::arrange(dplyr::desc(followers))
    })

    shiny::observeEvent(input$plot_click, label = "Plot click team logo", {

      point <- shiny::nearPoints(
        df = agg_follow(),
        coordinfo = input$plot_click,
        xvar = 'team',
        threshold = 42,
        maxpoints = 1,
        addDist = TRUE
      )

      if (nrow(point) != 0) {

        updateUI(session = parent,
                 state = "team",
                 team = point$team,
                 cheerleader = character(0))
      }
    })

    output$stats <- shiny::renderUI({
      makeStatsPage(ns("fat"),
                    ns("plot_click"),
                    ns("f1"),
                    ns("f2"),
                    ns("f3"),
                    ns("ajd"),
                    ns("ad"))
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

    # output$f1 <- shiny::renderPlot({
    #   fat_distro_plot[[4]]
    # })
    #
    # output$f2 <- shiny::renderPlot({
    #   fat_distro_plot[[2]]
    # })
    #
    # output$f3 <- shiny::renderPlot({
    #   fat_distro_plot[[3]]
    # })

    output$ajd <- ggiraph::renderGirafe({
      age_jitter_dist
    })

    output$ad <- shiny::renderPlot({
      age_dist
    })
  })
}


