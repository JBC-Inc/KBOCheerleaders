#' Server function for the application
#'
#' @param input Internal parameter for `{shiny}`
#' @param output Internal parameter for `{shiny}`
#' @param session Internal parameter for `{shiny}`
#'
#' @keywords internal
#'
app_server <- function(input, output, session) {

  mod_song_server("song", td)

  td <- shiny::reactive(label = "Selected Team Data", {

    list(
      name  = team_data$name[team_data$name == input$`team-team`],
      color = team_data$color[team_data$name == input$`team-team`],
      song  = team_data$song[team_data$name == input$`team-team`],
      photo = team_photos[which(team_data$name == input$`team-team`)],
      logo  = team_logos[team_data$name == input$`team-team`],
      cap   = team_caps[team_data$name == input$`team-team`]
    )
  })

  smm <- shiny::reactive(label = "Social Media Metrics", {

    list(
      youtube = youtube,
      instagram = instagram,
      tiktok = tiktok
    )
  })

  shiny::observeEvent(input$`team-team`, label = "Team Input", {

    updateUI(session, state = "team", team = input$`team-team`)
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$`cheer-cheerleader`, label = "Cheerleader Input", {

    shiny::req(length(input$`cheer-cheerleader`) > 0)
    updateUI(session, state = "cheer", cheerleader = input$`cheer-cheerleader`)
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$randteam, label = "Duplicate Team", {

    updateUI(session, state = "randteam")
  })

  shiny::observeEvent(input$randcheer, label = "Duplicate Cheer", {

    updateUI(session, state = "default")
  })

  shiny::observe(label = "plotly Select Cheerleader", {

    point <- plotly::event_data("plotly_click", source = "A")

    age_groupS <- levels(long$age_group)[round(point$x)]

    cheerleader <- long |>
      tidyr::drop_na() |>
      dplyr::group_by(team, color, name, age, age_group) |>
      dplyr::summarize(followers = sum(followers), .groups = 'drop') |>
      dplyr::filter(followers < 1200000) |>
      dplyr::filter(abs(followers - point$y) <= 1) |>
      dplyr::filter(age_group == age_groupS) |>
      dplyr::slice(1) |>
      dplyr::pull(name)

    team <- team_cheerleaders$team[team_cheerleaders$cheerleader == cheerleader]

    session$sendCustomMessage("handler1", list(cheerleader, team))
  }) |>
    shiny::bindEvent(plotly::event_data("plotly_click", source = "A"))

  mod_react_server("react", td)

  mod_stats_server(
    id = "stats",
    plot_click = shiny::reactive(input$plot_click),
    sesh = session
  )

  mod_team_server("team", td)

  mod_cheer_server("cheer", td, smm)

  top_count <- shiny::reactive(label = "Leaderboard Records", {
    length(unique(historic$datetime)) * 5
  })

  mod_leaderboard_server("leaderboard", top_count)
}
