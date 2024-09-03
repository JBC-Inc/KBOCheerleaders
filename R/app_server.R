#' Server function for the application
#'
#' @param input Internal parameter for `{shiny}`
#' @param output Internal parameter for `{shiny}`
#' @param session Internal parameter for `{shiny}`
#'
#' @keywords internal
#'
app_server <- function(input, output, session) {

  td <- shiny::reactive(label = "Selected Team Data", {

    team <- input$`team-team`

    list(
      name  = team_data$name[team_data$name == team],
      color = team_data$color[team_data$name == team],
      song  = team_data$song[team_data$name == team],
      photo = team_photos[which(team_data$name == team)],
      logo  = team_logos[team_data$name == team],
      cap   = team_caps[team_data$name == team]
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

  shiny::observeEvent(input$`stats-ajd_selected`, label = "Age Jitter Distribution", {

    click_data <- strsplit(input$`stats-ajd_selected`, "\\|")[[1]]
    cheerleader <- click_data[1]
    team <- click_data[2]

    session$sendCustomMessage("handler1", list(cheerleader, team))
    session$sendCustomMessage(type = 'stats-ajd_set', message = character(0))
  })

  mod_song_server("song", td)

  mod_react_server("react", td)

  mod_stats_server("stats", session)

  mod_team_server("team", td)

  mod_cheer_server("cheer", td, smm)

  mod_leaderboard_server("leaderboard", historic)
}
