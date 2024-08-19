
ui <- bslib::page_sidebar(

  addExternalResources(),

  theme = bslib::bs_theme(
    version = 5,
    base_font = "Roboto, sans-serif",
    bootswatch = "minty",
    danger = "#ff0000"
  ),

  title = makeTitle(),

  sidebar = bslib::sidebar(

    mod_team_ui("team")$select,
    mod_cheer_ui("cheer")$select,
    mod_song_ui("song")$switch,
    mod_song_ui("song")$ui_team,
    mod_song_ui("song")$ui_piki
  ),

  bslib::navset_tab(
    id = "tabs",
    bslib::nav_panel(
      value = "react",
      "Team Overview",
      mod_react_ui("react")$ui,
    ),
    bslib::nav_panel(
      value = "stats",
      "Team Followers",
      mod_stats_ui("stats")
    ),
    bslib::nav_panel(
      value = "visual",
      "Cheerleader Insights",
      mod_team_ui("team")$ui,
      mod_cheer_ui("cheer")$ui
    ),
    bslib::nav_panel(
      value = "leader",
      "Top Cheerleaders",
      mod_leaderboard_ui("leaderboard")$ui
    ),
    bslib::nav_spacer(),
    makeNavMenu(),
  )
)

server <- function(input, output, session) {

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

  mod_react_server("react", td)

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

  mod_stats_server("stats", agg_follow)

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

      updateUI(session = session,
               state = "followers",
               team = point$team,
               cheerleader = character(0))
    }
  })

  mod_team_server("team", td)

  mod_cheer_server("cheer", td, smm)

  mod_leaderboard_server("leaderboard")
}

shinyApp(ui = ui, server = server)





