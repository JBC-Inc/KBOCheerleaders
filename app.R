# UI ==========================================================================

ui <- bslib::page_sidebar(

  addExternalResources(),

  theme = bslib::bs_theme(
    version = 5,
    base_font = "Roboto, sans-serif",
    bootswatch = "minty"
    #primary = '#78C2AD',
    #secondary = '#F3969A',
    #success = '#56CC9D',
    #info = '#6CC3D5',
  ),

  title = makeTitle(),

  sidebar = bslib::sidebar(
    shiny::selectizeInput(
      inputId = "team",
      label = "Teams:",
      choices = c(team_data$name)
    ),

    shiny::uiOutput("cheerleaderUI"),

    mod_song_ui("song")$switch,
    mod_song_ui("song")$ui_team,
    mod_song_ui("song")$ui_piki
  ),

  bslib::navset_tab(
    id = "tabs",
    bslib::nav_panel(                    # reactable
      value = "react",
      "Team Overview",
    shiny::uiOutput("teamreact")
    ),
    bslib::nav_panel(                    # statistics
      value = "stats",
      "Team Followers",
      mod_stats_ui("stats")
    ),
    bslib::nav_panel(                    # team/cheerleader
      value = "visual",
      "Cheerleader Insights",
      shiny::uiOutput("team"),
      shiny::uiOutput("individual")
    ),
    bslib::nav_panel(                    # leaderboard
      value = "leader",
      "Top Cheerleaders",
      mod_leaderboard_ui("leaderboard")
    ),
    bslib::nav_spacer(),
    makeNavMenu(),
  )
)

# SERVER ======================================================================

server <- function(input, output, session) {

  mod_song_server("song", td)

  #----------------------------------------------------------------------------

  td <- shiny::reactive(label = "Selected Team Data", {
    list(
      name  = team_data$name[team_data$name == input$team],
      color = team_data$color[team_data$name == input$team],
      song  = team_data$song[team_data$name == input$team],
      photo = team_photos[which(team_data$name == input$team)],
      logo  = team_logos[team_data$name == input$team],
      cap   = team_caps[team_data$name == input$team]
    )
  })

  smm <- shiny::reactive(label = "Social Media Metrics", {
    list(
      youtube = youtube,
      instagram = instagram,
      tiktok = tiktok
    )
  })

  shiny::observeEvent(input$team, label = "Team Panel", {

    shiny::updateNavbarPage(inputId = "tabs", selected = "visual")

    shinyjs::show("team")
    shinyjs::hide("individual")

  }, ignoreInit = TRUE)

  shiny::observeEvent(input$cheerleader, label = "Cheerleader Panel", {

    shiny::updateNavbarPage(inputId = "tabs", selected = "visual")

    if (input$cheerleader != "") {
      shinyjs::hide("team")
      shinyjs::show("individual")
    }
  })

  # sidebar -------------------------------------------------------------------

  output$cheerleaderUI <- shiny::renderUI({

    shiny::req(input$team)

    cheerleaders <- team_cheerleaders |>
      dplyr::filter(team == input$team) |>
      dplyr::pull(cheerleader)

    shiny::radioButtons(
      inputId = "cheerleader",
      label = "Cheerleaders:",
      choices = sort(cheerleaders),
      selected = character(0)
    )
  })

  # Team Followers ------------------------------------------------------------

  mod_stats_server("stats", agg_follow)

  agg_follow <- shiny::reactive(label = "Followers Across Teams", {

    fat |>
      tidyr::drop_na() |>
      dplyr::group_by(team, color, logo) |>
      dplyr::summarize(followers = sum(followers), .groups = 'drop') |>
      dplyr::arrange(dplyr::desc(followers))
  })

  shiny::observe(label = "Plot click team logo", {

    point <- shiny::nearPoints(
      df = agg_follow(),
      coordinfo = input$plot_click,
      xvar = 'team',
      threshold = 42,
      maxpoints = 1,
      addDist = TRUE
    )

    if (nrow(point) != 0) {
      shiny::updateNavbarPage(inputId = "tabs", selected = "visual")
      shiny::updateSelectInput(session, "team", selected = point$team)
      shiny::updateSelectInput(session, "cheerleader", selected = "")
      shinyjs::show("team")
      shinyjs::hide("individual")
    }
  }) |>
    shiny::bindEvent(input$plot_click)

  # Reactable -----------------------------------------------------------------

  leader_data <- shiny::reactive(label = "Reactable Data", {
    teams <- ultra_combo |>
      dplyr::group_by(team) |>
      dplyr::summarize(members = dplyr::n_distinct(name), .groups = 'drop')

    uc <- ultra_combo |>
      dplyr::group_by(team, name) |>
      dplyr::summarize(
        followers = sum(dplyr::across(c(subs, instagram_followers, tiktok_followers)), na.rm = TRUE),

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
        reactable::reactableOutput("rt")
      )
    )
  })

  # Team/Cheerleader ----------------------------------------------------------

  # team

  output$team <- shiny::renderUI({
    makeTeam(td)
  })

  output$teamPhoto <- shiny::renderUI({

    if (input$team == "KT Wiz") {
      shiny::img(
        src = paste0("team_img/", td()$photo),
        style = "height: 600px;")
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

  # cheerleader

  output$individual <- shiny::renderUI({

    shiny::req(input$cheerleader)

    makeCheerleader(td, input$cheerleader, smm)
  })

  output$cheerleaderPhoto <- shiny::renderUI({

    image <- paste0("./cheerleader_img/", input$cheerleader, ".png")
    shiny::img(src = image, height = "320px")
  })

  output$cheerleaderBio <- DT::renderDataTable({

    DT::datatable(
      data = cheer_data[[input$cheerleader]]$table,
      class = "display",
      colnames = NULL,
      escape = FALSE,
      options = list(
        dom = "t",
        columnDefs = list(
          list(visible = FALSE, targets = 0)
          #list(width = '1000px', targets = 1)
        ),
        ordering = FALSE,
        pageLength = -1
      ),
      style = "auto"
    )
  })

  # Leaderboard ---------------------------------------------------------------

  shiny::observe(label = "Click Leaderboard Cheerleader", {
    shiny::updateSelectizeInput(session, "team", selected = input$team)
    shiny::updateSelectizeInput(session, "cheerleader", selected = input$cheerleader)
  })

  mod_leaderboard_server("leaderboard")

} # server

shinyApp(ui = ui, server = server)
