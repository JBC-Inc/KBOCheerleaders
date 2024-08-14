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

  title = shiny::tags$div(
    class = "d-flex justify-content-between align-items-center w-100",
    shiny::tags$div(class = "d-flex align-items-center",
      shiny::tags$img(
        src = "https://upload.wikimedia.org/wikipedia/en/thumb/5/59/KBO_League.svg/1920px-KBO_League.svg.png",
        height = "55px",
        style = "margin-right: 10px;"
      ),
      shiny::tags$h1("Cheerleaders!", style = "font-family: 'Bangers', cursive;")
    )
  ),

  sidebar = bslib::sidebar(
    shiny::selectizeInput(
      inputId = "team",
      label = "Teams:",
      choices = c(team_data$name)
    ),

    shiny::uiOutput("cheerleaderUI"),

    shinyWidgets::materialSwitch(
      inputId = "play_video",
      label = "Play Team Themesong",
      status = "danger"),
    shiny::uiOutput("teamsong"),

    shiny::br(),
    "Piki Piki Song",
    shiny::uiOutput("pikki")
  ),

  bslib::navset_tab(
    id = "tabs",
    bslib::nav_panel(                    # statistics
      value = "stats",
      "Team Stats",
      shiny::uiOutput("stats"),
      shiny::uiOutput("teamreact")
    ),
    bslib::nav_panel(                    # team/cheerleader
      value = "visual",
      "Team/Cheerleader",
      shiny::uiOutput("team"),
      shiny::uiOutput("individual")
    ),
    bslib::nav_panel(                    # leaderboard
      value = "leader",
      "Leaderboards",
      shiny::uiOutput("leaders")
    ),
    bslib::nav_spacer(),
    bslib::nav_menu(
      title = "References",
      bslib::nav_item(
        shiny::tags$a(
          shiny::tags$img(src="wikipedia.png", height = "20px", width = "20px"), "Wikipedia",
          href = "https://en.wikipedia.org/wiki/KBO_League", target = "_blank")),
      bslib::nav_item(
        shiny::tags$a(
          shiny::tags$img(src="namuwiki.svg", height = "20px", width = "20px"), "namuwiki",
          href = "https://en.namu.wiki/w/%EC%B9%98%EC%96%B4%EB%A6%AC%EB%8D%94/KBO%20%EB%A6%AC%EA%B7%B8",
          target = "_blank"))
    )
  )
)

# SERVER ======================================================================

server <- function(input, output, session) {

  output$pikki <- shiny::renderUI({
    shiny::tags$iframe(
      width="200",
      height="113",
      src="https://www.youtube.com/embed/Aj8IY4mQQGo?autoplay=0",
      frameborder="0",
      allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture",
      allowfullscreen=NA
    )
    })

  shiny::showModal(
    ui = shiny::modalDialog(
      title = shiny::div(
        shiny::h2("The Pikki Pikki Song Phenomenon",
                  style="color: #78C2AD;")
        ),
      introduction,
      footer = shiny::tagList(
        footer,
        shiny::modalButton("Dismiss"),
      ),
      size = "xl"
    )
  )

  #----------------------------------------------------------------------------

  td <- shiny::reactive(label = "Selected Team Data", {

    shiny::req(input$team)

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

  shiny::observeEvent(input$team, label = "Show/Hide Team", {

    shiny::updateNavbarPage(inputId = "tabs", selected = "visual")

    if (input$team != "") {
      shinyjs::show("team")
      shinyjs::show("valb")
      shinyjs::hide("individual")
    } else if (input$cheerleader != "") {
      shinyjs::hide("team")
      shinyjs::hide("valb")
      shinyjs::show("individual")
    }
  }, ignoreInit = TRUE)

  shiny::observe(label = "Show/Hide Cheerleader", {

    shiny::updateNavbarPage(inputId = "tabs", selected = "visual")

    if (input$cheerleader != "") {
      shinyjs::hide("team")
      shinyjs::hide("valb")
      shinyjs::show("individual")
    }
  }) |>
    shiny::bindEvent(input$cheerleader)

  selected_song <- shiny::reactive(label = "Song Selected", {
    shiny::req(input$team)
    song <- td()$song
    video_id <- sub(".*v=([^&]+).*", "\\1", song)
    autoplay <- if(input$play_video) "1" else "0"
    paste0("https://www.youtube.com/embed/", video_id, "?autoplay=", autoplay)
  })

  shiny::observe(label = "Play Themesong", {

    song <- selected_song()

    output$teamsong <- shiny::renderUI(
      shiny::tags$iframe(
        width="200",
        height="113",
        src=song,
        frameborder="0",
        allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture",
        allowfullscreen=NA)
    )
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

  # Team Stats ----------------------------------------------------------------

  agg_follow <- shiny::reactive(label = "Followers Across Teams", {

    fat |>
      tidyr::drop_na() |>
      dplyr::group_by(team, color, logo) |>
      dplyr::summarize(followers = sum(followers), .groups = 'drop') |>
      dplyr::arrange(dplyr::desc(followers))
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
         mapping = ggplot2::aes(image = paste0("www/team_logo/", logo)),
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

  output$stats <- shiny::renderUI({
    makeStatsPage()
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
      shiny::updateSelectInput(session, "team", selected = point$team)
      shiny::updateSelectInput(session, "cheerleader", selected = "")
      shiny::updateNavbarPage(inputId = "tabs", selected = "visual")
      shinyjs::show("team")
      shinyjs::show("valb")
      shinyjs::hide("individual")
    }
  }) |>
    shiny::bindEvent(input$plot_click)

    output$f1 <- shiny::renderPlot({
      fat_distro[[4]]
    })

    output$f2 <- shiny::renderPlot({
      fat_distro[[2]]
    })

    output$f3 <- shiny::renderPlot({
      fat_distro[[3]]
    })

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

    bslib::layout_column_wrap(
      width = NULL,
      fill = FALSE,
      style = bslib::css(grid_template_columns = "2.5fr 1fr"),

      bslib::card(
        id = "teamCard",
        full_screen = TRUE,
        bslib::card_header(
          style = paste("background-color:", td()$color, "; color: #ffffff;"),
          td()$name
        ),
        bslib::card_body(
          class = "cardb",
          fillable = TRUE,
          shiny::uiOutput("teamPhoto")
        )
      ),
      bslib::layout_column_wrap(
        width = NULL,
        fill = FALSE,
        style = bslib::css(flex_direction = "column"),

        bslib::card(
          id = "teamLogoCard",
          min_height = 324,
          max_height = 324,
          bslib::card_header(
            style = paste("background-color:", td()$color, "; color: #ffffff;"),
            paste0(td()$name, " Team Logo")
          ),
          bslib::card_body(
            class = "cardb",
            shiny::uiOutput("teamLogo")
          )
        ),

        bslib::card(
          id = "teamCapInsignia",
          min_height = 324,
          max_height = 324,
          bslib::card_header(
            style = paste("background-color:", td()$color, "; color: #ffffff;"),
            paste0(td()$name, " Cap Insignia")
          ),
          bslib::card_body(
            class = "cardb",
            shiny::uiOutput("capInsignia")
          )
        )
      )
    )
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
      style = "bootstrap5"
    )
  })

  # Leaderboard ---------------------------------------------------------------

  shiny::observe(label = "Click Leaderboard Cheerleader", {

    session$sendCustomMessage("set_team", input$team)
    shiny::updateSelectizeInput(session, "team", selected = input$team)

    session$sendCustomMessage("set_cheerleader", input$cheerleader)
    shiny::updateSelectizeInput(session, "cheerleader", selected = input$cheerleader)
  })

  output$leaders <- shiny::renderUI({
    makeLeaderboards()
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

} # server

shinyApp(ui = ui, server = server)



















