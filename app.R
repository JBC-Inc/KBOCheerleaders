# UI ==========================================================================

ui <- bslib::page_sidebar(

  addExternalResources(),

  theme = bslib::bs_theme(
    version = 5,
    # bootswatch = "zephyr"
    # base_font = bslib::font_google("Bangers"),
    ) |>
    bslib::bs_add_rules("
      body{
        font-size: 12px;
      }
      .cardb {
        text-align: center;
      }
      .fcon {
        height: 55px;
        margin-right: 10px;
      }
    "),

  title = shiny::tags$div(
    class = "d-flex justify-content-between align-items-center w-100",
    shiny::tags$div(class = "d-flex align-items-center",
      shiny::tags$img(
        src = paste0("https://upload.wikimedia.org/wikipedia/en/thumb/5/59/",
                     "KBO_League.svg/1920px-KBO_League.svg.png"),
        height = "55px",
        style = "margin-right: 10px;"
      ),
      shiny::tags$h1("Cheerleaders!", style = "font-family: 'Bangers', cursive;")
    ),
    # bslib::input_dark_mode(id = NULL, mode = NULL)
  ),

  sidebar = bslib::sidebar(
    shiny::selectizeInput(
      inputId = "team",
      label = "Teams:",
      choices = c("", team_data$name)
    ),
    shiny::uiOutput("cheerleaderUI"),
    shiny::uiOutput("song")
  ),

  bslib::navset_tab(
    id = "tabs",
    bslib::nav_panel(                    # statistics
      value = "stats",
      "Team Stats",
      shiny::uiOutput("stats")
    ),
    bslib::nav_panel(                    # team/cheerleader
      value = "visual",
      "Team/Cheerleader",
      shiny::uiOutput("team"),
      shiny::uiOutput("individual")
    ),
    bslib::nav_panel(                    # leaderboard
      value = "leader",
      "Leaderboard",
      shiny::uiOutput("leaders")
    )
  )
)

# SERVER ======================================================================

server <- function(input, output, session) {

  # bslib::bs_themer()

  shinyjs::hide("team")
  shinyjs::hide("valb")
  shinyjs::hide("individual")

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
    }

    song <- td()$song

    video_id <- sub(".*v=([^&]+).*", "\\1", song)

    song <- paste0("https://www.youtube.com/embed/", video_id, "?autoplay=1")

    output$song <- shiny::renderUI(
      shiny::tags$iframe(
        width="200",
        height="113",
        src=song,
        frameborder="0",
        allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture",
        allowfullscreen=NA)
    )
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

  output$stats <- shiny::renderUI({

    # bslib::card(
    #   id = "valb",
    #   class = "valb",
    #   bslib::layout_columns(
    #     bslib::value_box(
    #       title = "Team Member Count",
    #       42,
    #       showcase = bsicons::bs_icon("handbag")
    #     ),
    #     bslib::value_box(
    #       title = "Average Tenure",
    #       42,
    #       showcase = bsicons::bs_icon("handbag")
    #     ),
    #     bslib::value_box(
    #       title = "Social Media Views",
    #       42,
    #       showcase = bsicons::bs_icon("handbag")
    #     )
    #   )
    # ),

    bslib::layout_column_wrap(
      width = NULL,
      fill = FALSE,
      style = bslib::css(grid_template_columns = "2fr 1fr"),
      bslib::card(
        id = "stat",
        class = "stat-fat",
        bslib::card_header(
          "Total Cheerleader Social Media Followers by Team",
          class = "bg-dark"
        ),
        bslib::card_body(
          shiny::plotOutput("fat", click = "plot_click")
        ),
        bslib::card_footer(
          "Click Team to view.",
          class = "bg-info"
        )
      ),
      bslib::layout_column_wrap(
        width = NULL,
        fill = FALSE,
        style = bslib::css(flex_direction = "column"),
        bslib::card(id = "f1",
                    bslib::card_header(
                      "Distribution of Average Followers per Platform",
                      class = "bg-dark"
                    ),
                    bslib::card_body(
                      shiny::plotOutput("f1")
                    )
        ),
        bslib::card(
          bslib::card_header(
            "Distribution of Capped Average Followers per Platform",
            class = "bg-dark"
          ),
          bslib::card_body(
            shiny::plotOutput("f2")
          )
        ),
        bslib::card(
          bslib::card_header(
            "Distribution of Log-Transformed Average Followers per Platform",
            class = "bg-dark"
          ),
          bslib::card_body(
            shiny::plotOutput("f3")
          )
        ),
        bslib::card(
          bslib::card_header(
            "Distribution of Average Followers per Platform (95% Percentile)",
            class = "bg-dark"
          ),
          bslib::card_body(
            shiny::plotOutput("f4")
          )
        )
      )
    )
  })

  bigback <- shiny::reactive({
    fat |>
      tidyr::drop_na() |>
      dplyr::group_by(team, color, logo) |>
      dplyr::summarize(followers = sum(followers), .groups = 'drop') |>
      dplyr::arrange(dplyr::desc(followers))
  })

  output$fat <- shiny::renderPlot({

    bigback() |>
      ggplot2::ggplot(
        mapping = ggplot2::aes(
          x = reorder(team, -followers),
          y = followers,
          fill = color)) +
      ggplot2::geom_bar(stat = "identity") +
      ggimage::geom_image(ggplot2::aes(image = logo), size = 0.30) +
      ggplot2::scale_fill_identity() +
      ggplot2::scale_y_continuous(
        breaks = seq(0, 5000000, 500000),
        labels = scales::comma_format(), # scales::label_number(scale = 1e-6, suffix = "M")) +
        limits = c(0, 4000000)
      ) +
      ggplot2::labs(title = "", x = "", y = "") +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 14),
        axis.text.y = ggplot2::element_text(size = 14),
        plot.margin = ggplot2::margin(5, 5, 5, 5)
      )
  })

  shiny::observeEvent(input$plot_click, {

    click <- input$plot_click

    point <- shiny::nearPoints(bigback(),
                               coordinfo = click,
                               xvar = 'team',
                               threshold = 100,
                               maxpoints = 1,
                               addDist = TRUE)

    if (nrow(point) == 0) {
      shiny::updateNavbarPage(inputId = "tabs", selected = "stats")
    } else {
      shiny::updateSelectInput(session, "team", selected = point$team)
    }
  })

  output$f1 <- shiny::renderPlot({
    fat_distro[[1]]
  })

  output$f2 <- shiny::renderPlot({
    fat_distro[[2]]
  })

  output$f3 <- shiny::renderPlot({
    fat_distro[[3]]
  })

  output$f4 <- shiny::renderPlot({
    fat_distro[[4]]
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

  # cheer

  output$individual <- shiny::renderUI({

    shiny::req(input$cheerleader)

    yt <- dplyr::filter(smm()$youtube, name == input$cheerleader)
    inst <- dplyr::filter(smm()$instagram, name == input$cheerleader)
    tt <- dplyr::filter(smm()$tiktok, cheername == input$cheerleader)

    yt <- yt |> dplyr::filter(views > 200000)

    photoBio <- bslib::card(
      full_screen = TRUE,
      bslib::card_header(
        style = paste("background-color:", td()$color, "; color: #ffffff;"),
        input$cheerleader
      ),
      bslib::card_body(
        class = "cardb",
        fillable = TRUE,
        shiny::uiOutput("cheerleaderPhoto"),
      ),
      bslib::card_body(
        fillable = TRUE,
        DT::dataTableOutput("cheerleaderBio")
      )
    )

    if (nrow(yt) > 0) {

      yt <- bslib::card(
        id = "valb",
        class = "cardb",

        bslib::card_header(
          style = paste("background-color:", td()$color, "; color: #ffffff;"),
          paste0("YouTube Statistics for ", yt$title)
        ),

        bslib::value_box(
          title = "Subscribers",
          format(yt$subs, big.mark = ","),
          showcase = bsicons::bs_icon("youtube") # , size = "3rem")
        ),
        bslib::value_box(
          title = "Views",
          format(yt$views, big.mark = ","),
          showcase = bsicons::bs_icon("film") # , size = "3rem")
        ),
        bslib::value_box(
          title = "Videos",
          format(yt$count, big.mark = ","),
          showcase = bsicons::bs_icon("camera-video") # , size = "3rem")
        )
      )
    } else {
      yt <- NULL
    }

    if (nrow(inst) > 0) {

      insta <- bslib::card(
        id = "valb",
        class = "cardb",

        bslib::card_header(
          style = paste("background-color:", td()$color, "; color: #ffffff;"),
          paste0("Instagram Statistics for ",
                 stringr::str_replace_all(inst$name, "/", ""))
        ),
        bslib::value_box(
          title = "Followers",
          format(inst$followers, big.mark = ","),
          showcase = bsicons::bs_icon("instagram") # , size = "3rem")
        )
      )
    } else {
      insta <- NULL
    }

    if (nrow(tt) > 0) {

      tiktok <- bslib::card(
        id = "valb",
        class = "cardb",

        bslib::card_header(
          style = paste("background-color:", td()$color, "; color: #ffffff;"),
          paste0("TikTik Statistics for ", tt$name)
        ),
        bslib::value_box(
          title = "Followers",
          format(as.numeric(tt$followers), big.mark = ","),
          # showcase = bsicons::bs_icon("tiktok", size = "3rem")
          showcase = htmltools::img(src = "social_icons/tiktok.webp",
                                    alt = "tiktok icon",
                                    height = "64",
                                    width = "64")
        ),
        bslib::value_box(
          title = "Likes",
          format(as.numeric(tt$likes), big.mark = ","),
          showcase = bsicons::bs_icon("heart") # , size = "3rem")
        )
      )
    } else {
      tiktok <- NULL
    }

    bslib::layout_column_wrap(
      width = NULL,
      fill = FALSE,
      style = bslib::css(grid_template_columns = "2fr 1fr"),
      photoBio,
      bslib::layout_column_wrap(
        width = NULL,
        fill = FALSE,
        style = bslib::css(flex_direction = "column"),
        yt,
        insta,
        tiktok
      )
    )
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

  shiny::observe({
    session$sendCustomMessage("set_team", input$team)
    shiny::updateSelectizeInput(session, "team", selected = input$team)

    session$sendCustomMessage("set_cheerleader", input$cheerleader)
    shiny::updateSelectizeInput(session, "cheerleader", selected = input$cheerleader)
  })

  output$leaders <- shiny::renderUI({

    yt <- bslib::card(
      id = "ytleader",
      full_screen = TRUE,
      bslib::card_header(
        style = paste("background-color: #ff0000; color: #ffffff;"),
        "Top 5 YouTube Subscribers/Views"
      ),
      bslib::card_body(
        fillable = TRUE,
        gt::gt_output("leaderYT")
      ),
      bslib::card_footer(
        "Click Cheerleader to view.",
        class = "bg-info"
      )
    )

    inst <- bslib::card(
      id = "instleader",
      full_screen = TRUE,
      bslib::card_header(
        style = paste("background-color: #000000; color: #ffffff;"),
        "Top 5 Insagram Followers"
      ),
      bslib::card_body(
        fillable = TRUE,
        gt::gt_output("leaderInst")
      ),
      bslib::card_footer(
        "Click Cheerleader to view.",
        class = "bg-info"
      )
    )

    tt <- bslib::card(
      id = "ttleader",
      full_screen = TRUE,
      bslib::card_header(
        style = paste("background-color: #000000; color: #ffffff;"),
        "Top 5 TikTok Followers/Likes"
      ),
      bslib::card_body(
        fillable = TRUE,
        gt::gt_output("leaderTT")
      ),
      bslib::card_footer(
        "Click Cheerleader to view.",
        class = "bg-info"
      )
    )

    bslib::layout_column_wrap(
      width = 1/2,
      heights_equal = "row",
      yt, inst, tt
    )
  })

  output$leaderYT <- gt::render_gt({
    ultra_combo |>
      dplyr::select(team,
                    logo,
                    photo,
                    link,
                    subs,
                    views,
                    count,
                    avg_views_per_video) |>
      dplyr::arrange(dplyr::desc(views)) |>
      dplyr::slice_head(n = 5) |>

      gt::gt() |>
      gt::cols_hide(columns = c(team)) |>
      gt::fmt_markdown(columns = c(logo, photo, link)) |>
      gt::cols_label(
        logo = "Team Logo",
        photo = "Cheerleader",
        link = "Name",
        subs = "Subscribers",
        views = "Views",
        count = "Videos",
        avg_views_per_video = "Avg Views/Video"
      ) |>
      gt::tab_style(
        style = gt::cell_text(align = "center"),
        locations = gt::cells_column_labels(dplyr::everything())
      ) |>
      gt::tab_style(
        style = gt::cell_text(align = "center"),
        locations = gt::cells_body(columns = dplyr::everything())
      ) |>
      gt::fmt_number(
        columns = c(subs, views, count, avg_views_per_video),
        decimals = 0,
        use_seps = TRUE
      )
  })

  output$leaderInst <- gt::render_gt({
    ultra_combo |>
      dplyr::select(team,
                    logo,
                    photo,
                    link,
                    instagram_followers) |>
      dplyr::arrange(dplyr::desc(instagram_followers)) |>
      dplyr::slice_head(n = 5) |>

      gt::gt() |>
      gt::cols_hide(columns = c(team)) |>
      gt::fmt_markdown(columns = c(logo, photo, link)) |>
      gt::cols_label(
        logo = "Team Logo",
        photo = "Cheerleader",
        link = "Name",
        instagram_followers = "Followers",
      ) |>
      gt::tab_style(
        style = gt::cell_text(align = "center"),
        locations = gt::cells_column_labels(dplyr::everything())
      ) |>
      gt::tab_style(
        style = gt::cell_text(align = "center"),
        locations = gt::cells_body(columns = dplyr::everything())
      ) |>
      gt::fmt_number(
        columns = c(instagram_followers),
        decimals = 0,
        use_seps = TRUE
      )
  })

  output$leaderTT <- gt::render_gt({
    ultra_combo |>
      dplyr::mutate(likes_followers = as.integer(likes/tiktok_followers)) |>
      dplyr::select(team,
                    logo,
                    photo,
                    link,
                    tiktok_followers,
                    likes,
                    likes_followers
                    ) |>
      dplyr::arrange(dplyr::desc(tiktok_followers)) |>
      dplyr::slice_head(n = 5) |>

      gt::gt() |>
      gt::cols_hide(columns = c(team)) |>
      gt::fmt_markdown(columns = c(logo, photo, link)) |>
      gt::cols_label(
        logo = "Team Logo",
        photo = "Cheerleader",
        link = "Name",
        tiktok_followers = "Followers",
        likes = "Likes",
        likes_followers = "Likes/Followers"
      ) |>
      gt::tab_style(
        style = gt::cell_text(align = "center"),
        locations = gt::cells_column_labels(dplyr::everything())
      ) |>
      gt::tab_style(
        style = gt::cell_text(align = "center"),
        locations = gt::cells_body(columns = dplyr::everything())
      ) |>
      gt::fmt_number(
        columns = c(tiktok_followers, likes, likes_followers),
        decimals = 0,
        use_seps = TRUE
      )
  })

} # server

shinyApp(ui = ui, server = server)



















