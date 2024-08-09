# UI ==========================================================================

ui <- bslib::page_sidebar(

  shinyjs::useShinyjs(),

  theme = bslib::bs_theme(
    version = 5
    # bootswatch = "united"
    # base_font = bslib::font_google("Bangers"),
    ) |>
    bslib::bs_add_rules("
      body{
        font-size: 12px;
      }
      .cardb {
        text-align: center;
      }
      .views {
        color: #ff0000;
      }
      .fcon {
        height: 55px;
        margin-right: 10px;
      }
    "),

  tags$style(type='text/css', "
    .selectize-input {
      font-size: 12px;
      line-height: 1rem;
      border-radius: 7px;
    }
    .selectize-dropdown {
      font-size: 12px;
      line-height: 1rem;
    }
    table td:first-child {
      width: 200px;
    }

    /* card value box formatting */

    .html-fill-container {
      display: block !important;
    }
    .value-box-showcase.html-fill-item.html-fill-container {
      padding: 5px;
    }
    .value-box-area.html-fill-item.html-fill-container {
      padding: 5px;
    }
    .bslib-value-box .value-box-value {
      font-size: 1rem;
    }

    .bslib-grid .bslib-mb-spacing {
      display: flex !important;
    }

  "),

  shiny::tags$head(
    shiny::tags$link(
      rel = "stylesheet",
      href = "https://fonts.googleapis.com/css2?family=Bangers&display=swap")
  ),

  title = shiny::tags$div(
    class = "d-flex justify-content-between align-items-center w-100",

    shiny::tags$div(
      class = "d-flex align-items-center",

      shiny::tags$img(
        src = paste0("https://upload.wikimedia.org/wikipedia/en/thumb/5/59/",
                     "KBO_League.svg/1920px-KBO_League.svg.png"),
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
      choices = c("", team_data$name)
    ),

    shiny::uiOutput("cheerleaderUI")
  ),

  shiny::uiOutput("team"),

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

  shiny::uiOutput("individual")
)

# SERVER ======================================================================

server <- function(input, output, session) {

  shinyjs::hide("team")
  shinyjs::hide("valb")
  shinyjs::hide("individual")

  td <- shiny::reactive(label = "Selected Team Data", {

    shiny::req(input$team)

    list(
      name  = team_data$name[team_data$name == input$team],
      color = team_data$color[team_data$name == input$team],
      photo = team_photos[which(team_data$name == input$team)],
      logo  = team_data$logo[team_data$name == input$team],
      cap   = team_data$insignia[team_data$name == input$team]
    )
  })

  smm <- shiny::reactive(label = "Social Media Metrics", {

    list(
      youtube = youtube,
      instagram = instagram,
      tiktok = tiktok
    )
  })

  shiny::observe(label = "Show/Hide Team", {

    if (input$team != "") {
      shinyjs::show("team")
      shinyjs::show("valb")
      shinyjs::hide("individual")
    }
  }) |>
    shiny::bindEvent(input$team)

  shiny::observe(label = "Show/Hide Cheerleader", {

    if (input$cheerleader != "") {
      shinyjs::hide("team")
      shinyjs::hide("valb")
      shinyjs::show("individual")
    }
  }) |>
    shiny::bindEvent(input$cheerleader)

  # sidebar -------------------------------------------------------------------

  output$cheerleaderUI <- shiny::renderUI({

    cheerleaders <- team_cheerleaders |>
      dplyr::filter(team == input$team) |>
      dplyr::pull(cheerleader)

    shiny::radioButtons(
      inputId = "cheerleader",
      label = "Cheerleaders:",
      choices = cheerleaders,
      selected = character(0)
    )
  })

  # main ----------------------------------------------------------------------

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

  output$individual <- shiny::renderUI({

    yt <- dplyr::filter(smm()$youtube, name == input$cheerleader)
    inst <- dplyr::filter(smm()$instagram, name == input$cheerleader)
    tt <- dplyr::filter(smm()$tiktok, cheername == input$cheerleader)

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
          style = paste("background-color:",
                        td()$color, "; color: #ffffff;"),
          paste0("YouTube Statistics for ", yt$title)
        ),

        bslib::value_box(
          title = "Subscribers",
          format(yt$subs, big.mark = ","),
          showcase = bsicons::bs_icon("youtube")
        ),
        bslib::value_box(
          title = "Views",
          format(yt$views, big.mark = ","),
          showcase = bsicons::bs_icon("film", class = "views")
        ),
        bslib::value_box(
          title = "Videos",
          format(yt$count, big.mark = ","),
          showcase = bsicons::bs_icon("camera-video")
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
          showcase = bsicons::bs_icon("instagram")
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
          style = paste("background-color:",
                        td()$color, "; color: #ffffff;"),
          paste0("TikTik Statistics for ", tt$name)
        ),
        bslib::value_box(
          title = "Followers",
          format(as.numeric(tt$followers), big.mark = ","),
          showcase = bsicons::bs_icon("tiktok")
        ),
        bslib::value_box(
          title = "Likes",
          format(as.numeric(tt$likes), big.mark = ","),
          showcase = bsicons::bs_icon("heart")
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

  # main team components ------------------------------------------------------

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

    shiny::img(src = td()$logo, height = "242px")
  })

  output$capInsignia <- shiny::renderUI({

    shiny::img(src = td()$cap, height = "242px")
  })

  # main cheerleader components -----------------------------------------------

  output$cheerleaderPhoto <- shiny::renderUI({

    image <- paste0("./cheerleader_img/", input$cheerleader, ".png")
    shiny::img(src = image, height = "320px")
  })

  output$cheerleaderBio <- DT::renderDataTable({

    DT::datatable(
      data = cheer_data[[input$cheerleader]]$table,
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
      )
    )
  })

} # server

shinyApp(ui = ui, server = server)



