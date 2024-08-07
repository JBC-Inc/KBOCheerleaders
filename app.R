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
      .valb {
        /* border-width: 0; */
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
    .html-fill-container {
      display: block !important;
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

    # shiny::tags$div(
    #   class = "d-flex align-items-center",
    #   shiny::tags$img(src = "https://shiny.posit.co/images/shiny-solo.png", class = "fcon"),
    #   shiny::tags$img(src = "https://glue.tidyverse.org/logo.png", class = "fcon"),
    #   shiny::tags$img(src = "https://httr2.r-lib.org/logo.png", class = "fcon"),
    #   shiny::tags$img(src = "https://purrr.tidyverse.org/logo.png", class = "fcon"),
    #   shiny::tags$img(src = "https://rvest.tidyverse.org/logo.png", class = "fcon"),
    #   shiny::tags$img(src = "shinyjs.png", class = "fcon"),
    #   shiny::tags$img(src = "https://stringr.tidyverse.org/logo.png", class = "fcon"),
    #   shiny::tags$img(src = "https://avatars.githubusercontent.com/u/22032646?s=200&v=4", class = "fcon")
    # )
  ),

  sidebar = bslib::sidebar(

    shiny::selectizeInput(
      inputId = "team",
      label = "Teams:",
      choices = c("", teams)
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

  # shiny::observe(label = "Authenticate", priority = 300, {
  #   if (is.null(global_token)) {
  #     authenticateYouTube()
  #   }
  # })

  shinyjs::hide("team")
  shinyjs::hide("valb")
  shinyjs::hide("individual")

  team_page <- shiny::reactive(label = "Team page content", {

    shiny::req(input$team != "")

    url <- paste0(input$team)

    page <- httr2::request(url) |>
      httr2::req_perform() |>
      httr2::resp_body_html()

    photo_url <- getTeamPhoto(page)

    list(
      page = page,
      photo_url = photo_url
      )
    })

  team_info <- shiny::reactive(label = "Team name/colors", {

    shiny::req(input$team)

    team_name <- names(teams)[which(unlist(teams) == input$team)]

    team_color <- team_colors[team_name][[1]]

    list(
      name = team_name,
      color = team_color
    )
  })

  cheerleader_page <- shiny::reactive(label = "Cheerleader page content", {

    shiny::req(input$team != "")

    page <- fetchCheerleaderPage(wiki_url, input$cheerleader)

    if (is.null(page)) {
      return(
        list(
          table = data.frame("NA" = "The requested cheerleader page is not available."),
          bio_table = data.frame(),
          links = data.frame()
        )
      )
    }

    # shiny::req(input$team != "")
    #
    # url <- paste0(wiki_url, input$cheerleader)
    #
    # page <- httr2::request(url) |>
    #   httr2::req_perform() |>
    #   httr2::resp_body_html()

    xml_tables <- rvest::html_nodes(page, "table")

    df_tables <- lapply(xml_tables, rvest::html_table, fill = TRUE)

    index <- bioTableIndex(df_tables, c("nationality", "birth"))

    bio_table <- xml_tables[[index]]

    hrefs <- bio_table |>
      rvest::html_nodes("a") |>
      rvest::html_attr("href")

    social_links <- hrefs[grepl("http", hrefs)]

    social_icons <- matchIconsToLinks(social_links, keyword_image_mapping)

    # account for missing values
    social_links <- social_links[social_icons != ""]
    social_icons <- social_icons[social_icons != ""]

    table <- extractBioTable(df_tables, c("nationality", "birth"))

    html_content <- createHTML(social_links, social_icons)

    # sometimes tables have 2 row headers
    if (ncol(table) == 2) {
      table <- table |>
        dplyr::mutate(X2 = dplyr::if_else(X1 %in% c("link", "site", "SNS"), html_content, X2)) |>
        dplyr::filter(X1 != X2,
                      !X1 %in% c("support team", "platform", "signature"),
                      !grepl("youtube", X1, ignore.case = TRUE))
    } else if (ncol(table) == 3) {
      table <- table |>
        dplyr::mutate(X3 = dplyr::if_else(X1 %in% c("link", "site", "SNS"), html_content, X3)) |>
        dplyr::select(-X1) |>
        dplyr::rename(X1 = X2, X2 = X3) |>
        dplyr::filter(X1 != X2,
                      !X1 %in% c("support team", "platform", "signature"))
    }

    table <- table |>
      dplyr::mutate(X1 = stringr::str_to_title(X1)) |>
      dplyr::mutate(X2 = ifelse(
        !stringr::str_detect(X1, "Link|Site|Sns"),
        stringr::str_to_title(X2),
        X2)) |>

      # blood type
      dplyr::mutate(X2 = stringr::str_replace_all(X2, "(?i)\\bab\\b", "AB")) |>

      # Myers-Briggs Type Indicator
      dplyr::mutate(X1 = dplyr::case_when(
        X1 == "Mbti" ~ "MBTI",
        TRUE ~ X1)) |>
      dplyr::mutate(X2 = dplyr::case_when(
        X1 == "MBTI" ~ stringr::str_to_upper(X2),
        TRUE ~ X2
      )) |>

      # birth instances
      dplyr::mutate(previous_birth = cumsum(X1 == "Birth")) |>
      dplyr::mutate(X1 = dplyr::case_when(
        X1 == "Birth" ~ "Birthday",
        TRUE ~ X1
      )) |>
      dplyr::mutate(X1 = dplyr::case_when(
        X1 == "Birthday" & previous_birth > 1 ~ "Birthplace",
        TRUE ~ X1
      )) |>

      # agency instances
      dplyr::mutate(previous_agency = cumsum(X1 == "Agency")) |>
      dplyr::mutate(X1 = dplyr::case_when(
        X1 == "Agency" & previous_agency > 1 ~ "previous_agency",
        TRUE ~ X1
      )) |>
      dplyr::filter(X1 != "previous_agency") |>

      # cleanup
      dplyr::mutate(X2 = stringr::str_replace_all(X2, "\\[.*?\\]", "")) |>
      dplyr::mutate(X2 = stringr::str_replace_all(X2, "\\(\\s*", "("),
                    X2 = stringr::str_replace_all(X2, "\\s*\\)", ")")) |>
      dplyr::select(-previous_birth, -previous_agency)

    list(
      table = table,
      bio_table = bio_table,
      links = social_links
      )
  })

  cheerleader <- shiny::reactive(label = "Cheerleader name", {

    cheerleaders <- getCheerleaders(input$team)

    names(cheerleaders)[which(unlist(cheerleaders) == input$cheerleader)]
  })

  smm <- shiny::reactive(label = "Social Media Metrics", {

    links <- cheerleader_page()$links

    yt_link <- links[grepl("youtube", links)]
    insta_link <- links[grepl("instagram", links)]
    tiktok_link <- links[grepl("tiktok", links)]

    youtube <- ytChannelStats(yt_link)

    tiktok <- tiktokStats(tiktok_link)

    # # Instagram ---------------------------------------------------------------
    #
    # if (length(insta_link) != 0 ) {
    #   meta_content <- rvest::read_html(insta_link) |>
    #     as.character()
    #
    #   # page <- httr2::request(insta_link) |>
    #   #   httr2::req_perform() |>
    #   #   httr2::resp_body_json()
    #   #
    #   # parsed <- rvest::read_html(page)
    #   #
    #   # head <- rvest::html_element(parsed, "head")
    #
    #   inst_followers <- stringr::str_extract(
    #     string = meta_content,
    #     pattern = '(?<=content=")(.*?)(?= Followers)'
    #   )
    #
    #   inst_name <- stringr::str_extract(insta_link, "(?<=/)[^/]+/?$")
    # } else {
    #   inst_followers <- NA
    #   inst_name <- NA
    # }
    #
    list(
      youtube = youtube,
      tiktok = tiktok
    )
  }) |>
    shiny::bindEvent(input$cheerleader)

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

    shiny::req(input$team != "")

    cheerleaders <- getCheerleaders(input$team)

    # shiny::selectizeInput(
    #   inputId = "cheerleader",
    #   label = "Select Cheerleader:",
    #   choices = c("", cheerleaders)
    #   )

    shiny::radioButtons(
      inputId = "cheerleader",
      label = "Cheerleaders:",
      choices = cheerleaders, # cheerleaders[order(names(cheerleaders))],
      selected = character(0)
    )
  })

  # main ----------------------------------------------------------------------

  output$team <- shiny::renderUI({

    shiny::req(team_info())

    bslib::layout_column_wrap(
      width = NULL,
      fill = FALSE,
      style = bslib::css(grid_template_columns = "2.5fr 1fr"),

      bslib::card(
        id = "teamCard",

        bslib::card_header(
          style = paste("background-color:", team_info()$color, "; color: #ffffff;"),
          team_info()$name
        ),
        bslib::card_body(
          class = "cardb",
          shiny::uiOutput("teamPhoto") # , inline = TRUE)
        )
      ),

      bslib::layout_column_wrap(
        width = NULL,
        heights_equal = "row",

        bslib::card(
          id = "teamLogoCard",
          min_height = 324,
          max_height = 324,
          bslib::card_header(
            style = paste("background-color:", team_info()$color, "; color: #ffffff;"),
            paste0(team_info()$name, " Team Logo")
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
            style = paste("background-color:", team_info()$color, "; color: #ffffff;"),
            paste0(team_info()$name, " Cap Insignia")
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

    shiny::req(team_info())
    shiny::req(cheerleader())
    shiny::req(smm())

    createCheerleaderUI(team_info, cheerleader, smm)
  })

  # main team components ------------------------------------------------------

  output$teamPhoto <- shiny::renderUI({

    team <- names(teams)[teams == input$team]

    if (team == "KT Wiz") {
      shiny::img(
        src = "https://i.ytimg.com/vi/OeCJXyFxJDQ/maxresdefault.jpg",
        # src = "https://kpopping.com/documents/57/4/850/Wiz-N-fullPicture.webp?v=87cb1",
        style = "height: 600px;"
        )
    } else if (team == "Samsung Lions") {
      shiny::img(
        src = "https://www.samsunglions.com/en/img/img_cheerleader2017_en.jpg", height = "100%")
    } else {
      shiny::img(src = team_page()$photo_url, height = "100%")
    }
  })

  output$teamLogo <- shiny::renderUI({

    logo <- team_logos[names(teams)[teams == input$team]][[1]]

    shiny::img(src = logo, height = "242px")
  })

  output$capInsignia <- shiny::renderUI({

    logo <- cap_insignia[names(teams)[teams == input$team]][[1]]

    shiny::img(src = logo, height = "242px")
  })

  # main cheerleader components -----------------------------------------------

  output$cheerleaderPhoto <- shiny::renderUI({

    bio_table <- cheerleader_page()$bio_table

    photo_url <- getCheerleaderPhoto(bio_table)

    shiny::img(src = photo_url, height = "320px")
  })

  output$cheerleaderBio <- DT::renderDataTable({

    DT::datatable(
      data = cheerleader_page()$table,
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
