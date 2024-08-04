# UI ==========================================================================

ui <- bslib::page_sidebar(

  shinyjs::useShinyjs(),

  theme = bslib::bs_theme(version = 5) |>
    bslib::bs_add_rules("
      body{
        font-size: 12px;
      }
      .cardt {
        /* max-height: 600px; */
      }
      .cardh {
        background-color: #ff0000 !important;
        height: 42px;
      }
      .valb {
        /* border-width: 0; */
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
  "),



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
      shiny::tags$h1("Cheerleaders!")
    ),

    shiny::tags$div(
      class = "d-flex align-items-center",
      shiny::tags$img(src = "https://shiny.posit.co/images/shiny-solo.png", class = "fcon"),
      shiny::tags$img(src = "https://glue.tidyverse.org/logo.png", class = "fcon"),
      shiny::tags$img(src = "https://httr2.r-lib.org/logo.png", class = "fcon"),
      shiny::tags$img(src = "https://purrr.tidyverse.org/logo.png", class = "fcon"),
      shiny::tags$img(src = "https://rvest.tidyverse.org/logo.png", class = "fcon"),
      shiny::tags$img(src = "shinyjs.png", class = "fcon"),
      shiny::tags$img(src = "https://stringr.tidyverse.org/logo.png", class = "fcon"),
      shiny::tags$img(src = "https://avatars.githubusercontent.com/u/22032646?s=200&v=4", class = "fcon")
    )
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

  shinyjs::hide("team")
  shinyjs::hide("valb")
  shinyjs::hide("individual")

  # populate cheerleader dropdown

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
      choices = cheerleaders,
      selected = character(0)
    )
    })

  team_page <- shiny::reactive(label = "Team page content", {

    shiny::req(input$team != "")

    url <- paste0(input$team)

    httr2::request(url) |>
      httr2::req_perform() |>
      httr2::resp_body_html()
    })

  team_name <- shiny::reactive(label = "Team name", {
    names(teams)[which(unlist(teams) == input$team)]
  })

  team_color <- shiny::reactive(label = "Team colors", {
    team_colors[team_name()][[1]]
  })

  cheerleader_page <- shiny::reactive(label = "Cheerleader page content", {

    shiny::req(input$team != "")

    url <- paste0(wiki_url, input$cheerleader)

    page <- httr2::request(url) |>
      httr2::req_perform() |>
      httr2::resp_body_html()

    xml_tables <- rvest::html_nodes(page, "table")

    df_tables <- lapply(xml_tables, rvest::html_table, fill = TRUE)

    index <- bioTableIndex(df_tables, "nationality")

    bio_table <- xml_tables[[index]]

    hrefs <- bio_table |>
      html_nodes("a") |>
      html_attr("href")

    social_links <- hrefs[grepl("http", hrefs)]

    social_icons <- matchIconsToLinks(social_links, keyword_image_mapping)

    # account for missing values
    social_links <- social_links[social_icons != ""]
    social_icons <- social_icons[social_icons != ""]

    table <- extractBioTable(df_tables, "nationality")

    html_content <- createHTML(social_links, social_icons)

    table <- table |>
      dplyr::mutate(X2 = ifelse(X1 == "link", html_content, X2)) |>
      dplyr::mutate(X2 = ifelse(X1 == "site", html_content, X2)) |>
      dplyr::mutate(X2 = ifelse(X1 == "SNS", html_content, X2))

    # Multi-column case
    if (ncol(table) == 2) {
      table <- table |>
        dplyr::filter((X1 != X2)) |>
        dplyr::filter(!X1 %in% c("support team", "platform", "signature")) |>
        dplyr::filter(!grepl("youtube", X1, ignore.case = TRUE))

    } else if (ncol(table) == 3) {
      table <- table |>
        dplyr::select(-X1) |>
        dplyr::rename(X1 = X2) |>
        dplyr::rename(X2 = X3) |>
        dplyr::filter((X1 != X2)) |>
        dplyr::filter(!X1 %in% c("support team", "platform", "signature"))
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
      bio_table = bio_table
      )
  })


  shiny::observe(label = "Show/Hide Team", {

    if (input$team != "") {

      shinyjs::show("team")
      shinyjs::show("valb")
      shinyjs::hide("individual")

      output$team <- shiny::renderUI({
        bslib::layout_columns(
          bslib::card(
            id = "teamCard",
            class = "cardt",
            bslib::card_header(
              # class = "cardh",
              style = paste("background-color:", team_color(), "; color: #ffffff;"),
              team_name()
            ),
            bslib::card_body(
              fillable = FALSE,
              fill = FALSE,
              shiny::uiOutput("teamPhoto", inline = TRUE)
            )
          )
        )
      })
    }
  }) |>
    shiny::bindEvent(input$team)

  shiny::observe(label = "Show/Hide Cheerleader", {

    if (input$cheerleader != "") {

      shinyjs::hide("team")
      shinyjs::hide("valb")
      shinyjs::show("individual")

      cheerleaders <- getCheerleaders(input$team)

      cheerleader <- names(cheerleaders)[which(unlist(cheerleaders) == input$cheerleader)]

      output$individual <- shiny::renderUI({

        bslib::card(
          bslib::card_header(
            style = paste("background-color:", team_color(), "; color: #ffffff;"),
            cheerleader
          ),
          shiny::uiOutput("cheerleaderPhoto", inline = TRUE ),
          DT::dataTableOutput("cheerleaderBio")
        )
      })
    }
  }) |>
    shiny::bindEvent(input$cheerleader)


  output$teamPhoto <- shiny::renderUI({

    page <- team_page()

    photo_url <- getTeamPhoto(page)

    team <- names(teams)[teams == input$team]

    if (team == "KT Wiz") {
      shiny::img(
        src = "https://kpopping.com/documents/57/4/850/Wiz-N-fullPicture.webp?v=87cb1", height = "100%")
    } else if (team == "Samsung Lions") {
      shiny::img(
        src = "https://www.samsunglions.com/en/img/img_cheerleader2017_en.jpg", height = "100%")
    } else {
      shiny::img(src = photo_url, height = "100%")
    }
  })

  output$cheerleaderPhoto <- shiny::renderUI({

    bio_table <- cheerleader_page()$bio_table

    photo_url <- getCheerleaderPhoto(bio_table)

    shiny::img(src = photo_url, height = "420px")
  })

  output$cheerleaderBio <- DT::renderDataTable({

    DT::datatable(
      data = cheerleader_page()$table,

      colnames = NULL,
      escape = FALSE,

      options = list(
        dom = "t",
        columnDefs = list(
          list(visible = FALSE, targets = 0),

          list(width = '1000px', targets = 1)


        ),
        ordering = FALSE,
        pageLength = -1
      )
    )
  })

} # server


# utilities ===================================================================

# Cheerleader Team Page -------------------------------------------------------

getTeamPhoto <- function(page) {

  page |>
    rvest::html_node("tr td img[src$='.webp']") |>
    rvest::html_attr("src")
}

getCheerleaders <- function(team_url) {

  html_content <- httr2::request(team_url) |>
    httr2::req_perform() |>
    httr2::resp_body_html()

  xml_tables <- rvest::html_nodes(html_content, "table")

  df_tables <- lapply(xml_tables, rvest::html_table, fill = TRUE)

  table_index <- cheerTableIndex(df_tables)

  cheerleader_table <- xml_tables[[table_index]]

  names <- cheerleader_table |>
    rvest::html_nodes("a") |>
    rvest::html_text()

  links <- cheerleader_table |>
    rvest::html_nodes("a") |>
    rvest::html_attr("href")

  idx_keep <- !grepl("\\[\\d+\\]", names)

  names <- stringr::str_to_title(names[idx_keep])
  links <- links[idx_keep]

  setNames(links, names)
}

cheerTableIndex <- function(tables) {

  for (i in seq_along(tables)) {
    table <- tables[[i]]
    if (ncol(table) == 4) {
      if (any(table$X2 == "name")) {
        return(i)
      }
    }
  }
  return(NULL)
}

# Cheerleader Page ------------------------------------------------------------

getCheerleaderPhoto <- function(bio_table) {

  bio_images <- bio_table |>
    rvest::html_nodes("tr td img[src$='.webp']") |>
    rvest::html_attr("src")

  bio_images[1]
}

# Helpers ---------------------------------------------------------------------

matchIconsToLinks <- function(links, mapping) {

  matched_icons <- vector("character", length(links))
  for (i in seq_along(links)) {
    link <- links[i]
    for (keyword in names(mapping)) {
      if (grepl(keyword, link, ignore.case = TRUE)) {
        matched_icons[i] <- mapping[[keyword]]
        break
      }
    }
  }
  return(matched_icons)
}

extractBioTable <- function(tibble_list, value) {

  for (tbl in tibble_list) {
    if (any(tbl[[1]] == value, na.rm = TRUE)) {
      tbl <- tbl |> dplyr::select(dplyr::where(~ !any(is.na(.))))
      return(tbl)
    }
  }
}

createHTML <- function(links, icons) {

  purrr::map2(links, icons, ~glue::glue(
    '<a href="{.x}" target="_blank"><img src="{.y}" width="42" height="42"></a>&nbsp'
  )) |> paste(collapse = "")
}

bioTableIndex <- function(tables, keyword) {

  for (i in seq_along(tables)) {
    table <- tables[[i]]
    if (any(!is.na(table$X1))) {
      if (any(table$X1 == keyword)) {
        return(i)
      }
    }
  }
}












shinyApp(ui = ui, server = server)








