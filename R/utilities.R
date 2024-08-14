
#' Generate stats tab
#'
#' This is the main plot(s) viewable when the app loads.
#' ggplot with aggregate team followers counts and distributions
#' of followers by platform.
#'
#' These are all the UI cards that contain the actual plot outputs
#'
#' @return bslib::page_fillable
#'
makeStatsPage <- function() {

  bslib::page_fillable(

    # Followers aggregate team

    bslib::layout_column_wrap(
      width = '900px',
      fixed_width = TRUE,

      style = bslib::css(grid_template_columns = "4fr 1fr"),
      bslib::card(
        id = "stat-fat",
        class = "stat-fat",
        bslib::card_header(
          bslib::tooltip(
            shiny::span(
              "Total Cheerleader Social Media Followers by Team (YouTube, Instagram, TikTok)",
              bsicons::bs_icon("question-circle-fill")
            ),
            "Click Team Logo to view the page."),
          class = "bg-dark"
        ),
        bslib::card_body(
          shiny::plotOutput("fat", click = "plot_click", height = '600px')
        ),
        # bslib::card_footer("Click Team `Logo` to view.", class = "bg-info")
      )
    ),

    # distributions

    bslib::layout_columns(
      width = '100%',
      col_widths = 3,
      fixed_width = TRUE,

      bslib::card(
        id = "f1",
        # class = "f1",
        height = 269,
        full_screen = TRUE,
        # bslib::card_header("Distribution Avg Followers per Platform", class = "bg-dark"),
        bslib::card_header("Average Followers per Platform (95% Percentile)", class = "bg-dark"),
        bslib::card_body(shiny::plotOutput("f1", height = '100%'))
      ),
      bslib::card(
        id = "f2",
        height = 269,
        full_screen = TRUE,
        bslib::card_header("Capped Average Followers per Platform", class = "bg-dark"),
        bslib::card_body(shiny::plotOutput("f2", height = '100%'))
      ),
      bslib::card(
        id = "f3",
        height = 269,
        full_screen = TRUE,
        bslib::card_header("Log-Transformed Average Followers per Platform", class = "bg-dark"),
        bslib::card_body(shiny::plotOutput("f3", height = '100%'))
      ),

      bslib::card(
        id = "legend-card",
        class = "legend-card",
        bslib::card_header("Legend", class = "bg-dark text-white"),
        bslib::card_body(
          # Using HTML to create the legend layout
          tags$div(
            class = "legend-container",
            tags$div(class = "legend-item", tags$div(class = "color-box red"), tags$span("YouTube")),
            tags$div(
              class = "legend-item",
              tags$div(class = "color-box purple"),
              tags$span("Instagram")
            ),
            tags$div(
              class = "legend-item",
              tags$div(class = "color-box black"),
              tags$span("TikTok")
            )
          )
        ),
        bslib::card_footer("Average Followers Aggregate Teams.", class = "bg-info")
      )
      # bslib::card(
      #   id = "f4",
      #   height = 269,
      #   full_screen = TRUE,
      #   bslib::card_header("Average Followers per Platform (95% Percentile)", class = "bg-dark"),
      #   bslib::card_body(shiny::plotOutput("f4", height = '100%'))
      # )
    )
  )
}

#' Generate the reactable table
#'
#' Reactable table with Cheerleader Teams and individual cheerleader
#' stats such as total followers, YouTube subscribers, Instagram followers,
#' and TikTok followers arranged in descending order.
#'
#' @param leader_data reactive list of ultra_combo and teams
#'
#' @return `reactable::reactable`
#'
makeReactable <- function(leader_data) {

  # remove.packages("reactR)    https://github.com/glin/reactable/issues/388
  # devtools::install_github("react-R/reactR@v0.5.0")

  reactable::reactable(
    data = leader_data()$teams,
    columns = list(
      team = reactable::colDef(
        name = "Team",
        cell = function(value) {
          img_src <- knitr::image_uri(sprintf("www/team_logo/%s.png", value))
          shiny::div(
            class = "team",
            shiny::img(alt = "", src = img_src, height = "42px", alt = ""),
            shiny::div(shiny::span(class = "team-name", value))
          )
        }
      ),
      members = reactable::colDef(name = "Members")
    ),
    details = function(index) {
      cheerleaders <- leader_data()$uc |>
        dplyr::filter(team == leader_data()$teams$team[index]) |>
        dplyr::arrange(desc(followers)) |>
        dplyr::select(name, link, followers, subs,
                      instagram_followers,
                      tiktok_followers, team)

      reactable::reactable(
        data = cheerleaders,
        defaultPageSize = 20,
        columns = list(
          name = reactable::colDef(
            name = "Cheerleader",
            width = 242,
            headerStyle = list(fontWeight = 700),
            cell = function(value, index) {
              img_src <- knitr::image_uri(
                sprintf("www/cheerleader_img/%s.png", value))
              shiny::div(
                class = "cheerleader-photo-container",
                style = "display: flex;",
                shiny::tags$img(
                  alt = "",
                  src = img_src,
                  height = "50px",
                  class = "cheerleader-photo",
                  `data-team` = cheerleaders$team[index],
                  `data-name` = value
                ),
                shiny::div(shiny::span(class = "team-name", value))
              )
            }
          ),
          link = reactable::colDef(
            name = "Profile Link",
            width = 142,
            cell = function(value) {
              shiny::tags$a("namuwiki.com",
                            href = paste0(wiki_url, value),
                            target = "_blank")
            }
          ),
          followers = reactable::colDef(
            name = "Total Followers",
            width = 242,
            cell = function(value) {
              width <- paste0(value * 100 / max(cheerleaders$followers), "%")
              value <- format(value, big.mark = ",")
              value <- format(value, width = 9, justify = "right")
              bar <- shiny::div(
                class = "bar-chart",
                style = list(marginRight = "0.375rem"),
                shiny::div(
                  class = "bar",
                  style = list(width = width, backgroundColor = "#fc5185")
                )
              )
              shiny::div(class = "bar-cell",
                         shiny::span(class = "number", value),
                         bar)
            }
          ),
          subs = reactable::colDef(
            name = "YouTube Subscribers",
            width = 242,
            cell = function(value) {
              width <- paste0(value * 100 / max(cheerleaders$subs), "%")
              value <- format(value, big.mark = ",")
              value <- format(value, width = 9, justify = "right")
              if (max(cheerleaders$subs) == 0) {
                shiny::div(class = "bar-cell", shiny::span(class = "number", value))
              } else {
                bar <- shiny::div(
                  class = "bar-chart",
                  style = list(marginRight = "0.375rem"),
                  shiny::div(
                    class = "bar",
                    style = list(
                      width = width,
                      backgroundColor = "#ff0000"
                    )
                  )
                )
                shiny::div(class = "bar-cell", shiny::span(class = "number", value), bar)
              }
            }
          ),
          instagram_followers = reactable::colDef(
            name = "Instagram Followers",
            width = 242,
            cell = function(value) {
              width <- paste0(value * 100 / max(cheerleaders$instagram_followers), "%")
              value <- format(value, big.mark = ",")
              value <- format(value, width = 9, justify = "right")
              bar <- shiny::div(
                class = "bar-chart",
                style = list(marginRight = "0.375rem"),
                shiny::div(
                  class = "grade-bar",
                  style = list(width = width)
                )
              )
              shiny::div(class = "bar-cell", shiny::span(class = "number", value), bar)
            }
          ),
          tiktok_followers = reactable::colDef(
            name = "TikTok Followers",
            width = 242,
            cell = function(value) {
              width <- paste0(value * 100 / max(cheerleaders$tiktok_followers), "%")
              value <- format(value, big.mark = ",")
              value <- format(value, width = 9, justify = "right")
              if (max(cheerleaders$tiktok_followers) == 0) {
                shiny::div(class = "bar-cell", shiny::span(class = "number", value))
              } else {
                bar <- shiny::div(
                  class = "bar-chart",
                  style = list(marginRight = "0.375rem"),
                  shiny::div(
                    class = "bar",
                    style = list(
                      width = width,
                      backgroundColor = "#000000"
                    )
                  )
                )
                shiny::div(class = "bar-cell", shiny::span(class = "number", value), bar)
              }
            }
          ),
          team = reactable::colDef(
            show = FALSE
          )
        ), # list
        outlined = TRUE,
        highlight = TRUE
      )
    },
    onClick = "expand",
    rowStyle = list(cursor = "pointer")
  )
}

#' Generate Cheerleader Image, Biography and Social Metric cards.
#'
#' @param td team_data reactive
#' @param cheerleader input$cheerleader selected cheerleader
#' @param smm social media metrics reactive
#'
#' @return Cheerleader bslib UI elements
#'
makeCheerleader <- function(td, cheerleader, smm) {

  yt <- dplyr::filter(smm()$youtube, name == cheerleader)
  inst <- dplyr::filter(smm()$instagram, name == cheerleader)
  tt <- dplyr::filter(smm()$tiktok, cheername == cheerleader)

  yt <- yt |> dplyr::filter(views > 200000)

  photoBio <- bslib::card(
    full_screen = TRUE,
    bslib::card_header(
      style = paste("background-color:", td()$color, "; color: #ffffff;"),
      cheerleader
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
}

#' Generate `gt` tables for YouTube Leaderboard
#'
#' Function takes all cheerleader social media metrics and generates
#' the YouTube leaderboard for top 5 cheerleaders with the most
#' YouTube followers.
#'
#' @param ultra_combo aggregate cheerleader social media metrics.
#'
#' @return
#'
makegtYT <- function(ultra_combo) {

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
      link = "Name/Wiki",
      subs = "Subscribers",
      views = "Views",
      count = "Videos",
      avg_views_per_video = "Views/Video"
    ) |>
    gt::data_color(
      columns = c(avg_views_per_video),
      fn = scales::col_numeric(
        #palette = c("red", "yellow", "green"),
        palette = c("#FF9999", "#FFFF99", "#99FF99"),
        domain = NULL)
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
}

#' Generate `gt` tables for Instagram Leaderboard
#'
#' Function takes all cheerleader social media metrics and generates
#' the Instagram leaderboard for top 5 cheerleaders with the most
#' Instagram followers.
#'
#' @param ultra_combo aggregate cheerleader social media metrics.
#'
#' @return
#'
makegtInst <- function(ultra_combo) {

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
      link = "Name/Wiki",
      instagram_followers = "Followers",
    ) |>
    gt::data_color(
      columns = c(instagram_followers),
      fn = scales::col_numeric(
        palette = c("#FF9999", "#FFFF99", "#99FF99"),
        domain = NULL)
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
}

#' Generate `gt` tables for TikTok Leaderboard
#'
#' Function takes all cheerleader social media metrics and generates
#' the Tiktok leaderboard for top 5 cheerleaders with the most
#' TikTok followers.
#'
#' @param ultra_combo aggregate cheerleader social media metrics.
#'
#' @return
#'
makegtTT <- function(ultra_combo) {

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
      link = "Name/Wiki",
      tiktok_followers = "Followers",
      likes = "Likes",
      likes_followers = "Likes/Followers"
    ) |>
    gt::data_color(
      columns = c(likes_followers),
      fn = scales::col_numeric(
        palette = c("#FF9999", "#FFFF99", "#99FF99"),
        domain = NULL)
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
}

#' Generate Leaderboard Cards
#'
#' Houses the 3 Leaderboards
#'
#' @return
#'
makeLeaderboards <- function() {

  yt <- bslib::card(
    id = "ytleader",
    full_screen = TRUE,
    bslib::card_header(
      style = paste("background-color: #ff0000; color: #ffffff;"),
      bsicons::bs_icon("youtube"),
      "Top 5 YouTube Subscribers/Views"
    ),
    bslib::card_body(
      fillable = TRUE,
      gt::gt_output("leaderYT")
    ),
    bslib::card_footer(
      "Click Team Logo/Cheerleader Photo to view.",
      style = paste("background-color: #ff0000; color: #ffffff;")
    )
  )

  inst <- bslib::card(
    id = "instleader",
    full_screen = TRUE,
    bslib::card_header(
      style = paste(
        "fill: rgba(255, 255, 255, 0.6) !important;
        background: linear-gradient(45deg, #FFD600, #FF7A00, #FF0069, #D300C5, #7638FA);"
      ),
      bsicons::bs_icon("instagram"),
      "Top 5 Insagram Followers"
    ),
    bslib::card_body(
      fillable = TRUE,
      gt::gt_output("leaderInst")
    ),
    bslib::card_footer(
      "Click Team Logo/Cheerleader Photo to view.",
      style = paste(
        "fill: rgba(255, 255, 255, 0.6) !important;
        background: linear-gradient(45deg, #FFD600, #FF7A00, #FF0069, #D300C5, #7638FA);"
      )
    )
  )

  tt <- bslib::card(
    id = "ttleader",
    full_screen = TRUE,
    bslib::card_header(
      style = paste("background-color: #000000; color: #ffffff;"),
      bsicons::bs_icon("tiktok"),
      "Top 5 TikTok Followers/Likes"
    ),
    bslib::card_body(
      fillable = TRUE,
      gt::gt_output("leaderTT")
    ),
    bslib::card_footer(
      "Click Team Logo/Cheerleader Photo to view.",
      style = paste("background-color: #000000; color: #ffffff;")
    )
  )

  bslib::layout_column_wrap(
    width = '60%',
    fixed_width = TRUE,
    heights_equal = "row",
    yt,
    inst,
    tt
  )
}














