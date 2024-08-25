
#' Generate stats tab
#'
#' This is the main plot(s) viewable when the app loads.
#' ggplot with aggregate team followers counts and distributions
#' of followers by platform.
#'
#' These are all the UI cards that contain the actual plot outputs
#' @param fat followers aggregate teams
#' @param f1 ggplot2 distribution
#' @param f2 ggplot2 distribution
#' @param f3 ggplot2 distribution
#'
#' @return bslib::page_fillable
#' @keywords internal
#'
makeStatsPage <- function(fat, f1, f2, f3, ajd, ad) {

  bslib::page_fillable(

    # Followers aggregate team ----------

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
          shinycssloaders::withSpinner(
            ui_element = shiny::plotOutput(fat, click = "plot_click", height = '600px'),
            image = "www/favicon-32x32.png",
            image.width = 242,
            image.height = 242,
            caption = "...LOADING..."
          )
        ),
        # bslib::card_footer("Click Team `Logo` to view.", class = "bg-info")
      )
    ),

    # distributions ---------------------

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
        bslib::card_body(
          shiny::plotOutput(f1, height = '100%'))
      ),
      bslib::card(
        id = "f2",
        height = 269,
        full_screen = TRUE,
        bslib::card_header("Capped Average Followers per Platform", class = "bg-dark"),
        bslib::card_body(shiny::plotOutput(f2, height = '100%'))
      ),
      bslib::card(
        id = "f3",
        height = 269,
        full_screen = TRUE,
        bslib::card_header("Log-Transformed Average Followers per Platform", class = "bg-dark"),
        bslib::card_body(shiny::plotOutput(f3, height = '100%'))
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
    ),

    # Age Jitter Dist -------------------

    bslib::layout_columns(
      width = '100%',
      col_widths = 12,
      fixed_width = TRUE,

      bslib::card(
        id = "ajd",
        height = 742,
        full_screen = TRUE,
        bslib::card_header(
          bslib::tooltip(
            shiny::span(
              "Distribution of Followers + Subscribers by Age Group (1 outlier removed for clarity)",
              bsicons::bs_icon("question-circle-fill")
            ),
            "Click Cheerleader to view teh page."),
          class = "bg-dark"
        ),
        bslib::card_body(
          shinycssloaders::withSpinner(
            ui_element = plotly::plotlyOutput(ajd, height = '669px'),
            image = "www/favicon-32x32.png",
            image.width = 242,
            image.height = 242,
            caption = "...LOADING..."
          )
        )
      )
    ),

    # Age Dist --------------------------

    bslib::layout_columns(
      width = '100%',
      col_widths = 12,
      fixed_width = TRUE,

      bslib::card(
        id = "ad",
        height = 650,
        full_screen = TRUE,
        bslib::card_header(
          "Distribution of Social Media Platforms by Age Group",
          class = "bg-dark"),
        bslib::card_body(
          shinycssloaders::withSpinner(
            ui_element = shiny::plotOutput(ad, height = '570px'),
            image = "www/favicon-32x32.png",
            image.width = 242,
            image.height = 242,
            caption = "...LOADING..."
          )
        )
      )
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
#' @keywords internal
#'
makeReactable <- function(leader_data) {

  # remove.packages("reactR")    https://github.com/glin/reactable/issues/388
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
#' @param cheerPhoto namespace for cheerPhoto ui
#' @param cheerBio namespace for cheerBio ui
#'
#' @return Cheerleader bslib UI elements
#' @keywords internal
#'
makeCheerleader <- function(td, smm, cheerleader, cheerPhoto, cheerBio) {

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
      shiny::uiOutput(cheerPhoto),
    ),
    bslib::card_body(
      fillable = TRUE,
      DT::dataTableOutput(cheerBio)
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
#' the YouTube leaderboard for top (x) cheerleaders with the most
#' YouTube followers.
#'
#' @param ultra_combo aggregate cheerleader social media metrics.
#' @param top_count product of number of weeks of historic data and records.
#' ex. 3 weeks data for top 5 cheerleaders = 15
#'
#' @return gt
#' @keywords internal
#'
makegtYT <- function(historic, top_count) {

  historic |>
    dplyr::filter(cat == "youtube") |>
    dplyr::arrange(dplyr::desc(subs)) |>
    dplyr::slice_head(n = top_count()) |>
    dplyr::group_by(name) |>
    dplyr::summarize(
      plot = list(rev(subs)),
      logo = unique(logo),
      photo = unique(photo),
      link = unique(link),
      subs = max(subs),
      views = max(views),
      count = max(count),
      avg_views_per_video = max(avg_views_per_video),
      .groups = "drop") |>

    dplyr::arrange(dplyr::desc(subs)) |>

    gt::gt() |>
    gt::cols_hide(columns = c(name)) |>
    gt::fmt_markdown(columns = c(logo, photo, link)) |>
    gt::cols_label(
      plot = "Trendline",
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
        palette = c("#FF9999", "#FFFF99", "#99FF99"),
        domain = NULL
      )
    ) |>
    gt::tab_style(
      style = gt::cell_text(align = "center"),
      locations = gt::cells_body(columns = everything())
    ) |>
    gt::fmt_number(
      columns = c(subs, views, count, avg_views_per_video),
      decimals = 0,
      use_seps = TRUE
    ) |>
    gt::cols_move(columns = plot, after = subs) |>
    gtExtras::gt_plt_sparkline(plot, type = "shaded", fig_dim = c(7, 30))
}

#' Generate `gt` tables for Instagram Leaderboard
#'
#' Function takes all cheerleader social media metrics and generates
#' the Instagram leaderboard for top (x) cheerleaders with the most
#' Instagram followers.
#'
#' @param ultra_combo aggregate cheerleader social media metrics.
#' @param top_count product of number of weeks of historic data and records.
#' ex. 3 weeks data for top 5 cheerleaders = 15
#'
#' @return gt
#' @keywords internal
#'
makegtInst <- function(historic, top_count) {

  historic |>
    dplyr::filter(cat == "instagram") |>
    dplyr::arrange(dplyr::desc(instagram_followers)) |>
    dplyr::slice_head(n = top_count()) |>
    dplyr::group_by(name) |>
    dplyr::summarize(
      plot = list(rev(instagram_followers)),
      logo = unique(logo),
      photo = unique(photo),
      link = unique(link),
      instagram_followers = max(instagram_followers),
      .groups = "drop") |>

    dplyr::arrange(dplyr::desc(instagram_followers)) |>

    gt::gt() |>
    gt::cols_hide(columns = c(name)) |>
    gt::fmt_markdown(columns = c(logo, photo, link)) |>
    gt::cols_label(
      plot = "Followers Trend",
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
    gt::fmt_number(
      columns = c(instagram_followers),
      decimals = 0,
      use_seps = TRUE
    ) |>
    gt::cols_move(columns = plot, after = instagram_followers) |>
    gtExtras::gt_plt_sparkline(plot, type = "shaded", fig_dim = c(7, 30))

}

#' Generate `gt` tables for TikTok Leaderboard
#'
#' Function takes all cheerleader social media metrics and generates
#' the Tiktok leaderboard for top 5 cheerleaders with the most
#' TikTok followers.
#'
#' @param ultra_combo aggregate cheerleader social media metrics.
#' @param top_count product of number of weeks of historic data and records.
#' ex. 3 weeks data for top 5 cheerleaders = 15
#'
#' @return gt
#' @keywords internal
#'
makegtTT <- function(historic, top_count) {

  historic |>
    dplyr::mutate(likes_followers = as.integer(likes/tiktok_followers)) |>
    dplyr::filter(cat == "tiktok") |>
    dplyr::arrange(dplyr::desc(tiktok_followers)) |>
    dplyr::slice_head(n = top_count()) |>
    dplyr::group_by(name) |>
    dplyr::summarize(
      plot = list(rev(tiktok_followers)),  # Keep historical data for sparklines
      logo = unique(logo),
      photo = unique(photo),
      link = unique(link),
      tiktok_followers = max(tiktok_followers),
      likes = max(likes),
      likes_followers = as.integer(max(likes) / max(tiktok_followers)),
      .groups = "drop") |>

    dplyr::arrange(dplyr::desc(tiktok_followers)) |>

    gt::gt() |>
    gt::cols_hide(columns = c(name)) |>
    gt::fmt_markdown(columns = c(logo, photo, link)) |>
    gt::cols_label(
      plot = "Followers Trend",
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
    gt::fmt_number(
      columns = c(tiktok_followers, likes, likes_followers),
      decimals = 0,
      use_seps = TRUE
    ) |>
    gt::cols_move(columns = plot, after = tiktok_followers) |>
    gtExtras::gt_plt_sparkline(plot, type = "shaded", fig_dim = c(7, 30))
}

#' Generate Leaderboard Cards
#'
#' Houses the 3 gt Leaderboard tables
#'
#' @param leaderYT namespace for youtube leaderboard
#' @param leaderInst namespace for instagram leaderboard
#' @param leaderTT namespace for tiktok leaderboard
#'
#' @return nice bslib card layout
#' @keywords internal
#'
makeLeaderboards <- function(leaderYT, leaderInst, leaderTT) {

  yt <- bslib::card(
    id = "ytleader",
    full_screen = TRUE,
    bslib::card_header(
      style = paste("background-color: #ff0000; color: #ffffff;"),
      bsicons::bs_icon("youtube"),
      "Top YouTube Subscribers/Views"
    ),
    bslib::card_body(
      fillable = TRUE,
      shinycssloaders::withSpinner(
        ui_element = gt::gt_output(leaderYT),
        image = "www/favicon-32x32.png",
        image.width = 242,
        image.height = 242,
        caption = "...LOADING..."
      )
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
      "Top Insagram Followers"
    ),
    bslib::card_body(
      fillable = TRUE,
      shinycssloaders::withSpinner(
        ui_element = gt::gt_output(leaderInst),
        image = "www/favicon-32x32.png",
        image.width = 242,
        image.height = 242,
        caption = "...LOADING..."
      )
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
      "Top TikTok Followers/Likes"
    ),
    bslib::card_body(
      fillable = TRUE,
      shinycssloaders::withSpinner(
        ui_element = gt::gt_output(leaderTT),
        image = "www/favicon-32x32.png",
        image.width = 242,
        image.height = 242,
        caption = "...LOADING..."
      )
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

#' Make the team photo, logo and hat insignia card layouts.
#'
#' @param td reactive team data
#' @param teamPhoto namespace for team photo ui
#' @param teamLogo namespace for team logo ui
#' @param capInsignia namespace for cap insignia ui
#'
#' @return nice bslib card layout
#' @keywords internal
#'
makeTeam <- function(td, teamPhoto, teamLogo, capInsignia) {

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
        shiny::uiOutput(teamPhoto)
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
          shiny::uiOutput(teamLogo)
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
          shiny::uiOutput(capInsignia)
        )
      )
    )
  )
}

#' Shiny app title
#'
#' @return shiny tags for the app title
#' @keywords internal
#'
makeTitle <- function() {
  shiny::tags$div(
    class = "d-flex justify-content-between align-items-center w-100",
    shiny::tags$div(
      class = "d-flex align-items-center",
      shiny::tags$img(
        src = "https://upload.wikimedia.org/wikipedia/en/thumb/5/59/KBO_League.svg/1920px-KBO_League.svg.png",
        height = "55px",
        style = "margin-right: 10px;"
        ),
      shiny::tags$h1("Cheerleaders!", style = "font-family: 'Bangers', cursive;")
    )
  )
}

#' Shiny app navbar menu w/links
#'
#' @return bslib::nav_menu object
#' @keywords internal
#'
makeNavMenu <- function() {
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
}

#' Manage visibility and updates for team/cheerleader UI's
#'
#' Reduce redundancy improve readability
#'
#' @param session Current Shiny Session Environment
#' @param state Current state of reactive input to handle.
#' @param selected_team input$'team-team'
#' @param selected_cheerleader input$'cheer-cheerleader'
#'
#' @return commands to be sent to update the app UI.
#' @keywords internal
#'
updateUI <- function(session,
                     state,
                     team = NULL,
                     cheerleader = NULL) {

  shiny::updateNavbarPage(session, inputId = "tabs", selected = "visual")

  switch(
    state,
    "team" = {
      shinyjs::show("team-ui", asis = TRUE)
      shinyjs::hide("individual-ui", asis = TRUE)
      shiny::updateSelectizeInput(session, "team-team", selected = team)
    },
    "cheer" = {
      shinyjs::hide("team-ui", asis = TRUE)
      shinyjs::show("individual-ui", asis = TRUE)
      shiny::updateRadioButtons(session, "cheer-cheerleader", selected = cheerleader)
    },
    "randteam" = {
      shinyjs::show("team-ui", asis = TRUE)
      shinyjs::hide("individual-ui", asis = TRUE)
      shiny::updateRadioButtons(session, "cheer-cheerleader", selected = character(0))
    },
    "followers" = {
      shinyjs::show("team-ui", asis = TRUE)
      shinyjs::hide("individual-ui", asis = TRUE)
      shiny::updateSelectizeInput(session, "team-team", selected = team)
      shiny::updateRadioButtons(session, "cheer-cheerleader", selected = character(0))
    },
    "default" = {

    }
  )
}





















