# utilities ===================================================================

# Team Page -------------------------------------------------------------------

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

  table_index <- cheerTableIndex(df_tables, "name")

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

  names <- names |> stringr::str_replace("Excellent", "Woo Su-han")
  names <- names |> stringr::str_replace("Joo Eun Lee", "Lee Ju-eun")
  names <- names |> stringr::str_replace("Native People", "Won Min-Ju")
  names <- names |> stringr::str_replace("Excellent", "Woo Su-han")
  names <- names |> stringr::str_replace("binary", "Lee Jin")
  names <- names |> stringr::str_replace("launching", "Jin Su-Hwa")
  names <- names |> stringr::str_replace("calligraphy", "Seo Yeeun")
  names <- names |> stringr::str_replace("this thumb", "Lee Um-ji")
  names <- names |> stringr::str_replace("Irian", "Lee Ri-an")

  setNames(links, names)
}

cheerTableIndex <- function(tables, keyword) {

  for (i in seq_along(tables)) {
    table <- tables[[i]]
    if (ncol(table) == 4) {
      if (any(table$X2 == keyword)) {
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

bioTableIndex <- function(tables, keywords) {

  for (i in seq_along(tables)) {
    table <- tables[[i]]
    if (any(!is.na(table$X1))) {
      if (any(table$X1 %in% keywords)) {
        return(i)
      }
    }
  }
}

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

extractBioTable <- function(tibble_list, values) {

  for (tbl in tibble_list) {
    if (any(tbl[[1]] %in% values, na.rm = TRUE)) {
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

ytChannelStats <- function(yt_link) {

  if (length(yt_link) == 0) {
    return(list(title = NA, count = NA, subs = NA, views = NA))
  }
  yt_link <- yt_link[1]

  if (stringr::str_detect(yt_link, "@")) {
    channel_id <- tryCatch({
      page <- rvest::read_html(yt_link)|> rvest::html_text2()
      sub(".*channel_id=([A-Za-z0-9_-]+).*", "\\1", page)
    }, error = function(e) NULL)
  } else {
    channel_id <- str_extract(yt_link, "(?<=/)[^/.]{24}")
  }

  if (!is.null(channel_id)) {
    channel_stats <- tryCatch({
      tuber::get_channel_stats(channel_id = channel_id)
    }, error = function(e) {
      list(title = NA, count = NA, subs = NA, views = NA)
    })
  } else {
    channel_stats <- list(title = NA, count = NA, subs = NA, views = NA)
  }

  channel_stats
}

tiktokStats <- function(tiktok_link) {

  if (length(tiktok_link) == 0) {
    return(list(name = NA, followers = NA, likes = NA))
  }

  tiktok_link <- tiktok_link[1]

  page <- tryCatch({
    httr2::request(tiktok_link) %>%
      httr2::req_perform() %>%
      httr2::resp_body_string()
  }, error = function(e) {
    return(NULL)
  })

  tt_name <- stringr::str_extract(tiktok_link, "(?<=/)[^/]+/?$")

  if (!is.null(page)) {
    stats <- stringr::str_extract(page, '"stats":\\{[^}]*\\}')
    tt_followers <- str_extract(stats, '(?<=followerCount":)\\d+')
    tt_likes <- str_extract(stats, '(?<=heart":)\\d+')
  } else {
    tt_followers <- NA
    tt_likes <- NA
  }

  list(
    name = tt_name,
    followers = tt_followers,
    likes = tt_likes
  )
}

createCheerleaderUI <- function(team_info, cheerleader, smm) {

  photoBio <- bslib::card(
    bslib::card_header(
      style = paste("background-color:", team_info()$team_color, "; color: #ffffff;"),
      cheerleader()
    ),
    bslib::card_body(
      class = "cardb",
      fillable = FALSE,
      shiny::uiOutput("cheerleaderPhoto"),
    ),
    bslib::card_body(
      fillable = FALSE,
      DT::dataTableOutput("cheerleaderBio")
    )
  )

  if (!any(sapply(smm()$youtube, is.null))) {

    yt <- bslib::card(
      id = "valb",
      class = "cardb",

      bslib::card_header(
        style = paste("background-color:", team_info()$team_color, "; color: #ffffff;"),
        paste0("YouTube Statistics for ", smm()$youtube$title)
      ),

      bslib::value_box(
        title = "Subscribers",
        format(as.numeric(smm()$youtube$subs), big.mark = ","),
        showcase = bsicons::bs_icon("youtube")
      ),
      bslib::value_box(
        title = "Views",
        format(as.numeric(smm()$youtube$views), big.mark = ","),
        showcase = bsicons::bs_icon("film", class = "views")
      ),
      bslib::value_box(
        title = "Videos",
        format(as.numeric(smm()$youtub$count), big.mark = ","),
        showcase = bsicons::bs_icon("camera-video")
      )
    )
  } else {
    yt <- NULL
  }

  # if (!any(sapply(smm()$instagram, is.na))) {
  #
  #   insta <- bslib::card(
  #     id = "valb",
  #     class = "cardb",
  #
  #     bslib::card_header(
  #       style = paste("background-color:", team_color(), "; color: #ffffff;"),
  #       paste0("Instagram Statistics for ",
  #              stringr::str_replace_all(smm()$instagram$name, "/", ""))
  #     ),
  #     bslib::value_box(
  #       title = "Followers",
  #       smm()$instagram$followers,
  #       showcase = bsicons::bs_icon("instagram")
  #     )
  #   )
  # } else {
  #   insta <- NULL
  # }
  #
  if (!any(sapply(smm()$tiktok, is.na))) {

    tiktok <- bslib::card(
      id = "valb",
      class = "cardb",

      bslib::card_header(
        style = paste("background-color:", team_info()$team_color, "; color: #ffffff;"),
        paste0("TikTik Statistics for ", smm()$tiktok$name)
      ),
      bslib::value_box(
        title = "Followers",
        smm()$tiktok$followers,
        showcase = bsicons::bs_icon("tiktok")
      ),
      bslib::value_box(
        title = "Likes",
        smm()$tiktok$likes,
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
      style = bslib::css(grid_template_columns = "1fr"),
      yt, tiktok
    )
  )
}














