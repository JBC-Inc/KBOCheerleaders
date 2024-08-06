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

  names <- names |> stringr::str_replace_all(c(
    "Excellent"     = "Woo Su-han",
    "Joo Eun Lee"   = "Lee Ju-eun",
    "Native People" = "Won Min-Ju",
    "Binary"        = "Lee Jin",
    "Launching"     = "Jin Su-Hwa",
    "Calligraphy"   = "Seo Yeeun",
    "this thumb"    = "Lee Um-ji",
    "Irian"         = "Lee Ri-an",
    "This Week"     = "Lee Geum-Ju"
  ))

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

fetchCheerleaderPage <- function(wiki_url, cheerleader) {

  url <- paste0(wiki_url, cheerleader)
  req <- httr2::request(url)

  if (is.null(req)) {
    return(NULL)
  }
  page <- tryCatch({
    resp <- httr2::req_perform(req)
    if (httr2::resp_status(resp) < 400) {
      httr2::resp_body_html(resp)
    } else {
      message("The requested cheerleader page is not available.")
      return(NULL)
    }
  }, error = function(e) {
    # message("Error in fetching the cheerleader page: ", e$message)
    return(NULL)
  })
  return(page)
}

getCheerleaderPhoto <- function(bio_table) {

  if (class(bio_table) == "xml_node") {
  bio_images <- bio_table |>
    rvest::html_nodes("tr td img[src$='.webp']") |>
    rvest::html_attr("src")
  return(bio_images[1])
  } else {
    return(NULL)
  }
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

  extract_channel_id <- function(url) {

    if (stringr::str_detect(url, "@")) {
      page <- tryCatch({
        rvest::read_html(url) %>% rvest::html_text2()
      }, error = function(e) NULL)
      if (!is.null(page)) {
        channel_id <- sub(".*channel_id=([A-Za-z0-9_-]+).*", "\\1", page)
        if (nchar(channel_id) == 24) {
          return(channel_id)
        }
      }
    } else {
      channel_id <- stringr::str_extract(url, "(?<=/)[^/.]{24}")
      if (!is.na(channel_id) && nchar(channel_id) == 24) {
        return(channel_id)
      } else (
        return(NULL)
      )
    }
    return(NULL)
  }

  if (length(yt_link) == 0) {
    return(list(title = NA, count = NA, subs = NA, views = NA))
  }

  # browser()
  #
  # # sometimes multiple links , need to get the 1st link that has 24 char channel ID
  #
  # yt_link <- yt_link[1]
  #
  # if (stringr::str_detect(yt_link, "@")) {
  #   channel_id <- tryCatch({
  #     page <- rvest::read_html(yt_link)|> rvest::html_text2()
  #     sub(".*channel_id=([A-Za-z0-9_-]+).*", "\\1", page)
  #   }, error = function(e) NULL)
  # } else {
  #   channel_id <- str_extract(yt_link, "(?<=/)[^/.]{24}")
  # }
  #
  # if (!is.null(channel_id)) {
  #   channel_stats <- tryCatch({
  #     tuber::get_channel_stats(channel_id = channel_id)
  #   }, error = function(e) {
  #     list(title = NA, count = NA, subs = NA, views = NA)
  #   })
  # } else {
  #   return(list(title = NA, count = NA, subs = NA, views = NA))
  # }
  #
  # list(
  #   title = channel_stats$snippet$title,
  #   subs  = channel_stats$statistics$subscriberCount,
  #   views = channel_stats$statistics$viewCount,
  #   count = channel_stats$statistics$videoCount
  # )

  # Iterate through the list of links to find the first valid one
  for (link in yt_link) {

    channel_id <- extract_channel_id(link)
    if (!is.null(channel_id)) {
      channel_stats <- tryCatch({
        tuber::get_channel_stats(channel_id = channel_id)
      }, error = function(e) {
        list(title = NA, count = NA, subs = NA, views = NA)
      })
      return(list(
        title = channel_stats$snippet$title,
        subs  = channel_stats$statistics$subscriberCount,
        views = channel_stats$statistics$viewCount,
        count = channel_stats$statistics$videoCount
      ))
    }
  }

  return(list(title = NA, count = NA, subs = NA, views = NA))
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
      style = paste("background-color:", team_info()$color, "; color: #ffffff;"),
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

  if (!any(is.na(smm()$youtube))) {

    yt <- bslib::card(
      id = "valb",
      class = "cardb",

      bslib::card_header(
        style = paste("background-color:", team_info()$color, "; color: #ffffff;"),
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
  if (!any(is.na(smm()$tiktok))) {

    tiktok <- bslib::card(
      id = "valb",
      class = "cardb",

      bslib::card_header(
        style = paste("background-color:", team_info()$color, "; color: #ffffff;"),
        paste0("TikTik Statistics for ", smm()$tiktok$name)
      ),
      bslib::value_box(
        title = "Followers",
        format(as.numeric(smm()$tiktok$followers), big.mark = ","),
        showcase = bsicons::bs_icon("tiktok")
      ),
      bslib::value_box(
        title = "Likes",
        format(as.numeric(smm()$tiktok$likes), big.mark = ","),
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

























