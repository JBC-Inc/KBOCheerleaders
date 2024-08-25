
# httr2 -----------------------------------------------------------------------

#' Fetch and Parse Content from URLs
#'
#' This function retrieves content from one or more URLs using the `httr2`
#' package. It supports returning either parsed HTML documents or raw HTML
#' strings. The function includes error handling for failed requests, non-200
#' HTTP status codes, and other issues.
#'
#' @details The function performs HTTP GET requests and either parses the HTML
#' content or returns it as a string, depending on the `return_type` argument.
#' If an error occurs (e.g., network issues, invalid URLs), or if the HTTP
#' status is 400 or above, the function will handle the error gracefully and
#' return `NULL` for that URL.
#'
#' @param urls A character vector containing one or more URLs. If a single URL
#' is provided as a string, it will be converted to a vector internally.
#' @param return_type A character string specifying the return type. Options
#' are `"html"` (default) to return parsed HTML documents using
#' `httr2::resp_body_html()` or `"string"` to return raw HTML as strings
#' using `httr2::resp_body_string()`.
#'
#' @return If a single URL is provided, the function returns the requested
#' content (HTML document or raw HTML string). If multiple URLs are provided,
#' the function returns a list of the requested content (or `NULL` if a request
#'  fails).
#'
#' @keywords internal
#'
fetchHTML <- function(urls, return_type = c("html", "string")) {
  return_type <- match.arg(return_type)

  # Ensure `urls` is a vector, even if a single URL is provided
  if (!is.vector(urls)) {
    urls <- c(urls)
  }

  # Function to handle a single request
  get_content <- function(url) {
    req <- httr2::request(url)
    if (is.null(req)) {
      return(NULL)
    }

    tryCatch({
      resp <- httr2::req_perform(req)
      status <- httr2::resp_status(resp)

      if (status < 400) {
        if (return_type == "html") {
          return(httr2::resp_body_html(resp))
        } else if (return_type == "string") {
          raw_html <- httr2::resp_body_string(resp)
          if (nzchar(raw_html)) {
            return(raw_html)
          } else {
            return(NULL)
          }
        }
      } else {
        message("Request failed for URL: ", url, " (status: ", status, ")")
        return(NULL)
      }
    }, error = function(e) {
      message("Error fetching URL: ", url, " (error: ", e$message, ")")
      return(NULL)
    })
  }

  # Apply the function to a list of URLs
  contents <- lapply(urls, get_content)

  # Return the content if a single URL was provided, otherwise return a list
  if (length(contents) == 1) {
    return(contents[[1]])
  }
  return(contents)
}

# Team Page -------------------------------------------------------------------

#' Get cheerleaders
#'
#' Query the team wiki page for a list of cheerleaders and url to their
#' personal pages.
#'
#' @param team_url team_data$url
#'
#' @return character vector of cheerleader url links and names
#' @keywords internal
#'
getCheerleaders <- function(team_url) {

  # html_content <- httr2::request(team_url) |>
  #   httr2::req_perform() |>
  #   httr2::resp_body_html()

  html_content <- fetchHTML(team_url, "html")

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
    "This Thumb"    = "Lee Um-ji",
    "Irian"         = "Lee Ri-an",
    "This Week"     = "Lee Geum-Ju"
  ))

  setNames(links, names)
}

#' Get the index of the cheerleader table
#'
#' @param tables all the tables on the team page
#' @param keyword keyword vector to identify common string which all
#'                cheerleader tables share
#'
#' @return integer of the table index the cheerleader list belongs to.
#' @keywords internal
#'
cheerTableIndex <- function(tables, keyword) {
  index <- purrr::map_lgl(tables, ~ {
    if (ncol(.x) == 4) {
      any(.x$X2 == keyword)
    } else {
      FALSE
    }
  })

  result <- which(index)

  if (length(result) > 0) {
    return(as.integer(result[1]))
  } else {
    return(NULL)
  }
}

#' Get Team Page Cheerleaders
#'
#' Query the team wiki page for a list of cheerleaders and url to their
#' personal pages. Iterates through a list of all the cheerleaders for
#' all teams.
#'
#' @param team_url team_data$url
#'
#' @return data.frame with the team, cheerleader name and their page url.
#' @keywords internal
#'
getTeamCheerleaders <- function(team_url) {

  cheerleaders <- data.frame(team = character(),
                             cheerleader = character(),
                             link = character())

  for (team in seq_along(team_url)) {
    cheer_data <- getCheerleaders(team_url[[team]])
    for(cheerleader in names(cheer_data)) {

      cheerleaders <- rbind(cheerleaders,
                            data.frame(
                              team = team_data$name[team],
                              cheerleader = cheerleader,
                              link = cheer_data[[cheerleader]]))
    }
  }
  return(cheerleaders)
}

#' Get Team Photos
#'
#' Iterate through all the team page data to download each team photo.
#'
#' @param team_data team_data(name, url, color, song)
#'
#' @return side effect is to download the team photo for each team and store
#' it in the local directory.
#' @keywords internal
#'
getTeamPhotos <- function(team_data) {

  if (!dir.exists("./www/team_img")) {
    dir.create("./www/team_img", recursive = TRUE)
  }

  for (team in seq_along(team_data$url)) {

    html_content <- fetchHTML(team_data$url[[team]], "html")

    team_image_url <- html_content |>
      rvest::html_node("tr td img[src$='.webp']") |>
      rvest::html_attr("src")

    download.file(url = paste0("https:", team_image_url),
                  destfile = paste0("./www/team_img/", team_data$name[[team]], ".webp"),
                  mode = "wb")
  }

  # lotte giants manual replacement
  file.remove("./www/team_img/Lotte Giants.webp")
  file.copy(from = "./www/Lotte Giants.webp",
            to = "./www/team_img/Lotte Giants.webp")

  file.remove("./www/team_img/KT Wiz.webp")
  file.remove("./www/team_img/Samsung Lions.webp")

  # # case KT WIZ
  # file.remove("./www/team_img/KT Wiz.webp")
  #
  # download.file(url = "https://i.ytimg.com/vi/OeCJXyFxJDQ/maxresdefault.jpg",
  #               destfile = paste0("./www/team_img/KT Wiz.jpg"),
  #               mode = "wb")
  #
  # # case Samsung Lions
  # file.remove("./www/team_img/Samsung Lions.webp")
  #
  # download.file(url = "https://www.samsunglions.com/en/img/img_cheerleader2017_en.jpg",
  #               destfile = paste0("./www/team_img/Samsung Lions.jpg"),
  #               mode = "wb")
  #
  # print(magick::image_read(paste0("./www/team_img/", team_data$name[[7]], ".webp")))
}

#' Save team logos
#'
#' Download the team logo.
#'
#' @param team_logos team_logos
#'
#' @return side effect is to download the team_logos/* images.
#' @keywords internal
#'
getTeamLogos <- function(team_logos) {
  dir_path <- "./www/team_logo"

  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
  }

  purrr::walk2(
    .x = team_logos, names(team_logos),
    .f = ~ download.file(
      url = .x,
      destfile = paste0(dir_path, "/", .y, ".png"),
      mode = "wb"
    )
  )
}

#' Save team cap insignia
#'
#' @param team_caps team_caps
#'
#' @return side effect is to download the team_cap/* images.
#' @keywords internal
#'
getTeamCap <- function(team_caps) {
  dir_path <- "./www/team_cap"

  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
  }

  purrr::walk2(
    .x = team_caps, names(team_caps),
    .f = ~ download.file(
      url = .x,
      destfile = paste0(dir_path, "/", .y, ".png"),
      mode = "wb"
    )
  )
}

# Cheerleader page ------------------------------------------------------------

#' Get Cheerleader Page
#'
#' Will iterate over all the cheerleaders page links and extract their biography.
#'
#' Cheerleader Name
#   table     tibble with (bio + links).
#   bio_table html node (biography table) needed for image.
#   links     char vector (social media links).
#'
#' @param wiki_url string wiki url prefix
#' @param cheerleader_url string wiki cheerleader url suffix
#' @param values values that exist in the cheerleader table to match upon
#'
#' @return returns a named list of
#'  - name/index Cheerleaders name
#'  - data.frame the cheerleaders biography
#'  - list the cheerleaders biography as html node
#'  - character vector of social media links
#' @keywords internal
#'
getCheerleaderPage <- function(wiki_url, cheerleader_url, values) {

  url <- paste0(wiki_url, cheerleader_url)

  # special case Hanseul Kim
  if (url == "https://en.namu.wiki/w/%EA%B9%80%ED%95%9C%EC%8A%AC(%EC%B9%98%EC%96%B4%EB%A6%AC%EB%8D%94)") {
    url = "https://en.namu.wiki/w/%EA%B9%80%ED%95%9C%EC%8A%AC(1996)"
  }

  html_content <- fetchHTML(url, "html")

  if (is.null(html_content)) {
    return(list(
      table = data.frame("NA" = "The requested cheerleader page is not available."),
      bio_table = data.frame(),
      links = data.frame()))
  }

  xml_tables <- rvest::html_nodes(html_content, "table")

  df_tables <- lapply(xml_tables, rvest::html_table, fill = TRUE)

  # index <- bioTableIndex(df_tables, values)

  index <- purrr::map_lgl(df_tables, function(table) {
    any(!is.na(table$X1)) && any(table$X1 %in% values)
  }) |>
    which() |>
    dplyr::first()

  bio_table <- xml_tables[[index]]

  hrefs <- bio_table |>
    rvest::html_nodes("a") |>
    rvest::html_attr("href")

  social_links <- hrefs[grepl("http", hrefs)]

  social_icons <- purrr::map_chr(social_links, function(link) {
    keyword <- purrr::detect(
      names(keyword_image_mapping), ~
        stringr::str_detect(link, stringr::regex(.x, ignore_case = TRUE)))
    if (!is.null(keyword)) {
      file.path("social_icons", keyword_image_mapping[[keyword]])
    } else {
      NA_character_
    }
  })

  valid_links <- !is.na(social_icons) & social_icons != ""

  social_links <- social_links[valid_links]
  social_icons <- social_icons[valid_links]

  html_content <- purrr::map2(social_links, social_icons, ~glue::glue(
    '<a href="{.x}" target="_blank"><img src="{.y}" width="42" height="42"></a>&nbsp'
  )) |> paste(collapse = "")

  table <- df_tables|>
    purrr::detect(~ any(.x[[1]] %in% values, na.rm = TRUE)) |>
    dplyr::select(dplyr::where(~ !any(is.na(.))))

  table <- table |> dplyr::filter(!(dplyr::row_number() == 1 & X1 == ""))

  # sometimes tables have 2 row headers
  if (ncol(table) == 2) {
    table <- table |>
      dplyr::mutate(X1 = ifelse(X1 %in% c("", "|", "||", "|||", "||||",
                                          "||[7]|||", "||[2]|||"), "link", X1),
                    X2 = dplyr::if_else(X1 %in% c("link", "site", "SNS"), html_content, X2)) |>
      dplyr::filter(X1 != X2,
                    !X1 %in% c("support team", "platform", "signature"),
                    !grepl("youtube", X1, ignore.case = TRUE))
  } else if (ncol(table) == 3) {
    table <- table |>
      dplyr::mutate(X2 = ifelse(X2 %in% c("|", ""), "link", X2),
                    X3 = dplyr::if_else(X1 %in% c("link", "site", "SNS"), html_content, X3)) |>
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

    dplyr::mutate(X2 = stringr::str_replace_all(X2, "\\[.*?\\]", "")) |>
    dplyr::mutate(X2 = stringr::str_replace_all(X2, "\\(\\s*", "("),
                  X2 = stringr::str_replace_all(X2, "\\s*\\)", ")")) |>

    # blood type
    dplyr::mutate(X2 = stringr::str_replace_all(X2, "(?i)\\bab\\b", "AB")) |>

    # Myers-Briggs Type Indicator
    dplyr::mutate(X1 = dplyr::case_when(
      X1 == "Mbti" ~ stringr::str_to_upper(X1),
      TRUE ~ X1)) |>
    dplyr::mutate(X2 = dplyr::case_when(
      X1 == "MBTI" ~ stringr::str_to_upper(X2),
      TRUE ~ X2
    )) |>

    # birth instances
    dplyr::mutate(previous_birth = cumsum(X1 == "Birth")) |>
    dplyr::mutate(X1 = dplyr::case_when(
      X1 == "Birth" ~ "Birthday",
      X1 == "Birth Date" ~ "Birthday",
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
    dplyr::select(-previous_birth, -previous_agency)

  list(
    table = table,
    bio_table = bio_table,
    links = social_links
  )
}

#' Get Cheerleader Data
#'
#' Iterate over team cheerleader URLs to make the list of HTML, data.frame
#' and link vectors.
#'
#' @param wiki_url namuwiki cheerleader page prefix
#' @param team_cheerleaders data.frame with team, cheerleader and namu wiki url
#' @param values values to match on that exist in all cheerleader tables.
#'
#' @return list all cheerleaders bio(HTML, data.frame) and social media links
#' @keywords internal
#'
cheerData <- function(wiki_url, team_cheerleaders, values) {

  team_cheerleaders$link |>
    purrr::map(~ getCheerleaderPage(wiki_url, .x, values)) |>
    purrr::set_names(team_cheerleaders$cheerleader)
}

#' Get Cheerleader Photo
#'
#' @param bio_table html node from cheer_data$cheerleader$bio_table
#' @param cheer_data all cheerleader table data
#'
#' @return side effect is to save the image to /cheerleader_img
#' @keywords internal
#'
getCheerleaderPhotos <- function(bio_tables, cheer_data) {

  if (!dir.exists("./www/cheerleader_img")) {
    dir.create(dir_path, recursive = TRUE)
  }
  for (cheerleader in seq_along(bio_tables)) {

    bio_image <- stringr::str_extract(
      string = bio_tables[cheerleader],
      pattern = "//[^\\s]+\\.webp"
      )

    if (!is.na(bio_image)) {
      bio_image <- paste0("https:", bio_image)

      temp_file <- tempfile(fileext = ".webp")

      download.file(bio_image, temp_file, mode = "wb")

      img <- magick::image_read(temp_file)

      saveas <- paste0("./www/cheerleader_img/",
                       names(cheer_data)[cheerleader], ".png")

      magick::image_write(img, saveas, format = "png")
    }
  }
}

# Social Media Functions ------------------------------------------------------

# YouTube ---------------------------------------------------------------------

#' Extract YouTube channel ID from a given URL.
#'
#' The YouTube channel ID will either be found in the url or in the raw HTML
#' for the page. The pattern to search of is a 24 character length string.
#'
#' @param url Cheerleader YouTube URL
#'
#' @return channel_id
#' @keywords internal
#'
extractChannelID <- function(url) {

  if (stringr::str_detect(url, "@")) {

    page <- tryCatch({
      response <- fetchHTML(url, "string")
      if (nzchar(response)) {
        rvest::read_html(response) |>
          rvest::html_text2()
      } else {
        NULL
      }
    }, error = function(e) {
      message("Error fetching page: ", e$message)
      NULL
    })

    if (!is.null(page)) {
      channel_id <- stringr::str_sub(page, start = stringr::str_locate(page, "channel_id=")[2] + 1)
      channel_id <- stringr::str_extract(channel_id, "[A-Za-z0-9_-]+")
      if (nchar(channel_id) == 24) {
        return(channel_id)
      }
    }
  } else {
    channel_id <- stringr::str_extract(url, "(?<=/)[^/.]{24}")
    if (!is.na(channel_id) && nchar(channel_id) == 24) {
      return(channel_id)
    }
  }

  return(NULL)
}

#' Get Cheerleader YouTube Statistics.
#'
#' @param cheer_data data.frame of cheerleader data.
#'
#' @return data.frame with YouTube statistics
#'  - Cheerleader name
#'  - YouTube channel
#'  - Subscriber count
#'  - Video view count
#'  - Video count
#'  - Cheerleader team
#'  - Type of social media platform
#' @keywords internal
#'
getYouTube <- function(cheer_data) {

  youtube <- data.frame()

  blank <- data.frame(name = NA, title = NA, subs = NA, views = NA, count = NA)

  for (cheerleader in seq_along(names(cheer_data))) {
    links <- cheer_data[[cheerleader]][[2]]
    yt_link <- links[grepl("youtube", links)]
    # yt_link <- yt_link[1]

    if (length(yt_link) != 0) {
      for (link in yt_link) {
        channel_id <- extractChannelID(link)
        if (!is.null(channel_id)) {
          channel_stats <- tryCatch({
            tuber::get_channel_stats(channel_id = channel_id)
          }, error = function(e) {
            return(blank)
          })

          youtube <- rbind(
            youtube,
            data.frame(
              name  = names(cheer_data)[cheerleader],
              title = channel_stats$snippet$title,
              subs  = as.integer(channel_stats$statistics$subscriberCount),
              views = as.integer(channel_stats$statistics$viewCount),
              count = as.integer(channel_stats$statistics$videoCount)
            ))
        }
      } # for link in links
    }
  }
  youtube
}

# instagram -------------------------------------------------------------------

#' Get Cheerleader Instagram Statistics.
#'
#' @param cheer_data data.frame of cheerleader data.
#'
#' @return data.frame with Instagram statistics
#'  - Cheerleader name
#'  - Instagram account name
#'  - Instagram followers
#'  - Cheerleader team
#'  - Type of social media platform
#' @keywords internal
#'
getInstagram <- function(cheer_data) {

  instagram <- data.frame()

  blank <- data.frame(name = NA, team = NA, followers = NA)

  for (cheerleader in seq_along(names(cheer_data))) {
    links <- cheer_data[[cheerleader]][[2]]
    insta_link <- links[grepl("instagram", links)]

    if (length(insta_link) != 0) {
      insta_link <- insta_link[1]      # get the first one
      meta_content <- rvest::read_html(insta_link) |>
        as.character()

      inst_followers <- stringr::str_extract(
        string = meta_content,
        pattern = '(?<=content=")(.*?)(?= Followers)')

      inst_name <- stringr::str_extract(
        string = insta_link,
        pattern = "(?<=/)[^/]+/?$")
    } else {
      inst_followers <- NA
      inst_name <- NA
    }

    instagram <- rbind(
      instagram,
      data.frame(
        name = names(cheer_data)[cheerleader],
        acct = inst_name,
        followers = inst_followers
      ))
  }
  instagram |>
    dplyr::mutate(
      followers = stringr::str_replace(followers, "K", "000"),
      followers = stringr::str_replace(followers, "M", "000000"),
      followers = stringr::str_replace(followers, ",", "")) |>
    dplyr::mutate(followers = as.integer(followers))
}

# tiktok ----------------------------------------------------------------------

#' Get Cheerleader TikTok Statistics.
#'
#' @param cheer_data data.frame of cheerleader data.
#'
#' @return data.frame with Instagram statistics
#'  - Cheerleader name
#'  - TikTok account name
#'  - TikTok follower count
#'  - TikTok likes count
#'  - Cheerleader team
#'  - Type of social media platform
#' @keywords internal
#'
getTikTok <- function(cheer_data) {

  tiktok <- data.frame()

  blank <- data.frame(cheername = NA, name = NA,
                      followers = NA, likes = NA)

  for (cheerleader in seq_along(names(cheer_data))) {

    # if(names(cheer_data)[cheerleader] == "Yeonjung Kim") {browser()}

    links <- cheer_data[[cheerleader]][[2]]
    tt_link <- links[grepl("tiktok", links)]

    if (length(tt_link) != 0) {
      tt_link <- tt_link[1]

      page <- fetchHTML(tt_link, "string")

      tt_name <- stringr::str_extract(tt_link, "(?<=/)[^/]+/?$")

      if (!is.null(page)) {
        stats <- stringr::str_extract(page, '"stats":\\{[^}]*\\}')
        tt_followers <- stringr::str_extract(stats, '(?<=followerCount":)\\d+')
        tt_likes <- stringr::str_extract(stats, '(?<=heart":)\\d+')
      } else {
        tt_followers <- NA
        tt_likes <- NA
      }

      tiktok <- rbind(
        tiktok,
        data.frame(cheername = names(cheer_data)[cheerleader],
                   name = tt_name,
                   followers = tt_followers,
                   likes = tt_likes))
    }
  }

  tiktok <- tiktok |>
    dplyr::mutate(followers = as.integer(followers),
                  likes = as.integer(likes))

  tiktok
}

# Ultra Combo -----------------------------------------------------------------

#' All cheerleader/team data with social media metrics
#'
#' @param team_cheerleaders Cheerleader team, name and wiki url
#' @param cheer_data cheerleader data table/links
#' @param youtube youtube
#' @param instagram instagram
#' @param tiktok tiktok
#'
#' @return data.frame with 14 columns
#' @keywords internal
#'
ultraCombo <- function(team_cheerleaders, cheer_data, youtube, instagram, tiktok) {

  yt <- youtube |> dplyr::select(name, subs, views, count, cat)
  inst <- instagram |> dplyr::select(name, followers, cat)
  tt <- tiktok |> dplyr::select(cheername, followers, likes, cat)
  tt <- tt |> dplyr::rename(name = cheername)

  ultra_combo <- yt |>
    dplyr::full_join(inst, by = c("name", "cat")) |>
    dplyr::full_join(tt, by = c("name", "cat")) |>
    dplyr::mutate(instagram_followers = followers.x,
                  tiktok_followers = followers.y) |>
    dplyr::select(-c(followers.x, followers.y))

  ultra_combo <- ultra_combo |>
    dplyr::left_join(team_cheerleaders, by = c("name" = "cheerleader"))

  # add team logo url and color
  # names(team_logos) <- team_data$name
  team_logos_df <- data.frame(name = team_data$name,
                              team_img = unlist(team_logos),
                              color = team_data$color)

  ultra_combo <- ultra_combo |>
    dplyr::left_join(team_logos_df , by = c("team" = "name"))

  ultra_combo <- ultra_combo |>
    dplyr::mutate(
      logo =
        glue::glue(
          '<img height=50 src="www/team_logo/{team_img}"
            class="team-photo"
            data-tt="{team}">
          </img>')
    ) |>
    dplyr::mutate(
      photo =
        glue::glue(
          '<img height=50 src="www/cheerleader_img/{name}.png"
             class="cheerleader-photo"
             data-team="{team}"
             data-name="{name}">
            </img>')
    ) |>
    dplyr::mutate(logo = stringr::str_replace(logo, "Wiz\\.webp", "Wiz.jpg")) |>
    dplyr::mutate(logo = stringr::str_replace(logo, "Lions\\.webp", "Lions.jpg")) |>
    dplyr::mutate(avg_views_per_video = as.integer(views / count)) |>
    dplyr::mutate(link = glue::glue('<a href="{wiki_url}{link}" target="_blank">{name}</a>'))

  # add age to ultra-combo
  age <- processCheerleaders(cheer_data)

  ultra_combo |>
    dplyr::left_join(age, by = c("name" = "Name")) |>
    dplyr::mutate(age = as.numeric(Age)) |>
    dplyr::select(-Age)
}

#' Extracts age from the "Birth" or "Age" row of the cheerleaders table.
#'
#' @param text the value (X2) record of matching "Birthday" (X1) record.
#'
#' @return Cheerleaders Age or NA if could not extract.
#' @keywords internal
#'
extractAge <- function(text) {

  if (grepl(pattern = "Age", text)) {
    inside_parentheses <- stringr::str_extract(text, "\\(.*\\)")
    age_match <- stringr::str_extract(inside_parentheses, "\\d+")
  } else {
    age_match <- stringr::str_extract(text, "(?<=\\()\\d+(?=\\D*\\))")
  }
  if (is.na(age_match)) {
    return(NA)
  }
  age_match
}

#' Process Cheerleader
#'
#' This function processes cheer_data, extracting age information from each
#' data frame and returning a consolidated tibble with the cheerleader
#' names and their respective ages.
#'
#' @param cheer_data A named list of data frames, where each data frame
#' should have a `table` element containing cheerleader information.
#' The names of the list elements are used to label the output.
#'
#' @return A tibble with two columns:
#' \itemize{
#'   \item \code{Name}: The name of the cheerleader
#'   \item \code{Age}: The extracted age of the cheerleader, or \code{NA}
#' }
#' @keywords internal
#'
processCheerleaders <- function(cheerleaders_list) {

  cheerleaders_list |>
    purrr::imap_dfr(function(data, name) {
      tibble_data <- data$table

      age <- tibble_data |>
        dplyr::filter(X1 %in% c("Birthday", "Birth Date", "Year Of Birth")) |>
        dplyr::pull(X2) |>
        purrr::map_chr(extractAge) |>
        na.omit() |>
        magrittr::extract(1)

      tibble::tibble(
        Name = name,
        Age = if (!is.null(age)) age else NA_character_
      )
    })
}

#' Aggregate Team Followers plot
#'
#' Allows faster loading times.
#'
#' @param ultra_combo followers across teams data.frame
#'
#' @return ggplot2 with team logo images
#' @keywords internal
#'
fatPlot <- function(ultra_combo) {

  ultra_combo |>
    dplyr::rowwise() |>
    dplyr::mutate(followers = sum(c(subs, instagram_followers, tiktok_followers), na.rm =TRUE)) |>
    dplyr::group_by(team, color, team_img) |>
    dplyr::summarize(followers = sum(followers, na.rm =TRUE), .groups ="drop") |>
    dplyr::arrange(dplyr::desc(followers)) |>

    ggplot2::ggplot(
      ggplot2::aes(x = reorder(team, -followers),
                   y = followers,
                   fill = color)) +
    ggplot2::geom_bar(stat = "identity") +
    ggimage::geom_image(
      mapping = ggplot2::aes(image = paste0("www/team_logo/", team_img)),
      size = 0.2) +
    ggplot2::scale_fill_identity() +
    ggplot2::scale_y_continuous(
      breaks = seq(0, 5000000, 1000000),
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
}

#' Followers Across Teams Distribution Graphs
#'
#' @param ultra_combo all the datas
#'
#' @return list of distribution graphs
#'  - normal distribution
#'  - capped outliers @ 100k
#'  - log scale 1k - 1M
#'  - 95% percentile
#' @keywords internal
#'
fatDistroPlot <- function(ultra_combo) {

  new_combo <- ultra_combo |>
    dplyr::rowwise() |>
    dplyr::mutate(followers = sum(c(subs, instagram_followers,
                                    tiktok_followers), na.rm =TRUE)) |>
    dplyr::group_by(team, cat, followers) |>
    dplyr::summarize(avg_followers = mean(followers), .groups ="drop") |>
    dplyr::arrange(dplyr::desc(followers))

  f1 <- new_combo |>
    dplyr::group_by(team, cat, followers) |>
    dplyr::summarize(avg_followers = mean(followers), .groups ="drop") |>
    dplyr::arrange(dplyr::desc(followers)) |>

    ggplot2::ggplot(ggplot2::aes(x = avg_followers, fill = cat)) +
    ggplot2::geom_density(alpha = 0.6) +

    ggplot2::scale_x_continuous(
      breaks = seq(0, 10000000, 500000),
      labels = scales::comma_format()) +
    ggplot2::scale_fill_manual(values = c("youtube" = "red",
                                          "tiktok" = "black",
                                          "instagram" = "purple")) +

    ggplot2::labs(title = "",
                  fill = "Social Media Platform",
                  x = "",
                  y = "") +
    ggplot2::theme_minimal() +
    ggplot2::theme(plot.margin = ggplot2::margin(0, 0, 0, 0),
                   legend.position = "none")

  # CAPPED OUTLIERS @ 100,000
  f2 <- new_combo |>
    dplyr::group_by(team, cat) |>
    dplyr::summarize(avg_followers = mean(followers), .groups = 'drop') |>
    dplyr::mutate(avg_followers = pmin(avg_followers, 100000)) |>
    ggplot2::ggplot(ggplot2::aes(x = avg_followers, fill = cat)) +
    ggplot2::geom_density(alpha = 0.6) +
    ggplot2::scale_x_continuous(
      breaks = seq(0, 200000, 25000),
      labels = scales::comma_format()) +
    ggplot2::scale_fill_manual(values = c("youtube" = "red",
                                          "tiktok" = "black",
                                          "instagram" = "purple")) +
    ggplot2::labs(title = "", fill = "Social Media Platform",
                  x = "Average Followers per Team",
                  y = "Density") +
    ggplot2::theme_minimal() +
    ggplot2::theme(plot.margin = ggplot2::margin(0, 0, 0, 0),
                   legend.position = "none")

  # LOG SCALE
  f3 <- new_combo |>
    tidyr::drop_na() |>
    dplyr::group_by(team, cat) |>
    dplyr::summarize(avg_followers = mean(followers), .groups = 'drop') |>
    dplyr::mutate(log_avg_followers = log1p(avg_followers)) |>
    ggplot2::ggplot(ggplot2::aes(x = log_avg_followers, fill = cat)) +
    ggplot2::geom_density(alpha = 0.6) +
    ggplot2::scale_fill_manual(values = c("youtube" = "red",
                                          "tiktok" = "black",
                                          "instagram" = "purple")) +
    ggplot2::scale_x_continuous(
      labels = function(x) scales::comma_format()(exp(x) - 1),
      breaks = log1p(c(1000, 10000, 100000, 1000000)),
      limits = log1p(c(1000, 1000000))
    ) +
    ggplot2::labs(
      title = "", fill = "Social Media Platform",
      x = "Log(Average Followers per Team)",
      y = "Density") +
    ggplot2::theme_minimal() +
    ggplot2::theme(plot.margin = ggplot2::margin(0, 0, 0, 0),
                   legend.position = "none")

  # DENSITY w/TRIMMED
  average_followers <- new_combo |>
    dplyr::group_by(team, cat) |>
    dplyr::summarize(avg_followers = mean(followers), .groups = 'drop')

  # Trim extreme values (e.g., keep only the bottom 95% of data)
  threshold <- quantile(average_followers$avg_followers, 0.95, na.rm = TRUE)
  trimmed_data <- average_followers |>
    dplyr::filter(avg_followers <= threshold)

  # Plot the density of trimmed average followers by platform category
  f4 <- trimmed_data |>
    ggplot2::ggplot(ggplot2::aes(x = avg_followers, fill = cat)) +
    ggplot2::geom_density(alpha = 0.6) +
    ggplot2::scale_x_continuous(
      breaks = seq(0, 200000, 25000),
      labels = scales::comma_format()) +
    ggplot2::scale_fill_manual(values = c("youtube" = "red",
                                          "tiktok" = "black",
                                          "instagram" = "purple")) +
    ggplot2::labs(title = "", fill = "Social Media Platform",
                  x = "Average Followers per Team",
                  y = "Density") +
    ggplot2::theme_minimal() +
    ggplot2::theme(plot.margin = ggplot2::margin(0, 0, 0, 0),
                   legend.position = "none")

  return(
    list(
      f1 = f1,
      f2 = f2,
      f3 = f3,
      f4 = f4
    ))
}


#' Ultra Combo transposed long for aggregate followers
#'
#' @param ultra_combo all the cheerleader data
#'
#' @return reshape long ultra_combo
#' @keywords internal
#'
makeUltraLong <- function(ultra_combo) {

  ultra_combo |>
    dplyr::select(team, color, name, age, subs, instagram_followers, tiktok_followers) |>
    tidyr::pivot_longer(
      cols = c(subs, instagram_followers, tiktok_followers),
      names_to = "platform",
      values_to = "followers") |>
    tidyr::drop_na(followers) |>
    dplyr::mutate(
      age_group = cut(
        age,
        breaks = c(15, 20, 25, 30, 40, 45),
        right = FALSE,
        labels = c("15-19", "20-24", "25-29", "30-39", "40-50")
      )
    )

}


#' Create interactive age distribution
#'
#' @param ultra_combo All the cheerleader and team data
#'
#' @return plotly object
#' @keywords internal
#'
ageJitterDist <- function(long, team_data) {

  team_colors <- c(
    setNames(team_data$color, team_data$name)
  )

  gg_point <- long |>
    tidyr::drop_na() |>
    dplyr::group_by(team, color, name, age, age_group) |>
    dplyr::summarize(followers = sum(followers), .groups = 'drop') |>
    dplyr::filter(followers < 750000) |>

    ggplot2::ggplot(
      ggplot2::aes(
        x = as.factor(age_group),
        y = followers)
      ) +
    ggplot2::geom_boxplot(
      fill = "transparent",
      outliers = FALSE,
      color = "black"
      ) +
    ggiraph::geom_jitter_interactive(
      ggplot2::aes(
        size = followers,
        color = team,
        tooltip = paste(name, "\n", team, "\n", format(followers, big.mark = ",")),
        data_id = paste(name, team, followers, sep = "|")
      ),
      width = .21,
      show.legend = TRUE
    ) +
    ggplot2::scale_color_manual(values = team_colors) +
    ggplot2::scale_y_continuous(
      breaks = seq(0, 1000000, 100000),
      labels = scales::comma_format()
      ) +
    ggplot2::labs(
      y = "Followers",
      x = "Age Group",
      caption = "*Omission of outliers > 1,000,000 followers",
      color = "Team"
    ) +
    ggplot2::guides(
      size = "none"
    ) +
    ggplot2::theme_minimal()

  ggiraph::girafe(
    ggobj = gg_point,
    width_svg = 12,
    # height_svg = 5.42,
    options = list(
      ggiraph::opts_sizing(rescale = TRUE),
      ggiraph::opts_hover(css = "fill:default;r:7;"),
      ggiraph::opts_selection(css = "fill:default"),
      ggiraph::opts_tooltip(opacity = .69, use_fill = TRUE)
      )
    )
}

#' Distribution of platforms by age groups
#'
#' @param ultra_combo all the cheerleader data with ages
#'
#' @return side effect is to return ggplot2 object with platform count
#' metrics by age groups.
#'
#' @keywords internal
#'
ageDist <- function(long) {

  platform_colors <- c(
    "subs" = "#ff0000",               # Red for subs
    "tiktok_followers" = "#000000",   # Black for tiktok_followers
    "instagram_followers" = "purple" # Base color for instagram followers
  )

  long |>
    tidyr::drop_na() |>
    dplyr::count(age_group, platform, na.rm = TRUE) |>
    ggplot2::ggplot(
      ggplot2::aes(
        x = age_group,
        y = n,
        fill = platform)
      ) +

    ggplot2::geom_bar(
      stat = "identity",
      position = "stack",
      alpha = 0.7,
      width = 0.42
      ) +

    ggplot2::scale_fill_manual(
      values = platform_colors,
      labels = c("Instagram", "YouTube", "TikTok")
      ) +

    ggplot2::scale_y_continuous(
      breaks = seq(0, 200, by = 10),
      labels = scales::comma_format()
      ) +

    ggplot2::labs(
      # title = "Distribution of Platforms by Age Group",
      x = "Age Group",
      y = "",
      fill = "Platform"
      ) +

    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
}

# Backup/Historical -----------------------------------------------------------

#' Backup Data
#'
#' Backup /data and /www and all contents. Store in timestamped folder.
#'
#' ex. KBO_Cheerleaders_Backup_20240817_201241/data
#'                                            /www/cheerleader_img
#'                                            /www/social_icons
#'                                            /www/team_cap
#'                                            /www/team_img
#'                                            /www/team_logo
#' @return side effect is to make the historical backup data that will later
#' be extracted for 'historical' trendlines within the leaderboards.
#' @keywords internal
#'
backup <- function() {

  data_dir <- "./data"
  www_dir <- "./www"
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")  # Format as YYYYMMDD_HHMMSS
  backup_dir <- file.path(
    paste0("C:/Users/cragg/OneDrive/code/github/KBO_Cheerleaders_Backup_", timestamp))

  if (!dir.exists(backup_dir)) {
    dir.create(backup_dir)
  }

  # /data =====================================================================

  data_files <- list.files(data_dir, pattern = "\\.(rda|rds)$", full.names = TRUE)

  copy_files <- function(files, dest_dir) {
    for (file in files) {
      dest_file <- file.path(dest_dir, basename(file))
      file.copy(file, dest_file, overwrite = TRUE)
    }
  }

  copy_files(data_files, backup_dir)

  # /www ======================================================================
  copy_directory_recursively <- function(src_dir, dest_dir) {

    # Create the destination directory if it doesn't exist
    if (!dir.exists(dest_dir)) {
      dir.create(dest_dir, recursive = TRUE)
    }

    items <- list.files(src_dir, full.names = TRUE, recursive = TRUE)

    for (item in items) {
      relative_path <- sub(paste0("^", normalizePath(src_dir, winslash = "/"), "/"), "",
                           normalizePath(item, winslash = "/"))
      dest_path <- file.path(dest_dir, relative_path)

      if (file.info(item)$isdir) {
        if (!dir.exists(dest_path)) {
          dir.create(dest_path, recursive = TRUE)
        }
      } else {
        parent_dir <- dirname(dest_path)
        if (!dir.exists(parent_dir)) {
          dir.create(parent_dir, recursive = TRUE)
        }
        file.copy(item, dest_path, overwrite = TRUE)
      }
    }
  }

  copy_directory_recursively(www_dir, file.path(backup_dir, "www"))

  cat("Backup completed. Files have been copied to:", backup_dir, "\n")
}

#' Create historical ultra_combo
#'
#' Will be run weekly after backups to gather historical social media metrics.
#' Used in leaderboard sparkline plots.
#'
#' @param base_path backup folders directory
#'
#' @return data.frame with cheerleader historical social media metrics
#' @keywords internal
#'
loadHistoricalData <- function(base_path = "../") {

  backup_dirs <- list.dirs(base_path, recursive = TRUE, full.names = TRUE)
  backup_dirs <- backup_dirs[grepl(".*Backup.*", backup_dirs, ignore.case = TRUE)]

  historic <- data.frame()

  for (dir in backup_dirs) {

    rda_file <- file.path(dir, "ultra_combo.rda")

    if (file.exists(rda_file)) {
      load(rda_file)

      if (exists("ultra_combo")) {

        datetime_str <- dir |>
          basename() |>
          stringr::str_extract("\\d{8}_\\d{6}")

        datetime <- lubridate::ymd_hms(datetime_str, tz = "UTC")

        ultra_combo <- ultra_combo |>
          dplyr::mutate(datetime = datetime)

        historic <- dplyr::bind_rows(historic, ultra_combo)
      }
    }
  }
  return(historic)
}
