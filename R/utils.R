# Extraction Functions =========================================================

# Team Page -------------------------------------------------------------------

#' Get cheerleaders
#'
#' Query the team wiki page for a list of cheerleaders and url to their
#' personal pages.
#'
#' @param team_url
#'
#' @return character vector of cheerleader url links and names
#'
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
#'
cheerTableIndex <- function(tables, keyword) {

  index <- purrr::map_lgl(tables, ~ {
    if (ncol(.x) == 4) {
      any(.x$X2 == keyword)
    } else {
      FALSE
    }
  })

  which(index)[1] %>% as.integer() %>% {ifelse(length(.) > 0, ., NULL)}
}

#' Get Team Page Cheerleaders
#'
#' Query the team wiki page for a list of cheerleaders and url to their
#' personal pages. Iterates through a list of all the cheerleaders for
#' all teams.
#'
#' @param team_url
#'
#' @return data.frame with the team, cheerleader name and their page url.
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
#' @param team_data
#'
#' @return side effect is to download the team photo for each team and store
#' it in the local directory.
#'
getTeamPhotos <- function(team_data) {

  if (!dir.exists("./www/team_img")) {
    dir.create("./www/team_img", recursive = TRUE)
  }

  for (team in seq_along(team_data$url)) {

    html_content <- httr2::request(team_data$url[[team]]) |>
      httr2::req_perform() |>
      httr2::resp_body_html()

    team_image_url <- html_content |>
      rvest::html_node("tr td img[src$='.webp']") |>
      rvest::html_attr("src")

    download.file(url = paste0("https:", team_image_url),
                  destfile = paste0("./www/team_img/", team_data$name[[team]], ".webp"),
                  mode = "wb")
  }

  # manual cleanup (for now)

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

getTeamLogos <- function(team_logos) {
  if (!dir.exists("./www/team_logo")) {
    dir.create("./www/team_logo", recursive = TRUE)
  }
  for (team in seq_along(team_logos)) {
    download.file(
      url = team_logos[[team]],
      destfile = paste0("./www/team_logo/", names(team_logos[team]), ".png"),
      mode = "wb"
    )
  }
}

getTeamCap <- function(team_caps) {
  if (!dir.exists("./www/team_cap")) {
    dir.create("./www/team_cap", recursive = TRUE)
  }
  for (team in seq_along(team_caps)) {
    download.file(
      url = team_caps[[team]],
      destfile = paste0("./www/team_cap/", names(team_caps[team]), ".png"),
      mode = "wb"
    )
  }
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
#' @param values values that exist in the cheerleader table
#'
#' @return returns a named list of
#'  - name/index Cheerleaders name
#'  - data.frame the cheerleaders biography
#'  - list the cheerleaders biography as html node
#'  - character vector of social media links
#'
getCheerleaderPage <- function(wiki_url, cheerleader_url, values) {

  url <- paste0(wiki_url, cheerleader_url)
  req <- httr2::request(url)

  if (is.null(req)) {
    return(NULL)
  }

  html_content <- tryCatch({
    resp <- httr2::req_perform(req)
    status <- httr2::resp_status(resp)
    if (status < 400) {
      httr2::resp_body_html(resp)
    } else {
      message("The requested cheerleader page is not available.", status)
      return(NULL)
    }
  }, error = function(e) {
    message("Error in fetching the cheerleader page: ", e$message)
    return(NULL)
  })

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

  # social_icons <- matchIconsToLinks(social_links, keyword_image_mapping)

  social_icons <- purrr::map_chr(social_links, function(link) {
    keyword <- purrr::detect(
      names(keyword_image_mapping), ~
        stringr::str_detect(link, stringr::regex(.x, ignore_case = TRUE)))
    if (!is.null(keyword)) {
      file.path("/social_icons", keyword_image_mapping[[keyword]])
    } else {
      NA_character_
    }
  })

  # if (cheerleader_url == "/w/%EC%84%9C%EC%9C%A0%EB%A6%BC") {
  #   browser()
  # }

  # account for missing values
  valid_links <- !is.na(social_icons) & social_icons != ""

  social_links <- social_links[valid_links]
  social_icons <- social_icons[valid_links]

  # createHTML(social_links, social_icons)

  html_content <- purrr::map2(social_links, social_icons, ~glue::glue(
    '<a href="{.x}" target="_blank"><img src="{.y}" width="42" height="42"></a>&nbsp'
  )) |> paste(collapse = "")

  # extractBioTable(df_tables, values = c("nationality", "birth"))

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
}

cheerData <- function(wiki_url, team_cheerleaders, values) {

  team_cheerleaders$link |>
    purrr::map(~ getCheerleaderPage(wiki_url, .x, values)) |>
    purrr::set_names(team_cheerleaders$cheerleader)
}

#' Get Cheerleader Photo
#'
#' @param bio_table html node from cheer_data$cheerleader$bio_table
#'
#' @return side effect is to save the image to /cheerleader_img
#'
getCheerleaderPhotos <- function(bio_tables) {

  if (!dir.exists("./www/cheerleader_img")) {
    dir.create(dir_path, recursive = TRUE)
  }
  for (cheerleader in seq_along(bio_tables)) {

    bio_image <- stringr::str_extract(string = bio_tables[cheerleader],
                                      pattern = "//[^\\s]+\\.webp")

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

# Social Media ----------------------------------------------------------------

# YouTube ---------------------------------------------------------------------

extract_channel_id <- function(url) {
  if (stringr::str_detect(url, "@")) {
    page <- tryCatch({
      response <- httr2::request(url) |>
        httr2::req_perform() |>
        httr2::resp_body_string()

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

  # Return NULL if no valid channel_id is found
  return(NULL)
}
# for each girl, use either youtube, instagram or tiktok links to gather social media metrics

getYouTube <- function(cheer_data) {

  youtube <- data.frame()

  blank <- data.frame(name = NA, title = NA, subs = NA, views = NA, count = NA)

  for (cheerleader in seq_along(names(cheer_data))) {
    links <- cheer_data[[cheerleader]][[2]]
    yt_link <- links[grepl("youtube", links)]
    # yt_link <- yt_link[1]

    if (length(yt_link) != 0) {
      for (link in yt_link) {
        channel_id <- extract_channel_id(link)
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

getTikTok <- function(cheer_data) {

  tiktok <- data.frame()

  blank <- data.frame(cheername = NA, name = NA,
                      followers = NA, likes = NA)

  for (cheerleader in seq_along(names(cheer_data))) {
    links <- cheer_data[[cheerleader]][[2]]
    tt_link <- links[grepl("tiktok", links)]

    if (length(tt_link) != 0) {
      tt_link <- tt_link[1]
      page <- tryCatch({
        httr2::request(tt_link) %>%
          httr2::req_perform() %>%
          httr2::resp_body_string()
      }, error = function(e) {
        return(NULL)
      })

      tt_name <- stringr::str_extract(tt_link, "(?<=/)[^/]+/?$")

      if (!is.null(page)) {
        stats <- stringr::str_extract(page, '"stats":\\{[^}]*\\}')
        tt_followers <- str_extract(stats, '(?<=followerCount":)\\d+')
        tt_likes <- str_extract(stats, '(?<=heart":)\\d+')
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









