
# find cheerleader table ------------------------------------------------------

get_cheerleaders <- function(team_url) {

  # browser()
  #
  #
  # df_tables <- rvest::read_html(team_url) |>
  #   rvest::html_nodes("table") |>
  #   rvest::html_table(fill = TRUE)
  #
  # table_index <- findCheerleaderTable(df_tables)
  #
  #
  # xml_tables <- httr2::request(team_url) |>
  #   httr2::req_perform() |>
  #   httr2::resp_body_html() |>
  #   rvest::html_nodes("table")
  #
  #
  #
  # cheerleader_table <- xml_tables[[table_index]]
  #
  # names <- cheerleader_table |>
  #   rvest::html_nodes("a") |>
  #   rvest::html_text()
  #
  # links <- cheerleader_table |>
  #   rvest::html_nodes("a") |>
  #   rvest::html_attr("href")





  # Fetch html content
  html_content <- httr2::request(team_url) |>
    httr2::req_perform() |>
    httr2::resp_body_html()

  # XML node tables
  xml_tables <- rvest::html_nodes(html_content, "table")

  # data.frame tables
  df_tables <- lapply(xml_tables, rvest::html_table, fill = TRUE)

  # table with 4 columns
  table_index <- findCheerleaderTable(df_tables)

  # subset XML node and extract name/link
  cheerleader_table <- xml_tables[[table_index]]

  names <- cheerleader_table |>
    rvest::html_nodes("a") |>
    rvest::html_text()

  links <- cheerleader_table |>
    rvest::html_nodes("a") |>
    rvest::html_attr("href")

  setNames(c(" ", links), c(" ", names))





















  # page <- req |>
  #    httr2::resp_body_html()

  # cheerleaders <- page %>%
  #   rvest::html_nodes("table") %>% # Adjust the CSS selector based on the actual page structure
  #   rvest::html_table(fill = TRUE) %>%
  #   .[[1]] %>%
  #   .$name

  #  tables <- page |>
  #   html_nodes("table")

  # html_table(fill = TRUE)          converts table to data.frame


  # cheerleader_table <- tables[[9]]
  #
  # cheerleader_table <- findCheerleaderTable(tables)
  #
  #
  #
  # browser()
  #
  #
  # names <- cheerleader_table |> rvest::html_nodes("a") |> rvest::html_text()
  #
  # links <- cheerleader_table |> rvest::html_nodes("a") |> rvest::html_attr("href")
  #
  # setNames(c(" ", links), c(" ", names))


  # cheerleader_table <- cheerleader_table[-c(1:2), ]
  # colnames(cheerleader_table) <- c("number", "name", "debut", "activity_period")
  # cheerleader_table$name <- sapply(cheerleader_table$name, function(name) {
  #   # Split the name by spaces and hyphens
  #   parts <- str_split(name, " |-")[[1]]
  #
  #   # Check if the name parts fit expected patterns
  #   if (length(parts) >= 2) {
  #     # Extract last name (first part) and first name (remaining parts)
  #     last_name <- parts[1]
  #     first_name_parts <- parts[-1]
  #
  #     # Retain hyphens in first names and concatenate them
  #     first_name <- paste(first_name_parts, collapse = " ")
  #
  #     # Return in "FirstName LastName" format
  #     return(paste(first_name, last_name))
  #   } else {
  #     # If name doesn't fit the pattern, return as is
  #     return(name)
  #   }
  # })

  # cheerleader_table |>
  #   mutate(formatted_name = case_when(
  #   str_count(name, " ") == 1 ~ {
  #     parts <- str_split(name, " ", simplify = TRUE)
  #     last_name <- parts[, 1]
  #     first_name <- parts[, 2]
  #     str_c(first_name, last_name, sep = " ")
  #   },
  #   str_count(name, " ") == 2 ~ {
  #     parts <- str_split(name, " ", simplify = TRUE)
  #     last_name1 <- parts[, 1]
  #     last_name2 <- parts[, 2]
  #     first_name <- parts[, 3]
  #     paste0(first_name, " ", last_name1, "-", last_name2)
  #   },
  #   TRUE ~ name  # If none of the conditions are met, retain the original name
  # ))

  # cheerleader_table$name









  # df <- tibble(name = c("lastname first-name", "lastname firstname", "lastname lastname firstname"))
  #
  # df |>
  #   mutate(formatted_name = case_when(
  #     str_count(name, " ") == 1 ~ {
  #       parts <- str_split(name, " ", simplify = TRUE)
  #       last_name <- parts[, 1]
  #       first_name <- parts[, 2]
  #       str_c(first_name, last_name, sep = " ")
  #     },
  #     str_count(name, " ") == 2 ~ {
  #       parts <- str_split(name, " ", simplify = TRUE)
  #       last_name1 <- parts[, 1]
  #       last_name2 <- parts[, 2]
  #       first_name <- parts[, 3]
  #       paste0(first_name, " ", last_name1, "-", last_name2)
  #     },
  #     TRUE ~ name  # If none of the conditions are met, retain the original name
  #   ))
}



# cheerleader photo -----------------------------------------------------------


# library(rvest)
#
# get_cheerleader_photo <- function(url) {
#   # Read the HTML content of the page
#   page <- read_html(url)
#
#   # Extract the image URL from the 'src' attribute
#   img_src <- page %>%
#     html_node("img") %>%
#     html_attr("src")
#
#   # Convert relative URL to absolute URL if necessary
#   img_src <- url_absolute(img_src, url)
#
#   return(img_src)
# }
#
# # Helper function to handle relative URLs
# url_absolute <- function(src, base_url) {
#   if (grepl("^https?://", src)) {
#     return(src)  # Already an absolute URL
#   } else {
#     return(paste0("https:", src))  # Convert to absolute URL
#   }
# }
#
# # Example usage
# cheerleader_url <- "https://example.com/cheerleader-page"
# cheerleader_photo_url <- get_cheerleader_photo(cheerleader_url)
# print(cheerleader_photo_url)



# UI --------------------------------------------------------------------------

# Define UI
# ui <- shiny::fluidPage(
#   shiny::titlePanel("KBO League Cheerleaders"),
#
#   shiny::sidebarLayout(
#     shiny::sidebarPanel(
#       shiny::selectInput("team", "Select Team:", choices = teams, selected = NULL),
#       shiny::uiOutput("cheerleaderUI")
#     ),
#     shiny::mainPanel(
#       shiny::uiOutput("cheerleaderPhoto"),
#       shiny::uiOutput("cheerleaderBio")
#     )
#   )
# )

# PARSE CHEERLEADER PAGE DATA -------------------------------------------------

# parseCheerleaderBio <- function(bio_table, df_tables) {
#
#   hrefs <- bio_table |>
#     html_nodes("a") |>
#     html_attr("href")
#
#   social_links <- hrefs[grepl("http", hrefs)]
#
#   social_icons <- matchIconsToLinks(social_links, keyword_image_mapping)
#
#   # account for missing values
#   social_links <- social_links[social_icons != ""]
#   social_icons <- social_icons[social_icons != ""]
#
#   table <- extractBioTable(df_tables, "nationality")
#
#   html_content <- createHTML(social_links, social_icons)
#
#   table <- table |>
#     dplyr::mutate(X2 = ifelse(X1 == "link", html_content, X2)) |>
#     dplyr::mutate(X2 = ifelse(X1 == "site", html_content, X2)) |>
#     dplyr::mutate(X2 = ifelse(X1 == "SNS", html_content, X2))
#
#   if (ncol(table) == 2) {
#     table |>
#       dplyr::filter((X1 != X2)) |>
#       dplyr::filter(!X1 %in% c("support team", "platform", "signature"))
#   } else if (ncol(table) == 3) {
#     table |>
#       dplyr::select(-X1) |>
#       dplyr::rename(X1 = X2) |>
#       dplyr::rename(X2 = X3) |>
#       dplyr::filter((X1 != X2)) |>
#       dplyr::filter(!X1 %in% c("support team", "platform", "signature"))
#   }
# }


# YOUTUBE STATISTICS ==========================================================

if (length(yt_link) != 0) {
  if (length(yt_link) > 1) {
    yt_link <- yt_link[1]
  }
  if (stringr::str_detect(yt_link, "@")) {
    channel_id <- tryCatch({
      page <- rvest::read_html(yt_link) |> rvest::html_text2()
      sub(".*channel_id=([A-Za-z0-9_-]+).*", "\\1", page)
    }, error = function(e) {
      return(NULL)
    })
  } else {
    channel_id <- stringr::str_extract(yt_link, "(?<=/)[^/.]{24}")
    channel_stats <- tuber::get_channel_stats(channel_id = channel_id)
  }

  if (is.null(channel_id)){
    channel_stats <- list(title = NA, count = NA, subs = NA, views = NA)
  }
} else {
  channel_stats <- list(title = NA, count = NA, subs = NA, views = NA)
}


# if (length(yt_link) != 0) {
#   if (length(yt_link) > 1) {
#     yt_link <- yt_link[1]
#   }
#
#   # everything after last slash
#   channel_id <- stringr::str_extract(yt_link, "(?<=/)[^/]+$") # [^/]+$
#
#   # case normal id 24 chars
#   if (!grepl("@", channel_id)) {
#     channel_id <- stringr::str_sub(channel_id, 1, 24)
#   } else {
#     # case @username
#     html_text <- rvest::read_html(yt_link) |> rvest::html_text2()
#     channel_id <- sub(".*channel_id=([A-Za-z0-9_-]+).*", "\\1", html_text)
#   }
#   channel_stats <- tuber::get_channel_stats(channel_id = channel_id)
# } else {
#   channel_stats <- list(title = NA, count = NA, subs = NA, views = NA)
# }

if (length(tiktok_link) != 0) {

  page <- httr2::request(tiktok_link) |>
    httr2::req_perform() |>
    httr2::resp_body_string()



  # html <- rvest::read_html(tiktok_link) |>
  #   as.character()

  tt_name <- stringr::str_extract(tiktok_link, "(?<=/)[^/]+/?$")

  stats <- stringr::str_extract(
    string = page,
    pattern = '"stats":\\{[^}]*\\}'
  )

  tt_followers <- stringr::str_extract(stats, '(?<=followerCount":)\\d+')

  tt_likes <- stringr::str_extract(stats, '(?<=heart":)\\d+')
} else {
  tt_name <- NA
  tt_followers <- NA
  tt_likes <- NA
}

# Cheerleader page ============================================================

# Cheerleader Name List name
#------------------------------------------------------------------------------
#   table     tibble with (bio + links)
#   bio_table html node (biography table)
#   links     char vector (social media links)

cheerData <- function(wiki_url, team_cheerleaders, values) {

  # cheer_data_list <- list()
  #
  # for (cheerleader in seq_along(team_cheerleaders$link)) {
  #
  #   cheer_data <- getCheerleaderPage(wiki_url,
  #                                    team_cheerleaders$link[cheerleader],
  #                                    values)
  #
  #   cheer_data_list[[team_cheerleaders$cheerleader[cheerleader]]] <- cheer_data
  # }
  #
  # cheer_data_list

  team_cheerleaders$link %>%
    purrr::map(~ getCheerleaderPage(wiki_url, .x, values)) %>%
    purrr::set_names(team_cheerleaders$cheerleader)
}




cheer_data <- cheerData(wiki_url, team_cheerleaders, c("nationality", "birth"))

usethis::use_data(cheerleader_page_data, overwrite = TRUE)

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
    first()

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
      keyword_image_mapping[[keyword]]
    } else {
      NA_character_
    }
  })

  # account for missing values
  social_links <- social_links[social_icons != ""]
  social_icons <- social_icons[social_icons != ""]

  # html_content <- createHTML(social_links, social_icons)

  html_content <- purrr::map2(social_links, social_icons, ~glue::glue(
    '<a href="{.x}" target="_blank"><img src="{.y}" width="42" height="42"></a>&nbsp'
  )) |> paste(collapse = "")

  # table <- extractBioTable(df_tables, c("nationality", "birth"))

  table <- df_tables|>
    purrr::detect(~ any(.x[[1]] %in% values, na.rm = TRUE)) %>%
    dplyr::select(dplyr::where(~ !any(is.na(.))))

  table <- table |> dplyr::filter(!(dplyr::row_number() == 1 & X1 == ""))

  # sometimes tables have 2 row headers
  if (ncol(table) == 2) {
    table <- table |>
      dplyr::mutate(X1 = ifelse(X1 %in% c("|", ""), "link", X1),
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

bioTableIndex <- function(tables, keywords) {

  # for (i in seq_along(tables)) {
  #   table <- tables[[i]]
  #   if (any(!is.na(table$X1))) {
  #     if (any(table$X1 %in% keywords)) {
  #       return(i)
  #     }
  #   }
  # }

  purrr::map_lgl(tables, function(table) {
    any(!is.na(table$X1)) && any(table$X1 %in% keywords)
  }) |>
    which() |>
    first()
}

matchIconsToLinks <- function(links, mapping) {

  # matched_icons <- vector("character", length(links))
  # for (i in seq_along(links)) {
  #   link <- links[i]
  #   for (keyword in names(mapping)) {
  #     if (grepl(keyword, link, ignore.case = TRUE)) {
  #       matched_icons[i] <- mapping[[keyword]]
  #       break
  #     }
  #   }
  # }
  # return(matched_icons)

  purrr::map_chr(links, function(link) {
    keyword <- purrr::detect(
      names(mapping), ~
        stringr::str_detect(link, stringr::regex(.x, ignore_case = TRUE)))
    if (!is.null(keyword)) {
      mapping[[keyword]]
    } else {
      NA_character_
    }
  })
}

createHTML <- function(links, icons) {

  purrr::map2(links, icons, ~glue::glue(
    '<a href="{.x}" target="_blank"><img src="{.y}" width="42" height="42"></a>&nbsp'
  )) |> paste(collapse = "")
}

extractBioTable <- function(tibble_list, values) {

  # for (tbl in tibble_list) {
  #   if (any(tbl[[1]] %in% values, na.rm = TRUE)) {
  #     tbl <- tbl |> dplyr::select(dplyr::where(~ !any(is.na(.))))
  #     return(tbl)
  #   }
  # }

  tibble_list |>
    purrr::detect(~ any(.x[[1]] %in% values, na.rm = TRUE)) %>%
    dplyr::select(dplyr::where(~ !any(is.na(.))))
}


# photo (we need bio to get the photos)

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


# team_page <- shiny::reactive(label = "Team page content", {
#
#   shiny::req(input$team != "")
#
#   url <- paste0(input$team)
#
#   page <- httr2::request(url) |>
#     httr2::req_perform() |>
#     httr2::resp_body_html()
#
#   photo_url <- getTeamPhoto(page)
#
#   list(
#     page = page,
#     photo_url = photo_url
#     )
#   })

# team_info <- shiny::reactive(label = "Team name/colors", {
#
#   shiny::req(input$team)
#
#   browser()
#
#   team_name <- names(teams)[which(unlist(teams) == input$team)]
#
#   team_color <- team_colors[team_name][[1]]
#
#   list(
#     name = team_name,
#     color = team_color
#   )
# })

# cheerleader_page_data

# cheerleader_page <- shiny::reactive(label = "Cheerleader page content", {

# shiny::req(input$team != "")

# page <- fetchCheerleaderPage(wiki_url, input$cheerleader)
#
# if (is.null(page)) {
#   return(
#     list(
#       table = data.frame("NA" = "The requested cheerleader page is not available."),
#       bio_table = data.frame(),
#       links = data.frame()
#     )
#   )
# }
#
# # shiny::req(input$team != "")
# #
# # url <- paste0(wiki_url, input$cheerleader)
# #
# # page <- httr2::request(url) |>
# #   httr2::req_perform() |>
# #   httr2::resp_body_html()
#
# xml_tables <- rvest::html_nodes(page, "table")
#
# df_tables <- lapply(xml_tables, rvest::html_table, fill = TRUE)
#
# index <- bioTableIndex(df_tables, c("nationality", "birth"))
#
# bio_table <- xml_tables[[index]]
#
# hrefs <- bio_table |>
#   rvest::html_nodes("a") |>
#   rvest::html_attr("href")
#
# social_links <- hrefs[grepl("http", hrefs)]
#
# social_icons <- matchIconsToLinks(social_links, keyword_image_mapping)
#
# # account for missing values
# social_links <- social_links[social_icons != ""]
# social_icons <- social_icons[social_icons != ""]
#
# html_content <- createHTML(social_links, social_icons)
#
# table <- extractBioTable(df_tables, c("nationality", "birth"))
#
# table <- table |> dplyr::filter(!(row_number() == 1 & X1 == ""))
#
# # sometimes tables have 2 row headers
# if (ncol(table) == 2) {
#   table <- table |>
#     dplyr::mutate(X1 = ifelse(X1 %in% c("|", ""), "link", X1),
#                   X2 = dplyr::if_else(X1 %in% c("link", "site", "SNS"), html_content, X2)) |>
#     dplyr::filter(X1 != X2,
#                   !X1 %in% c("support team", "platform", "signature"),
#                   !grepl("youtube", X1, ignore.case = TRUE))
# } else if (ncol(table) == 3) {
#   table <- table |>
#     dplyr::mutate(X2 = ifelse(X2 %in% c("|", ""), "link", X2),
#                   X3 = dplyr::if_else(X1 %in% c("link", "site", "SNS"), html_content, X3)) |>
#     dplyr::select(-X1) |>
#     dplyr::rename(X1 = X2, X2 = X3) |>
#     dplyr::filter(X1 != X2,
#                   !X1 %in% c("support team", "platform", "signature"))
# }
#
# table <- table |>
#   dplyr::mutate(X1 = stringr::str_to_title(X1)) |>
#   dplyr::mutate(X2 = ifelse(
#     !stringr::str_detect(X1, "Link|Site|Sns"),
#     stringr::str_to_title(X2),
#     X2)) |>
#
#   # blood type
#   dplyr::mutate(X2 = stringr::str_replace_all(X2, "(?i)\\bab\\b", "AB")) |>
#
#   # Myers-Briggs Type Indicator
#   dplyr::mutate(X1 = dplyr::case_when(
#     X1 == "Mbti" ~ "MBTI",
#     TRUE ~ X1)) |>
#   dplyr::mutate(X2 = dplyr::case_when(
#     X1 == "MBTI" ~ stringr::str_to_upper(X2),
#     TRUE ~ X2
#   )) |>
#
#   # birth instances
#   dplyr::mutate(previous_birth = cumsum(X1 == "Birth")) |>
#   dplyr::mutate(X1 = dplyr::case_when(
#     X1 == "Birth" ~ "Birthday",
#     TRUE ~ X1
#   )) |>
#   dplyr::mutate(X1 = dplyr::case_when(
#     X1 == "Birthday" & previous_birth > 1 ~ "Birthplace",
#     TRUE ~ X1
#   )) |>
#
#   # agency instances
#   dplyr::mutate(previous_agency = cumsum(X1 == "Agency")) |>
#   dplyr::mutate(X1 = dplyr::case_when(
#     X1 == "Agency" & previous_agency > 1 ~ "previous_agency",
#     TRUE ~ X1
#   )) |>
#   dplyr::filter(X1 != "previous_agency") |>
#
#   # cleanup
#   dplyr::mutate(X2 = stringr::str_replace_all(X2, "\\[.*?\\]", "")) |>
#   dplyr::mutate(X2 = stringr::str_replace_all(X2, "\\(\\s*", "("),
#                 X2 = stringr::str_replace_all(X2, "\\s*\\)", ")")) |>
#   dplyr::select(-previous_birth, -previous_agency)
#
# list(
#   table = table,
#   bio_table = bio_table,
#   links = social_links
#   )
# })

# cheerleader <- shiny::reactive(label = "Cheerleader name", {
#
#   browser()
#
#   cheerleaders <- getCheerleaders(input$team)
#
#   names(cheerleaders)[which(unlist(cheerleaders) == input$cheerleader)]
# })






# ORIGINAL YT STATS ===========================================================================
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

# extract_channel_id <- function(url) {
#
#   if (stringr::str_detect(url, "@")) {
#
#     browser()
#
#     page <- tryCatch({
#       rvest::read_html(url) |>
#         rvest::html_text2()
#     }, error = function(e) NULL)
#     if (!is.null(page)) {
#       channel_id <- sub(".*channel_id=([A-Za-z0-9_-]+).*", "\\1", page)
#       if (nchar(channel_id) == 24) {
#         return(channel_id)
#       }
#     }
#
#   } else {
#     channel_id <- stringr::str_extract(url, "(?<=/)[^/.]{24}")
#     if (!is.na(channel_id) && nchar(channel_id) == 24) {
#       return(channel_id)
#     } else (
#       return(NULL)
#     )
#   }
#   return(NULL)
# }

# smm <- shiny::reactive(label = "Social Media Metrics", {
#
#   NULL
#
#   # links <- cheerleader_page()$links
#   #
#   # yt_link <- links[grepl("youtube", links)]
#   # insta_link <- links[grepl("instagram", links)]
#   # tiktok_link <- links[grepl("tiktok", links)]
#   #
#   # youtube <- ytChannelStats(yt_link)
#   #
#   # tiktok <- tiktokStats(tiktok_link)
#   #
#   # # # Instagram ---------------------------------------------------------------
#   # #
#   # # if (length(insta_link) != 0 ) {
#   # #   meta_content <- rvest::read_html(insta_link) |>
#   # #     as.character()
#   # #
#   # #   # page <- httr2::request(insta_link) |>
#   # #   #   httr2::req_perform() |>
#   # #   #   httr2::resp_body_json()
#   # #   #
#   # #   # parsed <- rvest::read_html(page)
#   # #   #
#   # #   # head <- rvest::html_element(parsed, "head")
#   # #
#   # #   inst_followers <- stringr::str_extract(
#   # #     string = meta_content,
#   # #     pattern = '(?<=content=")(.*?)(?= Followers)'
#   # #   )
#   # #
#   # #   inst_name <- stringr::str_extract(insta_link, "(?<=/)[^/]+/?$")
#   # # } else {
#   # #   inst_followers <- NA
#   # #   inst_name <- NA
#   # # }
#   # #
#   # list(
#   #   youtube = youtube,
#   #   tiktok = tiktok
#   # )
# }) |>
#   shiny::bindEvent(input$cheerleader)













# bslib::layout_column_wrap(
#   width = NULL,
#   # fill = FALSE,
#   style = bslib::css(grid_template_columns = "2fr 1fr"),
#   photoBio,
#   bslib::layout_column_wrap(
#     width = NULL,
#     # fill = FALSE,
#     style = bslib::css(grid_template_columns = "1fr"),
#     yt, insta, tiktok
#   )
# )

# Cheerleader UI ==============================================================

createCheerleaderUI <- function(td, cheerleader, smm) {

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

  yt <- dplyr::filter(smm()$youtube, name == cheerleader)

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

  inst <- dplyr::filter(smm()$instagram, name == cheerleader)

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

  tt <- dplyr::filter(smm()$tiktok, cheername == cheerleader)

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

} #


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
