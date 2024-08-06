
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
