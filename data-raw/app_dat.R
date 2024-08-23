# # Team Page Data ==============================================================
#
# team_cheerleaders <-getTeamCheerleaders(team_data$url)
#
# usethis::use_data(team_cheerleaders, overwrite = TRUE)
#
# getTeamPhotos(team_data)
#
# team_photos <- list.files("./www/team_img")
#
# usethis::use_data(team_photos, overwrite = TRUE)
#
# # Cheerleader Page Data =======================================================
#
# cheer_data <- cheerData(wiki_url = wiki_url,
#                         team_cheerleaders = team_cheerleaders,
#                         values = c("nationality", "birth"))
#
# # store html nodes in .rds
# bio_tables <- purrr::map(names(cheer_data), ~ {
#   cheerleader_data <- cheer_data[[.x]]
#   as.character(cheerleader_data$bio_table)
# })
#
# names(bio_tables) <- names(cheer_data)
#
# readr::write_rds(bio_tables, "./data/bio_tables.rds")
#
# # clean cheer data
# # remove bio_table
# cheer_data <- lapply(cheer_data, function(item) {
#   item[!names(item) %in% "bio_table"]
# })
# # omit NA links
# cheer_data <- purrr::map(cheer_data, ~ {
#   if ("links" %in% names(.x)) {
#     .x$links <- .x$links[!is.na(.x$links)]
#   }
#   .x
# })
# # omit dupe link
# cheer_data$`Hannah Kim`$table <- cheer_data$`Hannah Kim`$table[-1, ]
# cheer_data$`Hyein Na`$table <- cheer_data$`Hyein Na`$table[-1, ]
#
# usethis::use_data(cheer_data, overwrite = TRUE)
#
# # get cheerleader photos
#
# getCheerleaderPhotos(bio_tables)
#
# # Social Media Data ===========================================================
#
# # YouTube =====================================================================
#
# global_token <- NULL
#
# authenticateYouTube <- function() {
#
#   # make the token
#   # YTAnalytics::youtube_oauth(
#   #   clientId = Sys.getenv("YT_CLIENT_ID"),
#   #   clientSecret = Sys.getenv("YT_CLIENT_SECRET")
#   #   )
#
#   # file.remove(".httr-oauth")
#   #
#   # tuber::yt_oauth(
#   #   app_id = Sys.getenv("YT_CLIENT_ID"),
#   #   app_secret = Sys.getenv("YT_CLIENT_SECRET"),
#   #   token = '.httr-oauth'
#   # )
#   # global_token <<- TRUE
#
#   # shiny::observe(label = "Authenticate", priority = 300, {
#   #   if (is.null(global_token)) {
#   #     authenticateYouTube()
#   #   }
#   # })
#
#   # if (is.null(global_token)) {
#   #   authenticateYouTube()
#   # }
# }
#
# youtube <- getYouTube(cheer_data)
# lookup <- setNames(team_cheerleaders$team, team_cheerleaders$cheerleader)
# youtube$team <- lookup[youtube$name]
# youtube$cat <- "youtube"
#
# usethis::use_data(youtube, overwrite = TRUE)
#
# # Instagram ===================================================================
#
# instagram <- getInstagram(cheer_data)
#
# glimpse(instagram)
#
# lookup <- setNames(team_cheerleaders$team, team_cheerleaders$cheerleader)
# instagram$team <- lookup[instagram$name]
# instagram$cat <- "instagram"
#
# usethis::use_data(instagram, overwrite = TRUE)
#
# # TikTok ======================================================================
#
# tiktok <- getTikTok(cheer_data)
#
# glimpse(tiktok)
#
# lookup <- setNames(team_cheerleaders$team, team_cheerleaders$cheerleader)
# tiktok$team <- lookup[tiktok$cheername]
# tiktok$cat <- "tiktok"
#
# usethis::use_data(tiktok, overwrite = TRUE)
#
# # ultra_combo =================================================================
#
# ultra_combo <- ultraCombo(youtube, instagram, tiktok)
#
# usethis::use_data(ultra_combo, overwrite = TRUE)
#
# # Followers Across Teams ======================================================
#
# fat_plot <- fatPlot(ultra_combo)
#
# usethis::use_data(fat_plot, overwrite = TRUE)
#
# fat_distro_plot <- fatDistroPlot(ultra_combo)
#
# usethis::use_data(fat_distro, overwrite = TRUE)

#####

KBODataUpdate <- function() {

  # Source all functions to backup/generate new data:

  source("./R/data_mining.R")

  #   backup()
  #   getTeamCheerleaders()
  #   getTeamPhotos()
  #   cheerData()
  #   getCheerleaderPhotos()
  #   getYouTube()
  #   getInstagram()
  #   getTikTok()
  #   ultra_combo(youtube, instagram, tiktok)
  #   fat_plot(ultra_combo)
  #   fat_dist_plot(ultra_combo)

  # Backup --------------------------------------------------------------------
  #
  # ./data/*
  # ./www/*

  backup()

  # Create and Save Data:

  source("./data-raw/sources.R")

  #   wiki_url
  #   team_data
  #   team_logos              getTeamLogos()
  #   team_caps               getTeamCap()
  #   keyword_image_mapping
  #   introduction
  #   footer

  # Team Cheerleaders/Team Photos ---------------------------------------------

  team_cheerleaders <-getTeamCheerleaders(team_data$url)
  usethis::use_data(team_cheerleaders, overwrite = TRUE)
  print("Successfully made team_cheerleaders.rda")

  getTeamPhotos(team_data)
  team_photos <- list.files("./www/team_img")
  usethis::use_data(team_photos, overwrite = TRUE)
  print("Successfully made team_photos.rda")

  # Cheerleader Data ----------------------------------------------------------

  cheer_data <- cheerData(wiki_url = wiki_url,
                          team_cheerleaders = team_cheerleaders,
                          values = c("nationality", "birth"))
  # store html nodes in .rds
  bio_tables <- purrr::map(names(cheer_data), ~ {
    cheerleader_data <- cheer_data[[.x]]
    as.character(cheerleader_data$bio_table)
  })
  names(bio_tables) <- names(cheer_data)
  readr::write_rds(bio_tables, "./data/bio_tables.rds")
  print("Successfully made bio_data.rds")


  cheer_data <- lapply(cheer_data, function(item) {
    item[!names(item) %in% "bio_table"]
  })
  cheer_data <- purrr::map(cheer_data, ~ {
    if ("links" %in% names(.x)) {
      .x$links <- .x$links[!is.na(.x$links)]
    }
    .x
  })
  cheer_data$`Hannah Kim`$table <- cheer_data$`Hannah Kim`$table[-1, ]
  cheer_data$`Hyein Na`$table <- cheer_data$`Hyein Na`$table[-1, ]
  usethis::use_data(cheer_data, overwrite = TRUE)

  # cheerleader photos
  getCheerleaderPhotos(bio_tables, cheer_data)
  print("Successfully made cheer_data.rda and bio_data.rds")

  # Social Media Data ---------------------------------------------------------

  # authenticate youtube
  # file.remove(".httr-oauth")
  # tuber::yt_oauth(
  #   app_id = Sys.getenv("YT_CLIENT_ID"),
  #   app_secret = Sys.getenv("YT_CLIENT_SECRET")
  #   )
  #
  # Sys.sleep(3)

  # youtube
  youtube <- getYouTube(cheer_data)
  lookup <- setNames(team_cheerleaders$team, team_cheerleaders$cheerleader)
  youtube$team <- lookup[youtube$name]
  youtube$cat <- "youtube"

  # instagram
  instagram <- getInstagram(cheer_data)

  lookup <- setNames(team_cheerleaders$team, team_cheerleaders$cheerleader)
  instagram$team <- lookup[instagram$name]
  instagram$cat <- "instagram"

  # tiktok
  tiktok <- getTikTok(cheer_data)

  lookup <- setNames(team_cheerleaders$team, team_cheerleaders$cheerleader)
  tiktok$team <- lookup[tiktok$cheername]
  tiktok$cat <- "tiktok"

  usethis::use_data(youtube, overwrite = TRUE)
  usethis::use_data(instagram, overwrite = TRUE)
  usethis::use_data(tiktok, overwrite = TRUE)

  print("Successfully made YouTube.rds, Instagram.rds and TikTok.rds")

  # Ultra Combo ---------------------------------------------------------------

  ultra_combo <- ultraCombo(team_cheerleaders,
                            cheer_data,
                            youtube,
                            instagram,
                            tiktok
                            )

  usethis::use_data(ultra_combo, overwrite = TRUE)

  historic <- loadHistoricalData("../")

  usethis::use_data(historic, overwrite = TRUE)

  fat_plot <- fatPlot(ultra_combo)
  usethis::use_data(fat_plot, overwrite = TRUE)

  fat_distro_plot <- fatDistroPlot(ultra_combo)
  usethis::use_data(fat_distro_plot, overwrite = TRUE)

  print("Successfully made ultra_combo.rds, histori.rds, fat_plot.rds, fat_distro_plot.rds")

  long <- makeUltraLong(ultra_combo)
  usethis::use_data(long, overwrite = TRUE)

  age_jitter_dist <- ageJitterDist(long, team_data)
  usethis::use_data(age_jitter_dist, overwrite = TRUE)

  age_dist <- ageDist(long)
  usethis::use_data(age_dist, overwrite = TRUE)

}



# KBODataUpdate()

# backup()
# 8 weeks of observations
#
# 8.1.24  original made on 8.17.24
# 8.17.24 2 weeks of social media metrics
# 8.24.24 1 wk
# 8.31.24 1 wk
# 9.7.24  1 wk
# 9.14.24 1 wk
# 9.21.24 1 wk






































