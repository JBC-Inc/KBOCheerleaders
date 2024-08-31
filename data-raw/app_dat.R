
KBODataUpdate <- function(use_data = FALSE) {

  # Source all functions to backup/generate new data:

  source("./data-raw/data_mining.R")

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

  # Create and Save Data:

  source("./data-raw/sources.R")

  #   wiki_url
  #   team_data
  #   team_logos              getTeamLogos()
  #   team_caps               getTeamCap()
  #   keyword_image_mapping
  #   introduction
  #   footer

  # TODO: MANUAL OVERRIDE

  # # Team Cheerleaders/Team Photos ---------------------------------------------
  #
  # team_cheerleaders <-getTeamCheerleaders(team_data$url)
  # if (use_data) {
  #   usethis::use_data(team_cheerleaders, overwrite = TRUE)
  # }
  # print("Successfully made team_cheerleaders.rda")
  #
  # getTeamPhotos(team_data)
  # team_photos <- list.files("./www/team_img")
  # if (use_data) {
  #   usethis::use_data(team_photos, overwrite = TRUE)
  # }
  # print("Successfully made team_photos.rda")
  #
  # # Cheerleader Data ----------------------------------------------------------
  #
  # cheer_data <- cheerData(wiki_url = wiki_url,
  #                         team_cheerleaders = team_cheerleaders,
  #                         values = c("nationality", "birth"))
  # # store html nodes in .rds
  # bio_tables <- purrr::map(names(cheer_data), ~ {
  #   cheerleader_data <- cheer_data[[.x]]
  #   as.character(cheerleader_data$bio_table)
  # })
  # names(bio_tables) <- names(cheer_data)
  # if (use_data) {
  #   readr::write_rds(bio_tables, "./data/bio_tables.rds")
  # }
  # print("Successfully made bio_data.rds")
  #
  #
  # cheer_data <- lapply(cheer_data, function(item) {
  #   item[!names(item) %in% "bio_table"]
  # })
  # cheer_data <- purrr::map(cheer_data, ~ {
  #   if ("links" %in% names(.x)) {
  #     .x$links <- .x$links[!is.na(.x$links)]
  #   }
  #   .x
  # })
  #
  # cheer_data$`Hannah Kim`$table <- cheer_data$`Hannah Kim`$table[-1, ]
  # cheer_data$`Hyein Na`$table <- cheer_data$`Hyein Na`$table[-1, ]
  #
  # if (use_data) {
  #   usethis::use_data(cheer_data, overwrite = TRUE)
  # }
  #
  # # cheerleader photos
  # getCheerleaderPhotos(bio_tables, cheer_data)
  # print("Successfully made cheer_data.rda and bio_data.rds")

  # Social Media Data ---------------------------------------------------------

  # # authenticate youtube
  # file.remove(".httr-oauth")
  # tuber::yt_oauth(
  #   app_id = Sys.getenv("YT_CLIENT_ID"),
  #   app_secret = Sys.getenv("YT_CLIENT_SECRET")
  #   )
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

  if (use_data) {
    usethis::use_data(youtube, overwrite = TRUE)
    usethis::use_data(instagram, overwrite = TRUE)
    usethis::use_data(tiktok, overwrite = TRUE)
  }

  print("Successfully made YouTube.rda, Instagram.rda and TikTok.rda")

  # Ultra Combo ---------------------------------------------------------------

  ultra_combo <- ultraCombo(team_cheerleaders,
                            cheer_data,
                            youtube,
                            instagram,
                            tiktok
                            )

  if (use_data) {
    usethis::use_data(ultra_combo, overwrite = TRUE)
  }

  # Historic made from each weeks ultra_combo ---------------------------------

  historic <- loadHistoricalData("../")

  if (use_data) {
    usethis::use_data(historic, overwrite = TRUE)
  }

  # Ultra plots ---------------------------------------------------------------

  fat_plot <- fatPlot(ultra_combo)

  if (use_data) {
    usethis::use_data(fat_plot, overwrite = TRUE)
  }

  fat_distro_plot <- fatDistroPlot(ultra_combo)

  if (use_data) {
    usethis::use_data(fat_distro_plot, overwrite = TRUE)
  }

  print("Successfully made ultra_combo.rda, histori.rda, fat_plot.rda, fat_distro_plot.rda")

  # Long + long plots ---------------------------------------------------------

  long <- makeUltraLong(ultra_combo)

  if (use_data) {
    usethis::use_data(long, overwrite = TRUE)
  }

  age_jitter_dist <- ageJitterDist(long, team_data)

  if (use_data) {
    usethis::use_data(age_jitter_dist, overwrite = TRUE)
  }

  age_dist <- ageDist(long)

  if (use_data) {
    usethis::use_data(age_dist, overwrite = TRUE)
  }

  print("successfully made long.rda, age_jitter_dist.rda, age_dist.rda")

  # Backup --------------------------------------------------------------------
  #
  # ./data/*
  # ./www/*

  # backup()
}



# KBODataUpdate(use_data = TRUE)

# backup()
# 8 weeks of observations
#
# 8.1.24    INIT
# 8.17.24   2 weeks collected
# 8.24.24   collected
# 8.31.24
# 9.7.24
# 9.14.24
# 9.21.24






































