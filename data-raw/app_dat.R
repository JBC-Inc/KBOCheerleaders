
wiki_url <- "https://en.namu.wiki"

usethis::use_data(wiki_url, internal = FALSE, overwrite = TRUE)

###############################################################################
############################# TEAM DATA #######################################
###############################################################################

# "Doosan Bears"  = "",
# "Hanwha Eagles" = "",
# "Kia Tigers"    = "",
# "Kiwoom Heros"  = "",
# "KT Wiz"        = "",
# "LG Twins"      = "",
# "Lotte Giants"  = "",
# "NC Dinos"      = "",
# "Samsung Lions" = "",
# "SSG Landers"   = ""

name <- c(
  "Doosan Bears",
  "Hanwha Eagles",
  "Kia Tigers",
  "Kiwoom Heros",
  "KT Wiz",
  "LG Twins",
  "Lotte Giants",
  "NC Dinos",
  "Samsung Lions",
  "SSG Landers"
  )

url = c(
  "https://en.namu.wiki/w/%EB%91%90%EC%82%B0%20%EB%B2%A0%EC%96%B4%EC%8A%A4/%EC%B9%98%EC%96%B4%EB%A6%AC%EB%8D%94",
  "https://en.namu.wiki/w/%ED%95%9C%ED%99%94%20%EC%9D%B4%EA%B8%80%EC%8A%A4/%EC%B9%98%EC%96%B4%EB%A6%AC%EB%8D%94",
  "https://en.namu.wiki/w/KIA%20%ED%83%80%EC%9D%B4%EA%B1%B0%EC%A6%88/%EC%B9%98%EC%96%B4%EB%A6%AC%EB%8D%94#s-5.1",
  "https://en.namu.wiki/w/%ED%82%A4%EC%9B%80%20%ED%9E%88%EC%96%B4%EB%A1%9C%EC%A6%88/%EC%B9%98%EC%96%B4%EB%A6%AC%EB%8D%94",
  "https://en.namu.wiki/w/kt%20wiz/%EC%B9%98%EC%96%B4%EB%A6%AC%EB%8D%94",
  "https://en.namu.wiki/w/LG%20%ED%8A%B8%EC%9C%88%EC%8A%A4/%EC%B9%98%EC%96%B4%EB%A6%AC%EB%8D%94",
  "https://en.namu.wiki/w/%EB%A1%AF%EB%8D%B0%20%EC%9E%90%EC%9D%B4%EC%96%B8%EC%B8%A0/%EC%B9%98%EC%96%B4%EB%A6%AC%EB%8D%94",
  "https://en.namu.wiki/w/NC%20%EB%8B%A4%EC%9D%B4%EB%85%B8%EC%8A%A4/%EC%B9%98%EC%96%B4%EB%A6%AC%EB%8D%94",
  "https://en.namu.wiki/w/%EC%82%BC%EC%84%B1%20%EB%9D%BC%EC%9D%B4%EC%98%A8%EC%A6%88/%EC%B9%98%EC%96%B4%EB%A6%AC%EB%8D%94",
  "https://en.namu.wiki/w/SSG%20%EB%9E%9C%EB%8D%94%EC%8A%A4/%EC%B9%98%EC%96%B4%EB%A6%AC%EB%8D%94"
  )

color = c(
  "#131230",
  "#ff6600",
  "#EC0F32",
  "#820023",
  "#000000",
  "#a50034",
  "#041e42",
  "#071d3d",
  "#074CA1",
  "#CF112D"
  )

themesong = c(
  "https://www.youtube.com/watch?v=IeyOr91IuyA",
  "https://www.youtube.com/watch?v=45IrJtdGtDk",
  "https://www.youtube.com/watch?v=6RiMyqT3_t0",
  "https://www.youtube.com/watch?v=antR6UYqZKk",
  "https://www.youtube.com/watch?v=bsWBORqEjpI",
  "https://www.youtube.com/watch?v=CeGiBg9eXG0",
  "https://www.youtube.com/watch?v=NSR5kAxIEi0",
  "https://www.youtube.com/watch?v=QmgeKEe-LEE",
  "https://www.youtube.com/watch?v=AmQC5K6_HUs",
  "https://www.youtube.com/watch?v=zX7uot4biaQ"
)

team_data <-
  data.frame(
    name = name,
    url = url,
    color = color,
    song = themesong
  )

usethis::use_data(team_data, internal = FALSE, overwrite = TRUE)

###############################################################################
############################# TEAM LOGO #######################################
###############################################################################

team_logos = list(
  "Doosan Bears"  = "https://upload.wikimedia.org/wikipedia/en/thumb/9/98/Doosan_Bears.svg/1280px-Doosan_Bears.svg.png",
  "Hanwha Eagles" = "https://upload.wikimedia.org/wikipedia/en/thumb/d/d3/Hanwha_Eagles.svg/1920px-Hanwha_Eagles.svg.png",
  "Kia Tigers"    = "https://upload.wikimedia.org/wikipedia/en/e/e0/Kia_Tigers_2017_New_Team_Logo.png",
  "Kiwoom Heros"  = "https://upload.wikimedia.org/wikipedia/en/4/4f/Kiwoom_Heroes.png",
  "KT Wiz"        = "https://upload.wikimedia.org/wikipedia/en/thumb/e/e5/KT_Wiz.svg/1024px-KT_Wiz.svg.png",
  "LG Twins"      = "https://upload.wikimedia.org/wikipedia/commons/4/41/LG_Twins_2017.png",
  "Lotte Giants"  = "https://upload.wikimedia.org/wikipedia/en/thumb/6/65/Lotte_Giants.svg/1280px-Lotte_Giants.svg.png",
  "NC Dinos"      = "https://upload.wikimedia.org/wikipedia/en/thumb/5/54/NC_Dinos_Emblem.svg/1280px-NC_Dinos_Emblem.svg.png",
  "Samsung Lions" = "https://upload.wikimedia.org/wikipedia/en/thumb/0/0e/Samsung_Lions.svg/1280px-Samsung_Lions.svg.png",
  "SSG Landers"   = "https://upload.wikimedia.org/wikipedia/en/8/86/SSG_Landers.png"
)

getTeamLogos(team_logos)

team_logos <- list.files("./www/team_logo")

usethis::use_data(team_logos, overwrite = TRUE)

###############################################################################
############################# TEAM CAP ########################################
###############################################################################

team_caps = list(
  "Doosan Bears"  = "https://upload.wikimedia.org/wikipedia/commons/thumb/a/a2/Doosan_Bears_insignia.svg/1024px-Doosan_Bears_insignia.svg.png",
  "Hanwha Eagles" = "https://upload.wikimedia.org/wikipedia/en/thumb/d/d4/Hanwha_Eagles_cap_logo.svg/1024px-Hanwha_Eagles_cap_logo.svg.png",
  "Kia Tigers"    = "https://upload.wikimedia.org/wikipedia/en/8/8c/Kia_Tigers_2017_New_insignia.png",
  "Kiwoom Heros"  = "https://upload.wikimedia.org/wikipedia/commons/6/6e/Kiwoom_Heroes_insignia.png",
  "KT Wiz"        = "https://upload.wikimedia.org/wikipedia/commons/thumb/8/80/KT_Wiz_insignia.svg/1024px-KT_Wiz_insignia.svg.png",
  "LG Twins"      = "https://upload.wikimedia.org/wikipedia/commons/thumb/c/c6/LG_Twins_Cap_Logo.svg/1024px-LG_Twins_Cap_Logo.svg.png",
  # "Lotte Giants"  = "https://upload.wikimedia.org/wikipedia/commons/b/bf/Busan_Giants_cap_insignia.svg",
  "NC Dinos"      = "https://upload.wikimedia.org/wikipedia/en/0/09/NC_Dinos_cap_insignia.png",
  "Samsung Lions" = "https://upload.wikimedia.org/wikipedia/commons/thumb/4/44/Samsung_Lions_insignia.svg/1024px-Samsung_Lions_insignia.svg.png",
  "SSG Landers"   = "https://upload.wikimedia.org/wikipedia/commons/2/2f/SSG_Landers_insignia.png"
)

getTeamCap(team_caps)

team_caps <- list.files("./www/team_cap")

usethis::use_data(team_caps, overwrite = TRUE)

###############################################################################
#################### Social Media Icon Mapping ################################
###############################################################################

keyword_image_mapping = list(
  afreecatv  = "afreecatv.jpg",
  facebook   = "facebook.png",
  instagram  = "instagram.png",
  likey      = "likey.jpg",
  naver      = "naver.jpg",
  threads    = "threads.jpg",
  tiktok     = "tiktok.jpg",
  `x.com`    = "twitter.jpg",
  youtube    = "youtube.png"
)

usethis::use_data(keyword_image_mapping, overwrite = TRUE)

###############################################################################
############################ TEAM PAGE DATA ###################################
###############################################################################

team_cheerleaders <-getTeamCheerleaders(team_data$url)

usethis::use_data(team_cheerleaders, overwrite = TRUE)

getTeamPhotos(team_data)

team_photos <- list.files("./www/team_img")

usethis::use_data(team_photos, overwrite = TRUE)

###############################################################################
######################## CHEERLEADER PAGE DATA ################################
###############################################################################

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

# clean cheer data
# remove bio_table
cheer_data <- lapply(cheer_data, function(item) {
  item[!names(item) %in% "bio_table"]
})
# omit NA links
cheer_data <- purrr::map(cheer_data, ~ {
  if ("links" %in% names(.x)) {
    .x$links <- .x$links[!is.na(.x$links)]
  }
  .x
})
# omit dupe link
cheer_data$`Hannah Kim`$table <- cheer_data$`Hannah Kim`$table[-1, ]
cheer_data$`Hyein Na`$table <- cheer_data$`Hyein Na`$table[-1, ]

usethis::use_data(cheer_data, overwrite = TRUE)

# get cheerleader photos

getCheerleaderPhotos(bio_tables)

###############################################################################
######################## SOCIAL MEDIA METRICS #################################
###############################################################################

# YouTube =====================================================================

# Authentication ==============================================================

global_token <- NULL

authenticateYouTube <- function() {

  # make the token
  # YTAnalytics::youtube_oauth(
  #   clientId = Sys.getenv("YT_CLIENT_ID"),
  #   clientSecret = Sys.getenv("YT_CLIENT_SECRET")
  #   )

  # file.remove(".httr-oauth")
  #
  # tuber::yt_oauth(
  #   app_id = Sys.getenv("YT_CLIENT_ID"),
  #   app_secret = Sys.getenv("YT_CLIENT_SECRET"),
  #   token = '.httr-oauth'
  # )
  # global_token <<- TRUE

  # shiny::observe(label = "Authenticate", priority = 300, {
  #   if (is.null(global_token)) {
  #     authenticateYouTube()
  #   }
  # })

  # if (is.null(global_token)) {
  #   authenticateYouTube()
  # }
}

youtube <- getYouTube(cheer_data)
lookup <- setNames(team_cheerleaders$team, team_cheerleaders$cheerleader)
youtube$team <- lookup[youtube$name]
youtube$cat <- "youtube"

usethis::use_data(youtube, overwrite = TRUE)

# Instagram ===================================================================

instagram <- getInstagram(cheer_data)

glimpse(instagram)

lookup <- setNames(team_cheerleaders$team, team_cheerleaders$cheerleader)
instagram$team <- lookup[instagram$name]
instagram$cat <- "instagram"

usethis::use_data(instagram, overwrite = TRUE)

# TikTok ======================================================================

tiktok <- getTikTok(cheer_data)

glimpse(tiktok)

lookup <- setNames(team_cheerleaders$team, team_cheerleaders$cheerleader)
tiktok$team <- lookup[tiktok$cheername]
tiktok$cat <- "tiktok"

usethis::use_data(tiktok, overwrite = TRUE)

# Followers Across Teams ======================================================

fat <- makeFat(youtube, instagram, tiktok)

usethis::use_data(fat, overwrite = TRUE)

fat_plot <- followersAcrossTeams(fat)

usethis::use_data(fat_plot, overwrite = TRUE)









