
wiki_url <- "https://en.namu.wiki"

usethis::use_data(wiki_url, internal = FALSE, overwrite = TRUE)

###############################################################################
############################# GEN TEAM DATA ###################################
###############################################################################

# 11 Doosan Bears   https://en.namu.wiki/w/%EB%91%90%EC%82%B0%20%EB%B2%A0%EC%96%B4%EC%8A%A4/%EC%B9%98%EC%96%B4%EB%A6%AC%EB%8D%94
#  8 Hanwha Eagles  https://en.namu.wiki/w/%ED%95%9C%ED%99%94%20%EC%9D%B4%EA%B8%80%EC%8A%A4/%EC%B9%98%EC%96%B4%EB%A6%AC%EB%8D%94
# 12 Kia Tigers     https://en.namu.wiki/w/KIA%20%ED%83%80%EC%9D%B4%EA%B1%B0%EC%A6%88/%EC%B9%98%EC%96%B4%EB%A6%AC%EB%8D%94#s-5.1
#  8 Kiwoom Heros   https://en.namu.wiki/w/%ED%82%A4%EC%9B%80%20%ED%9E%88%EC%96%B4%EB%A1%9C%EC%A6%88/%EC%B9%98%EC%96%B4%EB%A6%AC%EB%8D%94
#  8 KT Wiz         https://en.namu.wiki/w/kt%20wiz/%EC%B9%98%EC%96%B4%EB%A6%AC%EB%8D%94
#  9 LG Twins       https://en.namu.wiki/w/LG%20%ED%8A%B8%EC%9C%88%EC%8A%A4/%EC%B9%98%EC%96%B4%EB%A6%AC%EB%8D%94
# 10 Lotte Giants   https://en.namu.wiki/w/%EB%A1%AF%EB%8D%B0%20%EC%9E%90%EC%9D%B4%EC%96%B8%EC%B8%A0/%EC%B9%98%EC%96%B4%EB%A6%AC%EB%8D%94
# 10 NC Dinos       https://en.namu.wiki/w/NC%20%EB%8B%A4%EC%9D%B4%EB%85%B8%EC%8A%A4/%EC%B9%98%EC%96%B4%EB%A6%AC%EB%8D%94
# 11 Samsung Lions  https://en.namu.wiki/w/%EC%82%BC%EC%84%B1%20%EB%9D%BC%EC%9D%B4%EC%98%A8%EC%A6%88/%EC%B9%98%EC%96%B4%EB%A6%AC%EB%8D%94
# 10 SSG Landers    https://en.namu.wiki/w/SSG%20%EB%9E%9C%EB%8D%94%EC%8A%A4/%EC%B9%98%EC%96%B4%EB%A6%AC%EB%8D%94

teams = list(
  "Doosan Bears"  = "https://en.namu.wiki/w/%EB%91%90%EC%82%B0%20%EB%B2%A0%EC%96%B4%EC%8A%A4/%EC%B9%98%EC%96%B4%EB%A6%AC%EB%8D%94",
  "Hanwha Eagles" = "https://en.namu.wiki/w/%ED%95%9C%ED%99%94%20%EC%9D%B4%EA%B8%80%EC%8A%A4/%EC%B9%98%EC%96%B4%EB%A6%AC%EB%8D%94",
  "Kia Tigers"    = "https://en.namu.wiki/w/KIA%20%ED%83%80%EC%9D%B4%EA%B1%B0%EC%A6%88/%EC%B9%98%EC%96%B4%EB%A6%AC%EB%8D%94#s-5.1",
  "Kiwoom Heros"  = "https://en.namu.wiki/w/%ED%82%A4%EC%9B%80%20%ED%9E%88%EC%96%B4%EB%A1%9C%EC%A6%88/%EC%B9%98%EC%96%B4%EB%A6%AC%EB%8D%94",
  "KT Wiz"        = "https://en.namu.wiki/w/kt%20wiz/%EC%B9%98%EC%96%B4%EB%A6%AC%EB%8D%94",
  "LG Twins"      = "https://en.namu.wiki/w/LG%20%ED%8A%B8%EC%9C%88%EC%8A%A4/%EC%B9%98%EC%96%B4%EB%A6%AC%EB%8D%94",
  "Lotte Giants"  = "https://en.namu.wiki/w/%EB%A1%AF%EB%8D%B0%20%EC%9E%90%EC%9D%B4%EC%96%B8%EC%B8%A0/%EC%B9%98%EC%96%B4%EB%A6%AC%EB%8D%94",
  "NC Dinos"      = "https://en.namu.wiki/w/NC%20%EB%8B%A4%EC%9D%B4%EB%85%B8%EC%8A%A4/%EC%B9%98%EC%96%B4%EB%A6%AC%EB%8D%94",
  "Samsung Lions" = "https://en.namu.wiki/w/%EC%82%BC%EC%84%B1%20%EB%9D%BC%EC%9D%B4%EC%98%A8%EC%A6%88/%EC%B9%98%EC%96%B4%EB%A6%AC%EB%8D%94",
  "SSG Landers"   = "https://en.namu.wiki/w/SSG%20%EB%9E%9C%EB%8D%94%EC%8A%A4/%EC%B9%98%EC%96%B4%EB%A6%AC%EB%8D%94"
)

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

cap_insignia = list(
  "Doosan Bears"  = "https://upload.wikimedia.org/wikipedia/commons/thumb/a/a2/Doosan_Bears_insignia.svg/1024px-Doosan_Bears_insignia.svg.png",
  "Hanwha Eagles" = "https://upload.wikimedia.org/wikipedia/en/thumb/d/d4/Hanwha_Eagles_cap_logo.svg/1024px-Hanwha_Eagles_cap_logo.svg.png",
  "Kia Tigers"    = "https://upload.wikimedia.org/wikipedia/en/8/8c/Kia_Tigers_2017_New_insignia.png",
  "Kiwoom Heros"  = "https://upload.wikimedia.org/wikipedia/commons/6/6e/Kiwoom_Heroes_insignia.png",
  "KT Wiz"        = "https://upload.wikimedia.org/wikipedia/commons/thumb/8/80/KT_Wiz_insignia.svg/1024px-KT_Wiz_insignia.svg.png",
  "LG Twins"      = "https://upload.wikimedia.org/wikipedia/commons/thumb/c/c6/LG_Twins_Cap_Logo.svg/1024px-LG_Twins_Cap_Logo.svg.png",
  "Lotte Giants"  = "https://upload.wikimedia.org/wikipedia/commons/b/bf/Busan_Giants_cap_insignia.svg",
  "NC Dinos"      = "https://upload.wikimedia.org/wikipedia/en/0/09/NC_Dinos_cap_insignia.png",
  "Samsung Lions" = "https://upload.wikimedia.org/wikipedia/commons/thumb/4/44/Samsung_Lions_insignia.svg/1024px-Samsung_Lions_insignia.svg.png",
  "SSG Landers"   = "https://upload.wikimedia.org/wikipedia/commons/2/2f/SSG_Landers_insignia.png"
)

team_colors = list(
  "Doosan Bears"  = "#131230",
  "Hanwha Eagles" = "#ff6600",
  "Kia Tigers"    = "#EC0F32",
  "Kiwoom Heros"  = "#820023",
  "KT Wiz"        = "#000000",
  "LG Twins"      = "#a50034",
  "Lotte Giants"  = "#041e42",
  "NC Dinos"      = "#071d3d",
  "Samsung Lions" = "#074CA1",
  "SSG Landers"   = "#CF112D"
)

team_names <- names(teams)

team_data <- data.frame(name = team_names,
                        url = sapply(team_names, function(name) teams[[name]]),
                        logo = sapply(team_names, function(name) team_logos[[name]]),
                        insignia = sapply(team_names, function(name) cap_insignia[[name]]),
                        color = sapply(team_names, function(name) team_colors[[name]]))

usethis::use_data(team_data, internal = FALSE, overwrite = TRUE)

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

usethis::use_data(keyword_image_mapping, internal = FALSE, overwrite = TRUE)

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

###############################################################################
############################ TEAM PAGE DATA ###################################
###############################################################################

team_cheerleaders <-getTeamCheerleaders(team_data$url)

usethis::use_data(team_cheerleaders, internal = FALSE, overwrite = TRUE)

getTeamPhotos(team_data)

team_photos <- list.files("./www/team_img")

usethis::use_data(team_photos, internal = FALSE, overwrite = TRUE)

###############################################################################
######################## CHEERLEADER PAGE DATA ################################
###############################################################################

cheer_data <- cheerData(wiki_url = wiki_url,
                        team_cheerleaders = team_cheerleaders,
                        values = c("nationality", "birth"))

# html nodes cant be stored as *.rda
bio_tables <- purrr::map(names(cheer_data), ~ {
  cheerleader_data <- cheer_data[[.x]]
  as.character(cheerleader_data$bio_table)
})

names(bio_tables) <- names(cheer_data)

readr::write_rds(bio_tables, "./data/bio_tables.rds")

cheer_data <- purrr::map(cheer_data, ~ {
  .x$bio_table <- NULL
  .x
})

usethis::use_data(cheer_data, internal = FALSE, overwrite = TRUE)

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

  file.remove(".httr-oauth")

  tuber::yt_oauth(
    app_id = Sys.getenv("YT_CLIENT_ID"),
    app_secret = Sys.getenv("YT_CLIENT_SECRET"),
    token = '.httr-oauth'
  )
  global_token <<- TRUE

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

usethis::use_data(youtube, internal = FALSE, overwrite = TRUE)

# Cheerleaders / Teams with over 1,000,000 views

youtube |>
  filter(views > 1000000) |>
  ggplot(aes(x = reorder(paste0(team, "\n", name), -views), y = views)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  scale_y_continuous(breaks = seq(0, 100000000, 10000000),
                     labels = scales::label_number(scale = 1e-6, suffix = "M")) +
  labs(title = "Greater than 1,000,000 views", x = "", y = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Instagram ===================================================================

instagram <- getInstagram(cheer_data)

glimpse(instagram)

usethis::use_data(instagram, internal = FALSE, overwrite = TRUE)

# TikTok ======================================================================

tiktok <- getTikTok(cheer_data)

glimpse(tiktok)

usethis::use_data(tiktok, internal = FALSE, overwrite = TRUE)











#==============================================================================
#==============================================================================
#==============================================================================
#==============================================================================
#==============================================================================
#==============================================================================
#==============================================================================
#==============================================================================

# wiki_url    "https://en.namu.wiki"

# team_data
#
# Rows: 10
# Columns: 5
# $ name     <chr> "Doosan Bears", "Hanwha Eagles", "Kia Tigers", "Kiwoom Heros", "KT Wiz", "LG Twins", "Lo…
# $ url      <chr> "https://en.namu.wiki/w/%EB%91%90%EC%82%B0%20%EB%B2%A0%EC%96%B4%EC%8A%A4/%EC%B9%98%EC%96…
# $ logo     <chr> "https://upload.wikimedia.org/wikipedia/en/thumb/9/98/Doosan_Bears.svg/1280px-Doosan_Bea…
# $ insignia <chr> "https://upload.wikimedia.org/wikipedia/commons/thumb/a/a2/Doosan_Bears_insignia.svg/102…
# $ color    <chr> "#131230", "#ff6600", "#EC0F32", "#820023", "#000000", "#a50034", "#041e42", "#071d3d", …

# team_photos
#
# chr [1:10] "Doosan Bears.webp" "Hanwha Eagles.webp" "Kia Tigers.webp" "Kiwoom Heros.webp" ...

# team_cheerleaders
#
# Rows: 97
# Columns: 3
# $ team        <chr> "Doosan Bears", "Doosan Bears", "Doosan Bears", "Doosan Bears", "Doosan Bears", "Doos…
# $ cheerleader <chr> "Hyunsuk Seo", "Jeong Hee-Jeong", "Dayoung Lee", "Hyeji Ahn", "Sohyun Park", "Hyejin …
# $ link        <chr> "/w/%EC%84%9C%ED%98%84%EC%88%99(%EC%B9%98%EC%96%B4%EB%A6%AC%EB%8D%94)", "/w/%EC%A0%95…

# cheer_data
#
# List of 97    example
# $ Hyunsuk Seo    :List of 2
# ..$ table: tibble [12 × 2] (S3: tbl_df/tbl/data.frame)
# ..$ links: chr [1:4] "https://www.facebook.com/hs0422" "https://www.instagram.com/seo_hsss"...

# bio_tables (for image url extraction)
# List of 97
# $ Hyunsuk Seo    : chr "<table class=\"wiki-table\" style=\"background-color:#ffffff; width:100%;
#                         border:2px solid #131230;\" data-dark"| __truncated__

# keyword_image_mapping
#
# List of 9
# $ afreecatv: chr "afreecatv.jpg"
# $ facebook : chr "facebook.png"
# $ instagram: chr "instagram.png"
# $ likey    : chr "likey.jpg"
# $ naver    : chr "naver.jpg"
# $ threads  : chr "threads.jpg"
# $ tiktok   : chr "tiktok.jpg"
# $ x.com    : chr "twitter.jpg"
# $ youtube  : chr "youtube.png"
















