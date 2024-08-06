
# Authentication =============================================================================================

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
    app_secret = Sys.getenv("YT_CLIENT_SECRET")
    # token = ''
  )
  global_token <<- TRUE
}

# if (is.null(global_token)) {
#   authenticateYouTube()
# }

# Cheerleader count, Team Name, Wiki Page
#=============================================================================================================

wiki_url <- "https://en.namu.wiki"

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

keyword_image_mapping = list(
  afreecatv  = "https://i.namu.wiki/i/-QT1tHmj_mAyV22XswhfG5VbnZjS_f_48Q9U-pjgyO7fi6PzdKY7_TeMiNv86NCmzukDToQYPlcTppGEIemtIQ.svg",
  facebook   = "https://i.namu.wiki/i/QxMj0QTY1co3G8YZ7mrvoMYZpiKWk17wwO9-VErvk_boO5niKsi1uslWFz1Sn0cabZR12sU0MwayFAHRQezqDw.svg",
  instagram  = "https://i.namu.wiki/i/Zxhx01JuKKKxJXexvn1q7RYqkXTuJ5CHVCBPWZtPFglv6wVXwrjwdtnietzC93sijekOjC13DBO6E-z9HTUwl1cUn9hry9Zo3_VNC4ccRPzvYU9N_uqN4WJRm2phTaU5QBamj7ZhybVtU039Q_kEpg.svg",
  likey      = "https://i.namu.wiki/i/9eW5YO2AxM7Bntn__P2_l1MqmDjRH-uC11TtviiFRkt2UGmeeGoErm83sCi6WRicpQSy-ZE5wqyupTLSkLNy0uR3Avg2MQ3LDedckjXcQ0GIuNr79_w9F7T5a0Sw4czEWMyL-9aJUZzXAg4h1BzUow.svg",
  naver      = "https://i.namu.wiki/i/TY6ZEq_xIPxfVE8K2-z-Ar3AY1YWn4Uq-MtGQxo-MnSm0pOhGCtJTrU51Ghy9Rwx9sBCbNPSmFq92VgwVHqa-OYfxb0j5giYnI8XZwnkYVYDlZ-j0cUoK1IpoqZ7biK6T9toATtoBnnFQfbRTfvv9Q.svg",
  threads    = "https://i.namu.wiki/i/USdGq7GJ0UjNm1xs5WsG2U_EiVaQdZLokvL2zNDGGlTSnblXtgkYbniSsz4HLUXxC_xPYzRD_Vq9QHo5P4UglLzOU-qZW4g-X9Z2FqyyuWg7NFzxyVk6-SgTaRJtqxp3hqv5fQiSUAhMTdIKSbzblw.svg",
  tiktok     = "https://i.namu.wiki/i/8wydteGCtIyC9qAVgIUndFBRaqq1z1onH5Nk3j_wbKRPBxIEeO-XX-m4DrDyv5rufaUj9vpd-nyZsi1l5m_IihAVoyK5Lfi0rDDV1kuSkfFd-KCbh3CMorivR4jrj0D8_m7iNG5OxyH9j97GMdjBXA.svg",
  `x.com`    = "https://i.namu.wiki/i/jBQ6v78CaBnATxXt9esV0juqHYdjpd7XHRSW4JhG-Pn9tWlqKuYprL8KQMCMpg-ObU-Jx6QPOhwM7VaWE_u3khTpOEubdkOoEnojnxkPyQqkQ8hu7QKwpGaiP5dqHyF2XMe0ft3VE7s6jTdraeSUKA.svg",
  youtube    = "https://i.namu.wiki/i/0pCL8w8sKgZWCk0vaUO3_JRI-_ytvVk0DL7vzKB0Gp6rwOsOdBBDYNEbNpr2vn9FY1vYiYdeHr4tH2413EQWgA.svg"
)

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











