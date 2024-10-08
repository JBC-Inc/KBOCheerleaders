
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
  "https://www.youtube.com/watch?v=d9ulphHCWSs",
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

# intro data

introduction <-
  shiny::HTML(
    '<div style="padding: 20px; border-radius: 8px; background-color: #f9f9f9; box-shadow: 0 0 10px rgba(0, 0, 0, 0.1);">
    <p style="font-size: 16px;">The Pikki Pikki song, originally by <a href="https://youtu.be/M5HXKSR1hKc?t=42" target="_blank">Olive Beat\'s Lecon Studios</a>, has become a viral sensation thanks to its association with KBO Baseball League cheerleaders. The song is famously linked to the Kia Tigers, where cheerleaders use it to celebrate and subtly taunt opposing teams with a unique, low-energy dance.</p>
    <p style="font-size: 16px;">This cheer has sparked the Pikki Pikki Challenge and gained widespread attention on social media platforms. However, this Shiny app is not just about the Kia Tigers. It covers all KBO Baseball League cheerleading teams, focusing on their social media presence. The app aims to identify which cheerleaders have the most likes, followers, and subscribers across YouTube, Instagram, and TikTok.</p>
    <p style="font-size: 16px;">By analyzing social media metrics, the app highlights the most popular cheerleaders, showcasing their influence and reach. This comprehensive approach allows fans to explore and appreciate the vibrant cheerleading culture across the entire KBO League. Enjoy!</p>
  </div>'
  )

usethis::use_data(introduction, overwrite = TRUE)

footer <-
  shiny::HTML(
    '<footer style="padding: 10px; border-top: 1px solid #ddd; text-align: center; background-color: #f1f1f1;">
      <p style="font-size: 14px; color: #555;">Information about the KBO League and cheerleading teams is available on the following sources:</p>
      <p style="font-size: 14px;">
        <a href="https://en.wikipedia.org/wiki/KBO_League" target="_blank" style="color: #0056b3; text-decoration: none;">Wikipedia: KBO League</a><br>
        <a href="https://en.namu.wiki/w/%EC%B9%98%EC%96%B4%EB%A6%AC%EB%8D%94/KBO%20%EB%A6%AC%EA%B7%B8" target="_blank" style="color: #0056b3; text-decoration: none;">Namu Wiki: KBO League Cheerleaders</a>
      </p>
    <p style="font-size: 14px; color: #555;">Both sources operate under Creative Commons licenses. Wikipedia content is available under the <a href="https://en.wikipedia.org/wiki/Wikipedia:Copyrights#Licenses" target="_blank" style="color: #0056b3; text-decoration: none;">Creative Commons Attribution-Share-Alike License</a>, and Namu Wiki operates under its own <a href="https://en.namu.wiki/w/%ED%81%AC%EB%A6%AC%EC%97%90%EC%9D%B4%ED%8B%B0%EB%B8%8C%20%EC%BB%A4%EB%A8%BC%EC%A6%88%20%EB%9D%BC%EC%9D%B4%EC%84%A0%EC%8A%A4" target="_blank" style="color: #0056b3; text-decoration: none;">Namu Wiki License</a>.</p>
    </footer>')

usethis::use_data(footer, overwrite = TRUE)

