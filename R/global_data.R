# data ----------------------------------
data("team_data")                        # name, url, color, theme song
data("team_cheerleaders")                # team, cheerleader, wiki link

data("cheer_data")                       # Cheerleader = list(table, links)
bio_tables <-                            # Cheerleader = list(bio_table(html))
  readr::read_rds("./data/bio_tables.rds")

data("keyword_image_mapping")            # List(name, img name)

  # cheerleader social media ------------
  data("youtube")                       # youtube stats
  data("instagram")                     # instagram stats
  data("tiktok")                        # tiktok stats

  # social media statistics -------------
  data("fat")                           # followers all teams
  data("fat_plot")                      # followers all teams plot
  data("fat_dist")                      # distributions data
  data("fat_distro")                    # distributions plots

  data("ultra_combo")                   # leaderboard

# values --------------------------------
data("wiki_url")                        # wiki url prefix

data("team_photos")                     # char vector team photo
data("team_logos")                      # char vector team logos
data("team_caps")                       # char vector team caps













