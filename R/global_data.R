
# data ----------------------------------
data("team_data", package = "KBOCheerleaders")                    # name, url, color, theme song
data("team_cheerleaders", package = "KBOCheerleaders")            # team, cheerleader, wiki link

data("cheer_data", package = "KBOCheerleaders")                   # Cheerleader = list(table, links)
bio_tables <-                                                     # Cheerleader = list(bio_table(html))
  readr::read_rds("./data/bio_tables.rds")

data("keyword_image_mapping", package = "KBOCheerleaders")        # List(name, img name)

  # cheerleader social media ------------
  data("youtube", package = "KBOCheerleaders")                    # youtube stats
  data("instagram", package = "KBOCheerleaders")                  # instagram stats
  data("tiktok", package = "KBOCheerleaders")                     # tiktok stats

  # ultra combo -------------------------
  data("ultra_combo", package = "KBOCheerleaders")                # leaderboards, reactable, fat plot/distro
  data("historic", package = "KBOCheerleaders")                   # historical ultra_combos (trend lines)

  # social media statistics -------------
  data("fat_plot", package = "KBOCheerleaders")                   # followers all teams plot
  data("fat_distro_plot", package = "KBOCheerleaders")            # distributions plots


# values --------------------------------
data("wiki_url", package = "KBOCheerleaders")                     # wiki url prefix

data("team_photos", package = "KBOCheerleaders")                  # char vector team photo
data("team_logos", package = "KBOCheerleaders")                   # char vector team logos
data("team_caps", package = "KBOCheerleaders")                    # char vector team caps

data("introduction", package = "KBOCheerleaders")
data("footer", package = "KBOCheerleaders")









