options(scipen = 999)

glimpse(youtube)
# Rows: 27
# Columns: 7
# $ name  <chr> "Hyunsuk Seo", "Park Ki-Ryang", "Ha Ji Won", "Nayeon Kim", "Jeong Ga-Ye", "Hannah Kim", "Ha…
# $ title <chr> "서현숙TV", "박기량TV", "원하지", "나욤찌__Nayeon", "정가예", "퀸한나", "숏한나", "Veen", "…
# $ subs  <int> 124000, 56300, 3810, 10600, 190, 53200, 445, 722, 66000, 42900, 28100, 5460, 9360, 489, 567…
# $ views <int> 53857484, 13193426, 0, 656172, 550, 13711901, 41586, 11061, 8144872, 7904426, 5840374, 1257…
# $ count <int> 530, 158, 0, 82, 1, 91, 21, 25, 3, 12, 366, 29, 21, 25, 22, 42, 5, 17, 15, 9, 336, 36, 1, 4…
# $ team  <chr> "Doosan Bears", "Doosan Bears", "Hanwha Eagles", "Hanwha Eagles", "Kia Tigers", "Kia Tigers…
# $ cat   <chr> "youtube", "youtube", "youtube", "youtube", "youtube", "youtube", "youtube", "youtube", "yo…

glimpse(instagram)
# Rows: 97
# Columns: 5
# $ name      <chr> "Hyunsuk Seo", "Jeong Hee-Jeong", "Dayoung Lee", "Hyeji Ahn", "Sohyun Park", "Hyejin Mo…
# $ acct      <chr> "seo_hsss", "_jung_u", "da_yeom_", "hhhhh__ji/", "ssxhyxx_97", "yll_suly27", "peppermin…
# $ followers <int> 396000, 65000, 9652, 41000, 6703, 43000, 16000, 252000, 3284, 2049, 1950, 166000, 96000…
# $ team      <chr> "Doosan Bears", "Doosan Bears", "Doosan Bears", "Doosan Bears", "Doosan Bears", "Doosan…
# $ cat       <chr> "instagram", "instagram", "instagram", "instagram", "instagram", "instagram", "instagra…

glimpse(tiktok)
# Rows: 11
# Columns: 6
# $ cheername <chr> "Yeonjung Kim", "Park So-Young", "Nayeon Kim", "Hannah Kim", "Yeom Sebin", "Lee Ju-eun"…
# $ name      <chr> "@90_allie", "@sy030_905", "@naaa_.y12", "@pink_hannaaa", "@beena.s2?_t=8hJgs5vicxG&_r=…
# $ followers <int> 44400, 10100, 3680, 33900, 876, 289000, 2700000, NA, 406200, 10000, 346500
# $ likes     <int> 152000, 44100, 21700, 116000, 3280, 4400000, 20700000, NA, 2400000, 41800, 3700000
# $ team      <chr> "Hanwha Eagles", "Hanwha Eagles", "Hanwha Eagles", "Kia Tigers", "Kia Tigers", "Kia Tig…
# $ cat       <chr> "tiktok", "tiktok", "tiktok", "tiktok", "tiktok", "tiktok", "tiktok", "tiktok", "tiktok…


# YOUTUBE Cheerleaders w/ Team with over 1,000,000 views

youtube |>
  filter(views > 1000000) |>
  ggplot(aes(x = reorder(paste0(team, "\n", name), -views), y = views)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  scale_y_continuous(breaks = seq(0, 100000000, 10000000),
                     labels = scales::label_number(scale = 1e-6, suffix = "M")) +
  labs(title = "Greater than 1,000,000 views", x = "", y = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# followers/subs_across_teams ===========================================
yt <- youtube |> dplyr::select(team, subs)
colnames(yt) <- c("team", "followers")
ist <- instagram |> dplyr::select(team, followers)
tt <- tiktok |> dplyr::select(team, followers)

fat <- dplyr::bind_rows(yt, ist, tt)
#=========================================================================

team_colors <- data.frame(name = team_data$name, color = team_data$color)

fat <- fat |>
  left_join(team_colors, by = c("team" = "name"))

team_logos_df <- data.frame(
  name = team_data$name,
  logo = unlist(team_logos)
)

fat <- fat |>
  left_join(team_logos_df , by = c("team" = "name"))

fat |>
  drop_na() |>
  group_by(team, color, logo) |>
  summarize(followers = sum(followers), .groups = 'drop') |>
  arrange(desc(followers)) |>

  ggplot(aes(reorder(team, -followers), followers, fill = color)) +
  geom_bar(stat = "identity") +
  scale_fill_identity() +
  scale_y_continuous(breaks = seq(0, 5000000, 500000),
                     labels = scales::label_number(scale = 1e-6, suffix = "M")) +
  labs(title = "", x = "", y = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.margin = margin(5, 5, 5, 5)  ) +
  ggimage::geom_image(aes(image = paste0("www/team_logo/", logo)), size = 0.22)

usethis::use_data(fat, internal = FALSE, overwrite = TRUE)





# density of followers by platform
yt <- youtube |> dplyr::select(team, subs, cat)
colnames(yt) <- c("team", "followers", "cat")
inst <- instagram |> dplyr::select(team, followers, cat)
tt <- tiktok |> dplyr::select(team, followers, cat)

fat_dist <- dplyr::bind_rows(yt, inst, tt)

usethis::use_data(fat_dist, overwrite = TRUE)

# FOLLOWERS ALL TEAMS DISTRIBUTION BY PLATFORM ===============================
fatDistro <- function(fat_dist) {

f1 <- fat_dist %>%
  dplyr::group_by(team, cat) %>%
  dplyr::summarize(avg_followers = mean(followers), .groups = 'drop') |>

  ggplot2::ggplot(ggplot2::aes(x = avg_followers, fill = cat)) +
  ggplot2::geom_density(alpha = 0.6) +

  ggplot2::scale_x_continuous(breaks = seq(0, 10000000, 500000), labels = scales::comma_format()) +
  ggplot2::scale_fill_manual(values = c("youtube" = "red",
                                        "tiktok" = "black",
                                        "instagram" = "purple")) +

  ggplot2::labs(title = "",
                fill = "Social Media Platform",
                x = "",
                y = "") +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.margin = ggplot2::margin(0, 0, 0, 0),
                 #plot.margin = ggplot2::margin(5, 5, 5, 5)
                 legend.position = "none")

# CAPPED OUTLIERS @ 100,000
f2 <- fat_dist |>
  dplyr::group_by(team, cat) |>
  dplyr::summarize(avg_followers = mean(followers), .groups = 'drop') |>
  dplyr::mutate(avg_followers = pmin(avg_followers, 100000)) |>
  ggplot2::ggplot(aes(x = avg_followers, fill = cat)) +
  ggplot2::geom_density(alpha = 0.6) +
  ggplot2::scale_x_continuous(
    breaks = seq(0, 200000, 25000),
    labels = scales::comma_format()) +
  ggplot2::scale_fill_manual(values = c("youtube" = "red",
                                        "tiktok" = "black",
                                        "instagram" = "purple")) +
  ggplot2::labs(title = "", fill = "Social Media Platform",
                x = "Average Followers per Team",
                y = "Density") +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.margin = ggplot2::margin(0, 0, 0, 0),
                 #plot.margin = ggplot2::margin(5, 5, 5, 5)
                 legend.position = "none")

# LOG SCALE
f3 <- fat_dist |>
  dplyr::group_by(team, cat) |>
  dplyr::summarize(avg_followers = mean(followers), .groups = 'drop') |>
  dplyr::mutate(log_avg_followers = log1p(avg_followers)) |>
  ggplot2::ggplot(aes(x = log_avg_followers, fill = cat)) +
  ggplot2::geom_density(alpha = 0.6) +
  ggplot2::scale_fill_manual(values = c("youtube" = "red",
                                        "tiktok" = "black",
                                        "instagram" = "purple")) +
  ggplot2::scale_x_continuous(
    labels = function(x) scales::comma_format()(exp(x) - 1),
    breaks = log1p(c(1000, 10000, 100000, 1000000))
  ) +
  ggplot2::labs(
    title = "", fill = "Social Media Platform",
    x = "Log(Average Followers per Team)",
    y = "Density") +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.margin = ggplot2::margin(0, 0, 0, 0),
                 #plot.margin = ggplot2::margin(5, 5, 5, 5)
                 legend.position = "none")

# DENSITY w/TRIMMED
average_followers <- fat_dist |>
  dplyr::group_by(team, cat) |>
  dplyr::summarize(avg_followers = mean(followers), .groups = 'drop')

# Trim extreme values (e.g., keep only the bottom 95% of data)
threshold <- quantile(average_followers$avg_followers, 0.95, na.rm = TRUE)
trimmed_data <- average_followers |>
  dplyr::filter(avg_followers <= threshold)

# Plot the density of trimmed average followers by platform category
f4 <-
  ggplot2::ggplot(trimmed_data, ggplot2::aes(x = avg_followers, fill = cat)) +
  ggplot2::geom_density(alpha = 0.6) +
  ggplot2::scale_x_continuous(
    breaks = seq(0, 200000, 25000),
    labels = scales::comma_format()) +
  ggplot2::scale_fill_manual(values = c("youtube" = "red",
                                        "tiktok" = "black",
                                        "instagram" = "purple")) +
  ggplot2::labs(title = "", fill = "Social Media Platform",
                x = "Average Followers per Team",
                y = "Density") +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.margin = ggplot2::margin(0, 0, 0, 0),
                 #plot.margin = ggplot2::margin(5, 5, 5, 5)
                 legend.position = "none")

  return(
    list(
      f1 = f1,
      f2 = f2,
      f3 = f3,
      f4 = f4
  ))

}

fat_distro <- fatDistro(fat_dist)

usethis::use_data(fat_distro, overwrite = TRUE)

fat_distro[[1]]
fat_distro[[2]]
fat_distro[[3]]
fat_distro[[4]]

# by followers per cheerleader - may be useful * * * * * * * * *  * ** * *  * * * *
# fat %>%
#   group_by(team, cat) |>
#   summarize(avg_followers = mean(followers / cheerleaders), .groups = 'drop')
#
# # Plot the density of average followers by platform
# ggplot(average_followers, aes(x = avg_followers, fill = platform)) +
#   geom_density(alpha = 0.6) +  # Create the density plot with transparency
#   labs(title = "Distribution of Average Followers per Cheerleader by Platform",
#        x = "Average Followers per Cheerleader",
#        y = "Density") +
#   theme_minimal() +
#   theme(legend.position = "top")

#=========================================================================
#=========================================================================
#=========================================================================
#=========================================================================
#=========================================================================

glimpse(youtube)
# Rows: 27
# Columns: 7
# $ name  <chr> "Hyunsuk Seo", "Park Ki-Ryang", "Ha Ji Won", "Nayeon Kim", "Jeong Ga-Ye", "Hannah Kim", "Ha…
# $ title <chr> "서현숙TV", "박기량TV", "원하지", "나욤찌__Nayeon", "정가예", "퀸한나", "숏한나", "Veen", "…
# $ subs  <int> 124000, 56300, 3810, 10600, 190, 53200, 445, 722, 66000, 42900, 28100, 5460, 9360, 489, 567…
# $ views <int> 53857484, 13193426, 0, 656172, 550, 13711901, 41586, 11061, 8144872, 7904426, 5840374, 1257…
# $ count <int> 530, 158, 0, 82, 1, 91, 21, 25, 3, 12, 366, 29, 21, 25, 22, 42, 5, 17, 15, 9, 336, 36, 1, 4…
# $ team  <chr> "Doosan Bears", "Doosan Bears", "Hanwha Eagles", "Hanwha Eagles", "Kia Tigers", "Kia Tigers…
# $ cat   <chr> "youtube", "youtube", "youtube", "youtube", "youtube", "youtube", "youtube", "youtube", "yo…

glimpse(instagram)
# Rows: 97
# Columns: 5
# $ name      <chr> "Hyunsuk Seo", "Jeong Hee-Jeong", "Dayoung Lee", "Hyeji Ahn", "Sohyun Park", "Hyejin Mo…
# $ acct      <chr> "seo_hsss", "_jung_u", "da_yeom_", "hhhhh__ji/", "ssxhyxx_97", "yll_suly27", "peppermin…
# $ followers <int> 396000, 65000, 9652, 41000, 6703, 43000, 16000, 252000, 3284, 2049, 1950, 166000, 96000…
# $ team      <chr> "Doosan Bears", "Doosan Bears", "Doosan Bears", "Doosan Bears", "Doosan Bears", "Doosan…
# $ cat       <chr> "instagram", "instagram", "instagram", "instagram", "instagram", "instagram", "instagra…

glimpse(tiktok)
# Rows: 11
# Columns: 6
# $ cheername <chr> "Yeonjung Kim", "Park So-Young", "Nayeon Kim", "Hannah Kim", "Yeom Sebin", "Lee Ju-eun"…
# $ name      <chr> "@90_allie", "@sy030_905", "@naaa_.y12", "@pink_hannaaa", "@beena.s2?_t=8hJgs5vicxG&_r=…
# $ followers <int> 44400, 10100, 3680, 33900, 876, 289000, 2700000, NA, 406200, 10000, 346500
# $ likes     <int> 152000, 44100, 21700, 116000, 3280, 4400000, 20700000, NA, 2400000, 41800, 3700000
# $ team      <chr> "Hanwha Eagles", "Hanwha Eagles", "Hanwha Eagles", "Kia Tigers", "Kia Tigers", "Kia Tig…
# $ cat       <chr> "tiktok", "tiktok", "tiktok", "tiktok", "tiktok", "tiktok", "tiktok", "tiktok", "tiktok…

yt <- youtube |> select(name, subs, views, count, cat)
inst <- instagram |> select(name, followers, cat)
tt <- tiktok |> select(cheername, followers, likes, cat)
tt <- tt |> rename(name = cheername)
ultra_combo <- yt |>
  full_join(inst, by = c("name", "cat")) |>
  full_join(tt, by = c("name", "cat")) |>
  mutate(instagram_followers = followers.x,
         tiktok_followers = followers.y) |>
  select(-c(followers.x, followers.y))

# add team
ultra_combo <- ultra_combo %>%
  left_join(team_cheerleaders, by = c("name" = "cheerleader"))

# add team logo url

# names(team_logos) <- team_data$name
team_logos_df <- data.frame(
  name = team_data$name,
  team_img = unlist(team_logos)
)

ultra_combo <- ultra_combo |>
  left_join(team_logos_df , by = c("team" = "name"))

# Rows: 135
# Columns: 11
# $ name                <chr> "Hyunsuk Seo", "Park Ki-Ryang", "Ha Ji Won", "Nayeon Kim", "Jeong Ga-Ye", "Han…
# $ subs                <int> 124000, 56300, 3810, 10600, 190, 53200, 445, 722, 66000, 42900, 28100, 5460, 9…
# $ views               <int> 53857484, 13193426, 0, 656172, 550, 13711901, 41586, 11061, 8144872, 7904426, …
# $ count               <int> 530, 158, 0, 82, 1, 91, 21, 25, 3, 12, 366, 29, 21, 25, 22, 42, 5, 17, 15, 9, …
# $ cat                 <chr> "youtube", "youtube", "youtube", "youtube", "youtube", "youtube", "youtube", "…
# $ likes               <int> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
# $ instagram_followers <int> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
# $ tiktok_followers    <int> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
# $ team                <chr> "Doosan Bears", "Doosan Bears", "Hanwha Eagles", "Hanwha Eagles", "Kia Tigers"…
# $ link                <chr> "/w/%EC%84%9C%ED%98%84%EC%88%99(%EC%B9%98%EC%96%B4%EB%A6%AC%EB%8D%94)", "/w/%E…
# $ logo                <chr> "https://upload.wikimedia.org/wikipedia/en/thumb/9/98/Doosan_Bears.svg/1280px-…


# YouTube -------------------------------------------------------------------------------
# team_icon     photo   name(link)    subscribers   views   count   average views

# most views/subs - YouTube
ultra_combo |> arrange(desc(views))            # Hyunsuk Seo  53,857,484    Doosan Bears
ultra_combo |> arrange(desc(subs))             # Hyunsuk Seo     124,000    Doosan Bears
ultra_combo |> arrange(desc(views)) |>
  mutate(avg_views_per_video = as.integer(views / count)) |>
  slice_head(n = 5)

# YouTube     name      subs    views count   average_Views_per_video
# 1       Hyunsuk Seo 124000 53857484   530    101,617
# 2          Heo Sumi  31900 26223961   336     78,047
# 3        Hannah Kim  53200 13711901    91    150,680
# 4     Park Ki-Ryang  56300 13193426   158     83,502
# 5        Lee Ju-eun  66000  8144872     3  2,714,957 * * *

# Instagram -----------------------------------------------------------------------------
# team icon     photo   name(link)    followers

# most followers - instagram
ultra_combo |> arrange(desc(followers.x)) |>   # Lee Ju-eun      755,000    Kia Tigers
  slice_head(n = 5)

# name subs views cat           followers.x
# 1  Lee Ju-eun   instagram      755000 * * *
# 2 Hyunsuk Seo   instagram      396000
# 3   Haeri Kim   instagram      388000
# 4   Ha Ji Won   instagram      307000
# 5  Hannah Kim   instagram      275000


# TikTok --------------------------------------------------------------------------------
# team icon     photo   name(link     followers     likes   likes/follow ratio

# most followers/likes - tiktok
ultra_combo |> arrange(desc(followers.y))     # Haeri Kim     2,700,000   KT Wiz
ultra_combo |> arrange(desc(likes))           # Haeri Kim    20,700,000   KT Wiz
ultra_combo |> arrange(desc(followers.y)) |>
  mutate(likes_per_follower = as.integer(likes / followers.y)) |>
  slice_head(n = 5)

#             name  cat     followers.y likes       likes_per_follower
# 1      Haeri Kim  tiktok  2700000     20700000    7
# 2     Moknakyung  tiktok  406200      2400000     5
# 3 Seong Hyo-Ryun  tiktok  346500      3700000     10
# 4     Lee Ju-eun  tiktok  289000      4400000     15 * * *
# 5   Yeonjung Kim  tiktok  44400       152000      3


glimpse(ultra_combo)

ultra_combo <- ultra_combo |>
  mutate(
    logo =
      glue::glue(
        '<img height=50 src="www/team_logo/{team_img}"
        class="team-photo"
        data-tt="{team}">
        </img>'
        )
    ) |>
  mutate(
    photo =
      glue::glue(
        '<img height=50 src="www/cheerleader_img/{name}.png"
             class="cheerleader-photo"
             data-team="{team}"
             data-name="{name}">
             </img>'
        )
    ) |>
  mutate(logo = str_replace(logo, "Wiz\\.webp", "Wiz.jpg")) |>
  mutate(logo = str_replace(logo, "Lions\\.webp", "Lions.jpg")) |>
  mutate(avg_views_per_video = as.integer(views / count)) |>
  mutate(link = glue::glue('<a href="{wiki_url}{link}" target="_blank">{name}</a>'))

usethis::use_data(ultra_combo, overwrite = TRUE)

youtube_uc <- ultra_combo |> select(team, logo, photo, link, subs,
                                    views, count, avg_views_per_video)

youtube_uc |>
  dplyr::arrange(dplyr::desc(views)) |>
  dplyr::slice_head(n = 5) |>
  gt::gt() |>
  gt::cols_hide(columns = c(team)) |>
  gt::fmt_markdown(
    columns = c(logo, photo, link)
  ) |>
  gt::cols_label(
    logo = "Team Logo",
    photo = "Cheerleader Photo",
    link = "Cheerleader",
    subs = "Subscribers",
    views = "Views",
    count = "Videos",
    avg_views_per_video = "Avg Views/Video"
    ) |>
  gt::tab_style(
    style = gt::cell_text(align = "center"),
    locations = gt::cells_column_labels(dplyr::everything())
  ) |>
  gt::tab_style(
    style = gt::cell_text(align = "center"),
    locations = gt::cells_body(columns = dplyr::everything())
  ) |>
  gt::fmt_number(
    columns = c(subs, views, count, avg_views_per_video),
    decimals = 0,
    use_seps = TRUE
    )

































