
#' Shiny Application User Interface
#'
#' @return Shiny App ui
#' @keywords internal
#'
app_ui <- function() {
  bslib::page_sidebar(
    addExternalResources(),

    theme = bslib::bs_theme(
      version = 5,
      base_font = "Roboto, sans-serif",
      bootswatch = "minty",
      danger = "#ff0000"
    ),

    title = makeTitle(),

    sidebar = bslib::sidebar(
      mod_team_ui("team")$select,
      mod_cheer_ui("cheer")$select,
      mod_song_ui("song")$switch,
      mod_song_ui("song")$ui_team,
      mod_song_ui("song")$ui_piki
    ),

    bslib::navset_tab(
      id = "tabs",
      bslib::nav_panel(
        value = "react",
        "Team Overview",
        mod_react_ui("react")$ui
        ),
      bslib::nav_panel(
        value = "stats",
        "Team Followers",
        mod_stats_ui("stats")$ui
        ),
      bslib::nav_panel(
        value = "visual",
        "Cheerleader Insights",
        mod_team_ui("team")$ui,
        mod_cheer_ui("cheer")$ui
        ),
      bslib::nav_panel(
        value = "leader",
        "Top Cheerleaders",
        mod_leaderboard_ui("leaderboard")$ui
        ),
      bslib::nav_spacer(),
      makeNavMenu(),
    )
  )
}

