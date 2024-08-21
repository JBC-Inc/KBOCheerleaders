#' Team Data
#'
#' A dataset containing information about various cheerleading teams.
#'
#' This dataset includes columns such as team names, locations, and performance metrics.
#'
#' @format A data frame with X rows and Y columns:
#' \describe{
#'   \item{name}{A character vector of team names.}
#'   \item{url}{A character vector of team namuwiki urls.}
#'   \item{color}{A character vector of team themesong urls on YouTube.}
#' }
#' @source "Extracted from namuwiki pages."
#' @examples
#' data(team_data)
#' head(team_data)
"team_data"

#' Cheerleaders Data
#'
#' A dataset containing information about cheerleaders from different teams.
#'
#' This dataset includes columns for team names, cheerleader names, and links to their profiles.
#'
#' @format A data frame with 97 rows and 3 columns:
#' \describe{
#'   \item{team}{A character vector of team names.}
#'   \item{cheerleader}{A character vector of cheerleader names.}
#'   \item{link}{A character vector of URLs to cheerleaders' profile pages.}
#' }
#' @source "Extracted from namuwiki pages."
#' @examples
#' data(team_cheerleaders)
#' head(team_cheerleaders)
"team_cheerleaders"

#' Cheerleader Details
#'
#' A dataset containing detailed information about cheerleaders, including personal attributes and social media links.
#'
#' This dataset is a list where each element corresponds to a cheerleader. Each cheerleader contains:
#' \describe{
#'   \item{table}{A tibble with 12 rows and 2 columns detailing personal attributes. The columns are:
#'     \describe{
#'       \item{X1}{A character vector describing the type of attribute (e.g., "Birthday", "Birthplace").}
#'       \item{X2}{A character vector providing the value for each attribute.}
#'     }
#'   }
#'   \item{links}{A character vector containing URLs to the cheerleader's social media profiles.}
#' }
#' @format A list of cheerleader details where each entry is a list with two components:
#' \describe{
#'   \item{table}{A tibble with personal information attributes.}
#'   \item{links}{A character vector of social media profile URLs.}
#' }
#' @source "Extracted from namuwiki cheerleader profile pages."
#' @examples
#' data(cheer_data)
#' # Display details for the first cheerleader
#' cheer_data[[1]]
"cheer_data"

#' Ultra Combo Cheerleader Metrics
#'
#' A dataset containing metrics for cheerleaders, including social media statistics and team affiliations.
#'
#' This dataset contains 138 rows and 15 columns with information on cheerleaders' social media metrics and team details. The columns include:
#' \describe{
#'   \item{name}{A character vector with the names of the cheerleaders.}
#'   \item{subs}{An integer vector representing the number of subscribers or followers.}
#'   \item{views}{An integer vector with the number of views.}
#'   \item{count}{An integer vector indicating the number of items or posts.}
#'   \item{cat}{A character vector denoting the platform (e.g., "youtube").}
#'   \item{likes}{An integer vector for the number of likes, with `NA` indicating missing data.}
#'   \item{instagram_followers}{An integer vector for Instagram followers, with `NA` for missing data.}
#'   \item{tiktok_followers}{An integer vector for TikTok followers, with `NA` for missing data.}
#'   \item{team}{A character vector indicating the team the cheerleader belongs to.}
#'   \item{link}{A character vector containing HTML links to the cheerleader's profile.}
#'   \item{team_img}{A character vector with filenames of team images.}
#'   \item{color}{A character vector specifying the team colors in hexadecimal format.}
#'   \item{logo}{A character vector containing HTML for team logos.}
#'   \item{photo}{A character vector containing HTML for cheerleader photos.}
#'   \item{avg_views_per_video}{An integer vector for the average number of views per video, with `NA` for missing data.}
#' }
#' @format A tibble with 138 rows and 15 columns.
#' @source "Collected from social media profiles (YouTube, Instagram, TikTok)."
#' @examples
#' data(ultra_combo)
#' head(ultra_combo)
"ultra_combo"

