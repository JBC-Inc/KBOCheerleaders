# library(dplyr)
# library(reactable)
#
# # CAR DATA, ID, DATE, MANUFACTURER, MODEL, TYPE, PRICE
# #
# data <- MASS::Cars93[18:47, ] %>%
#   mutate(ID = as.character(18:47), Date = seq(as.Date("2019-01-01"), by = "day", length.out = 30)) %>%
#   select(ID, Date, Manufacturer, Model, Type, Price)
#
# glimpse(data)
#
# # Rows: 30
# # Columns: 6
# # $ ID           <chr> "18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31", …
# # $ Date         <date> 2019-01-01, 2019-01-02, 2019-01-03, 2019-01-04, 2019-01-05, 2019-01-06, 2019-01-07,…
# # $ Manufacturer <fct> Chevrolet, Chevrolet, Chrylser, Chrysler, Chrysler, Dodge, Dodge, Dodge, Dodge, Dodg…
# # $ Model        <fct> Caprice, Corvette, Concorde, LeBaron, Imperial, Colt, Shadow, Spirit, Caravan, Dynas…
# # $ Type         <fct> Large, Sporty, Large, Compact, Large, Small, Small, Compact, Van, Midsize, Sporty, S…
# # $ Price        <dbl> 18.8, 38.0, 18.4, 15.8, 29.5, 9.2, 11.3, 13.3, 19.0, 15.6, 25.8, 12.2, 19.3, 7.4, 10…
#
# sales_by_mfr <-
#   data |>
#     group_by(Manufacturer) |>
#     summarize(Quantity = n(), Sales = sum(Price))
#
# glimpse(sales_by_mfr)
#
# # Rows: 9
# # Columns: 3
# # $ Manufacturer <fct> Chevrolet, Chrylser, Chrysler, Dodge, Eagle, Ford, Geo, Honda, Hyundai
# # $ Quantity     <int> 2, 1, 2, 6, 2, 8, 2, 3, 4
# # $ Sales        <dbl> 56.8, 18.4, 45.3, 94.2, 31.5, 119.7, 20.9, 49.4, 41.9
#
# reactable(
#
#   sales_by_mfr,
#
#   details = function(index) {
#
#     sales <- data |>
#       filter(Manufacturer == sales_by_mfr$Manufacturer[index]) |>
#       select(-Manufacturer)
#
#     tbl <- reactable::reactable(
#       data = sales,
#       outlined = TRUE,
#       highlight = TRUE,
#       fullWidth = FALSE
#       )
#
#     htmltools::div(style = list(margin = "12px 45px"), tbl)
#   },
#   onClick = "expand",
#   rowStyle = list(cursor = "pointer")
# )

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

library(shiny)
library(reactable)
library(htmltools)

# Rows: 135
# Columns: 14
# $ name                <chr> "Hyunsuk Seo", "Park Ki-Ryang", "Ha Ji Won", "Nayeon Kim", "Jeong Ga-Ye", "Ha…
# $ subs                <int> 124000, 56300, 3810, 10600, 190, 53200, 445, 722, 66000, 42900, 28100, 5460, …
# $ views               <int> 53857484, 13193426, 0, 656172, 550, 13711901, 41586, 11061, 8144872, 7904426,…
# $ count               <int> 530, 158, 0, 82, 1, 91, 21, 25, 3, 12, 366, 29, 21, 25, 22, 42, 5, 17, 15, 9,…
# $ cat                 <chr> "youtube", "youtube", "youtube", "youtube", "youtube", "youtube", "youtube", …
# $ likes               <int> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
# $ instagram_followers <int> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
# $ tiktok_followers    <int> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
# $ team                <chr> "Doosan Bears", "Doosan Bears", "Hanwha Eagles", "Hanwha Eagles", "Kia Tigers…
# $ link                <glue> "<a href=\"https://en.namu.wiki/w/%EC%84%9C%ED%98%84%EC%88%99(%EC%B9%98%EC%9…
# $ team_img            <chr> "Doosan Bears.png", "Doosan Bears.png", "Hanwha Eagles.png", "Hanwha Eagles.p…
# $ logo                <chr> "<img height=50 src=\"www/team_logo/Doosan Bears.png\"></img>", "<img height=…
# $ photo               <glue> "<img height=50 src=\"www/cheerleader_img/Hyunsuk Seo.png\"\nclass=\"cheerle…
# $ avg_views_per_video <int> 101617, 83502, NA, 8002, 550, 150680, 1980, 442, 2714957, 658702, 15957, 4336…









library(shiny)

ui <- fluidPage(
  reactable::reactableOutput("react")
)

server <- function(input, output, session) {

  uc <- ultra_combo |> dplyr::distinct(team) |> dplyr::select(team)

  setwd("C:/Users/cragg/OneDrive/code/github/KBOCheerleaders")

  output$react <- reactable::renderReactable({

    reactable::reactable(
      uc,
      columns = list(
        team = reactable::colDef(cell = function(value) {

          img_src <- knitr::image_uri(sprintf("www/team_logo/%s.png", value))

          image <- htmltools::img(src = img_src, height = "24px", alt = "")

          tagList(
            div(style = "display: inline-block; width: 45px;", image),
            value
          )
        })
      )
    )



  })
}

shinyApp(ui, server)

























































































