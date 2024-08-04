library(shiny)
library(bslib)

ui <- page_sidebar(
  title = div(
    class = "d-flex justify-content-between align-items-center w-100",
    div(
      class = "d-flex align-items-center",
      tags$img(
        src = paste0("https://upload.wikimedia.org/wikipedia/en/thumb/5/59/",
                     "KBO_League.svg/1920px-KBO_League.svg.png"),
        height = "50px",
        style = "margin-right: 10px;"
      ),
      h1("Cheerleaders!")
    ),
    h2("ASLKDJASLDKJLAKSDJALSDJASLKJ", style = "margin: 0;")
  ),
  windowTitle = "KBO",
  sidebar = sidebar(
    # Sidebar content
    p("Sidebar content goes here")
  ),
  mainPanel = mainPanel(
    # Main content
    p("Main content goes here")
  )
)

server <- function(input, output) {
}

shinyApp(ui = ui, server = server)
