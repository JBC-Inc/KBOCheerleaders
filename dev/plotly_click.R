library(shiny)
library(tidyverse)

ui <- fluidPage(
  titlePanel("hover tooltips demo"),
  mainPanel(
    plotOutput("plot1", hover = hoverOpts(id = "plot_hover", delay = 100, delayType = "debounce")),



    uiOutput("hover_info") # , style = "pointer-events: none")
  )
)

server <- function(input, output) {

  output$plot1 <- renderPlot({

    mtcars %>%
      ggplot(aes(mpg, hp)) +
      geom_point()
  })

  output$hover_info <- renderUI({

    hover <- input$plot_hover

    point <- shiny::nearPoints(mtcars,
                               coordinfo = hover,
                               xvar = 'mpg',
                               yvar = 'hp',
                               threshold = 20,
                               maxpoints = 1,
                               addDist = TRUE)

    if (nrow(point) == 0) return(NULL)

    style <- paste0("position:absolute; z-index:100; background-color: #3c8dbc; color: #ffffff;",
                    "font-weight: normal; font-size: 11pt;",
                    "left:", hover$coords_css$x + 5, "px;",
                    "top:",  hover$coords_css$y + 5, "px;")

    wellPanel(
      style = style,
      p(HTML(paste0("Some info about car: <br/>MPG ", point$mpg, "<br/>HP ", point$hp)))
    )
  })
}

shinyApp(ui = ui, server = server)
