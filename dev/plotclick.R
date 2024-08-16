library(shiny)
library(tidyverse)

ui <- fluidPage(
  titlePanel("click me!"),
  mainPanel(
    plotOutput(
      outputId = "plot1",
      click = "plot_click"
      ),

    selectizeInput("select", "Cylinder", choices = c("4", "6", "8")),

    uiOutput("hover_info")
  )
)

server <- function(input, output, session) {

  cars <- reactive({
    mtcars |>
      group_by(cyl) |>
      summarize(avg_hp = mean(hp), .groups = 'drop')
  })


  output$plot1 <- renderPlot({
    cars() |>
      ggplot(aes(x = factor(cyl), y = avg_hp, fill = factor(cyl))) +
      geom_bar(stat = "identity") +
      labs(x = "Number of Cylinders", y = "Average Horsepower", fill = "Cylinders") +
      theme_minimal()
  })

  observeEvent(input$plot_click, {

    click <- input$plot_click

    point <- shiny::nearPoints(cars(),
                               coordinfo = click,
                               xvar = 'cyl',
                               # yvar = 'hp',
                               threshold = 300,
                               maxpoints = 1,
                               addDist = TRUE)

    updateSelectizeInput(session, "select", selected = point$cyl)
  })


  # output$hover_info <- renderUI({
  #
  #   hover <- input$plot_hover
  #
  #   point <- shiny::nearPoints(mtcars,
  #                              coordinfo = hover,
  #                              xvar = 'mpg',
  #                              yvar = 'hp',
  #                              threshold = 20,
  #                              maxpoints = 1,
  #                              addDist = TRUE)
  #
  #   if (nrow(point) == 0) return(NULL)
  #
  #   style <- paste0("position:absolute; z-index:100; background-color: #3c8dbc; color: #ffffff;",
  #                   "font-weight: normal; font-size: 11pt;",
  #                   "left:", hover$coords_css$x + 5, "px;",
  #                   "top:",  hover$coords_css$y + 5, "px;")
  #
  #   wellPanel(
  #     style = style,
  #     p(HTML(paste0("Some info about car: <br/>MPG ", point$mpg, "<br/>HP ", point$hp)))
  #   )
  # })
}

shinyApp(ui = ui, server = server)
