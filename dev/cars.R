library(shiny)
library(bslib)

ui <- fluidPage(
  bslib::layout_column_wrap(
    width = '100%',
    height = '400px',
    # Adjust according to your layout needs
    fixed_width = TRUE,

    style = bslib::css(grid_template_columns = "repeat(6, 1fr)"),
    # Four equal columns

    # First Plot Card
    bslib::card(
      full_screen = TRUE,
      bslib::card_header("Plot 1", class = "bg-dark"),
      bslib::card_body(shiny::plotOutput("plot1", height = '100%')),
      bslib::card_footer("Footer for Plot 1", class = "bg-info")
      ),

      # Second Plot Card
      bslib::card(
        full_screen = TRUE,
        bslib::card_header("Plot 2", class = "bg-dark"),
        bslib::card_body(shiny::plotOutput("plot2", height = '100%')),
        bslib::card_footer("Footer for Plot 2", class = "bg-info")
      ),

      # Third Plot Card
      bslib::card(
        full_screen = TRUE,
        bslib::card_header("Plot 3", class = "bg-dark"),
        bslib::card_body(shiny::plotOutput("plot3", height = '100%')),
        bslib::card_footer("Footer for Plot 3", class = "bg-info")
      ),

      # Fourth Plot Card
      bslib::card(
        full_screen = TRUE,
        bslib::card_header("Plot 4", class = "bg-dark"),
        bslib::card_body(shiny::plotOutput("plot4", height = '100%')),
        bslib::card_footer("Footer for Plot 4", class = "bg-info")
      )
    )
  )

server <- function(input, output, session) {
  output$plot1 <- renderPlot({
    plot(cars)
  })

  output$plot2 <- renderPlot({
    plot(pressure)
  })

  output$plot3 <- renderPlot({
    plot(mtcars$mpg, mtcars$cyl)
  })

  output$plot4 <- renderPlot({
    plot(mtcars$hp, mtcars$wt)
  })
}

shinyApp(ui, server)
