library(DT)

ui <- fluidPage(
  DTOutput("my_table")
)

server <- function(input, output) {
  output$my_table <- renderDT({
    datatable(
      mtcars,  # Example data
      options = list(
        columnDefs = list(
          list(
            width = '1000px',  # Set width of the first column
            targets = 0        # Targets the first column (0-indexed)
          ),
          list(
            width = '150px',  # Set width of the second column
            targets = 1        # Targets the second column
          ),
          list(
            width = '200px',  # Set width of the third column
            targets = 2        # Targets the third column
          )
          # Add more column definitions as needed
        )
      )
    )
  })
}

shinyApp(ui = ui, server = server)
