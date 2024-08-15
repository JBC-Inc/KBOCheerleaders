
mod_cheer_ui <- function(id) {
  ns <- shiny::NS(id)

  list(
    select = shiny:: uiOutput(ns("cheerleaderUI")),
    ui = shiny::uiOutput(ns("individual"))
  )
}

mod_cheer_server <- function(id, td, smm) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$cheerleaderUI <- shiny::renderUI({

      shiny::req(td())

      team_name <- td()$name
      cheerleaders <- team_cheerleaders |>
        dplyr::filter(team == team_name) |>
        dplyr::pull(cheerleader)

      shiny::radioButtons(
        inputId = ns("cheerleader"),
        label = "Cheerleaders:",
        choices = sort(cheerleaders),
        selected = character(0)
      )
    })

    output$individual <- shiny::renderUI({

      shiny::req(input$cheerleader)

      makeCheerleader(
        td,
        smm,
        input$cheerleader,
        ns("cheerPhoto"),
        ns("cheerBio"))
    })

    output$cheerPhoto <- shiny::renderUI({

      shiny::req(input$cheerleader)

      image <- paste0("./cheerleader_img/", input$cheerleader, ".png")
      shiny::img(src = image, height = "320px")
    })

    output$cheerBio <- DT::renderDataTable({

      shiny::req(input$cheerleader)

      DT::datatable(
        data = cheer_data[[input$cheerleader]]$table,
        class = "display",
        colnames = NULL,
        escape = FALSE,
        options = list(
          dom = "t",
          columnDefs = list(
            list(visible = FALSE, targets = 0)
            #list(width = '1000px', targets = 1)
          ),
          ordering = FALSE,
          pageLength = -1
        ),
        style = "auto"
      )
    })

  })
}

