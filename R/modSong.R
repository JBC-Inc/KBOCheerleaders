
#' Song UI
#'
#' @param id A unique identifier string for the module’s UI.
#'
#' @return list of Shiny reactive input and output:
#'  - Team song toggle switch
#'  - UI for team song
#'  - UI for piki piki song
#'
mod_song_ui <- function(id) {
  ns <- shiny::NS(id)

  list(
    switch = shinyWidgets::materialSwitch(
      inputId = ns("play_video"),
      label = "Play Team Themesong",
      status = "primary"),
    ui_team = shiny::uiOutput(ns("teamsong")),
    ui_piki = shiny::uiOutput(ns("pikki"))
  )
}

#' Song Server
#'
#' Hosts 2 songs, the team song can be turned off but the piki piki song cannot
#' Also home of the App startup informational modal.
#'
#' @param id An ID string that corresponds with the ID used to call the
#' module's UI function.
#'
#' @return reactive url of the selected team song.
#'
mod_song_server <- function(id, td) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    selected_song <- shiny::reactive(label = "Song Selected", {
      song <- td()$song
      video_id <- sub(".*v=([^&]+).*", "\\1", song)
      autoplay <- if(input$play_video) "1" else "0"
      paste0("https://www.youtube.com/embed/", video_id, "?autoplay=", autoplay)
    })

    shiny::observe(label = "Play Themesong", {
      song <- selected_song()
      output$teamsong <- shiny::renderUI(
        shiny::tags$iframe(
          width="200",
          height="113",
          src=song,
          frameborder="0",
          allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture",
          allowfullscreen=NA)
      )
    })

    output$pikki <- shiny::renderUI({
      shiny::tagList(
        "Piki Piki Song",
        shiny::tags$iframe(
          width="200",
          height="113",
          src="https://www.youtube.com/embed/Aj8IY4mQQGo?autoplay=0",
          frameborder="0",
          allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture",
          allowfullscreen=NA
        )
      )
    })

    shiny::showModal(
      ui = shiny::modalDialog(
        title = shiny::div(
          shiny::h2("The Pikki Pikki Song Phenomenon",
                    style="color: #78C2AD;")
        ),
        introduction,
        footer = shiny::tagList(
          footer,
          shiny::modalButton("Dismiss"),
        ),
        size = "xl"
      )
    )
  })
}








