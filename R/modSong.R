
mod_song_ui <- function(id) {
  ns <- shiny::NS(id)

  list(
    switch = shinyWidgets::materialSwitch(
      inputId = ns("play_video"),
      label = "Play Team Themesong",
      status = "danger"),
    ui_team = shiny::uiOutput(ns("teamsong")),
    ui_piki = shiny::uiOutput(ns("pikki"))
  )
}

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








