
addExternalResources <- function() {

  shiny::addResourcePath("www", "www")

  shiny::tags$head(

    shinyjs::useShinyjs(),

    shiny::tags$script(src = "script.js"),

    shiny::includeCSS(path = "./www/custom.css"),

    shiny::tags$link(
      rel = "icon",
      type = "image/png",
      sizes = "32x32",
      href = "www/favicon-32x32.png"
    ),

    shiny::tags$link(
      rel = "stylesheet",
      href = "https://fonts.googleapis.com/css2?family=Bangers&display=swap"
    )

    # shiny::tags$script(src = "https://kit.fontawesome.com/38ef58b8f5.js"),

    # shinyFeedback::useShinyFeedback(feedback = FALSE)
  )
}
