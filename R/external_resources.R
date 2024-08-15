
#' Add Shiny static resources to Shiny Server
#'
#' Make scripts available to their components.
#'
#' @return
#'
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

    # Bangers
    shiny::tags$link(
      rel = "stylesheet",
      href = "https://fonts.googleapis.com/css2?family=Bangers&display=swap"
    ),

    # Roboto
    shiny::tags$link(
      rel = "stylesheet",
      href = "https://fonts.googleapis.com/css2?family=Roboto:wght@400;700&display=swap"
      )

    # shiny::tags$script(src = "https://kit.fontawesome.com/38ef58b8f5.js"),

    # shinyFeedback::useShinyFeedback(feedback = FALSE)

    # shiny::tags$script(shiny::HTML("
    #   $(document).on('click', '.team-photo', function() {
    #     var team = $(this).data('tt');
    #
    #     console.log(team);
    #
    #     Shiny.setInputValue('team-team', team);
    #   });
    # ")),
    #
    # shiny::tags$script(shiny::HTML("
    #   $(document).on('click', '.cheerleader-photo', function() {
    #     var team = $(this).data('team');
    #     var name = $(this).data('name');
    #
    #     console.log(team);
    #     console.log(name);
    #
    #     Shiny.setInputValue('team-team', team);
    #     Shiny.setInputValue('cheer-cheerleader', name);
    #   });
    # ")),
  )
}
