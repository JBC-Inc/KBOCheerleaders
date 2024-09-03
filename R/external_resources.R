
#' Add Shiny static resources to Shiny Server
#'
#' Make scripts available to their components.
#'
#' @return adds external resources to the Shiny app by returning a
#' `tags$head` element that includes:
#'  - Resource Paths
#'  - JavaScript and CSS scripts
#'  - Favicon
#'  - Font Links
#'  - Custom JS Handles
#' @keywords internal
#'
addExternalResources <- function() {

  options(shiny.reactlog = TRUE)

  shiny::addResourcePath("www", "www")

  shiny::tags$head(

    shinyjs::useShinyjs(),

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
      ),

    # Reactable, Leaderboards, repeat same team/cheerleader

    shiny::tags$script(shiny::HTML("
      $(document).ready(function() {
        $(document).on('click', '.team-photo', function() {
          var team = $(this).data('tt');
          console.log('Team: ' + team);
          Shiny.setInputValue('team-team', team);
          Shiny.setInputValue('randteam', Math.random());
        });
       $(document).on('click', '.cheerleader-photo', function() {
         var team = $(this).data('team');
         var name = $(this).data('name');
         //console.log('Team: ' + team);
         //console.log('Name: ' + name);
         Shiny.setInputValue('team-team', team);
         Shiny.setInputValue('cheer-cheerleader', name);
         Shiny.setInputValue('randcheer', Math.random());
       });
      });
    ")),

    # ggiraph

    shiny::tags$script(shiny::HTML("
      Shiny.addCustomMessageHandler('handler1', function(message){
        var team = message[1];
        var name = message[0];
        //alert(`${name}, ${team}`);
        Shiny.setInputValue('team-team', team);
        Shiny.setInputValue('cheer-cheerleader', name);
        Shiny.setInputValue('randcheer', Math.random());
      });
    ")),

    # navbar page scroll

    shiny::tags$script(shiny::HTML("
      Shiny.addCustomMessageHandler('scrollToTop', function(message) {
        window.scrollTo({ top: 0, behavior: 'instant' });
      });
    "))
  )
}





