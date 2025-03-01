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
    ")),

    # Session Timeout //////////////////////////////////////////////////////////
    shiny::tags$script(shiny::HTML("
      var timeout;

      function resetTimeout() {
        clearTimeout(timeout);
        timeout = setTimeout(function() {
          Shiny.setInputValue('session_timeout', true); // Send message to Shiny
        }, 5000); // 5 seconds inactivity timeout
      }

      document.onload = resetTimeout;
      document.onmousemove = resetTimeout;
      document.onkeydown = resetTimeout;

      // Listen for session timeout and show a modal
      Shiny.addCustomMessageHandler('showTimeoutModal', function(message) {
        var modal = document.createElement('div');
        modal.id = 'timeout-modal';
        modal.style.position = 'fixed';
        modal.style.top = '0';
        modal.style.left = '0';
        modal.style.width = '100%';
        modal.style.height = '100%';
        modal.style.backgroundColor = 'rgba(0, 0, 0, 0.6)'; // Dim effect
        modal.style.display = 'flex';
        modal.style.alignItems = 'center';
        modal.style.justifyContent = 'center';
        modal.style.zIndex = '9999';

        var messageBox = document.createElement('div');
        messageBox.style.background = 'white';
        messageBox.style.padding = '20px';
        messageBox.style.borderRadius = '10px';
        messageBox.style.boxShadow = '0px 0px 10px rgba(0,0,0,0.5)';
        messageBox.innerHTML = '<p style=\"font-size:18px; text-align:center;\">Session Timed Out</p>'; # nolint

        var reloadBtn = document.createElement('button');
        reloadBtn.innerText = 'Reload App';
        reloadBtn.style.display = 'block';
        reloadBtn.style.margin = '10px auto';
        reloadBtn.style.padding = '10px 20px';
        reloadBtn.style.fontSize = '16px';
        reloadBtn.style.cursor = 'pointer';
        reloadBtn.onclick = function() {
          location.reload(); // Reload the app when button is clicked
        };

        messageBox.appendChild(reloadBtn);
        modal.appendChild(messageBox);
        document.body.appendChild(modal);
      });
    "))




  )
}