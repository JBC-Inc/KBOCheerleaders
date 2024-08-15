
$(document).on("click", ".cheerleader-photo", function() {

  var team = $(this).data("team");
  var name = $(this).data("name");

  console.log("Team: " + team);
  console.log("Name: " + name);

  Shiny.setInputValue("team", team);
  Shiny.setInputValue("cheerleader", name);
});

$(document).on("click", ".team-photo", function() {

  var team = $(this).data("tt");

  console.log("Team: " + team);

  Shiny.setInputValue("team", team);
  Shiny.setInputValue("cheerleader", "");
});


/*
Shiny.addCustomMessageHandler("set_team", function(message) {
  console.log("Received message for 'set_team':", JSON.stringify(message, null, 2));
});

// Custom message handler for "set_cheerleader"
Shiny.addCustomMessageHandler("set_cheerleader", function(message) {
  console.log("Received message for 'set_cheerleader':", JSON.stringify(message, null, 2));
});
*/
