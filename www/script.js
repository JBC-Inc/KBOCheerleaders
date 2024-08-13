
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
  /*Shiny.setInputValue("cheerleader", "");*/
});

