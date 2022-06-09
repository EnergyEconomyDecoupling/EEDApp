# JS code for rendering math using katex in tables, from:
# https://stackoverflow.com/questions/67436872/rendering-katex-in-multiple-tables-in-an-r-shiny-app/67436928?noredirect=1#comment119211884_67436928
render_katex <- "
$(document).on('shiny:value', function(event) {
  if((/stages_table$/).test(event.name) || (/results_table$/).test(event.name)){
    var matches = event.value.match(/(%%+[^%]+%%)/g);
    var newvalue = event.value;
    for(var i=0; i<matches.length; i++){
      var code = '' + matches[i].slice(2,-2);
      newvalue = newvalue.replace(matches[i], katex.renderToString(code));
    }
    event.value = newvalue;
  }
})
"
