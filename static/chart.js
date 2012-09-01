$(document).ready(function() {
  $.getJSON('/update', function(data) {
    console.log(data);
  });
});
