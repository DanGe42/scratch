$(document).ready( function() {
    var canvas = document.getElementById("pad");
    $('#send-button').click( function() {
        console.log("Uploading canvas data...");
        $.post('/upload', 
               { image: canvas.toDataURL('image/png') },
              function(data) {
                  // TODO: replace canvas with message
                  console.log("Success!");
              });
    });
});
