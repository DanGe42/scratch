$(document).ready( function() {
    var canvas = document.getElementById("pad");
    var $secret_form = $("#secret-form");
    $secret_form.submit(function() {
        var dataURL = canvas.toDataURL('image/png');
        $secret_form.children("input[name='image']").val(dataURL);
        console.log("Uploading canvas data...");
    });
});
