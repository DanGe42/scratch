window.addEventListener('load', canvasApp, false);

function canvasApp () {
    var canvas = document.getElementById("board");
    var ctx = canvas.getContext("2d");
}

function canvasSupport () {
    return Modernizr.canvas;
}