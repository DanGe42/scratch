window.addEventListener('load', canvasApp, false);

function canvasApp () {
    if (!canvasSupport()) {
        return;
    }

    var canvas = document.getElementById("board");
    var ctx = canvas.getContext("2d");

    // Constants
    var TOOLS = {
        FREEHAND: "freehand",
        LINE: "line"
    };

    var pixels = {}

    init();


    function init () {
        initPixels(pixels);

        // Canvas defaults
        ctx.lineCap = "round";
        ctx.lineWidth = 4;
        ctx.strokeStyle = "black";

        var mouseState = {
            isDown: false,
            previous: null
        };
        var editMode = {
            data: null,
            tool: TOOLS.FREEHAND,
            thickness: 4        // "normal" thickness
        }

        initForm(editMode);

        canvas.addEventListener('mousedown',
                                eventMouseDown.bind(null, mouseState, editMode),
                                false);
        canvas.addEventListener('mousemove',
                                eventMouseMove.bind(null, mouseState, editMode),
                                false);
        canvas.addEventListener('mouseup',
                                eventMouseUp.bind(null, mouseState, editMode),
                                false);
    }

    function initForm(state) {
        var handlerFactory = function(state, attr) {
            return function (e) {
                var target = e.target;
                state[attr] = target.value;
            };
        };

        var formElement;
        formElement = document.getElementById("tool");
        formElement.addEventListener('change',
                                     handlerFactory(state, "tool"), false);

        formElement = document.getElementById("thickness");
        formElement.addEventListener('change',
                                     handlerFactory(state, "thickness"), false);
        formElement.addEventListener('change',
                                     function (e) { ctx.lineWidth = e.target.value },
                                     false);

        formElement = document.getElementById("color");
        formElement.addEventListener('change',
                                     function (e) { ctx.strokeStyle = e.target.value },
                                     false);
    }

    function initPixels (pixels) {
        var pixelData;

        pixels["black"] = createPixel(ctx, 0, 0, 0, 255);
    }

    function createPixel (ctx, red, green, blue, alpha) {
        var pixelData = ctx.createImageData(1, 1);
        pixelData.data[0] = red;
        pixelData.data[1] = green;
        pixelData.data[2] = blue;
        pixelData.data[3] = alpha;

        return pixelData;
    }


    // --------------------------------------------------
    // Event handlers
    // --------------------------------------------------

    function eventMouseDown (state, editor, e) {
        if (e.button != 0) return;  // disregard other clicks

        var mouse = getMousePosition(e);
        state.isDown = true;

        switch (editor.tool) {
            case TOOLS.FREEHAND:
                drawPixel(ctx, mouse.x, mouse.y, "black");
                break;
            case TOOLS.LINE:
                editor.data = ctx.getImageData(0, 0, canvas.width, canvas.height);
                break;
        }
        state.previous = { x: mouse.x, y: mouse.y };
    }

    function eventMouseMove (state, editor, e) {
        if (e.button != 0) return;  // disregard other clicks

        if (!state.isDown) return;
        var mouse = getMousePosition(e);

        switch (editor.tool) {
            case TOOLS.FREEHAND:
                ctx.beginPath();
                ctx.moveTo(state.previous.x, state.previous.y);
                ctx.lineTo(mouse.x, mouse.y);
                ctx.stroke();

                state.previous = { x: mouse.x, y: mouse.y };
                break;
            case TOOLS.LINE:
                ctx.putImageData(editor.data, 0, 0) 

                ctx.beginPath();
                ctx.moveTo(state.previous.x, state.previous.y);
                ctx.lineTo(mouse.x, mouse.y);
                ctx.stroke();
                break;
        }
    }

    function eventMouseUp (state, editor, e) {
        if (e.button != 0) return;  // disregard other clicks

        state.previous = null;
        state.isDown = false;
        state.data = null;
    }

    // Helper function to get mouse event positions
    function getMousePosition (e) {
        var scroll = getScrollOffsets();
        var mouseX = e.clientX + scroll.x - canvas.offsetLeft;
        var mouseY = e.clientY + scroll.y - canvas.offsetTop;

        return { x: mouseX, y: mouseY };
    }

    // From David Flanagan's JavaScript: The Definitive Guide (6e)
    // See Example 15-8 (page 391) for more details
    // ------------------------------------------------------------
    // Return the current scrollbar offsets as the x and y properties of an object
    function getScrollOffsets(w) {
        // Use the specified window or the current window if no argument
        w = w || window;

        // This works for all browsers except IE versions 8 and before
        if (w.pageXOffset != null) return {x: w.pageXOffset, y:w.pageYOffset};

        // For IE (or any browser) in Standards mode
        var d = w.document;
        if (document.compatMode == "CSS1Compat") {
            return {x:d.documentElement.scrollLeft, y:d.documentElement.scrollTop};
        }

        // For browsers in Quirks mode
        return { x: d.body.scrollLeft, y: d.body.scrollTop };
    }


    // --------------------------------------------------
    // Drawing code
    // --------------------------------------------------

    function drawPixel (context, x, y, color) {
        pixel = pixels[color];
        context.putImageData(pixel, x, y);
    }

    function drawScreen () {
        drawPixel(ctx, 5,7, "black");
    }
}

function canvasSupport () {
    return Modernizr.canvas;
}
