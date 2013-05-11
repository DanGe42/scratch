// Requires jQuery

function canvasApp () {
    if (!canvasSupport()) {
        return;
    }

    var $canvas = $("#pad");
    var context = $canvas[0].getContext("2d");

    // Constants
    var TOOLS = {
        FREEHAND: "freehand",
        LINE: "line",
        ERASER: "eraser"
    };

    var drawDot = function (ctx, x, y, thickness) {
        ctx.beginPath();
        ctx.arc(x, y, thickness / 2, 0, 2 * Math.PI);
        ctx.fill();
    };
    var drawLine = function (ctx, x1, y1, x2, y2) {
        ctx.beginPath();
        ctx.moveTo(x1, y1);
        ctx.lineTo(x2, y2);
        ctx.stroke();
    };

    // Event listeners for the canvas
    var canvasListeners = {
        mousedown: function (mouseState, editor, ctx, e) {
            if (e.button != 0) return; // disregard other clicks
            var mouse = getMousePosition(e);
            mouseState.isDown = true;

            switch (editor.tool) {
                case TOOLS.FREEHAND:
                    drawDot(ctx, mouse.x, mouse.y, editor.thickness);
                    break;
                case TOOLS.LINE:
                    editor.data = ctx.getImageData(0, 0, $canvas[0].width, $canvas[0].height);
                    break;
                case TOOLS.ERASER:
                    editor.beforeEraseColor = ctx.strokeStyle;
                    ctx.strokeStyle = ctx.fillStyle = editor.backgroundColor;
                    drawDot(ctx, mouse.x, mouse.y, editor.thickness);
                    break;
            }
            mouseState.previous = { x: mouse.x, y: mouse.y };
        }

      , mousemove: function (mouseState, editor, ctx, e) {
            if (e.button != 0) return;  // disregard other clicks

            if (!mouseState.isDown) return;
            var mouse = getMousePosition(e);

            var previous = mouseState.previous;

            switch (editor.tool) {
                case TOOLS.FREEHAND:
                    drawLine(ctx, previous.x, previous.y, mouse.x, mouse.y);
                    mouseState.previous = { x: mouse.x, y: mouse.y };
                    break;
                case TOOLS.LINE:
                    ctx.putImageData(editor.data, 0, 0);
                    drawLine(ctx, previous.x, previous.y, mouse.x, mouse.y);
                    break;
                case TOOLS.ERASER:
                    drawLine(ctx, previous.x, previous.y, mouse.x, mouse.y);
                    mouseState.previous = { x: mouse.x, y: mouse.y };
                    break;
            }
        }

      , mouseup: function (mouseState, editor, ctx, e) {
            if (e.button != 0) return;  // disregard other clicks

            // Doesn't really make sense to handle a foreign mouseup event
            if (!mouseState.isDown) return;

            var previous = mouseState.previous;
            var mouse = getMousePosition(e);
            switch (editor.tool) {
                case TOOLS.LINE:
                    drawLine(ctx, previous.x, previous.y, mouse.x, mouse.y);
                    break;
                case TOOLS.ERASER:
                    ctx.strokeStyle = ctx.fillStyle = editor.beforeEraseColor;
                    break;
            }

            mouseState.previous = null;
            mouseState.isDown = false;
            editor.data = null;
        }
    };

    // Add event listeners to our form elements that control the canvas
    var initForm = function (editor, ctx) {
        $("#tool").change( function (e) {
            editor["tool"] = e.target.value;
        });

        $("#thickness").change( function (e) {
            var val = e.target.value;
            editor["thickness"] = val;
            context.lineWidth = val;
        });

        $("#color").change( function (e) {
            var val = e.target.value;
            context.strokeStyle = val;
            context.fillStyle = val;
        });
    };

    // init
    (function() {
        // Canvas defaults
        context.lineCap = "round";
        context.lineWidth = 4;
        context.strokeStyle = "black";

        var mouseState = {
            isDown: false,
            previous: null
        };
        var editMode = {
            backgroundColor: "#FFFFFF",
            beforeEraseColor: null,
            data: null,
            tool: TOOLS.FREEHAND,
            thickness: 4        // "normal" thickness
        }

        initForm(editMode, context);

        $canvas.mousedown(canvasListeners.mousedown
                          .bind(null, mouseState, editMode, context));
        $canvas.mousemove(canvasListeners.mousemove
                          .bind(null, mouseState, editMode, context));
        $canvas.mouseup(canvasListeners.mouseup
                        .bind(null, mouseState, editMode, context));
        $canvas.mouseout(canvasListeners.mouseup
                         .bind(null, mouseState, editMode, context));

    })();

    // --------------------------------------------------
    // Helper functions (these will get hoisted)
    // --------------------------------------------------

    // Helper function to get mouse event positions
    function getMousePosition (e) {
        var scroll = getScrollOffsets();
        var mouseX = e.clientX + scroll.x - $canvas[0].offsetLeft;
        var mouseY = e.clientY + scroll.y - $canvas[0].offsetTop;

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
}

function canvasSupport () {
    return Modernizr.canvas;
}

$(document).ready(canvasApp);
