
/*
 * GET home page.
 */

var option = function(value, name, selected) {
    var s = selected || false;

    return {
        value: value,
        name: name,
        selected: s
    };
};

// currently unused
var form = {
    tool: [ option('freehand', "Freehand"), option('line', "Line") ],
    thickness: [
        option(1, "Hairline"),
        option(2, "Thin"),
        option(4, "Normal", true),
        option(6, "Thick"),
        option(8, "Thicker"),
        option(10, "Thickest")
    ],
    color: [
        option('red', "Red"),
        option('blue', "Blue"),
        option('green', "Green"),
        option('orange', "Orange"),
        option('black', "Black", true),
        option('yellow', "Yellow")
    ]
};

exports.index = function(req, res){
  res.render('index', {
      title: 'Flockboard',
      tool: form.tool,
      thickness: form.thickness,
      color: form.color
  });
};
