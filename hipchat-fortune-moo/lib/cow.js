var exec = require('child_process').exec;

/*
 * callback format:
 * function (error, stdout, stderr)
 */

exports.moo = function(arg1, arg2) {
  var callback, image = "default";

  if (arg2 === undefined) {
    callback = arg1;
    image = "default";
  } else {
    callback = arg2;
    image = "default";

    if (arg1.match(/^\w(\w|-|\.)*$/)) {
      image = arg1;
    } else {
      console.warn("Image argument doesn't look like filename. Using default...");
    }
  }

  var command = "fortune | cowsay -f " + image;
  exec(command, callback);
};
