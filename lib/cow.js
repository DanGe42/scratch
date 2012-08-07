var exec = require('child_process').exec;

/*
 * callback format:
 * function (error, stdout, stderr)
 */

exports.moo = function(callback) {
  exec("fortune | cowsay", callback);
};
