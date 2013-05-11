
/*
 * GET home page.
 */

exports.index = function(req, res){
  res.render('index');
};

/*
 * GET embedded elements
 */

exports.embed = function(req, res){
  var csrf = req.session._csrf;
  res.render('embed', { token: csrf });
};

exports.upload = require("./upload").upload;
