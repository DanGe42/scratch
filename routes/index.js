var cow = require('../lib/cow');

/*
 * GET home page.
 */

exports.index = function(req, res){
  cow.moo(function (error, stdout, stderr) {
    if (error !== null) {
      res.send(500, "Oh noes!");
      return;
    };
    res.render('index', { title: 'Moo', moo: stdout });
  });
};
