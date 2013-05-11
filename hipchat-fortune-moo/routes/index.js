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

exports.status = function(job, req, res) {
  var status = "STOPPED";
  if (job["running"]) {
    status = "RUNNING";
  }
  res.render('status', { title: 'Status', status: status });
};

exports.killswitch = function(job, req, res) {
  console.log("hipjob stopped");
  job.stop();
  res.send(200, "No hard feelings");
};

exports.start = function(job, req, res) {
  console.log("hipjob started");
  job.start();
  res.send(200, "I'm back!");
};
