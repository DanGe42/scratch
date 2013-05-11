var CronJob = require('cron').CronJob,
    config  = require('./config').config,
    hipchat = require('./lib/hipchat'),
    time = require('time'),
    cow = require('./lib/cow');

var TZ = "US/Pacific";

var cowCallback = function (error, stdout, stderr) {
  if (error === null) {
    hipchat.sendToHipchat(config.hipchat, stdout, function(res) {
      var data = "";
      res.on('data', function (chunk) {
        data += chunk;
      });
      res.on('close', function (err) {
        console.error("The connection terminated unexpectedly.");
        console.error(err);
      });

      res.on('end', function () {
        var parsed = JSON.parse(data);
        if (res.statusCode === 200) {
          console.log("--> 200 OK! Message successfully posted!");
          console.log("The status of this request is -- %s'", parsed["status"]);
        }
        else {
          console.error("--> Bad status code returned: %d", res.statusCode);
          console.error("Error type: %s", parsed["type"]);
          console.error("Why: %s", parsed["message"]);
        }
      });

    });
  }
  else {
    console.error("An error occurred while executing the shell command.");
    console.error("Maybe stderr might help?");
    console.error(stderr);
    console.error("Or maybe the error dump from Node?");
    console.error(error);
  }
};

exports.mooJob = function() {
  // Run this cron job every two hours from 10:00 AM to 8:00 PM every day
  var job = new CronJob({
    cronTime: config["cron"],
    onTick: function() {
      console.log("--> Starting Hipchat job at %s...", (new Date()).toString());

      cow.moo('tux', cowCallback);
    },

    start: false,
    timeZone: TZ
  });

  return job;
};
