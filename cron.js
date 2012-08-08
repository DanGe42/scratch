var CronJob = require('cron').CronJob,
    config  = require('./config').config,
    hipchat = require('./lib/hipchat'),
    time = require('time'),
    cow = require('./lib/cow');

var onComplete = function() {
  console.log("Job finished at " + new Date());
};
var TZ = "America/Los Angeles";

exports.mooJob = function() {
  // Run this cron job every two hours from 10:00 AM to 8:00 PM every day
  var job = new CronJob({
    cronTime: '0 0 9-19 * * *',
    onTick: function() {
      console.log("Starting Hipchat job...");

      cow.moo(function (error, stdout, stderr) {
        if (error === null) {
          hipchat.sendToHipchat(config.hipchat, stdout, function(res) {
            var data = "";
            res.on('data', function (chunk) {
              data += chunk;
            });
            res.on('close', function (err) {
              console.log("The connection terminated unexpectedly.");
              console.log(err);
            });

            res.on('end', function () {
              var parsed = JSON.parse(data);
              if (res.statusCode === 200) {
                console.log("200 OK! Message successfully posted!");
                console.log("The status of this request is -- '" + parsed["status"]);
              }
              else {
                console.log("Bad status code returned: " + res.statusCode);
                console.log("Error type: " + parsed["type"]);
                console.log("Why: " + parsed["message"]);
              }
            });

          });
        }
        else {
          console.log("An error occurred while executing the shell command.");
          console.log("Maybe stderr might help?");
          console.log(stderr);
          console.log("Or maybe the error dump from Node?");
          console.log(error);
        }
      });
    },

    onComplete: onComplete,
    start: false,
    timeZone: TZ
  });

  return job;
};
