var https = require('https'),
    qs = require('querystring');

exports.sendToHipchat = function (config, msg, resCallback) {
  var post_data = qs.stringify({
    'room_id': config["room"] || "Test",
    'from': config["username"] || "IAmARobot",
    'message': msg,
    'message_format': "text",
    'notify': config["notify"] || 0,
    'color': config["color"] || "gray",
    'format': "json"
  });

  var options = {
    host: "api.hipchat.com",
    path: "/v1/rooms/message?auth_token=" + config["token"],
    method: 'POST',
    headers: {
      'Content-Type': 'application/x-www-form-urlencoded',
      'Content-Length': post_data.length
    }
  };

  var req = https.request(options, resCallback);
  req.write(post_data);
  req.end();
};

