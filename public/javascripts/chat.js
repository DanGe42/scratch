$(document).ready(function() {
  var socket = io.connect("http://localhost");

  var messages = $("#messages");
  var form = $("#send-form");
  var input = form.children("input[name='message']");
  var username = $("#username");
  var room_name = $("#roomname");

  form.submit(function(e) {
    var msg = input.val();
    socket.send(msg);

    input.val("");
    e.preventDefault();
  });

  socket.on('connect', function() {
    socket.on('username', function(data) {
      var name = data.name;
      username.html(name);
    });

    socket.on('room', function(data) {
      var room = data.room;
      room_name.html(room);
    });

    socket.on('info', function(data) {
      messages.val(messages.val() + '\n[INFO] ' + data.msg);
    });

    socket.on('error', function(data) {
      messages.val(messages.val() + '\n[ERROR] ' + data.msg);
    });

    socket.on('msg', function(data) {
      var insert = data.user + ": " + data.msg;
      messages.val(messages.val() + '\n' + insert);
    });
  });
});
