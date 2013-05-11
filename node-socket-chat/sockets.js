var intro = "Welcome to the Socket.io chat room.\n"
intro += "To join a room, enter '/join <room>'.\n"
intro += "To leave the room, enter '/part'.\n"
intro += "To identify yourself, enter '/nick <username>'."

exports.setup_sockets = function(io) {
  io.sockets.on('connection', function(socket) {
    var username = "user-" + socket.id.toString().slice(0, 6);
    var room = null;
    socket.emit('username', { name: username });

    socket.emit('info', { msg: intro });

    socket.on('message', function(msg) {
      // No point in handling no-content messages
      if (!msg) return;

      // if the message contains a command
      if (msg[0] === '/') {
        var tokens = msg.split(' ', 2);
        var command = tokens[0].slice(1);

        switch (command) {
          case "join":
            if (room) {
              socket.leave(room);
            }

            if (tokens[1]) {
              room = tokens[1];
              socket.join(room);
              socket.emit('room', { room: room });
              socket.emit('info', { msg: "You have joined the room '" + room + "'." });
              io.sockets.in(room).emit('info', { msg: username + " has joined the room." });
            }
            else {
              socket.emit('error', { msg: "Invalid /join command" });
            }
            break;
          case "part":
            if (room) {
              io.sockets.in(room).emit('info', { msg: username + " has left the room." });
              socket.leave(room);
              room = null;
              socket.emit('room', { room: null });
              socket.emit('info', { msg: "You have left the room '" + room + "'." });
            }
            else {
              socket.emit('error', { msg: "You are not in a room." });
            }
            break;
          case "nick":
            var name = tokens[1];

            if (name) {
              username = name;
              socket.emit('username', { name: username });
              socket.emit('info', { msg: "You are now known as '" + name + "'." });
            }
            else {
              socket.emit('error', { msg: "Invalid /nick command" });
            }
            break;
          default:
            socket.emit('error', { msg: "Invalid command " + command });
        }
      }
      else {
        if (!room) {
          socket.emit('error', { msg: "You are not in a room." });
          return;
        }

        io.sockets.in(room).emit('msg', { user: username, msg: msg });
      }
    });
  });
};
