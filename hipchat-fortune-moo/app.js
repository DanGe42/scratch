
/**
 * Module dependencies.
 */

var express = require('express')
  , routes = require('./routes')
  , http = require('http')
  , path = require('path')
  , mooJob = require('./cron').mooJob();

var app = express();

app.configure(function(){
  app.set('views', __dirname + '/views');
  app.set('view engine', 'jade');
  app.use(express.favicon());
  app.use(express.bodyParser());
  app.use(express.methodOverride());
  app.use(app.router);
  app.use(express.static(path.join(__dirname, 'public')));
});

app.configure('development', function(){
  app.set('port', process.env.PORT || 3000);
  app.use(express.logger('dev'));
  app.use(express.errorHandler());
});

app.configure('production', function() {
  app.set('port', process.env.PORT || 8080);
  app.use(express.logger('default'));
});

app.get('/', routes.index);
app.get('/status', routes.status.bind(null, mooJob));
app.get('/shutdown', routes.killswitch.bind(null, mooJob));
app.get('/start', routes.start.bind(null, mooJob));

http.createServer(app).listen(app.get('port'), function(){
  console.log("Express server listening on port " + app.get('port'));
});

mooJob.start();
