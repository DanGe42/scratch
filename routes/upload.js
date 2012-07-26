var fs = require('fs')
  , crypto = require('crypto');

/*
 * POST canvas data to server
 */

exports.upload = function(req, res) {
    console.log("Receiving upload...");
    var imageURL = req.body.image;
    var URLmatch = /^data\:image\/png;base64,(.+)$/
    var result = imageURL.match(URLmatch);
    if (!result) {
        console.log("Bad data!");
        res.writeHead(400, { "Content-Type": "text/plain" });
        res.write("Your upload failed.");
        res.end();
        return;
    }

    var image = new Buffer(result[1], 'base64');
    var shasum = crypto.createHash('sha1');
    shasum.update(result[1]);

    var filename = '/tmp/' + shasum.digest('hex').slice(0, 7) + ".png";

    console.log("Writing image to " + filename + "...");

    fs.writeFile(filename, image, function (err) {
        if (err) {
            res.writeHead(500, { "Content-Type": "text/plain" });
            res.write("Something went wrong :(");
            res.write(err);
            console.log(err);
            res.end();
            return;
        }

        console.log("Successfully wrote file to " + filename);
        res.writeHead(200, { "Content-Type": "text/plain" });
        res.write("Your upload was successful!");
        res.end();
    });
}
