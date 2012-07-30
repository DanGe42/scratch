var fs = require('fs')
  , crypto = require('crypto');


// Helper methods for .upload

// This method, despite its name, is limited to one use:
// it continues if a folder exists, and throws an error if something else
// weird occurred.
var throwErrorOr = function (error) {
    // Error 47 is EEXIST (folder already exists)
    if (error && error.errno !== 47) {
        console.log("Failed to create directory '" + error.path + "'");
        throw error;
    }
};

var uploadFile = function(filename, date, buffer, success) {
    fs.mkdir("uploads", '0777', function(err1) {
        throwErrorOr(err1);

        var date_folder = "uploads/" +
                        date.getUTCFullYear() + "-" + date.getUTCMonth() + "-" +
                        date.getUTCDate();

        fs.mkdir(date_folder, '0777', function(err2) {
            throwErrorOr(err2);

            var path = date_folder + "/" + filename;
            fs.writeFile(path, buffer, function(error) {
                if (error) {
                    throw error;
                }

                console.log("Successfully wrote to '" + path + "'");

                if (success) {
                    success();
                }
            });
        });
    });
};

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

    var now = new Date();

    try {
        var filename = shasum.digest('hex').slice(0, 7) + ".png";
        uploadFile(filename, now, image, function() {
            res.writeHead(200, { "Content-Type": "text/plain" });
            res.write("Your upload was successful!");
            res.end();
        });
    } catch (err) {
        res.writeHead(500, { "Content-Type": "text/plain" });
        res.write("Something went wrong :(");
        console.log(err);
        res.end();
    }
}
