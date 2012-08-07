The Hipchat Fortune Bot
=======================

This is a Hipchat bot that sends fortunes piped to `cowsay` every two hours to
a specified room.

How to use
----------

This Node app expects there to be a `./config.js` file containing configuration
details. At this time, it should look like this:

    exports.config = {
        "hipchat_token": "0123456789abcdef"
    };

