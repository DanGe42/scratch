The HipChat Fortune Bot
=======================

This is a [HipChat](https://www.hipchat.com/) bot that sends fortunes piped
to `cowsay` every two hours to a specified room.

See [this link](https://www.hipchat.com/docs/api) for more information on the
API.

How to use
----------

This Node app expects there to be a `config.js` file containing configuration
details in the root directory. At this time, it should look like this:

    exports.config = {
        hipchat: {
            "token": <hipchat auth token>,
            "username": "Moo",
            "color": "gray",
            "room": "Test room"
        },
        "cron": "0 0 4,6,8 * * 1-5" (crontab syntax extended with seconds field)
    };

Setup
-----
Currently, the setup script is not as complete as I hoped to make it; that will
be updated later in the future. But, here is a general summary of how to set
this up (assuming Ubuntu 12.04 LTS running on an Amazon EC2 instance):

1. Install git and clone this repo
2. Install node and npm
3. Install necessary Ubuntu packages (see next section)
4. Install "forever" (`npm install -g forever`) to allow app to run as daemon
5. Create a firewall rule to forward requests to port 80 to port 8080:
   `sudo iptables -t nat -A PREROUTING -p tcp --dport 80 -j REDIRECT --to 8080`
6. Run `npm install` within repo
7. Create config file, set `NODE_ENV=production`, and run the app with forever

Dependencies
------------
Node packages:

* Node ~0.8.x (could work on lower versions, but have not tested this yet)
* express 3.0.x
* node-cron 0.3.x
* time (for node-cron) 0.8.x

Required external packages (Ubuntu):

* cowsay
* fortune (i.e. fortune-mod, etc.)


Internal modules
----------------
* lib/cow.js - Provides interface with `cow | fortune` pipe
* lib/hipchat.js - Provides interface with room#message API endpoint

