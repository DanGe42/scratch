Usage
=====
To download and 'install' this project, make sure you have all dependencies
installed:

* Haskell (installation is OS-dependent)
* Hakyll (`cabal install hakyll`)

Then run

    git clone git://github.com/DanGe42/hakyll-website.git

This will clone my Git repository to a folder named `hakyll-website`. Change
your current directory to that folder and run

    ghc --make hakyll.hs

This will output an executable named `hakyll`. There are two main ways to run
this:

* `./hakyll` will compile the site into a directory called `_site`. This is the
primary way to deploy the site.
* `./hakyll preview` will start a local server on `0.0.0.0:8000` and will allow
you to live preview the site and any changes you make (except for hakyll.hs)
by visiting the site in your browser.
