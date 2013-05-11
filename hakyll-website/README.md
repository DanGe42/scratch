# Notice

This website has been deprecated. Please see
[my new website](https://github.com/DanGe42/hakyll4-website).

Running
=======
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

Brief Tour
==========
In this project, I have these directories

* css/
* images/
* js/
* posts/
* templates/

The `css/`, `images/`, and `js/` directories are all static files. There's 
nothing of much interest in there.

The `templates/` folder contains all of my html templates. Of particular 
interest are `default.html`, which acts as the base template to all of my pages,
and `post.html`, which acts as the base template to all of my blog posts. The
templates `blog.html` and `post_item.html` act together to list blog posts.

Finally, the `posts/` folder contains all of my blog posts. Upon site
compilation, all files within this folder will be routed to the site directory
`/blog/`.

Finally, I have these top-level files:

* home.html - My homepage
* hakyll.hs - The Haskell file that defines the routing and compilation rules
for my site.
