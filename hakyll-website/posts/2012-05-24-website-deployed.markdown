---
title: New website deployed, brought to you by Hakyll
description: First post on a static blog.
tags: deploy, hakyll
---

Welcome to the new version of my site. I first launched this in
early January with only a simple landing page
and a list of projects that I had worked on (employers love this
stuff, you know). It was written in pure HTML and CSS from the ground
up, sufficient for a simple two-page site.

Later on, I decided that I wanted a blogging platform; of course, pure
HTML and CSS does not scale easily. I had a number of options, including
Wordpress, but in the end, I chose not to install WordPress; it's too
heavy and restrictive for a simple site like mine. I would have also
had to pay for dynamic site hosting, which would increase my cost from
$$0.00/day to an expensive $$0.01/day with
[my hosting company](https://www.nearlyfreespeech.net/services/pricing).
(Still cheap, of course. This would have meant an additional $$0.30/month
to host a dynamic site on top of the $$0.20/month I'm already paying.)

I happened to be taking a
[Haskell course](http://www.cis.upenn.edu/~cis194/), and long
story short, I decided to try rewrite my site using
[Hakyll](http://jaspervdj.be/hakyll/), a Haskell static
site generator that sounds suspiciously like 
[Jekyll](http://jekyllrb.com/). Hakyll employs templates, which makes
it significantly easier to scale a simple website beyond two pages.

## Some (lengthy) thoughts on Hakyll

If you have some background in Haskell, Hakyll makes it simple to
create your own static site with a blogging
engine. For beginners, though, it can be intimidating, and I certainly
found it intimidating when I started this a couple of months ago.

My biggest hurdle was understanding what an
[Arrow](http://www.haskell.org/ghc/docs/latest/html/libraries/base/Control-Arrow.html)
was. Although Arrows are deeply rooted in
[category theory](http://en.wikipedia.org/wiki/Category_theory), a mathematics
degree is not required to use Arrows effectively. 
Since this article is about my website, if you would like to learn about
Arrows, you can read about [them](http://www.haskell.org/arrows/)
[on](http://www.haskell.org/haskellwiki/Arrow_tutorial)
[your](http://en.wikibooks.org/wiki/Haskell/Understanding_arrows)
[own](http://www.google.com/?q=haskell+arrow).

In many web frameworks like Rails, we write rules that instruct the
server return a page depending on the URL the client requests. In
Hakyll, we instead write rules that instruct Hakyll on what files
to generate and where to route them in the site directory based on
matching. For example, here is one of the routes from my
[hakyll.hs](https://github.com/DanGe42/hakyll-website/blob/master/hakyll.hs)
file:

    match "posts/*" $ do
        route $ setRoot `composeRoutes` routePost `composeRoutes` (setExtension "html")
        compile $ pageCompiler
            >>> arr (renderDateField "date" "%Y-%m-%d" "Unknown date")
            >>> arr (setPostTitle)
            >>> applyTemplateCompiler "templates/post.html"
            >>> applyTemplateCompiler "templates/default.html"
            >>> relativizeUrlsCompiler

This rule is the heart of the blogging engine. The first line,
`match "posts/*" $$ do`, matches all of my blog articles under the `posts/`
directory. The second line, `route $$ ...`, outputs my articles so that
they can be accessed at `/blog/yyyy-mm-dd-title.html`. Finally, the remaining
lines renders my articles by applying a couple of templates.

This all seems intimidating at first. Fortunately, the Hakyll website
provides a number of useful [tutorials](http://jaspervdj.be/hakyll/tutorials.html)
and [real-world examples](http://jaspervdj.be/hakyll/examples.html).

A very useful feature in Hakyll is Markdown support via
[pandoc](http://johnmacfarlane.net/pandoc/). This allows me to write my blog
articles in Markdown and have them converted to HTML seamlessly.

Hakyll makes it easy to live preview your site. Assuming you named
your file `hakyll.hs`, once you generate the file `hakyll`, you can
invoke `./hakyll preview` to start a simple server at `0.0.0.0:8000`,
which you can access to see your changes instantly. Once you are done,
you can invoke `./hakyll build`, which outputs your entire site into a folder
that you can export to your web host.

And, by the way, if you want to look at my website source, it is available
on [Github](https://github.com/DanGe42/hakyll-website).

## Moving forward

Of course, I am by no means completely finished with this site, as it will
always be a work in progress. For the site structure, I will be implementing
responsive design to make the site more friendly towards smaller screen sizes.
Another item on my list of goals is implementing blog post tag support so that
you will be able to click on a tag to see all pages with that tag.

At the moment, it is time-consuming to export the generated site to
my host manually. As soon as I become a git "ninja", I will add a hook that
would export my site automatically when I `git push` in order to streamline
deployments.

Thank you for reading this lengthy introduction, and I hope you enjoy my
better-looking landing page and blog.
