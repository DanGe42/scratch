Hakyll publish
==============

A simple Haskell "script" I wrote to help publish articles I'm writing for my
Hakyll-generated blog. Since it's not convenient to add the file metadata and
publication date until after the article is written, this script will add such
metadata.

What this does
--------------

This program prepends the following to the file:

    ---
    title: (title)
    ---

The resulting text will be placed in the file *(date string)-file*. For example,
if my file was named "hello-world.markdown", and I ran the program on May 16,
2013, the resulting file will be named "2013-5-16-hello-world.markdown".

How to use
----------

To compile, simply run the Makefile: `make`. This will produce a binary named
`publish`.

The usage format of this program is: `publish [-d yyyy,mm,dd] title file`, where
the `-d` flag is optional. For complete usage instructions, run `publish
--help`.
