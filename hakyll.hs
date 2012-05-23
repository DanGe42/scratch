{-# LANGUAGE OverloadedStrings #-}
import Control.Arrow ((>>>), arr)
import Data.Monoid (mempty)
import Data.Text (split, pack, unpack)

import System.FilePath

import Hakyll

main :: IO ()
main = hakyll $ do

    -- Static files (more or less)
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler
    
    match "js/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "robots.txt" $ do
        route   idRoute
        compile copyFileCompiler

    -- Home page
    match "home.html" $ do
        route $ customRoute (\_ -> toFilePath "index.html")
        compile $ readPageCompiler
            >>> addDefaultFields
            >>> arr applySelf
            >>> applyTemplateCompiler "templates/default.html"
            >>> relativizeUrlsCompiler

    -- Projects page
    match "projects.html" $ do
        route $ routeToDir
        compile $ readPageCompiler
            >>> addDefaultFields
            >>> arr applySelf
            >>> applyTemplateCompiler "templates/default.html"
            >>> relativizeUrlsCompiler

    -- Blogging engine
    match "posts/*" $ do
        route $ setRoot `composeRoutes` routePost `composeRoutes` (setExtension "html")
        compile $ pageCompiler
            >>> arr (renderDateField "date" "%Y-%m-%d" "Unknown date")
            >>> arr (setPostTitle)
            >>> applyTemplateCompiler "templates/post.html"
            >>> applyTemplateCompiler "templates/default.html"
            >>> relativizeUrlsCompiler

    match "blog.html" $ route routeToDir
    create "blog.html" $ constA mempty
        >>> arr (setField "title" "Recent Posts")
        >>> setFieldPageList recentFirst "templates/post_item.html" "posts" "posts/*"
        >>> applyTemplateCompiler "templates/blog.html"
        >>> applyTemplateCompiler "templates/default.html"
        >>> relativizeUrlsCompiler

    match "atom.xml" $ route idRoute
    create "atom.xml" $ requireAll_ "posts/*"
        >>> renderAtom feed

    -- The rest of the templates
    match "templates/*" $ compile templateCompiler


-------------------------------------------------------------------------------
-- Utility routing functions
-------------------------------------------------------------------------------
{- Routes a file to a folder path. For example, `routeToDir "foo.html"` routes
 - the file to "foo/index.html", or just simply "foo/".
 -}
routeToDir :: Routes
routeToDir = customRoute fileToDir
  where   -- fileToDir :: Identifier a -> FilePath
    fileToDir x = dropExtension (toFilePath x) </> "index.html"

{- Routes a file to one directory above. For example, `setRoot "/foo/bar.html"`
 - routes the file to "/bar.html"
 -}
setRoot :: Routes
setRoot = customRoute stripTopDir
  where
    stripTopDir = joinPath . tail . splitPath . toFilePath

{- Routes a file to the "blog/" directory -}
routePost :: Routes
routePost = customRoute toBlog
  where
    toBlog x = "blog" </> (toFilePath x)

--------------------------------------------------------------------------------
-- Miscellaneous Utilities
--------------------------------------------------------------------------------
{- The postTitle variable is used within each Disqus <script> insert to uniquely
 - identify each blog post. This function grabs the filename from the $path$
 - variable that is automatically set on each post and returns it.
 -}
setPostTitle :: Page a -> Page a
setPostTitle page = setField "postTitle" value page
  where
    value = unpack $ last $ split (=='/') $ pack path
    path = getField "path" page

--------------------------------------------------------------------------------
-- RSS Feed
--------------------------------------------------------------------------------
feed :: FeedConfiguration
feed = FeedConfiguration { feedTitle = "Daniel's Blog",
                           feedDescription = "Some ramblings",
                           feedAuthorName = "Daniel Ge",
                           feedRoot = "http://danielge.org/"
                         }
