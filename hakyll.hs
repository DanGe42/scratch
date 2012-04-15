{-# LANGUAGE OverloadedStrings #-}
import Control.Arrow ((>>>), arr)
import Data.Monoid (mempty)

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

    -- Blogging engine
    match "posts/*" $ do
        route $ setRoot `composeRoutes` routePost `composeRoutes` (setExtension "html")
        compile $ pageCompiler
            >>> arr (setField "scripts" "")
            >>> applyTemplateCompiler "templates/post.html"
            >>> applyTemplateCompiler "templates/default.html"
            >>> relativizeUrlsCompiler

    match "blog.html" $ route routeToDir
    create "blog.html" $ constA mempty
        >>> arr (setField "title" "Recent Posts")
        >>> arr (setField "scripts" "")
        >>> setFieldPageList recentFirst "templates/post_item.html" "posts" "posts/*"
        >>> applyTemplateCompiler "templates/blog.html"
        >>> applyTemplateCompiler "templates/default.html"
        >>> relativizeUrlsCompiler

    -- Projects page
    match "templates/projects.html" $ do
        route $ setRoot `composeRoutes` routeToDir
        compile $ pageCompiler
            >>> applyTemplateCompiler "templates/default.html"
            >>> relativizeUrlsCompiler

    -- The rest of the templates
    match "templates/*" $ compile templateCompiler


-------------------------------------------------------------------------------
-- Utility routing functions
-------------------------------------------------------------------------------
routeToDir :: Routes
routeToDir = customRoute fileToDir
  where   -- fileToDir :: Identifier a -> FilePath
    fileToDir x = dropExtension (toFilePath x) </> "index.html"

setRoot :: Routes
setRoot = customRoute stripTopDir
  where
    stripTopDir = joinPath . tail . splitPath . toFilePath

routePost :: Routes
routePost = customRoute toBlog
  where
    toBlog x = "blog" </> (toFilePath x)
