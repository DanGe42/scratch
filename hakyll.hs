{-# LANGUAGE OverloadedStrings #-}
import Control.Arrow ((>>>), arr)
import Data.Monoid (mempty)

import Hakyll

main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler
    
    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "templates/*" $ compile templateCompiler

    {-match "index.html" $ route idRoute
    create "index.html" $ constA mempty
        >>> arr (setField "title" "Home")
        >>> arr (setField "css" $ insertCSS Nothing)
        >>> arr (setField "scripts" $ insertJS Nothing)
        >>> applyTemplateCompiler "templates/index.html"
        >>> applyTemplateCompiler "templates/default.html"
        >>> relativizeUrlsCompiler-}
    match (list ["index.html"]) $ do
        route   $ setExtension "html"
        compile $ pageCompiler
            >>> applyTemplateCompiler "templates/default.html"
            >>> relativizeUrlsCompiler

    match "projects.html" $ route idRoute
    create "/projects/index.html" $ constA mempty
        >>> arr (setField "title" "Projects")
        >>> applyTemplateCompiler "templates/default.html"
        >>> applyTemplateCompiler "templates/projects.html"
        >>> relativizeUrlsCompiler

insertCSS :: Maybe String -> String
insertCSS Nothing = []
insertCSS (Just path) = "<link rel=\"stylesheet\" type=\"text/css\" href=\"" ++ path ++ "\">"

insertJS :: Maybe String -> String
insertJS Nothing = []
insertJS (Just path) = "<script type=\"text/javascript\" src=\"" ++ path ++ "\"></script>"
