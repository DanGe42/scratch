{-# LANGUAGE OverloadedStrings #-}
import Control.Arrow ((>>>), arr)
import Data.Monoid (mempty)

import System.FilePath

import Hakyll

main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler
    
    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "templates/projects.html" $ do
        route $ composeRoutes setRoot routeToDir
        compile $ pageCompiler
            >>> applyTemplateCompiler "templates/default.html"
            >>> relativizeUrlsCompiler

    match "home.html" $ do
        route $ customRoute (\_ -> toFilePath "index.html")
        compile $ pageCompiler
            >>> applyTemplateCompiler "templates/default.html"
            >>> relativizeUrlsCompiler

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

{-insertCSS :: Maybe String -> String
insertCSS Nothing = []
insertCSS (Just path) = "<link rel=\"stylesheet\" type=\"text/css\" href=\"" ++ path ++ "\">"

insertJS :: Maybe String -> String
insertJS Nothing = []
insertJS (Just path) = "<script type=\"text/javascript\" src=\"" ++ path ++ "\"></script>" -}
