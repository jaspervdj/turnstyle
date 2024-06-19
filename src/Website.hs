{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Data.Foldable  (for_)
import           Hakyll
import qualified System.Process as Process

main :: IO ()
main = hakyllWith config $ do
    match "spec/*.svg" $ do
        route idRoute
        compile copyFileCompiler
    match "examples/*-large.png" $ do
        route idRoute
        compile copyFileCompiler
    match "website/turnstyle.svg" $ do
        route $ constRoute "turnstyle.svg"
        compile copyFileCompiler
    match "website/preview.png" $ do
        route $ constRoute "preview.png"
        compile copyFileCompiler
    match "website/favicon.ico" $ do
        route $ constRoute "favicon.ico"
        compile copyFileCompiler
    let pages =
            [ ("README.md", "index.html")
            , ("spec/README.md", "spec/index.html")
            ]
    for_ pages $ \(src, dst) -> match src $ do
        route $ constRoute dst
        compile $ pandocCompiler >>=
            loadAndApplyTemplate "website/template.html" defaultContext >>=
            relativizeUrls
    match "website/style.css" $ do
        route $ constRoute "style.css"
        compile compressCssCompiler
    match "website/template.html" $ compile templateCompiler

config :: Configuration
config = defaultConfiguration
    { deploySite = \_  -> Process.rawSystem "rsync"
        [ "--checksum", "-ave", "ssh -p 2222"
        , "_site/", "jaspervdj@jaspervdj.be:jaspervdj.be/turnstyle/"
        ]
    }
