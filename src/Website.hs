{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Data.Foldable (for_)
import           Hakyll

main :: IO ()
main = hakyll $ do
    match "spec/*.svg" $ do
        route idRoute
        compile copyFileCompiler
    match "examples/*-large.png" $ do
        route idRoute
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
