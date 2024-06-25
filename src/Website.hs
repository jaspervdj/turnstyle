{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Data.Foldable  (for_)
import Control.Applicative (empty)
import           Hakyll
import qualified System.Process as Process

main :: IO ()
main = hakyllWith config $ do
    match "spec/*.svg" $ do
        route idRoute
        compile copyFileCompiler
    match ("examples/*.png" .||. "examples/*.svg") $ do
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
            [ ("README.md", "index.html", Just "Turnstyle")
            , ("spec/README.md", "spec/index.html", Nothing)
            ]
    for_ pages $ \(src, dst, mbTitle) -> match src $ do
        let ctx = maybe mempty (constField "title") mbTitle <>
                functionField "active" (\args _ -> do
                    if args == [dst] then pure "true" else empty) <>
                defaultContext
        route $ constRoute dst
        compile $ pandocCompiler >>=
            loadAndApplyTemplate "website/template.html" ctx >>=
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
