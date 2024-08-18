{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main (main) where

import           Control.Applicative (empty)
import           Data.Foldable       (for_)
import           Hakyll
import qualified System.Process      as Process
import qualified Text.Pandoc         as Pandoc

data Page = Page
    { pageSrc            :: Pattern
    , pageDst            :: String
    , pageTitle          :: Maybe String
    , pageNumberSections :: Bool
    }

pages :: [Page]
pages =
    [ Page "README.md" "index.html"  (Just "Turnstyle") False
    , Page "spec/README.md" "spec/index.html" Nothing True
    , Page "website/playground.html" "playground/index.html" (Just "Turnstyle Playground") False
    ]

main :: IO ()
main = hakyllWith config $ do
    match "spec/*.svg" $ do
        route idRoute
        compile copyFileCompiler
    match "turnstyle.js" $ do
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
    for_ pages $ \Page {..} -> match pageSrc $ do
        let ctx = maybe mempty (constField "title") pageTitle <>
                functionField "active" (\args _ -> do
                    if args == [pageDst] then pure "true" else empty) <>
                defaultContext
            readerOpts = defaultHakyllReaderOptions
            writerOpts = defaultHakyllWriterOptions
                { Pandoc.writerNumberSections = pageNumberSections
                }
        route $ constRoute pageDst
        compile $ pandocCompilerWith readerOpts writerOpts >>=
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
