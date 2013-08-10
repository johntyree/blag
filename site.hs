--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import Control.Applicative   ((<$>))
import Crypto.Hash.MD5
import Data.ByteString.Char8 (pack, unpack)
import Data.Char
import Data.Hex              (hex)
import Data.Monoid           ((<>))
import Hakyll
import Text.Pandoc


--------------------------------------------------------------------------------
main :: IO ()
main = hakyllWith config $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "favicon.ico" $ do
        route idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match (fromList ["about.md", "contact.md"]) $ do
        route   $ setExtension "html"
        compile $ pandocMathCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocMathCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx = listField "posts" postCtx (return posts)
                          <> constField "title" "Archives"
                          <> defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls


    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let indexCtx = listField "posts" postCtx (return posts)
                        <> constField "emailHash" emailHash

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" postCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler


config :: Configuration
config = defaultConfiguration
    { deployCommand = "rsync --checksum --delete -av _site/ \
                      \ tyree@john.bitsurge.net:john.bitsurge.net/blag"
    }

--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
       dateField "date" "%B %e, %Y"
    <> defaultContext


--------------------------------------------------------------------------------
pandocMathCompiler = pandocCompilerWith readers writers
  where
    readers = def { readerExtensions = pandocExtensions }
    writers = def {
              writerHTMLMathMethod = MathML (Just "")
            , writerHtml5 = True
            }

--------------------------------------------------------------------------------
-- gravatarLink :: String
emailHash = map toLower . unpack . hex . hash $ pack "johntyree@gmail.com"
