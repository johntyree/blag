--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import Crypto.Hash.MD5
import Data.ByteString.Char8 (pack, unpack)
import Data.Char             (toLower)
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

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match (fromList ["about.rst", "contact.markdown"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
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
            let indexCtx = listField "posts" postCtx (return (take 3 posts))
                        <> constField "title" "Home"
                        <> constField "emailHash" emailHash
                        <> defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler


--------------------------------------------------------------------------------

pandocMathCompiler = pandocCompilerWith readers writers
  where
    readers = def { readerExtensions = pandocExtensions }
    writers = def {
              writerHTMLMathMethod = MathML (Just "")
            , writerHtml5 = True
            }

postCtx :: Context String
postCtx = dateField "date" "%B %e, %Y"
       <> defaultContext

config :: Configuration
config = defaultConfiguration
    { deployCommand = "rsync --checksum --delete -av _site/ \
                      \ tyree@john.bitsurge.net:john.bitsurge.net/blag"
    }

emailHash = map toLower . unpack . hex . hash $ pack "johntyree@gmail.com"
