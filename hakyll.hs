{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Arrow (arr, (>>>))
import Data.Monoid (mempty)
import Hakyll

main :: IO ()
main = hakyllWith config $ do
    match "images/*" $ do
        route idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pageCompiler
            >>> arr (renderDateField "date" "%Y/%m/%d" "Date unknown")
            >>> renderTagsField "prettytags" (fromCapture "tags/*")
            >>> applyTemplateCompiler "templates/post.hamlet"
            >>> applyTemplateCompiler "templates/default.hamlet"
            >>> relativizeUrlsCompiler

    match "posts.html" $ route idRoute
    create "posts.html" $ constA mempty
        >>> arr (setField "title" "Posts")
        >>> requireA "tags" (setFieldA "tags" (renderTagList'))
        >>> setFieldPageList recentFirst
                "templates/postitem.hamlet" "posts" "posts/*"
        >>> applyTemplateCompiler "templates/posts.hamlet"
        >>> applyTemplateCompiler "templates/default.hamlet"
        >>> relativizeUrlsCompiler

    match "index.html" $ route idRoute
    create "index.html" $ constA mempty
        >>> arr (setField "title" "Home")
        >>> requireA "tags" (setFieldA "tags" (renderTagList'))
        >>> setFieldPageList (take 3 . recentFirst)
                "templates/postitem.hamlet" "posts" "posts/*"
        >>> applyTemplateCompiler "templates/index.hamlet"
        >>> applyTemplateCompiler "templates/default.hamlet"
        >>> relativizeUrlsCompiler

    create "tags" $
        requireAll "posts/*" (\_ ps -> readTags ps :: Tags String)

    match "tags/*" $ route $ setExtension ".html"
    metaCompile $ require_ "tags"
        >>> arr tagsMap
        >>> arr (map (\(t, p) -> (tagIdentifier t, makeTagList t p)))

    match "templates/*" $ compile templateCompiler

renderTagList' :: Compiler (Tags String) String
renderTagList' = renderTagList tagIdentifier

tagIdentifier :: String -> Identifier (Page String)
tagIdentifier = fromCapture "tags/*"

makeTagList :: String -> [Page String] -> Compiler () (Page String)
makeTagList tag posts =
    constA posts
        >>> pageListCompiler recentFirst "templates/postitem.hamlet"
        >>> arr (copyBodyToField "posts" . fromBody)
        >>> arr (setField "title" ("Posts tagged " ++ tag))
        >>> applyTemplateCompiler "templates/posts.hamlet"
        >>> applyTemplateCompiler "templates/default.hamlet"
        >>> relativizeUrlsCompiler


config :: HakyllConfiguration
config = defaultHakyllConfiguration { deployCommand = deploy }
    where deploy = "cp -r _site/* /var/www/blog/ && runhaskell hakyll.hs clean"
