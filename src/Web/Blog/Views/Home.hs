{-# LANGUAGE OverloadedStrings #-}

module Web.Blog.Views.Home (viewHome) where

-- import Data.Maybe
-- import Data.Monoid
-- import Web.Blog.Render
import Control.Monad.Reader
import Text.Blaze.Html5                      ((!))
import Web.Blog.Models
import Web.Blog.Models.Util
import Web.Blog.SiteData
import Web.Blog.Types
import Web.Blog.Util                         (renderFriendlyTime, renderDatetimeTime)
import qualified Data.Text                   as T
import qualified Database.Persist.Postgresql as D
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Internal         as I

viewHome :: [(D.Entity Entry,(T.Text,[Tag]))] -> SiteRender H.Html
viewHome eList =
  return $ 
    H.section $ do

      H.header $ 

        H.section $ do
          H.h1 $ H.toHtml $ siteDataTitle siteData
          H.p
            "Welcome to my blog."

      forM_ eList $ \eData -> do
        let
          (D.Entity _ e,(u,ts)) = eData

        H.article $ do

          H.header $ do

            H.a ! A.href (I.textValue u) $
              H.h2 $ H.toHtml $ entryTitle e

            H.time
              ! A.datetime (I.textValue $ T.pack $ renderDatetimeTime $ entryPostedAt e)
              ! A.pubdate "" 
              ! A.class_ "pubdate"
              $ H.toHtml $ renderFriendlyTime $ entryPostedAt e

            H.ul ! A.class_ "article-tags" $
              forM_ ts $ \t ->
                H.li $ H.toHtml $ tagLabel t

          H.div $
            
            entryLedeHtml e



