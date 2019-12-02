{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE FlexibleContexts        #-}

module Notescape.Icon where

import           Data.Text                     as T
import           Reflex.Dom.Core
import           Reflex.Utils
import           Control.Lens
import Data.Map

icon svgText = do
  (e, _) <- el' "svg" $ blank
  eDone  <- getPostBuild
  performEvent_ $ (rawInnerHtml e svgText) <$ eDone

svgSprite :: (DomBuilder t m, PostBuild t m) => Dynamic t (Map Text Text) -> Text -> m (Event t ())
svgSprite attr id = do
  (e, _) <- el' "a" $ elDynAttrNS ns "svg" attr
    $ elDynAttrNS ns "use" (constDyn $ "href" =: id) $ blank
  pure $ domEvent Click e
 where ns = Just "http://www.w3.org/2000/svg"

pencil 
  :: ( DomBuilder t m
  , PostBuild t m) 
  => Dynamic t (Map Text Text)
  -> m (Event t ())
pencil attr = svgSprite (mappend ("viewport" =: "0 0 20 20" <> "fill" =: "currentColor") <$> attr) "#entypo-pencil" 

pencilButton :: (DomBuilder t m, PostBuild t m) => m (Event t ())
pencilButton = do
  (e, _) <- elClass' "a" "item" $ 
    elClass "i" "gamepad icon" $ blank
  pure $ domEvent Click e