{-# LANGUAGE TemplateHaskell #-}

--{-# LANGUAGE OverloadedStrings #-}

module Split where

import Control.Lens ((^.))
import Control.Monad.IO.Class
import Data.FileEmbed (embedFile)
import Data.Text as T
import qualified Data.Text.Encoding as T
import Language.Javascript.JSaddle
import Reflex.Dom

script = T.decodeUtf8 $(embedFile "static/js/split-grid.js")

split :: (ToJSVal el) => Int -> el -> JSM ()
split gutterTrack gutterElement = do
  rowGutters <- obj
  gutterElement' <- toJSVal gutterElement
  rowGutters ^. jss "track" gutterTrack
  rowGutters ^. jss "element" gutterElement'
  options <- obj
  options ^. jss "rowGutters" [rowGutters]
  split <- jsgf "Split" [options]
  w <- jsg ("console" :: String)
  w ^. js1 ("log" :: String) gutterElement
  w ^. js1 ("log" :: String) rowGutters
  w ^. js1 ("log" :: String) options
  w ^. js1 ("log" :: String) split
  jsg1 ("alert" :: String) (1 :: Int)
  pure ()
