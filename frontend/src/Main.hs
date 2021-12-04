{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

--{-# LANGUAGE RankNTypes        #-}

module Main where

--                                                , mainWidgetWithHead

--                                                , mainWidgetWithHead

import Common.Controller.Message (Command (..))
--import qualified Language.Javascript.JSaddle.Warp
--                                               as Warp

import Control.Lens ((?~))
--import           Text.Pandoc                   as Pan

import Control.Lens ((^.))
import Control.Monad (void)
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class
import qualified Data.Aeson as Aeson
import Data.ByteString as B
  ( ByteString (..),
    readFile,
  )
import Data.ByteString.Lazy
  ( fromStrict,
    toStrict,
  )
import Data.Either.Extra (eitherToMaybe)
import Data.FileEmbed (embedFile)
import Data.Functor.Sum
import Data.List.NonEmpty hiding (take)
import Data.Map.Strict
  ( Map,
    singleton,
  )
import Data.Monoid ((<>))
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import qualified GHCJS.DOM.Element as Element
import GHCJS.DOM.HTMLElement
  ( IsHTMLElement,
    focus,
  )
import Language.Javascript.JSaddle hiding
  ( Command,
  )
import Notescape.Icon (pencil, pencilButton)
import Reflex.CodeMirror
import Reflex.CodeMirror
import Reflex.Dom hiding
  ( mainWidget,
    mainWidgetWithCss,
  )
import Reflex.Dom.Class ((=:))
import Reflex.Dom.Core
  ( mainWidget,
    mainWidgetWithCss,
  )
import Reflex.Dynamic (constDyn)
import Reflex.Utils
  ( buttonDynAttr,
    rawInnerHtml,
    script,
    setFocus,
    style,
  )
import qualified Split as Split
import Text.URI
import Prelude hiding (head)

--------------------------------------------------------------------------------
main = do
  mainWidgetWithHead headWidget $ app "ws://localhost:9000"

data Note
  = Note
      { content :: Text,
        tags :: [Text],
        uuid :: Text
      }

class HasContent a where
  html :: a -> Text

instance HasContent Note where
  html = (either (T.pack . show) id) . mdToHtml . content

headWidget :: (forall x. Widget x ())
headWidget = do
  elDynAttr "meta" (constDyn ("name" =: "viewport" <> "content" =: "width=device-width, initial-scale=1")) $ blank -- <meta name="viewport" content="width=device-width, initial-scale=1">
  style $ T.decodeUtf8 $(embedFile "static/css/codemirror.css")
  style $ T.decodeUtf8 $(embedFile "static/css/zenburn.css")
  style $ T.decodeUtf8 $(embedFile "static/css/tachyons.min.css")
  --  style $ T.decodeUtf8 $(embedFile "static/css/pandoc.css")
  style $ T.decodeUtf8 $(embedFile "static/css/semantic.min.css")
  style $ T.decodeUtf8 $(embedFile "static/css/layout.css")
  script $ T.decodeUtf8 $(embedFile "static/js/codemirror.js")
  script $ T.decodeUtf8 $(embedFile "static/js/muya.min.js")
  script $ T.decodeUtf8 $(embedFile "static/js/split-grid.js")
  --script $ T.decodeUtf8 $(embedFile "static/js/markdown.js")
  script $ T.decodeUtf8 $(embedFile "static/js/css.js")
  style $ T.decodeUtf8 $(embedFile "static/css/muya.min.css")
  style $ T.decodeUtf8 $(embedFile "static/css/codemirror-github-light.css")

--app
--  :: ( DomBuilder t m
--     , DomBuilderSpace m ~ GhcjsDomSpace
--     , MonadFix m
--     , MonadHold t m
--     , PostBuild t m
--     , PerformEvent t m
--     , TriggerEvent t m
--     , Prerender js t m
--     , MonadJSM m
--     , MonadJSM (Performable m)
--     , HasJSContext m
--     , IsHTMLElement (RawInputElement (DomBuilderSpace m))
--     )

app :: (MonadWidget t m, Prerender js t m, MonadHold t m) => Text -> m ()
app r = do
  elClass "div" "container pa2" $ do
    elClass "div" "searchbar" $ searchbar
    elClass "div" "resultset" $ blank
    elGutter <- _element_raw . fst <$> (elAttr' "div" ("class" =: "gutter") $ blank)
    elClass "div" "editor" $ editor
    ePostBuild <- getPostBuild
    performEvent_ $ liftJSM (Split.split 2 elGutter) <$ ePostBuild
  blank

searchbar :: (DomBuilder t m) => m (Dynamic t Text)
searchbar = elClass "div" "ui fluid icon input" $ do
  input <- inputElement $ def & initialAttributes .~ ("class" =: "w-100 h2 ba br2 b--moon-gray" <> "placeholder" =: "Query")
  elClass "i" "inverted circular search link icon" $ blank
  pure $ _inputElement_value input

editor :: (MonadWidget t m, MonadHold t m) => m (Dynamic t Text)
editor = elClass "div" "" $ do
  let editorAttr = ("class" =: "ui bottom attached segment")
      noteAttr = ("class" =: "ui bottom attached segment note" <> "id" =: "editor")
      mkHidden False = ("hidden" =: "")
      mkHidden True = mempty
  editing <- elClass "div" "ui top attached menu" $ do
    editButton <- pencilButton
    --editButton <- pencil $ constDyn ("class" =: "h1 w1 pa2 ml-auto hover-gray")
    --    pencil $ constDyn ("class" =: "h2 w2 outline")
    toggle False editButton
  content <- sourceView (mappend editorAttr <$> mkHidden <$> editing)
  liveView (mappend noteAttr <$> mkHidden . not <$> editing) content
  pure content

sourceView ::
  --  :: (DomBuilder t m, DomBuilderSpace m ~ GhcjsDomSpace, PostBuild t m)
  (MonadWidget t m) => Dynamic t (Map Text Text) -> m (Dynamic t Text)
sourceView attr = do
  elDynAttr "div" attr $ do
    cm <-
      codemirror $
        def
          & configuration_theme
          ?~ T.pack "github-light"
          & configuration_mode
          ?~ T.pack "css"
    holdDyn "" cm

--text <- textAreaElement $ def & initialAttributes .~ ("class" =: "outline")
--pure $ _textAreaElement_value text

--  holdDyn "" cm

--preview
--  :: ( PerformEvent t m
--     , MonadJSM (Performable m)
--     , DomBuilder t m
--     , PostBuild t m
--     , Element.IsElement (RawElement (DomBuilderSpace m))
--     )
--  => Dynamic t (Map Text Text)
--  -> Dynamic t Text
--  -> m ()
liveView ::
  (MonadWidget t m) => Dynamic t (Map Text Text) -> Dynamic t Text -> m ()
liveView attr rendered = do
  (e, _) <- elDynAttr' "div" attr $ dynText rendered
  performEvent_ (writePreview e rendered)
  pure ()

--writePreview
--  :: Element EventResult (DomBuilderSpace m) t
--  -> Dynamic t Text
--  -> Event t (Performable m ())
writePreview element rendered =
  rawInnerHtml element <$> (fmapMaybe ignoreErrors $ updated rendered)

ignoreErrors = (eitherToMaybe . mdToHtml)

--mdToHtml :: T.Text -> Either PandocError T.Text
--mdToHtml input =
--  runPure $ Pan.readMarkdown def input >>= Pan.writeHtml5String def
mdToHtml :: T.Text -> Either T.Text T.Text
mdToHtml t = Right t
