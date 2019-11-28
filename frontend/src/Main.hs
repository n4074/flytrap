{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
--{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecursiveDo       #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}

module Main where

import           Prelude                 hiding ( head )
import qualified Data.Aeson                    as Aeson
import           Data.ByteString               as B
                                                ( ByteString(..)
                                                , readFile
                                                )
import           Data.ByteString.Lazy           ( toStrict
                                                , fromStrict
                                                )
import           Data.Functor.Sum
import           Data.List.NonEmpty
import           Data.Monoid                    ( (<>) )
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as T
import           Data.Text                      ( Text )
import           Data.Map.Strict                ( Map
                                                , singleton
                                                )
import           GHCJS.DOM.HTMLElement          ( IsHTMLElement
                                                , focus
                                                )
import qualified GHCJS.DOM.Element             as Element
import           Language.Javascript.JSaddle
                                         hiding ( Command )
import           Reflex.Dom              hiding ( mainWidget
                                                , mainWidgetWithCss
                                                , mainWidgetWithHead
                                                )
import           Reflex.Dom.Core                ( mainWidget
                                                , mainWidgetWithCss
                                                , mainWidgetWithHead
                                                )
import           Reflex.Dynamic                 ( constDyn )
import           Reflex.Dom.Class               ( (=:) )
import           Reflex.Utils                   ( script
                                                , style
                                                , setFocus
                                                )
import           Control.Monad.Fix              ( MonadFix )
import           Control.Monad                  ( void )
import           Text.URI

import           Reflex.CodeMirror

import           Control.Monad.IO.Class
import           Data.FileEmbed                 ( embedFile )
--------------------------------------------------------------------------------
import           Common.Controller.Message      ( Command(..) )
import qualified Language.Javascript.JSaddle.Warp
                                               as Warp
import           Reflex.CodeMirror
import           Control.Lens                   ( (?~) )

--------------------------------------------------------------------------------
main = do
  Warp.run 12345 $ mainWidgetWithHead headWidget $ app "ws://localhost:9000"

headWidget :: (forall x . Widget x ())
headWidget = do
  style $ T.decodeUtf8 tachyons
  style $ T.decodeUtf8 codemirrorcss
  script $ T.decodeUtf8 codemirrorjs
 where
  tachyons      = $(embedFile "static/css/tachyons.min.css")
  codemirrorcss = $(embedFile "static/css/codemirror.css")
  codemirrorjs  = $(embedFile "static/js/codemirror.js")

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

app :: (MonadWidget t m, Prerender js t m) => Text -> m ()
app r = do
  rec panes r
  blank

panes
--  :: ( DomBuilder t m
--     , DomBuilderSpace m ~ GhcjsDomSpace
--     , PostBuild t m
--     , MonadFix m
--     , TriggerEvent t m
--     , PerformEvent t m
--     , MonadJSM m
--     , MonadJSM (Performable m)
--     , HasJSContext m
--     , MonadHold t m
--     )
  :: (MonadWidget t m) => Text -> m ()
panes r = elClass "div" "flex flex-column pa3" $ mdo

  let msgRecEv    = fmapMaybe decodeOneMsg wsRespEv
      eRecRespTxt = fmap showMsg msgRecEv

  throttledContent <- debounce 1 $ updated content

  wsRespEv         <- do
    let sendEv = fmap (pure . encodeOneMsg . Execute) throttledContent
    ws <- webSocket r $ def & webSocketConfig_send .~ (sendEv)

    return $ _webSocket_recv ws

  rendered  <- holdDyn "" eRecRespTxt -- foldDyn (\m ms -> ms ++ [m]) [] eRecRespTxt

  searchBar <- inputElement $ def & initialAttributes .~ ("class" =: "w-100")

  dynText $ value searchBar

  content <- elClass "div" "flex" $ do
    content <- editor
    preview rendered
    pure content
  blank
 where
  decodeOneMsg :: ByteString -> Maybe Command
  decodeOneMsg = Aeson.decode . fromStrict

  encodeOneMsg :: Command -> B.ByteString
  encodeOneMsg = toStrict . Aeson.encode

  showMsg :: Command -> Text
  showMsg = \case
    (Execute txt) -> txt
    (ListDir txt) -> "Listdir: " <> txt
    Exit          -> "Exit"



editor
--  :: (DomBuilder t m, DomBuilderSpace m ~ GhcjsDomSpace, PostBuild t m)
  :: (MonadWidget t m) => m (Dynamic t Text)
editor = do
  elDynAttr "div" (constDyn ("class" =: "outline w-50 pa3")) $ do 
      cm <- codemirror $ def & configuration_theme ?~ T.pack "zenburn" 
      holdDyn "" cm

    --text <- textAreaElement $ def & initialAttributes .~ ("class" =: "outline")
    --pure $ _textAreaElement_value text

--  holdDyn "" cm


preview
  :: ( PerformEvent t m
     , MonadJSM (Performable m)
     , DomBuilder t m
     , PostBuild t m
     , Element.IsElement (RawElement (DomBuilderSpace m))
     )
  => Dynamic t Text
  -> m ()
preview rendered = do
  (e, _) <- elDynAttr' "div" (constDyn ("class" =: "outline w-50 pa3"))
    $ dynText rendered
  performEvent_ (writePreview e rendered)
  pure ()

writePreview element rendered =
  (liftJSM . Element.setInnerHTML (_element_raw element)) <$> (updated rendered)

