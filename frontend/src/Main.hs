{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecursiveDo       #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}

module Main where

import Prelude hiding (head)
import qualified Data.Aeson as Aeson
import           Data.ByteString    as B (ByteString(..), readFile)
import           Data.ByteString.Lazy (toStrict, fromStrict)
import           Data.Functor.Sum
import           Data.List.NonEmpty
import           Data.Monoid        ((<>))
import qualified Data.Text          as T
import qualified Data.Text.Encoding          as T
import           Data.Text (Text)
import Data.Map.Strict (Map, singleton)
import           GHCJS.DOM.HTMLElement       (IsHTMLElement, focus)
import qualified GHCJS.DOM.Element as Element
import           Language.Javascript.JSaddle hiding (Command)
import           Reflex.Dom hiding (mainWidget, mainWidgetWithCss, mainWidgetWithHead)
import           Reflex.Dom.Core (mainWidget, mainWidgetWithCss, mainWidgetWithHead)
import           Reflex.Dynamic (constDyn)
import Reflex.Dom.Class ((=:))
import           Control.Monad.Fix (MonadFix)
import           Control.Monad      (void)
import           Text.URI

import Reflex.CodeMirror

import Control.Monad.IO.Class
import Data.FileEmbed (embedFile)
--------------------------------------------------------------------------------
import           Common.Controller.Message (Command(..))
import qualified Language.Javascript.JSaddle.Warp as Warp
import Reflex.CodeMirror

--------------------------------------------------------------------------------
--main :: IO ()
main = do
 -- css <- B.readFile "static/css/tachyons.min.css"
  Warp.run 12345 $ mainWidgetWithHead headWidget $ app "ws://localhost:9000"

style :: Text -> (forall x. Widget x ())
style = el "style" . text
script :: Text -> (forall x. Widget x ())
script = el "script" . text

headWidget :: (forall x. Widget x ()) 
--headWidget = el "script" $ pure ()
headWidget = do
  style $ T.decodeUtf8 tachyons
  style $ T.decodeUtf8 codemirrorcss
  script $ T.decodeUtf8 codemirrorjs
  where
    tachyons = $(embedFile "static/css/tachyons.min.css")
    codemirrorcss = $(embedFile "static/css/codemirror.css")
    codemirrorjs = $(embedFile "static/js/codemirror.js")
    
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

app ::  (MonadWidget t m,  Prerender js t m)
  => Text
  -> m ()
app r = do
  rec
    -- msgEvDyn <- widgetHold messageingWidget (messagingWidget <$ loggedInEv)
    msgEvDyn <- widgetHold messagingWidget (never)
    let
      msgSendEv = switch (current msgEvDyn)
      --msgRecEv = wsRespEv -- fmapMaybe id wsRespEv
      msgRecEv = fmapMaybe decodeOneMsg wsRespEv
      eRecRespTxt = fmap showMsg msgRecEv
      --loggedInEv = fmapMaybe loginEv msgRecEv
    wsRespEv <- do -- prerender (return (constDyn ())) do
      let sendEv = fmap (pure . encodeOneMsg . Execute) msgSendEv
      ws <- webSocket r $ def & webSocketConfig_send .~ sendEv

      return $ _webSocket_recv ws

    receivedMessages <- foldDyn (\m ms -> ms ++ [m]) [] eRecRespTxt
    codemirror $ def
    panes r
    void $ el "div" $ do
      el "p" $ text "Responses from the backend chat -server:"
      el "ul" $ simpleList receivedMessages (\m -> el "li" $ dynText m)
  blank
  where
    decodeOneMsg :: ByteString -> Maybe Command
    decodeOneMsg = Aeson.decode . fromStrict

    encodeOneMsg :: Command -> B.ByteString
    encodeOneMsg = toStrict . Aeson.encode

    showMsg :: Command -> Text
    showMsg = \case
      (Execute txt) -> txt
      (ListDir txt)  -> "Listdir: " <> txt
      Exit     -> "Exit"

-- | A button that can be enabled and disabled
buttonDynAttr :: (DomBuilder t m
            , PostBuild t m)
            => T.Text         -- ^ Label
            -> Dynamic t (Map Text Text) -- ^ enable or disable button
            -> m (Event t ())
buttonDynAttr label attrs = do
    --let attrs = ffor enabled $ \e -> monoidGuard (not e) $ "disabled" =: "disabled"
    (btn, _) <- elDynAttr' "button" attrs $ text label
    pure $ domEvent Click btn

panes :: 
      ( DomBuilder t m
      , DomBuilderSpace m ~ GhcjsDomSpace
      , PostBuild t m
      , MonadFix m
      , TriggerEvent t m
      , PerformEvent t m
      , MonadJSM m
      , MonadJSM (Performable m)
      , HasJSContext m
      , MonadHold t m
      ) 
      => Text 
      -> m ()
panes r = elClass "div" "flex flex-column pa3" $
  mdo
    let msgRecEv = fmapMaybe decodeOneMsg wsRespEv
        eRecRespTxt = fmap showMsg msgRecEv

    throttledContent <- debounce 1 $ updated content
    wsRespEv <- do
      let sendEv = fmap (pure . encodeOneMsg . Execute) throttledContent
      ws <- webSocket r $ def & webSocketConfig_send .~ (sendEv)

      return $ _webSocket_recv ws

    rendered <- holdDyn "" eRecRespTxt -- foldDyn (\m ms -> ms ++ [m]) [] eRecRespTxt
 
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
      (ListDir txt)  -> "Listdir: " <> txt
      Exit     -> "Exit"



editor :: (DomBuilder t m, DomBuilderSpace m ~ GhcjsDomSpace, PostBuild t m) => m (Dynamic t Text)
editor = do 
  text <- textAreaElement $ def & initialAttributes .~ ("class" =: "outline")
  pure $ _textAreaElement_value text

preview :: (PerformEvent t m
           , MonadJSM (Performable m)
           , DomBuilder t m
           , PostBuild t m
           , Element.IsElement (RawElement (DomBuilderSpace m))) => Dynamic t Text -> m ()
preview rendered = do
    (e,_) <- elDynAttr' "div" (constDyn ("class" =: "outline w-50 pa3")) 
      $ dynText rendered
    performEvent_ (writePreview e rendered)
    pure ()

--writePreview :: RawElement 
writePreview element rendered = (liftJSM . Element.setInnerHTML (_element_raw element)) <$> (updated rendered)

{-
  where
    loginEv = \case
      (S2Cwelcome _) -> Just () . 
      _ -> Nothing


    showMsg :: S2C -> Text
    showMsg = \case
      (S2Cbroadcast txt) -> txt
      (S2Cwelcome txt)  -> "Welcome! Users: " <> txt
      S2Cuserexists     -> "User already exists"
      S2Cnameproblem    -> "Name cannot contain punctuation or "

loginWidget
  :: ( DomBuilder t m
     , MonadFix m
     , PostBuild t m
     , PerformEvent t m
     , Prerender js t m
     )
  => m (Event t C2S)
loginWidget = el "div" $ do
  rec
    tn <- inputElement $ def
      & inputElementConfig_setValue .~ fmap (const "") eNewName
      & inputElementConfig_elementConfig . elementConfig_initialAttributes .~
        ("placeholder" =: "Enter Username")
    --doFocus tn
    bn <- button "Join"
    -- Clean the name a bit (todo, clean more):
    let eNewName = fmap T.strip
          $ tag (current $ value tn)
          $ leftmost [bn, keypress Enter tn]
  return $ C2Sjoin <$> eNewName
-}
messagingWidget
  :: ( DomBuilder t m
     , MonadFix m
     , PostBuild t m
     , PerformEvent t m
     , Prerender js t m
     , MonadJSM m
     , MonadJSM (Performable m)
     , IsHTMLElement (RawInputElement (DomBuilderSpace m))
     )
  => m (Event t Text)
messagingWidget = el "div" $ do
  rec
    t <- inputElement $ def & inputElementConfig_setValue .~ fmap (const "") newMessage
    b <- buttonDynAttr "Send" $ constDyn (singleton "class" "br4 ba pa2")
    --performEvent_ $ liftJSM (focus $ _inputElement_raw t) <$ newMessage
    setFocus t newMessage
    let newMessage = tag (current $ value t)
          $ leftmost [b, keypress Enter t]
  return $ newMessage

setFocus :: ( DomBuilder t m
     , PostBuild t m
     , PerformEvent t m
     , Prerender js t m
     , MonadJSM m
     , MonadJSM (Performable m)
     , IsHTMLElement (RawInputElement (DomBuilderSpace m))
     )
  => InputElement EventResult (DomBuilderSpace m) t
  -> Event t a
  -> m ()
setFocus el ev = performEvent_ $ liftJSM (focus $ _inputElement_raw el) <$ ev

doFocus
  :: ( DomBuilder t m
     , PostBuild t m
     , PerformEvent t m
     , Prerender js t m
     , MonadJSM m
     , MonadJSM (Performable m)
     , IsHTMLElement (RawInputElement (DomBuilderSpace m))
     )
  => InputElement EventResult (DomBuilderSpace m) t
  -> m ()
doFocus ie = prerender_ (pure ()) $ do
  pb <- getPostBuild
  let h = _inputElement_raw ie
  performEvent_ $ liftJSM (focus h) <$ pb
  pure ()