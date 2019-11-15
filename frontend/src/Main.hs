{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecursiveDo       #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

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
import           Language.Javascript.JSaddle hiding (Command)
import           Reflex.Dom hiding (mainWidget, mainWidgetWithCss)
import           Reflex.Dom.Core (mainWidget, mainWidgetWithCss)
import           Reflex.Dynamic (constDyn)
import Reflex.Dom.Class ((=:))
import           Control.Monad.Fix (MonadFix)
import           Control.Monad      (void)
import           Text.URI

--------------------------------------------------------------------------------
import           Common.Controller.Message (Command(..))
--import           Common.Route
--------------------------------------------------------------------------------
main :: IO ()
main = do
  css <- B.readFile "css/tachyons.min.css"
  run $ mainWidgetWithCss css $ app "ws://localhost:9000"
-- TODO
--  - factor out the performEvents (see keyboard -example)
--  - factor out the message forming
--  - factor out the textInput-button combos
--  - add close connection button and associated message handling
app
  :: ( DomBuilder t m
     , MonadFix m
     , MonadHold t m
     , PostBuild t m
     , PerformEvent t m
     , TriggerEvent t m
     , Prerender js t m
     , MonadJSM m
     , MonadJSM (Performable m)
     , HasJSContext m
     , IsHTMLElement (RawInputElement (DomBuilderSpace m))
     )
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