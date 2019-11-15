{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecursiveDo       #-}
{-# LANGUAGE FlexibleContexts #-}


module Main where

import           Data.Text.Encoding (encodeUtf8, decodeUtf8)
import           Reflex.Dom         hiding (mainWidget)
import           Reflex.Dom.Core    (mainWidget)
import           Language.Javascript.JSaddle (MonadJSM)
import Control.Monad.Fix (MonadFix)

main :: IO ()
main = run $ mainWidget app

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
     )
  => m ()
app = do
  header
  rec t <- inputElement $ def & inputElementConfig_setValue .~ fmap (const "") newMessage
      b <- button "Send"
      let newMessage = fmap ((:[]) . encodeUtf8)
            $ tag (current $ value t)
            $ leftmost [b, keypress Enter t]

  receivedMessages <- do -- prerender (return (constDyn [])) $ do
    ws <- webSocket "wss://echo.websocket.org" $ def
      & webSocketConfig_send .~ newMessage
    foldDyn (\m ms -> ms ++ [m]) [] $ _webSocket_recv ws

  el "p" $ text "Responses from the WebSocket.org echo service:"
  _ <- el "ul"
         $ simpleList receivedMessages
         $ \m -> el "li" $ dynText $ fmap decodeUtf8 m
  return ()

header :: DomBuilder t m => m ()
header = do
  el "strong" $ do
    text " WebSocket test page"
  el "p" $ do
    text "Send a message to the WebSocket.org: https://www.websocket.org/echo.html"
    text "'s websocket echo service:"