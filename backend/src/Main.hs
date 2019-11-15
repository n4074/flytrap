{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Control.Concurrent (forkIO, threadDelay)
import           Control.Concurrent.STM
import           Control.Monad.Trans (liftIO)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import           Data.ByteString.Lazy (fromStrict, toStrict)
import           Data.Functor (void)
import qualified Data.Map as Map
import           Network.WebSockets
import           Reflex
import           Reflex.Host.Basic
import           Reflex.Backend.WebSocket

import           Data.Aeson (decode, encode)
import           Common.Controller.Message

import Data.Acid

guest ::
  WsManager PendingConnection ->
  IO ()
guest wsm = basicHostForever $ mdo
  eIn <- wsData wsm
  dCount :: Dynamic t Int <- count eIn

  dMap <- foldDyn ($) Map.empty .
          mergeWith (.) $ [
            Map.insert <$> current dCount <@> eIn
          , flip (foldr Map.delete) <$> eRemoves
          ]

  dmeRemove <- list dMap $ \dv -> mdo
    wsd <- sample . current $ dv

    let
      eRx = fmapMaybe ((decode . fromStrict) :: B.ByteString -> Maybe Command) (_wsReceive ws)
      f c r = encode r -- $ B.append (BC.pack . show $ c) r
      --eTx = (\c r -> pure $ f c r) <$> current dCount <@> eRx
      eTx = pure . toStrict . encode <$> eRx
      eClose = (\(_, w, b) -> (w, b)) <$> _wsClosed ws
      wsc = WebSocketConfig waited eClose

    ws <- accept wsd wsc never

    performEvent_ $ (liftIO (putStrLn "Open")) <$ _wsOpen ws
    performEvent_ $ (liftIO . putStrLn . ("Rx: " ++) . show) <$> _wsReceive ws
    performEvent_ $ (liftIO . putStrLn . ("ERx: " ++) . show) <$> eRx
    performEvent_ $ (liftIO . putStrLn . ("Closed: " ++). show) <$> _wsClosed ws
    performEvent_ $ (liftIO . putStrLn . ("Tx: " ++) . show) <$> eTx
    performEvent_ $ (liftIO . putStrLn . ("Close: " ++). show) <$> eClose
    
    let 
      --action :: (a -> IO ()) -> Performable (BasicGuest t m) ()
      action s fire = liftIO $ do
        putStrLn $ "Waiting for " ++ (show s)
        forkIO $ do
          threadDelay 10000000
          putStrLn $ "Waited for " ++ (show s)
          fire s
          threadDelay 1000000
          putStrLn $ "Waited for " ++ (show s) ++ " again"
          fire s
          threadDelay 1000000
          putStrLn $ "Waited for " ++ (show s) ++ " again"
          fire s
        pure ()

    waited <- performEventAsync $ ffor (eTx) $ action

    pure $ _wsClosed ws

  let
    eRemoves = fmap Map.keys . switch . current . fmap mergeMap $ dmeRemove

  pure ()

--delayedAction event = liftIO . threadDelay 100000 <$ event

main :: IO ()
main = do
  wsm <- atomically $ mkWsManager 10
  void . forkIO $ guest wsm
  runServer "127.0.0.1" 9000 (handleConnection wsm)