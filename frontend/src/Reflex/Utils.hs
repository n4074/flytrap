{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Reflex.Utils where

import Data.Map
import Data.Text as T
import qualified GHCJS.DOM.Element as Element
import GHCJS.DOM.HTMLElement
  ( IsHTMLElement,
    focus,
  )
import Language.Javascript.JSaddle
import Reflex.Dom.Core

style :: Text -> (forall x. Widget x ())
style = el "style" . text

script :: Text -> (forall x. Widget x ())
script = el "script" . text

setFocus ::
  ( DomBuilder t m,
    PostBuild t m,
    PerformEvent t m,
    Prerender js t m,
    MonadJSM m,
    MonadJSM (Performable m),
    IsHTMLElement (RawInputElement (DomBuilderSpace m))
  ) =>
  InputElement EventResult (DomBuilderSpace m) t ->
  Event t a ->
  m ()
setFocus el ev = performEvent_ $ liftJSM (focus $ _inputElement_raw el) <$ ev

doFocus ::
  ( DomBuilder t m,
    PostBuild t m,
    PerformEvent t m,
    Prerender js t m,
    MonadJSM m,
    MonadJSM (Performable m),
    IsHTMLElement (RawInputElement (DomBuilderSpace m))
  ) =>
  InputElement EventResult (DomBuilderSpace m) t ->
  m ()
doFocus ie = prerender_ (pure ()) $ do
  pb <- getPostBuild
  let h = _inputElement_raw ie
  performEvent_ $ liftJSM (focus h) <$ pb
  pure ()

-- | A button that can be enabled and disabled
buttonDynAttr ::
  (DomBuilder t m, PostBuild t m) =>
  -- | Label
  T.Text ->
  -- | enable or disable button
  Dynamic t (Map Text Text) ->
  m (Event t ())
buttonDynAttr label attrs = do
  --let attrs = ffor enabled $ \e -> monoidGuard (not e) $ "disabled" =: "disabled"
  (btn, _) <- elDynAttr' "button" attrs $ text label
  pure $ domEvent Click btn

rawInnerHtml ::
  (MonadJSM m, Element.IsElement (RawElement d)) =>
  Element er d t ->
  Text ->
  m ()
rawInnerHtml e t = liftJSM $ Element.setInnerHTML (_element_raw e) t
