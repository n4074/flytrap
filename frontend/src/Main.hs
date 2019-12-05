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
import           Data.List.NonEmpty hiding (take)
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
                                                , buttonDynAttr
                                                , rawInnerHtml
                                                )
import           Control.Monad.Fix              ( MonadFix )
import           Control.Monad                  ( void )
import           Text.URI

import           Reflex.CodeMirror

import           Control.Monad.IO.Class
import           Data.FileEmbed                 ( embedFile )

import           Common.Controller.Message      ( Command(..) )
import qualified Language.Javascript.JSaddle.Warp
                                               as Warp
import           Reflex.CodeMirror
import           Control.Lens                   ( (?~) )
import           Text.Pandoc                   as Pan
import           Data.Either.Extra              ( eitherToMaybe )

import           Notescape.Icon                 ( pencil, pencilButton )
--------------------------------------------------------------------------------
main = do
  Warp.run 9090 $ mainWidgetWithHead headWidget $ app "ws://localhost:9000"

data Note = Note {
  content :: Text,
  tags :: [Text],
  uuid :: Text
}

mockdata = [loremIpsum]

class HasContent a where
  html :: a -> Text

instance HasContent Note where
  html = (either (T.pack . show) id) . mdToHtml . content

headWidget :: (forall x . Widget x ())
headWidget = do
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
  --svgSprites

  elClass "div" "container pa2" $ do
    elClass "div" "searchbar" $ do
      searchbar

    elClass "div" "resultset" $ blank
    
    elClass "div" "main-gutter" $ blank
      
    elClass "div" "editor" $ note (constDyn "") 

    blank

               
loremIpsum :: Text
loremIpsum = "Lorem ipsum dolor sit amet, consectetuer adipiscing elit. Aenean commodo ligula eget dolor. Aenean massa strong. Cum sociis natoque penatibus et magnis dis parturient montes, nascetur ridiculus mus. Donec quam felis, ultricies nec, pellentesque eu, pretium quis, sem. Nulla consequat massa quis enim. Donec pede justo, fringilla vel, aliquet nec, vulputate eget, arcu. In enim justo, rhoncus ut, imperdiet a, venenatis vitae, justo. Nullam dictum felis eu pede link mollis pretium. Integer tincidunt. Cras dapibus. Vivamus elementum semper nisi. Aenean vulputate eleifend tellus. Aenean leo ligula, porttitor eu, consequat vitae, eleifend ac, enim. Aliquam lorem ante, dapibus in, viverra quis, feugiat a, tellus. Phasellus viverra nulla ut metus varius laoreet. Quisque rutrum. Aenean imperdiet. Etiam ultricies nisi vel augue. Curabitur ullamcorper ultricies nisi."

loremIpsumShort :: Text
loremIpsumShort = "Lorem ipsum dolor sit amet, consectetuer adipiscing ..."

svgSprites
  :: ( DomBuilder t m
     , PostBuild t m
     , PerformEvent t m
     , Element.IsElement (RawElement (DomBuilderSpace m))
     , MonadJSM (Performable m)
     )
  => m ()
svgSprites = do
  (e, _) <- el' "div" $ blank
  eDone  <- getPostBuild
  performEvent_
    $  (rawInnerHtml e $ T.decodeUtf8 $(embedFile "static/svg/sprite.svg"))
    <$ eDone
  blank


--sidebar = elClass "div" 

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
panes :: (MonadWidget t m, MonadHold t m) => Text -> m ()
panes r =
  elClass "div" "ui main container" $ mdo

    let msgRecEv    = fmapMaybe decodeOneMsg wsRespEv
        eRecRespTxt = fmap showMsg msgRecEv

    throttledContent <- debounce 1 $ updated content

    wsRespEv         <- do
      let sendEv = fmap (pure . encodeOneMsg . Execute) throttledContent
      ws <- webSocket r $ def & webSocketConfig_send .~ (sendEv)

      return $ _webSocket_recv ws

    rendered  <- holdDyn "" eRecRespTxt -- foldDyn (\m ms -> ms ++ [m]) [] eRecRespTxt

    --searchBar <- inputElement $ def & initialAttributes .~ ("class" =: "w-100 h2 ba br2 b--moon-gray")
    dSb <- searchbar

    content <- note rendered 
    note rendered
    note rendered
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

searchbar :: (DomBuilder t m) => m (Dynamic t Text)
searchbar = elClass "div" "ui fluid icon input" $ do
    input <- inputElement $ def & initialAttributes .~ ("class" =: "w-100 h2 ba br2 b--moon-gray" <> "placeholder" =: "Query")
    elClass "i" "inverted circular search link icon" $ blank
    pure $ _inputElement_value input

note :: (MonadWidget t m, MonadHold t m) => Dynamic t Text -> m (Dynamic t Text)
note rendered = elClass "div" "" $ do
  let editorAttr = ("class" =: "ui bottom attached segment" )
      noteAttr   = ("class" =: "ui bottom attached segment note" <> "id" =: "editor")
      mkHidden False = ("hidden" =: "")
      mkHidden True  = mempty

  editing <- elClass "div" "ui top attached menu" $ do
    editButton <- pencilButton 
    --editButton <- pencil $ constDyn ("class" =: "h1 w1 pa2 ml-auto hover-gray")
--    pencil $ constDyn ("class" =: "h2 w2 outline")
    toggle False editButton

  content <- editor (mappend editorAttr <$> mkHidden <$> editing)
  preview (mappend noteAttr <$> mkHidden . not <$> editing) content
  pure content

editor
--  :: (DomBuilder t m, DomBuilderSpace m ~ GhcjsDomSpace, PostBuild t m)
  :: (MonadWidget t m) => Dynamic t (Map Text Text) -> m (Dynamic t Text)
editor attr = do
  elDynAttr "div" attr $ do
    cm <-
      codemirror
      $  def
      &  configuration_theme
      ?~ T.pack "github-light"
      &  configuration_mode
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
preview
  :: (MonadWidget t m) => Dynamic t (Map Text Text) -> Dynamic t Text -> m ()
preview attr rendered = do
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

mdToHtml :: T.Text -> Either PandocError T.Text
mdToHtml input =
  runPure $ Pan.readMarkdown def input >>= Pan.writeHtml5String def
