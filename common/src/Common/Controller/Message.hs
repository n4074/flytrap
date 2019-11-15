{-# LANGUAGE ScopedTypeVariables, DeriveGeneric #-}
module Common.Controller.Message where

import           Data.Aeson (ToJSON, FromJSON, toEncoding, parseJSON,
                            defaultOptions, Options,
                            genericToEncoding, genericParseJSON)
import qualified Data.Text as T
import           GHC.Generics (Generic)

options :: Options
options = defaultOptions -- { tagSingleConstructors = True }

data Command = Execute T.Text
             | ListDir T.Text
             | Exit
             deriving (Eq,Show, Generic)


instance ToJSON Command where toEncoding = genericToEncoding options
instance FromJSON Command where parseJSON = genericParseJSON options

data Response = Result T.Text
              deriving (Eq,Show, Generic)

instance ToJSON Response where toEncoding = genericToEncoding options
instance FromJSON Response where parseJSON = genericParseJSON options