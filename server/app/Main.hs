module Main where

import Import

import Data.Aeson (genericParseJSON, genericToJSON)
import Network.HTTP.Types.Header (hContentType)
import Network.HTTP.Types.Method (methodGet, methodPost)
import Network.HTTP.Types.Status
import Network.Wai
import Network.Wai.Handler.Warp

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.List as List

--------------------------------------------------------------------------------
-- Todoop server

main :: IO ()
main = do
  itemsRef <- newIORef []
  run 8080 (app itemsRef)

app :: IORef [Item] -> Application
app itemsRef req resp = do
  body <- strictRequestBody req

  case (req, body) of
    --
    -- GET /cycle
    --
    -- Return the item at the head of the cycle, or 412 if there are no items.
    --
    HttpGet ["cycle"] ->
      readIORef itemsRef >>= \case
        [] -> resp empty412
        item:_ -> resp (json200 item)

    --
    -- POST /cycle/new
    --
    -- Put a new item at the end of the cycle.
    --
    HttpPostJson ["cycle", "new"] item -> do
      atomicModifyIORef' itemsRef (\items -> (snoc items item, ()))
      resp empty201

    --
    -- POST /cycle/do
    --
    -- Cycle the cycle, or 412 if there are no items.
    --
    HttpPost ["cycle", "do"] -> do
      success <-
        atomicModifyIORef' itemsRef
          (\case
            [] -> ([], False)
            item:items -> (snoc items item, True))

      if success
        then resp empty412
        else resp empty202

    _ -> resp empty404

--------------------------------------------------------------------------------
-- Types

data Item = Item
  { itemName :: Text
  , itemFrequency :: Double
  } deriving (Generic, Show)

itemFieldLabelModifier :: String -> String
itemFieldLabelModifier = over _head toLower . drop 4

instance FromJSON Item where
  parseJSON = genericParseJSON Aeson.defaultOptions
    { Aeson.fieldLabelModifier = itemFieldLabelModifier }

instance ToJSON Item where
  toJSON = genericToJSON Aeson.defaultOptions
    { Aeson.fieldLabelModifier = itemFieldLabelModifier }

--------------------------------------------------------------------------------
-- Request pattern helpers

pattern HttpGet :: [Text] -> (Request, LByteString)
pattern HttpGet path <- (matchGet . fst -> Just path)

pattern HttpPost :: [Text] -> (Request, LByteString)
pattern HttpPost path <- (matchPost . fst -> Just path)

pattern HttpPostJson :: FromJSON a => [Text] -> a -> (Request, LByteString)
pattern HttpPostJson path x <- (matchPostJson -> Just (path, x))

-- Is this request a GET? If so, return the request path.
matchGet :: Request -> Maybe [Text]
matchGet req = do
  guard (requestMethod req == methodGet)
  pure (pathInfo req)

-- Is this request a POST? If so, return the request path.
matchPost :: Request -> Maybe [Text]
matchPost req = do
  guard (requestMethod req == methodPost)
  pure (pathInfo req)

-- Is this request a POST with JSON in the body? If so, return the request path
-- and the parsed JSON.
matchPostJson :: FromJSON a => (Request, LByteString) -> Maybe ([Text], a)
matchPostJson (req, body) = do
  guard (requestMethod req == methodPost)
  "application/json" <- List.lookup hContentType (requestHeaders req)
  x <- Aeson.decode body
  pure (pathInfo req, x)

--------------------------------------------------------------------------------
-- Response helpers

empty201 :: Response
empty201 = responseLBS status201 [] ""

empty202 :: Response
empty202 = responseLBS status202 [] ""

empty404 :: Response
empty404 = responseLBS status404 [] ""

empty412 :: Response
empty412 = responseLBS status412 [] ""

json200 :: ToJSON a => a -> Response
json200 = responseLBS status200 [] . Aeson.encode
