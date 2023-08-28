{-# LANGUAGE OverloadedStrings #-}

{-# HLINT ignore #-}

import Control.Monad
import Data.Text (Text)
import Data.Text qualified as T

readText :: (Read a) => Text -> a
readText = read . T.unpack

main = do
  pricesLen <- readLn @Int
  prices <- replicateM pricesLen $ do
    line <- T.pack <$> getLine
    let [item, price] = T.splitOn " " line
    pure (item, readText @Int price)

  ordersLen <- readLn @Int
  orders <- replicateM ordersLen $ do
    name <- T.pack <$> getLine
    ordersLen <- readLn @Int
    orders <- replicateM ordersLen $ do
      line <- T.pack <$> getLine
      let [quantity, item] = T.splitOn " " line
      pure (readText @Int quantity, item)
    pure (name, orders)

  print orders
