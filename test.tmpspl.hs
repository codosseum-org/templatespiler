import Control.Monad
main = do
    prices <- do
        count <- readLn @Int
        replicateM count $ do
            [item_tmp, price_tmp] <- words <$> getLine
            let item = item_tmp
            let price = read @Float price_tmp
            pure (item, price)


    orders <- do
        count <- readLn @Int
        replicateM count $ do
            name <- getLine
            order <- do
                count <- readLn @Int
                replicateM count $ do
                    [quantity_tmp, item_tmp] <- words <$> getLine
                    let quantity = read @Int quantity_tmp
                    let item = item_tmp
                    pure (quantity, item)


            pure (name, order)


    pure ()