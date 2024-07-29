{-# LANGUAGE CApiFFI #-}

foreign import capi "Sleep" sleepBlock :: Int -> IO ()
foreign export capi "Sleep" sleepExport :: Int -> IO ()
sleepExport = undefined
