{-# LANGUAGE InterruptibleFFI #-}

foreign import ccall interruptible "Sleep" sleepBlock :: Int -> IO ()
