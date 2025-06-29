-- | Copied from https://www.haskell.org/onlinereport/haskell2010/haskellch21.html#x29-25500021

module Data.Maybe(
     Maybe(Nothing, Just),
     isJust, isNothing,
     fromJust, fromMaybe, listToMaybe, maybeToList,
     catMaybes, mapMaybe,
     maybe
   ) where

maybe                  :: b -> (a -> b) -> Maybe a -> b
maybe n _ Nothing      =  n
maybe _ f (Just x)     =  f x

isJust                 :: Maybe a -> Bool
isJust (Just a)        =  True
isJust Nothing         =  False

isNothing              :: Maybe a -> Bool
isNothing              =  not . isJust

fromJust               :: Maybe a -> a
fromJust (Just a)      =  a
fromJust Nothing       =  error "Maybe.fromJust: Nothing"

fromMaybe              :: a -> Maybe a -> a
fromMaybe d Nothing    =  d
fromMaybe d (Just a)   =  a

maybeToList            :: Maybe a -> [a]
maybeToList Nothing    =  []
maybeToList (Just a)   =  [a]

listToMaybe            :: [a] -> Maybe a
listToMaybe []         =  Nothing
listToMaybe (a:_)      =  Just a

catMaybes              :: [Maybe a] -> [a]
catMaybes ms           =  [ m | Just m <- ms ]

mapMaybe               :: (a -> Maybe b) -> [a] -> [b]
mapMaybe f             =  catMaybes . map f
