module PreludeText (
    ReadS, ShowS,
    Read(readsPrec, readList),
    Show(showsPrec, show, showList),
    reads, shows, read, lex,
    showChar, showString, readParen, showParen ) where

-- The instances of Read and Show for
--      Bool, Maybe, Either, Ordering
-- are done via "deriving" clauses in Prelude.hs

import Prelude ()

{-
import Data.Char(isSpace, isAlpha, isDigit, isAlphaNum,
                 showLitChar, readLitChar, lexLitChar)

import Numeric(showSigned, showInt, readSigned, readDec, showFloat,
               readFloat, lexDigits)
-}

type  ReadS a  = String -> [(a,String)]
type  ShowS    = String -> String

class  Read a  where
    readsPrec        :: Int -> ReadS a
    readList         :: ReadS [a]

        -- Minimal complete definition:
        --      readsPrec
    readList         = readParen False (\r -> [pr | ("[",s)  <- lex r,
                                                    pr       <- readl s])
                       where readl  s = [([],t)   | ("]",t)  <- lex s] ++
                                        [(x:xs,u) | (x,t)    <- reads s,
                                                    (xs,u)   <- readl' t]
                             readl' s = [([],t)   | ("]",t)  <- lex s] ++
                                        [(x:xs,v) | (",",t)  <- lex s,
                                                    (x,u)    <- reads t,
                                                    (xs,v)   <- readl' u]

class  Show a  where
    showsPrec        :: Int -> a -> ShowS
    show             :: a -> String 
    showList         :: [a] -> ShowS

        -- Mimimal complete definition:
        --      show or showsPrec
    showsPrec _ x s   = show x ++ s

    show x            = showsPrec 0 x ""

    showList []       = showString "[]"
    showList (x:xs)   = showChar '[' . shows x . showl xs
                        where showl []     = showChar ']'
                              showl (x:xs) = showChar ',' . shows x .
                                             showl xs

reads            :: (Read a) => ReadS a
reads            =  readsPrec 0

shows            :: (Show a) => a -> ShowS
shows            =  showsPrec 0

read             :: (Read a) => String -> a
read s           =  case [x | (x,t) <- reads s, ("","") <- lex t] of
                         [x] -> x
                         []  -> error "Prelude.read: no parse"
                         _   -> error "Prelude.read: ambiguous parse"

showChar         :: Char -> ShowS
showChar         =  (:)

showString       :: String -> ShowS
showString       =  (++)

showParen        :: Bool -> ShowS -> ShowS
showParen b p    =  if b then showChar '(' . p . showChar ')' else p

readParen        :: Bool -> ReadS a -> ReadS a
readParen b g    =  if b then mandatory else optional
                    where optional r  = g r ++ mandatory r
                          mandatory r = [(x,u) | ("(",s) <- lex r,
                                                 (x,t)   <- optional s,
                                                 (")",u) <- lex t    ]

-- This lexer is not completely faithful to the Haskell lexical syntax.
-- Current limitations:
--    Qualified names are not handled properly
--    Octal and hexidecimal numerics are not recognized as a single token
--    Comments are not treated properly

lex              :: ReadS String
lex ""           =  [("","")]
lex (c:s)
   | isSpace c   =  lex (dropWhile isSpace s)
lex ('\'':s)     =  [('\'':ch++"'", t) | (ch,'\'':t)  <- lexLitChar s,
                                         ch /= "'" ]
lex ('"':s)      =  [('"':str, t)      | (str,t) <- lexString s]
                    where
                    lexString ('"':s) = [("\"",s)]
                    lexString s = [(ch++str, u)
                                         | (ch,t)  <- lexStrItem s,
                                           (str,u) <- lexString t  ]

                    lexStrItem ('\\':'&':s) =  [("\\&",s)]
                    lexStrItem ('\\':c:s) | isSpace c
                                           =  [("\\&",t) | 
                                               '\\':t <-
                                                   [dropWhile isSpace s]]
                    lexStrItem s           =  lexLitChar s

lex (c:s) | isSingle c = [([c],s)]
          | isSym c    = [(c:sym,t)       | (sym,t) <- [span isSym s]]
          | isAlpha c  = [(c:nam,t)       | (nam,t) <- [span isIdChar s]]
          | isDigit c  = [(c:ds++fe,t)    | (ds,s)  <- [span isDigit s],
                                            (fe,t)  <- lexFracExp s     ]
          | otherwise  = []    -- bad character
             where
              isSingle c =  c `elem` ",;()[]{}_`"
              isSym c    =  c `elem` "!@#$%&*+./<=>?\\^|:-~"
              isIdChar c =  isAlphaNum c || c `elem` "_'"

              lexFracExp ('.':c:cs) | isDigit c
                            = [('.':ds++e,u) | (ds,t) <- lexDigits (c:cs),
                                               (e,u)  <- lexExp t]
              lexFracExp s  = lexExp s

              lexExp (e:s) | e `elem` "eE"
                       = [(e:c:ds,u) | (c:t)  <- [s], c `elem` "+-",
                                                 (ds,u) <- lexDigits t] ++
                         [(e:ds,t)   | (ds,t) <- lexDigits s]
              lexExp s = [("",s)]

instance  Show Int  where
    showsPrec n = showsPrec n . toInteger
        -- Converting to Integer avoids
        -- possible difficulty with minInt

instance  Read Int  where
  readsPrec p r = [(fromInteger i, t) | (i,t) <- readsPrec p r]
        -- Reading at the Integer type avoids
        -- possible difficulty with minInt

instance  Show Integer  where
    showsPrec           = showSigned showInt

instance  Read Integer  where
    readsPrec p         = readSigned readDec

instance  Show Float  where 
    showsPrec p         = showFloat
           
instance  Read Float  where
    readsPrec p         = readSigned readFloat

instance  Show Double  where
    showsPrec p         = showFloat

instance  Read Double  where
    readsPrec p         = readSigned readFloat

instance  Show ()  where
    showsPrec p () = showString "()"

instance Read () where
    readsPrec p    = readParen False
                            (\r -> [((),t) | ("(",s) <- lex r,
                                             (")",t) <- lex s ] )
instance  Show Char  where
    showsPrec p '\'' = showString "'\\''"
    showsPrec p c    = showChar '\'' . showLitChar c . showChar '\''

    showList cs = showChar '"' . showl cs
                 where showl ""       = showChar '"'
                       showl ('"':cs) = showString "\\\"" . showl cs
                       showl (c:cs)   = showLitChar c . showl cs

instance  Read Char  where
    readsPrec p      = readParen False
                            (\r -> [(c,t) | ('\'':s,t)<- lex r,
                                            (c,"\'")  <- readLitChar s])

    readList = readParen False (\r -> [(l,t) | ('"':s, t) <- lex r,
                                               (l,_)      <- readl s ])
        where readl ('"':s)      = [("",s)]
              readl ('\\':'&':s) = readl s
              readl s            = [(c:cs,u) | (c ,t) <- readLitChar s,
                                               (cs,u) <- readl t       ]

instance  (Show a) => Show [a]  where
    showsPrec p      = showList

instance  (Read a) => Read [a]  where
    readsPrec p      = readList

-- Tuples

instance  (Show a, Show b) => Show (a,b)  where
    showsPrec p (x,y) = showChar '(' . shows x . showChar ',' .
                                       shows y . showChar ')'

instance  (Read a, Read b) => Read (a,b)  where
    readsPrec p       = readParen False
                            (\r -> [((x,y), w) | ("(",s) <- lex r,
                                                 (x,t)   <- reads s,
                                                 (",",u) <- lex t,
                                                 (y,v)   <- reads u,
                                                 (")",w) <- lex v ] )

-- Other tuples have similar Read and Show instances



