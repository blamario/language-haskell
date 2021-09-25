
{-# LANGUAGE ParallelListComprehensions #-}

module Foo where

foo = [ ()
      | () <- foo
      | () <- foo
      ]

