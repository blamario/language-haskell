{-# LANGUAGE TraditionalRecordSyntax #-}
{-# LANGUAGE GADTSyntax #-}

data Foo = Foo {unFoo :: Int}

data Bar where
  Bar :: {unBar :: Int} -> Bar

f :: Foo -> Bar
f Foo{unFoo = n} = Bar{unBar = n}
