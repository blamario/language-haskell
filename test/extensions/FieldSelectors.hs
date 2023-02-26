{-# LANGUAGE FieldSelectors #-}

data Foo = MkFoo { foo :: Int, bar :: Int, baz :: Foo }
bar = ()  -- does not conflict with `bar` field
baz = bar -- unambiguously refers to `bar` the unit value, not the field

f = foo -- disallowed
