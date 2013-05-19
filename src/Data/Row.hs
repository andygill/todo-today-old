{-# LANGUAGE GADTs, KindSignatures #-}

module Data.Row
        ( Row(..)
        , Schema(..)
        , field
        ) where

import Data.Default
import Data.Char

data Field :: * -> * where
  Field :: (Read f, Show f) => String -> (r -> f) -> (f -> r -> r) -> Field r

field :: (Read f, Show f) => String -> (r -> f) -> (f -> r -> r) -> Field r
field = Field

newtype Row d = Row d

class Default d => Schema d where
  fields :: [Field d]

instance Schema d => Show (Row d) where
  show (Row d) = unlines [ nm ++ ": " ++ show (proj d)
                         | Field nm proj _ <- fields
                         ]

instance Schema d => Read (Row d) where
  readsPrec i (ch:str) | isSpace ch = readsPrec i str
  readsPrec i str =
          case span isAlphaNum str of
            (nm,':':rest) -> case [ fld | fld@(Field nm' _ _) <- fields, nm == nm' ] of
                [Field _ _ inj] -> case reads rest of
                     [(f,rest')] -> case readsPrec i rest' of
                       [(Row a,rest'')] -> [(Row $ inj f a,rest'')]
                       _ -> []
                     _ -> []
                _ -> [] --- fail (nm ++ ": field not found")
            _ -> [(Row def,str)]

{-
data Foo = Foo { num :: Int, boo :: Bool, str ::  String}
        deriving (Show, Read)

instance Default Foo where
   def = Foo 1 True "Hello"

instance Schema Foo where
  fields = [ Field "num" num $ \ f r -> r { num = f }
           , Field "boo" boo $ \ f r -> r { boo = f }
           , Field "str" str $ \ f r -> r { str = f }
           ]
-}
