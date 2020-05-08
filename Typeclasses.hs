module Typeclasses where

data Trivial = Trivial'

instance Eq Trivial where
  Trivial' == Trivial' = True

data DayOfWeek = Mon | Tue | Weds | Thu | Fri | Sat | Sun deriving (Ord, Show, Enum)

instance Eq DayOfWeek where
  Mon == Mon   = True
  Tue == Tue   = True
  Weds == Weds = True
  Thu == Thu   = True
  Fri == Fri   = True
  Sat == Sat   = True
  Sun == Sun   = True
  _ == _       = False

data Date = Date DayOfWeek Int

instance Eq Date where
  (==) (Date weekday  num)
       (Date weekday' num') = weekday == weekday' && num == num'

-- Example of data constructor with a polimorfic argument

data Identity a = Identity a

-- Here we have to ensure that a implements Eq

instance Eq a => Eq (Identity a) where
  (==) (Identity v) (Identity v') = v == v'

-- Practice start

data TisAnInteger = TisAn Integer

instance Eq TisAnInteger where
  (==) (TisAn x) (TisAn y) = x == y

data TwoIntegers = Two Integer Integer

instance Eq TwoIntegers where
  (==) (Two x y) (Two x' y') = x == x' && y == y'

data StringOrInt = TisAnInt Int | TisAString String

instance Eq StringOrInt where
  (==) (TisAnInt x) (TisAnInt y)     = x == y
  (==) (TisAString x) (TisAString y) = x == y
  (==) _ _ = False

data Pair a = Pair a a

instance Eq a => Eq (Pair a) where
  (==) (Pair x y) (Pair x' y') = x == x' && y == y'

data Tuple a b = Tuple a b

instance (Eq a, Eq b) => Eq (Tuple a b) where
  (==) (Tuple x y) (Tuple x' y') = x == x' && y == y'

data Which a = ThisOne a | ThatOne a

instance Eq a => Eq (Which a) where
  (==) (ThisOne x) (ThisOne y) = x == y
  (==) (ThatOne x) (ThatOne y) = x == y
  (==) _ _ = False

data EitherOr a b = Hello a | Goodbye b

instance (Eq a, Eq b) => Eq (EitherOr a b) where
  (==) (Hello x) (Hello y) = x == y
  (==) (Goodbye x) (Goodbye y) = x == y
  (==) _ _ = False

-- Practice end

-- Ord

-- Enum
-- see enumAdd

-- Show

data Mood = Good

instance Show Mood where
  show Good = "Goody"

-- Read
-- Don't ever use this

-- Writing typeclasses

class Numerish a where
  fromNumber :: Integer -> a
  toNumber :: a -> Integer
  defaultNumber :: a

data Age = Age Integer deriving Show

instance Numerish Age where
  fromNumber n = Age n
  toNumber (Age n) = n
  defaultNumber = Age 18

-- This will work
a = defaultNumber :: Age