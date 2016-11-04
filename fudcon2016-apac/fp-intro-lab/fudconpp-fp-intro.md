% Lab: Introduction to Functional Programming
% Jens Petersen (juhp)
  <petersen@fedoraproject.org>
% FUDCon APAC at Phnom Penh 2016 Nov 5

## Functional Programming Lab with Jens Petersen @juhp

Please install _`hugs98`_ or _`ghc`_ in your Linux distro.

Or download from:

- <https://www.haskell.org/hugs/> (small, old)

- <https://www.haskell.org/ghc/> (big, new)

Alternatively: <http://tryhaskell.org/> (online limited cli)


# Learning functional programming makes you a better programmer!

# Learning Haskell makes you an better functional programmer!! ;-)

<div class="notes">
Not possibly to teach FP in 1 hour

but want to give a taste
</div>

## Requisites

Today we will use hugs98 or ghc for some parts.

## Install hugs
Hugs is old but small!

    sudo apt install hugs

or

    sudo dnf install hugs98

Installers for Windows and Mac

## Alternatives

if you can't install hugs or ghc:

http://tryhaskell.org/

- <https://www.schoolofhaskell.com/> (hs)

## Slides

<https://petersen.fedorapeople.org/talks/fudcon-apac-2016/>

generated with pandoc and reveal.js

from <http://github.com/juhp/presentations/>

# Why Functional Programming?

- Pure functions compose
- Referential transparency
- Immutable predictability

# Haskell?

## Haskell is a

- Functional
- Pure
- Lazy
- *Statically typed*

programming language.

## Functional

- functions are first-class
- evaluation of expressions rather than\
  executing instructions\
  (functional vs imperative)

## Pure

- variables and data structures are immutable
- expressions do not have “side effects”
- calling the same function with same args gives same result every time: *deterministic*

⇓

- Equational reasoning
- Easy refactoring
- Parallelism

## Lazy
- evaluation by need

⇓

- can easily define control structures
- allows handling of infinite data structures

## Statically typed

- Every Haskell expression has a type
- Types are type-checked at compile-time
- Programs with type errors or mismatches\
  will not compile

## Haskell 25 years old
designed by a committee of CS academics\
in the late 80's

to create a standard lazy\
functional programming language

![](https://wiki.haskell.org/wikiupload/4/4a/HaskellLogoStyPreview-1.png "Haskell Logo")

# Haskell uses indentation heavily like Python

## Haskell

- Glasgow Haskell Compiler
- Cabal packaging library
  - like Python's pip
- Hackage
  - source repository of open source Haskell packages
- cabal-install
  - package manager for Hackage packages

# Haskell Basics

## Basic types

`::` = "has type"


```haskell
True :: Bool

1 :: Int                 -- machine integers
2^2^2^2^2 -1 :: Integer  -- arbitrary precision

'a' :: Char
"Hello!" :: String

1.0 :: Double

() :: ()
```

## Functions

```haskell
add :: Integer -> Integer -> Integer
add m n = m + n

add 3 2 ⟹ 5 :: Integer

-- partial application
add 5 :: Integer -> Integer
```

## Functions vs operators
prefix vs infix

```haskell
2 + 4 ⟹ 6
-- prefix
(+) 2 4 ⟹ 6

div 4 2 ⟹ 2
-- infix
4 `div` 2 ⟹ 2
```

# Lists

## Lists
`[a]` is a list of elements of type `a`

The type variable `a` is an example of polymorphism.

## List examples
```haskell
-- empty list
[] :: [a]

[True, False, True] :: [Bool]

[1..5] :: [Int]

["Haskell", "is", "good"] :: [String]

```

Note `['a', 1, True]` is not a valid Haskell list!

## List append

```haskell
(++) :: [a] -> [a] -> [a]

[1, 2] ++ [3, 4] ⟹ [1, 2, 3, 4]
```

## Polymorphic list functions
```haskell
head :: [a] -> a
tail :: [a] -> [a]
length :: [a] -> Integer
```

```
head [1, 2, 3] ⟹ 1
tail [1, 2, 3] ⟹ [2, 3]
length [1, 2, 3] ⟹ 3

length $ show $ 2^2^2^2^2 -1 ⟹ 19729
```

## Cons constructor
```
a : as

1 : [2, 3] ⟹ [1, 2, 3]
```

## map

```haskell
map :: (a -> b) -> [a] -> [b]

map (add 3) [1, 2, 3] ⟹ [4, 5, 6]
```

# Laziness

## Laziness
Lazy evaluation allows infinite lists:

```haskell
naturals = [0..]
```

Fibonacci series

```haskell
fib :: Int -> Int
-- naive definition
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)
```

# Let and with

## let vs with

```haskell
let x = 2 in
  x*x + 3*x + 1

sumsquare x y = square x + square y
  where
  square u = u * u
```

# Comprehensions

## Comprehensions
```haskell
squares = [ x * x | x <- [0..] ]

-- "nested 'for' loop"
pairs = [(x,y) | x <- [1..10], y <- [1..10]]
```

Python's comprehensions come from Haskell

## Primes example

```haskell
primes = filterPrime [2..]
  where filterPrime (p:xs) =
          p : filterPrime [x | x <- xs, x `mod` p /= 0]
```

<div align="right">
<small>(from <http://haskell.org>)</small>
</div>

# Tuples

## Tuples
- `(a, b)` is a pair
- `(a, b ,c)` is a triple
- etc

```haskell
("Life", 42) :: (String, Int)
(False, 'a', 1.1) :: (Bool, Char, Float)
```

# Pattern matching

## Pattern matching
```haskell
take :: Int -> [a] -> [a]
take  0     _           =  []
take  _     []          =  []
take  n     (x:xs)      =  x : take (n-1) xs
```

## Case statement

```haskell
data Arch = X86 | X86_64 | ARMv7

case arch of
   X86 -> "/usr/lib"
   X86_64 -> "/usr/lib64"
   otherwise -> "/usr/lib"
```

# Types

## Static type safety

vs

- Python backtraces
- Java NullPointerException

## Type inference

type annotations are generally optional

## Data types
```haskell
data Maybe a = Just a | Nothing

data Bool = False | True

-- record types
data Point = Pt Float Float
pt = Pt 1.0 2.0

-- recursive datatype
data Tree a = Leaf a | Branch (Tree a) (Tree a)
```

# Monads and IO

## IO Monad
```haskell
putStrLn :: String -> IO ()

getLine :: IO String
```

### do blocks
```haskell
do
let greet = "Hello"
name <- getLine
putStrLn $ greet ++ " Your name is " ++ name
```


# Projects

- pandoc: markup conversion tool
- xmonad: X tiling Window Manager
- shake: build system library to replace Make
- hledger: accounting
- git-annex:  manage files across systems with git without checking them in
- Agda and idris: dependently-typed programming languages
- hakyll: static website generator
- yi: extensible customizable editor (like Emacs)
- turtle: shell programming

## Web frameworks
- [Yesod](http://www.yesodweb.com/)
- Snap
- Happstack
- Scotty

- Warp webserver can\
["provide performance on a par with nginx"](http://www.aosabook.org/en/posa/warp.html)\
and "three times faster than Node".


# Fedora Haskell

## Fedora Haskell SIG
- <https://fedoraproject.org/wiki/Haskell_SIG>

- IRC: \#fedora-haskell
- <haskell@lists.fedoraproject.org>

## Fedora Haskell Packaging
- \> 300 source packages
- cabal-rpm packaging tool
- copr repos

# Resources

## Resources

- <http://haskell.org>
- <http://haskell-lang.org>
- <http://haskellbook.com>
- <https://www.schoolofhaskell.com/>

- Other langs
  - https://maryrosecook.com/blog/post/a-practical-introduction-to-functional-programming (py)
  - http://underscorejs.org/ (js)


Contact: petersen@fedoraproject.org (@juhp)
